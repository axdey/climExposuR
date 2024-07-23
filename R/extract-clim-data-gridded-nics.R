#' This function extracts climate data from a nic file for a given shapefile and attribute
#' @param path_nic_files The path to the folder containing the NIC files
#' @param sf_file The shapefile containing the areas of interest. Currently handles sf or SpatVect files. 
#' @param admin_colname The name of the attribute in the shapefile that identifies the region
#' for which the climate variable needs to be extracted (e.g. "Zip"). Use "full_region" if the data needs to be extracted for the entire region. 
#' @export

func_extract_clim_data_shp <- function(path_nic_files, 
                                        shape_file, 
                                        admin_colname = "full_region",
                                        var_name = "tmax_noaa") {
        # Step-0: Detect the shapefile format and convert to SpatVector if it is an sf object
        if (class(shape_file) == "sf") {
                sv_file <- vect(shape_file)
        } else if (class(shape_file) == "SpatVector"){
                sv_file <- shape_file
        }
        # Step-1: Convert CRS to 4326
        sv_file_wgs84 <- project(sv_file, "EPSG:4326")
        
        # Step-2: Extract Clim data for admin units
        ## Get the list of nic files
        clim_files <- list.files(path = path_nic_files, pattern ="\\.nc")
        
        # Step-3: Loop to extract Clim data for admin units
        ## Initialize an empty data frame to store the results
        results_df <- data.frame()

        # Loop through each NIC file
        for (nic_file in clim_files) {
                # Read the NIC file as a SpatRaster
                #   nic_file <- clim_files[1]
                clim_data_rast <- rast(here(path_nic_files, nic_file))
  
                # Determine if the clim var needs to be extracted for the entire region or for a specific area
                if (admin_colname == "full_region") {
                        df_full_region <- data.frame()
                        ## Extract the area of interest from the climate data
                        area_clim_data_rast <- crop(clim_data_rast, sv_file_wgs84)
                        area_mean_clim <- extract(area_clim_data_rast, sv_file_wgs84, fun = mean, na.rm = TRUE)
                        ## Assuming climate data is daily, compile the mean climate for each day
                        clim_daily_mean <- sapply(area_mean_clim, mean, na.rm = TRUE)
                        ## Drop the first column which is the ID
                        clim_daily_mean <- clim_daily_mean[-1]                    
                        ## Convert to a data frame and add necessary columns
                        df_full_region <- as.data.frame(clim_daily_mean)
                        ### Add the clim column name
                        colnames(df_full_region) <- var_name
                } else {
                df_multiple_regions <- data.frame()
                # Loop through each area defined in the SpatVector
                for (i in 1:nrow(sv_file_wgs84)) {
                        # Extract the user-defined attribute
                        region_attribute <- sv_file_wgs84[[admin_colname]][,1][i]
                        # Extract the area of interest from the climate data
                        area_clim_data_rast <- crop(clim_data_rast, sv_file_wgs84[i, ])
                        area_mean_clim <- extract(area_clim_data_rast, sv_file_wgs84[i, ], fun = mean, na.rm = TRUE)

                        # Assuming climate data is daily, compile the mean climate for each day
                        clim_daily_mean <- sapply(area_mean_clim, mean, na.rm = TRUE)
                        # Drop the first column which is the ID
                        clim_daily_mean <- clim_daily_mean[-1]
    
                        # Convert to a data frame and add necessary columns
                        clim_df <- as.data.frame(clim_daily_mean)
                        ## Add the clim column name
                        colnames(clim_df) <- var_name
                        ## Add the region attribute
                        clim_df$region <- region_attribute

                        ## Bind the dataset and order variables
                        df_multiple_regions <- rbind(df_multiple_regions, clim_df)
                }
                }
                # Rbind and create vectors for dates
                if (admin_colname == "full_region") {
                        results_df <- rbind(results_df, df_full_region)
                        vec_date_full <- func_extract_dates_nic(path_nic_files) 
                } else {
                        results_df <- rbind(results_df, df_multiple_regions)
                        vec_date_multiple <- rep(func_extract_dates_nic(path_nic_files), nrow(sv_file_wgs84))
                }
        }
        ## Generate output
        if (admin_colname == "full_region") {
                results_df$dates <- vec_date_full
        } else {
                results_df$dates <- vec_date_multiple
        }
        return(results_df)
}


#' This function extracts dates from the nic files 
#' @param path_nic_files The path to the folder containing the NIC files
#' @return A vector of dates extracted from the NIC files
#' @export

func_extract_dates_nic <- function(path_nic_files) {
    # Step-0: List all the NIC files in the folder
    clim_files <- list.files(path = path_nic_files, pattern ="\\.nc")
    # Step-1: Loop to extract dates from the NIC files
    ## Initialize an empty vector to store the dates
    vec_dates <- c()
    ## Loop through each NIC file
    for (nic_file in clim_files) {
        ## Open the NetCDF file
        nc_data <- nc_open(here(path_nic_files, nic_file))
            ### Get the time variable
            time_var <- ncvar_get(nc_data, "time")
            time_units <- ncatt_get(nc_data, "time", "units")$value
            ### Extract the reference date from the time units
            ref_date_str <- sub(".*since ", "", time_units)
            ### Determine the time unit (days, hours, etc.)
            unit <- strsplit(time_units, " ")[[1]][1]
            ### Convert the reference date string to a Date or POSIXct object
            if(grepl("days", time_units)) {
                    ref_date <- as.Date(ref_date_str)
                    actual_dates <- ref_date + as.numeric(time_var)  # Add time_var as days
                    } else if(grepl("seconds", time_units)) {
                    ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
                    actual_dates <- ref_date + dseconds(as.numeric(time_var))  # Add time_var as seconds
                    } else if(grepl("minutes", time_units)) {
                    ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
                    actual_dates <- ref_date + dminutes(as.numeric(time_var))  # Add time_var as minutes
                    } else if(grepl("hours", time_units)) {
                    ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
                    actual_dates <- ref_date + dhours(as.numeric(time_var))  # Add time_var as hours
                    } else {
                    stop("Time units not recognized or supported")
            }
            ### Get the start and end dates
            start_date <- min(actual_dates) 
            end_date <- max(actual_dates)
            ### Close the NetCDF file when done
            nc_close(nc_data)
            dates <- seq(as.Date(start_date), as.Date(end_date), by="day")
            vec_dates <- c(vec_dates, dates)
            vec_dates <- as.Date(vec_dates)
    }
    return(vec_dates)
}