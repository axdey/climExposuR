# This function extracts climate data for a given shape file when there is only one NIC file in the folder. 
# The function takes the path to the NetCDF files, the shape file, the name of the attribute in the shape file, and the name of the NetCDF file as arguments. The function processes the shape file, extracts the climate data for each area defined in the shape file, and returns a data frame with the extracted climate data. The function also extracts the start and end dates from the NetCDF file and adds them to the results data frame. The function assumes that the climate data is daily and compiles the mean climate for each day. The function returns a data frame with the extracted climate data, including the dates, the attribute from the shape file, and the mean climate for each day.

func_extract_clim_data_shp <- function(path_nic_files, 
                                        sf_file, 
                                        sf_file_admin,
                                        nic_file_name = NULL) {
        # Step-1: Process sf file and create a SpatVector object
        ## Convert CRS
        sf_file_wgs84 <- st_transform(sf_file, crs = 4326)
        ## Convert sf object to SpatVector
        sf_file_sv <- vect(sf_file_wgs84)

        # Step-2: Extract Clim data for admin units
        ## Get the list of nic files
        clim_files <- list.files(path = path_nic_files, pattern ="\\.nc")
        
        # Step-3: Loop to extract Clim data for admin units
        ## Initialize an empty data frame to store the results
        results_df <- data.frame()

        # Read the NIC file as a SpatRaster
        nic_file <- nic_file_name
        clim_data_rast <- rast(here(path_nic_files, nic_file))
        clim_data_rast <- rotate(clim_data_rast)
        # Loop through each area defined in the SpatVector
        for (i in 1:nrow(sf_file_sv)) {
                # Extract the user-defined attribute
                area_attribute <- sf_file_sv[[sf_file_admin]][,1][i]
                # Extract the area of interest from the climate data
                area_clim_data_rast <- crop(clim_data_rast, sf_file_sv[i, ])
                area_mean_temp <- extract(area_clim_data_rast, sf_file_sv[i, ], fun = mean, na.rm = TRUE)

                # Assuming climate data is daily, compile the mean climate for each day
                clim_daily_mean <- sapply(area_mean_temp, mean, na.rm = TRUE)
                # Drop the first column which is the ID
                clim_daily_mean <- clim_daily_mean[-1]

                # Convert to a data frame and add necessary columns
                clim_df <- as.data.frame(clim_daily_mean)
                clim_df$Attribute <- area_attribute
                
                # Get start and end dates from the nc file
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

                dates <- seq(as.Date(start_date), as.Date(end_date), by=unit)

                # Add the dates to the results data frame
                clim_df$date <- dates

                # Bind the dataset and order variables
                results_df <- rbind(results_df, clim_df)
                results_df <- results_df |> select(date, Attribute, clim_daily_mean)
        }
        return(results_df)
}
