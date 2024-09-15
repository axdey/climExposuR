#' @name func_extract_clim_data_shp
#' @title Extract climate data for a shape file
#' @description This function extracts climate data for a shape file. 
#' The function reads the NetCDF files, extracts the climate data for each area of interest, and compiles the mean climate for each day. 
#' The function assumes that the climate data is daily and works only for regions like Zip codes, states, or countries.
#' @param path_nic_files Path to the NetCDF files
#' @param sf_file Shape file
#' @param sf_file_admin Administrative variable in the shape file
#' @param reference_date Reference date for the climate data
#' @param file_frequency Frequency of the climate data
#' @param start_index Start index of the NetCDF files
#' @param end_index End index of the NetCDF files
#' @return A data frame with the extracted climate data
#' @importFrom terra vect rast ext rotate crop extract
#' @importFrom sf st_transform
#' @importFrom ncdf4 nc_open ncvar_get ncatt_get nc_close
#' @importFrom lubridate as.Date dseconds dminutes dhours
#' @importFrom dplyr select
#' @export

func_extract_clim_data_shp <- function(path_nic_files, 
                                       sf_file, 
                                       sf_file_admin,
                                       reference_date = "1900-01-01", 
                                       file_frequency = "daily",
                                       start_index = 1,
                                       end_index = NULL) {
  # Step-1: Process sf file and create a SpatVector object
  sf_file_wgs84 <- st_transform(sf_file, crs = 4326)
  sf_file_sv <- vect(sf_file_wgs84)
  
  # Step-2: Get the list of nic files
  clim_files <- list.files(path = path_nic_files, pattern = "\\.nc$", full.names = TRUE)
  if (is.null(end_index)) end_index <- length(clim_files)
  clim_files <- clim_files[start_index:end_index]

  # Step-3: Initialize an empty list to store the results
  results_list <- list()

  # Function to process a single NC file
  process_nc_file <- function(nc_file_path) {
    clim_data_rast <- rast(nc_file_path)
    if ((ext(clim_data_rast)$xmin > ext(sf_file_sv[1,])$xmin) | (ext(clim_data_rast)$xmax < ext(sf_file_sv[1,])$xmax)) {
      clim_data_rast <- rotate(clim_data_rast)
    }
    
    file_results <- list()
    
    for (i in 1:nrow(sf_file_sv)) {
      area_attribute <- sf_file_sv[[sf_file_admin]][,1][i]
      area_clim_data_rast <- crop(clim_data_rast, sf_file_sv[i, ])
      area_mean_temp <- terra::extract(area_clim_data_rast, sf_file_sv[i, ], fun = mean, na.rm = TRUE)
      clim_daily_mean <- sapply(area_mean_temp[-1], mean, na.rm = TRUE)

      # Get dates
      terra_rast <- terra::rast(nc_file_path)
      if (!is.na(terra::time(terra_rast[[1]]))) {
        nc_data <- nc_open(nc_file_path)
        time_var <- ncvar_get(nc_data, "time")
        time_units <- ncatt_get(nc_data, "time", "units")$value
        ref_date_str <- sub(".*since ", "", time_units)
        unit <- strsplit(time_units, " ")[[1]][1]
        
        ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
        actual_dates <- switch(unit,
          "days" = ref_date + ddays(as.numeric(time_var)),
          "seconds" = ref_date + dseconds(as.numeric(time_var)),
          "minutes" = ref_date + dminutes(as.numeric(time_var)),
          "hours" = ref_date + dhours(as.numeric(time_var)),
          stop("Time units not recognized or supported")
        )
        nc_close(nc_data)
      } else if (!is.na(names(terra_rast[[1]])) && !is.null(reference_date) && !is.null(file_frequency)) {
        rast_filename <- names(terra_rast)
        rast_filename_num <- as.numeric(gsub("[^0-9]", "", rast_filename))
        actual_dates <- as.Date(reference_date) + rast_filename_num
      } else {
        stop("Time variable not found in the nc file")
      }
      
      if (length(actual_dates) != length(clim_daily_mean)) {
        warning("Mismatch between number of dates and data points. Using sequence.")
        actual_dates <- seq(min(actual_dates), by = "day", length.out = length(clim_daily_mean))
      }

      file_results[[i]] <- data.frame(
        date = actual_dates,
        Attribute = area_attribute,
        clim_daily_mean = clim_daily_mean
      )
    }
    
    return(do.call(rbind, file_results))
  }

  # Process all NC files
  results_list <- lapply(clim_files, process_nc_file)
  
  # Combine all results
  results_df <- do.call(rbind, results_list)
  
  return(results_df)
}