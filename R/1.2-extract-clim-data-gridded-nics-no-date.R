#' @name func_extract_clim_data_shp_nodate
#' @title Extract climate data for a shape file when the NetCDF files do not have date information
#' @description This function extracts climate data for a shape file, when the NetCDF files do not have date information.
#' The function reads the NetCDF files, extracts the climate data for each area of interest, and compiles the mean climate for each day. 
#' The function assumes that the climate data is daily and works only for regions like Zip codes, states, or countries.
#' @param path_nic_files Path to the NetCDF files
#' @param sf_file Shape file
#' @param sf_file_admin Administrative variable in the shape file
#' @param reference_date A character string specifying the reference date for the NetCDF files
#' @param file_frequency A character string specifying the frequency of the NetCDF files
#' @return A data frame with the extracted climate data
#' @importFrom terra vect rast ext rotate crop extract
#' @importFrom sf st_transform
#' @importFrom ncdf4 nc_open ncvar_get ncatt_get nc_close
#' @importFrom lubridate as.Date dseconds dminutes dhours
#' @importFrom dplyr select
#' @export

func_extract_clim_data_shp_nodate <- function(path_nic_files, 
                                       sf_file, 
                                       sf_file_admin, 
                                       reference_date = "1900-01-01", 
                                       file_frequency = "daily") {
  # Step-1: Process sf file and create a SpatVector object
  sf_file_wgs84 <- st_transform(sf_file, crs = 4326)
  sf_file_sv <- vect(sf_file_wgs84)
  
  # Step-2: Get the list of nic files
  clim_files <- list.files(path = path_nic_files, pattern = "\\.nc$", full.names = TRUE)
  
  clim_files <- clim_files[44:45] # Use this if you want to process only a subset of files 

  # Step-3: Initialize an empty data frame to store the results
  results_df <- data.frame()
  
  # Function to process a single NC file
  process_nc_file <- function(nc_file_path) {
    # Read the NC file as a SpatRaster
    clim_data_rast <- rast(nc_file_path)
    
    # Compare the extent of the raster and the sf file and rotate the raster if necessary
    if ((ext(clim_data_rast)$xmin > ext(sf_file_sv[1,])$xmin) | (ext(clim_data_rast)$xmax < ext(sf_file_sv[1,])$xmax)) {
      clim_data_rast <- rotate(clim_data_rast)
    }
    
    for (i in 1:nrow(sf_file_sv)) {
      # Extract the user-defined attribute
      area_attribute <- sf_file_sv[[sf_file_admin]][,1][i]
      
      # Extract the area of interest from the climate data
      area_clim_data_rast <- crop(clim_data_rast, sf_file_sv[i, ])
      area_mean_temp <- terra::extract(area_clim_data_rast, sf_file_sv[i, ], fun = mean, na.rm = TRUE)
      
      # Compile the mean climate for each day
      clim_daily_mean <- sapply(area_mean_temp, mean, na.rm = TRUE)
      clim_daily_mean <- clim_daily_mean[-1]  # Drop the first column which is the ID
      
      # Convert to a data frame and add necessary columns
      clim_df <- as.data.frame(clim_daily_mean)
      clim_df$Attribute <- area_attribute
      
      # Extract date from filename
      rast_filename <- names(clim_data_rast)
      rast_filename_num <- as.numeric(gsub("[^0-9]", "", rast_filename))
      rast_date <- as.Date(reference_date) + rast_filename_num
      
      # Add the date to the results data frame
      clim_df$date <- rast_date
      
      # Bind the dataset and order variables
      results_df <- rbind(results_df, clim_df)
      results_df <- results_df |> select(date, Attribute, clim_daily_mean)
    }
    return(results_df)
  }
  
  # Process all NC files
  for (nc_file in clim_files) {
    results_df <- rbind(results_df, process_nc_file(nc_file))
  }
  
  return(results_df)
}