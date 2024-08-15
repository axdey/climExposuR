#' @name func_extract_clim_data_shp
#' @title Extract climate data for a shape file
#' @description This function extracts climate data for a shape file. 
#' The function reads the NetCDF files, extracts the climate data for each area of interest, and compiles the mean climate for each day. 
#' The function assumes that the climate data is daily and works only for regions like Zip codes, states, or countries.
#' @param path_nic_files Path to the NetCDF files
#' @param sf_file Shape file
#' @param sf_file_admin Administrative variable in the shape file
#' @return A data frame with the extracted climate data
#' @examples see below
#' @importFrom terra vect rast ext rotate crop extract
#' @importFrom sf st_transform
#' @importFrom ncdf4 nc_open ncvar_get ncatt_get nc_close
#' @importFrom lubridate as.Date dseconds dminutes dhours
#' @importFrom dplyr select
#' @export

func_extract_clim_data_shp <- function(path_nic_files, 
                                       sf_file, 
                                       sf_file_admin) {
  # Step-1: Process sf file and create a SpatVector object
  ## Convert CRS
  sf_file_wgs84 <- st_transform(sf_file, crs = 4326)
  ## Convert sf object to SpatVector
  sf_file_sv <- vect(sf_file_wgs84)
  
  # Step-2: Get the list of nic files
  clim_files <- list.files(path = path_nic_files, pattern = "\\.nc$", full.names = TRUE)

  # Step-3: Initialize an empty data frame to store the results
  results_df <- data.frame()

  # Function to process a single NC file
  process_nc_file <- function(nc_file_path) {
    # Read the NC file as a SpatRaster
    clim_data_rast <- rast(nc_file_path)
    # compare the extent of the raster and the sf file and rotate the raster if necessary
    if ((ext(clim_data_rast)$xmin > ext(sf_file_sv[1,])$xmin) | (ext(clim_data_rast)$xmax < ext(sf_file_sv[1,])$xmax)) {
      clim_data_rast <- rotate(clim_data_rast)
    }
    
    for (i in 1:nrow(sf_file_sv)) {
      # Extract the user-defined attribute
      area_attribute <- sf_file_sv[[sf_file_admin]][,1][i]
      # Extract the area of interest from the climate data
      ext(clim_data_rast)
      ext(sf_file_sv[i, ])
      area_clim_data_rast <- crop(clim_data_rast, sf_file_sv[i, ])
      area_mean_temp <- terra::extract(area_clim_data_rast, sf_file_sv[i, ], fun = mean, na.rm = TRUE)

      # Assuming climate data is daily, compile the mean climate for each day
      clim_daily_mean <- sapply(area_mean_temp, mean, na.rm = TRUE)
      # Drop the first column which is the ID
      clim_daily_mean <- clim_daily_mean[-1]

      # Convert to a data frame and add necessary columns
      clim_df <- as.data.frame(clim_daily_mean)
      clim_df$Attribute <- area_attribute
      
      # Get start and end dates from the nc file
      nc_data <- nc_open(nc_file_path)
      time_var <- ncvar_get(nc_data, "time")
      time_units <- ncatt_get(nc_data, "time", "units")$value
      ref_date_str <- sub(".*since ", "", time_units)
      unit <- strsplit(time_units, " ")[[1]][1]
      
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
      
      start_date <- min(actual_dates) 
      end_date <- max(actual_dates)
      nc_close(nc_data)

      dates <- seq(as.Date(start_date), as.Date(end_date), by="days")

      # Add the dates to the results data frame
      clim_df$date <- dates

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

# Example
# pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
# pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)
# library(rnaturalearthdata)
# rm(list = ls())

# # Step-0: Point to folders that contain the raw WBGT and precip data ----
# path_processed <- here("2-data", "2.2-processed-data")
# here_tmax_wb_raw <- "Z:/Shared drives/Benmarhnia Lab/Arnab/common-datasets/climate-datasets/world-temp-wetbulb-max/test"


# # head(df_dhs_psu_geo_sf)

# # Step-2: load-function to extract climate data to DHS PSUs ----
# source(here("1-scripts", "dummy.R"))

# # Import India Shapefile
# path_india_state <- "Z:/Shared drives/Benmarhnia Lab/Arnab/common-datasets/gis-datasets/nfhs-shape-files-from-abhishek/dist-shp-file-701/"
# india_state_sf <- read_sf(here(path_india_state, "N5_state.shp"))


# # Step-3: Run the function to extract climate data for each PSU ----
# df_wbgt <- func_extract_clim_data_shp(path_nic_files = here_tmax_wb_raw, 
#                                       sf_file = india_state_sf, 
#                                       sf_file_admin = "STATE_NAME")


