#' @name func_extract_clim_data_polygon
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
#' @examples see the example usage below
#' @importFrom terra vect rast ext rotate crop extract
#' @importFrom sf st_transform
#' @importFrom ncdf4 nc_open ncvar_get ncatt_get nc_close
#' @importFrom lubridate dseconds dminutes dhours
#' @importFrom dplyr select
#' @export

func_extract_clim_data_polygon <- function(path_clim_files, 
                                 sf_file, 
                                 sf_file_admin,
                                 embedded_date_type = "actual",
                                 reference_date = "1900-01-01",
                                 start_index = 1,
                                 end_index = NULL,
                                 crs = 4326) {

  # Step-1: Process sf file and create a SpatVector object
  sf_file_wgs84 <- st_transform(sf_file, crs = crs)
  sf_file_sv <- vect(sf_file_wgs84)
  
  # Step-2: Get the list of clim files
  clim_files <- list.files(path = path_clim_files, pattern = "\\.nc$|\\.tif$", full.names = TRUE)
  if (is.null(end_index)) end_index <- length(clim_files)
  gridded_clim_files <- clim_files[start_index:end_index]
  
  # Step-3: Function to extract climate data from a single file
  process_clim_file <- function(grid_clim_file_cur) {
    # Read the NC file as a SpatRaster
    clim_data_rast <- rast(grid_clim_file_cur)
    # compare the extent of the raster and the sf file and rotate the raster if necessary
    if ((ext(clim_data_rast)$xmin > ext(sf_file_sv[1,])$xmin) | (ext(clim_data_rast)$xmax < ext(sf_file_sv[1,])$xmax)) {
      clim_data_rast <- rotate(clim_data_rast)
    }
    results_df <- data.frame()
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
      
      # Bind the dataset and order variables
      results_df <- rbind(results_df, clim_df)
      results_df <- results_df |> select(Attribute, clim_daily_mean)
    }
    return(results_df)
  }

  # Step-4: Function to extract dates from a single file
  process_dates <- function(grid_clim_file_cur) {
    tryCatch({
      file_type <- tools::file_ext(grid_clim_file_cur)
      terra_rast <- terra::rast(grid_clim_file_cur)
      terra_time <- terra::time(terra_rast)
      
      if (file_type == "nc" && !is.na(terra_time[1])) {
        nc_data <- nc_open(grid_clim_file_cur)
        time_var <- ncvar_get(nc_data, "time")
        time_units <- ncatt_get(nc_data, "time", "units")$value
        ref_date_str <- sub(".*since ", "", time_units)
        unit <- strsplit(time_units, " ")[[1]][1]
        
        ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
        actual_dates <- switch(unit,
          "days" = ref_date + days(time_var),
          "seconds" = ref_date + seconds(time_var),
          "minutes" = ref_date + minutes(time_var),
          "hours" = ref_date + hours(time_var),
          stop("Time units not recognized or supported")
        )
        nc_close(nc_data)
      } else if (embedded_date_type == "actual") {
        rast_filename <- names(terra_rast)
        rast_filename_num <- as.numeric(gsub("[^0-9]", "", rast_filename))
        actual_dates <- as.Date(as.character(rast_filename_num), format = "%Y%m%d")
      } else if (embedded_date_type == "cdc") {
        rast_filename <- names(terra_rast)
        rast_filename_num <- as.numeric(gsub("[^0-9]", "", rast_filename))
        actual_dates <- as.Date(reference_date) + rast_filename_num
      } else {
        stop("Unsupported file type or embedded_date_type")
      }
      
      if (length(actual_dates) != nlyr(terra_rast)) {
        warning("Mismatch between number of dates and raster layers. Using sequence.")
        actual_dates <- seq(min(actual_dates), by = "day", length.out = nlyr(terra_rast))
      }
      
      return(actual_dates)
    }, error = function(e) {
      warning(paste("Error processing dates for file:", grid_clim_file_cur, "\nError:", e$message))
      return(NULL)
    })
  }

  # Process all files
  results_list <- lapply(gridded_clim_files, function(file_cur) {
    clim_data <- process_clim_file(file_cur)
    dates <- process_dates(file_cur)
    
    if (!is.null(clim_data) && !is.null(dates)) {
      clim_data$date <- rep(dates, each = nrow(sf_file_sv))
      return(clim_data)
    } else {
      return(NULL)
    }
  })
  
  # Combine results and return
  results_df <- do.call(rbind, results_list)
  return(results_df)
}

# Example usage
## Paths
# path_tmax_noaa <- "~/Google Drive/Shared drives/Benmarhnia Lab/Arnab/common-datasets/climate-datasets/world-temp-drybulb-max/1979-2023/"
# path_tmax_gridmet <- "~/Google Drive/Shared drives/Benmarhnia Lab/Arnab/common-datasets/climate-datasets/gridMET/daily-data-US/t-max/"
# path_utci <- "~/Google Drive/Shared drives/Benmarhnia Lab/Arnab/project-datasets/manuscripts/clim-papers/heat/nola-heat-violence/raw-data/utci_daily_Lousiana_2011_2023/"

## Identify files
# clim_files_tif <- list.files(path = path_utci, pattern = "\\.tif$", full.names = TRUE) # tif files with actual dates embedded
# clim_files_gridmet <- list.files(path = path_tmax_gridmet, pattern = "\\.nc$", full.names = TRUE) # nc files with cdc dates embedded
# clim_files_noaa <- list.files(path = path_tmax_noaa, pattern = "\\.nc$", full.names = TRUE) # nc files with timevar in the nc file

## Identify single files
# grid_clim_file_cur_tif <- clim_files_tif[1]
# grid_clim_file_cur_gridmet <- clim_files_gridmet[1]
# grid_clim_file_cur_noaa <- clim_files_noaa[1]

## Rasterize 
# rast_tif <- terra::rast(grid_clim_file_cur_tif)
# rast_gridmet <- terra::rast(grid_clim_file_cur_gridmet)
# rast_noaa <- terra::rast(grid_clim_file_cur_noaa)

## Test the date function
# date_tif <- process_dates(grid_clim_file_cur_tif, embedded_date_type = "actual", reference_date = NULL)
# date_gridmet <- process_dates(grid_clim_file_cur_gridmet, embedded_date_type = "cdc", reference_date = "1900-01-01")
# date_noaa <- process_dates(grid_clim_file_cur_noaa, embedded_date_type = NULL, reference_date = NULL)

## Test the climate data function
# clim_data_tif <- process_clim_file(grid_clim_file_cur_tif)
# clim_data_gridmet <- process_clim_file(grid_clim_file_cur_gridmet)
# clim_data_noaa <- process_clim_file(grid_clim_file_cur_noaa)

## Test the main function
# zctas_nola_70 <- tigris::zctas(starts_with = c("70"))
# sf_file_wgs84 <- st_transform(zctas_nola_70, crs = 4326)
# sf_file_sv <- vect(sf_file_wgs84)
# sf_file_admin <- "Zip"
# start_index <- 1
# end_index <- 3
# crs <- 4326
# df_nola_zip_temp <- process_climate_data(path_clim_files = path_utci, 
#                                  sf_file = zctas_nola_nopd, 
#                                  sf_file_admin = "Zip",
#                                  embedded_date_type = "actual",
#                                  reference_date = NULL,
#                                  start_index = 1,
#                                  end_index = NULL,
#                                  crs = 4326)