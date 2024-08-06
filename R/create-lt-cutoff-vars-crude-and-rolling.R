#' @name flexi_percentile_cutoffs 
#' @title Calculate the rolling percentile for each date and geographic unit (e.g. Zip/psu)
#' @param DT A data.table or dataframe with columns 'date', variable of interest (e.g. tmax), and the geographic unit (e.g. Zip/PSU)
#' @param var_col The name of the column containing the variable of interest
#' @param ntile The percentile to calculate (default is 0.9)
#' @param perc_type The type of percentile to calculate (default is 'crude'). Can alse be "rolling". If 'rolling', num_days must be specified. Can also be "crude_doy"
#' @param num_days The number of days to consider in the rolling window (default is 3)
#' @param psu_col The name of the column containing the geographic unit (default is 'Zip')
#' @param num_cores_leave The number of cores to leave for other processes (default is 1)
#' @return The input data.table with an additional column 'percentile' containing the rolling/crude percentile
#' @importFrom data.table as.data.table is.data.table data.table
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom lubridate yday
#' @importFrom stats quantile
#' 
#'@examples
#' \dontrun{
#' library(data.table)
#' set.seed(123)
#' dates <- seq(as.Date("1995-01-01"), as.Date("2002-12-31"), by = "day")
#' zips <- c("90210", "10001")
#' DT <- data.table(
#'   date = rep(dates, each = length(zips)),
#'   Zip = rep(zips, times = length(dates)),
#'   tmax = runif(length(dates) * length(zips), 20, 45)
#' )
#' flexi_percentile_cutoffs(DT = DT, var_col = "tmax", ntile = 0.9, perc_type = "crude", psu_col = "Zip")
#' flexi_percentile_cutoffs(DT = DT, var_col = "tmax", ntile = 0.9, perc_type = "rolling", num_days = 3, psu_col = "Zip")
#' }
#'
#' @export


# Function to calculate percentiles for a time series 'flexibly' considering dates and rolling windows
flexi_percentile_cutoffs <- function(DT, var_col = "tmax", 
                              ntile = 0.9, 
                              perc_type = "crude", 
                              num_days = 3, 
                              psu_col = "Zip", 
                              num_cores_leave = 1) {
  # Input validation
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")
  if (!requireNamespace("parallel", quietly = TRUE)) stop("Package 'parallel' is required.")
  if (!requireNamespace("foreach", quietly = TRUE)) stop("Package 'foreach' is required.")
  if (!requireNamespace("doParallel", quietly = TRUE)) stop("Package 'doParallel' is required.")
  
  DT <- data.table::as.data.table(DT)
  if (!data.table::is.data.table(DT)) stop("Failed to convert DT to a data.table")
  
  if (!all(c("date", var_col, psu_col) %in% names(DT))) {
    stop("DT must contain 'date', var_col, and psu_col columns")
  }
  
  if (!(perc_type %in% c("crude", "crude_doy", "rolling"))) stop("perc_type must be either 'crude', 'crude_doy' or 'rolling'")
  if (perc_type == "rolling" && missing(num_days)) stop("num_days must be specified if perc_type is 'rolling'")

  # Prepare data
  DT[, date := as.Date(date)]
  DT[, month_day := format(date, "%m-%d")]
  DT[, doy := lubridate::yday(date)]
  
  # Function to get date window
  get_date_window <- function(date, num_days) {
    seq(date - as.difftime(num_days, unit="days"), date + as.difftime(num_days, unit="days"), by = "day")
  }

  # Get unique combinations of date and PSU
  unique_date_psu <- unique(DT[, .(date, PSU = get(psu_col))])
  
  # Create the percentile column name
  perc_col <- paste0("cutoff_", perc_type, "_", ntile * 100)
  
  # Set up parallel backend
  num_cores <- max(1, parallel::detectCores() - num_cores_leave)
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  # Parallelize the loop
  results <- foreach::foreach(i = 1:nrow(unique_date_psu), .combine = rbind, 
                              .packages = c("data.table")) %dopar% {
    current_date <- unique_date_psu$date[i]
    current_psu <- unique_date_psu$PSU[i]
    current_doy <- lubridate::yday(current_date)
    
    if (perc_type == "rolling") {
      vec_date_window <- get_date_window(current_date, num_days)
      vec_month_day <- format(as.Date(vec_date_window), "%m-%d")
      DT2 <- DT[month_day %in% vec_month_day & get(psu_col) == current_psu & date <= current_date]
    } else if (perc_type == "crude_doy") {
      DT2 <- DT[doy == current_doy & get(psu_col) == current_psu & date <= current_date]
    } else if (perc_type == "crude") {
      DT2 <- DT[get(psu_col) == current_psu & date <= current_date]
    }
    
    if (nrow(DT2) > 0) {
      percentile_value <- quantile(DT2[[var_col]], probs = ntile, na.rm = TRUE)
      new_row <- data.table::data.table(date = current_date, PSU = current_psu)
      new_row[[perc_col]] <- percentile_value
      new_row
    } else {
      NULL
    }
  }
  
  # Stop the cluster
  parallel::stopCluster(cl)
  
  return(results)
}

