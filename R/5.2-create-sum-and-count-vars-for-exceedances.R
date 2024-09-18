#' @name func_ident_mag_calc_mag
#' @title Calculate the sum and count of exceedances for each individual
#' @description This function takes a time series and identifies days that exceed a threshold. It also calculates the magnitude of exceedances for each day.
#' @param df A data.table object with columns: date, tmax, extreme_event, and exceedance.
#' @param vec_threshold_abs A vector of absolute thresholds. Can also be NULL.
#' @param vec_varnames_perc A vector of percentile thresholds. Can also be NULL.
#' @param clim_var The name of the climate variable.
#' @return A data.table object with sum and count variables for exceedances.
#' @importFrom data.table is.data.table setDT :=
#' @export
func_ident_mag_calc_mag <- function(df, vec_threshold_abs, vec_varnames_perc, clim_var) {
  # Convert dataframes to data.tables if they aren't already 
  !is.data.table(df) && setDT(df)
    
  # Ensure that the date column is a Date object
  df[, date := as.Date(date)]

  # Ensure that one of vec_threshold_abs and vec_varnames_perc are non-NULL
  if (is.null(vec_threshold_abs) && is.null(vec_varnames_perc)) {
    stop("At least one of vec_threshold_abs and vec_varnames_perc must be non-NULL")
  }

  # Identify exceedances and calculate magnitude for each threshold 
  ## Based on absolute values if vec_threshold_abs is not NULL
  if (!is.null(vec_threshold_abs)) {
    for (threshold in vec_threshold_abs) {
      ident_col <- paste0("ident_abs_", threshold)
      mag_col <- paste0("mag_abs_", threshold)
      df[, c(ident_col, mag_col) := .(
        fifelse(get(clim_var) >= threshold, 1, 0),
        fifelse(get(clim_var) >= threshold, get(clim_var) - threshold, 0)
      )]
    }
  }  
  ## Based on percentile values if vec_varnames_perc is not NULL
  if (!is.null(vec_varnames_perc)) {
    for (var in vec_varnames_perc) {
      threshold_name <- sub("cutoff_", "", var)
      ident_col <- paste0("ident_perc_", threshold_name)
      mag_col <- paste0("mag_perc_", threshold_name)
      df[, c(ident_col, mag_col) := .(
        fifelse(get(clim_var) >= get(var), 1, 0),
        fifelse(get(clim_var) >= get(var), get(clim_var) - get(var), 0)
      )]
    }
  }
  return(df)
}

