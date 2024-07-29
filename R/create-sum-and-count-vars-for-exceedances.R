#' @name func_ident_mag_calc_mag
#' @title Calculate the sum and count of exceedances for each individual
#' @description This function takes a time series and identifies days that exceed a threshold. It also calculates the magnitude of exceedances for each day.
#' @param df A data.table object with columns: date, tmax, extreme_event, and exceedance.
#' @param vec_threshold_abs A vector of absolute thresholds. Can also be NULL.
#' @param vec_varnames_perc A vector of percentile thresholds. Can also be NULL.
#' @param clim_var The name of the climate variable.
#' @return A data.table object with sum and count variables for exceedances.
#' @import data.table dplyr lubridate
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


#' @name func_calc_sum_count_exceed_period
#' @title Calculate the sum and count of exceedances for each individual for a specific period
#' @description This function calculates the sum and count of exceedances for each individual for a specific period
#' @param df_input A data.table object with columns: date, tmax, extreme_event, and exceedance.
#' @param df_target A data.table object with columns: start_date_var, psu_var, add_days, and subtract_days.
#' @param start_date_var The name of the start date variable.
#' @param add_days The number of days to add to the start date. Cant be NULL if subtract_days is NULL.
#' @param subtract_days The number of days to subtract from the start date. Cant be NULL if add_days is NULL.
#' @param psu_var The name of the PSU variable.
#' @return A data.table object with sum and count variables for exceedances.
#' @import data.table
#' @export

func_calc_sum_count_exceed_period <- function(df_input, df_target, start_date_var, add_days, subtract_days, psu_var) {
    # Ensure that the input and target dataframes are data.table
    setDT(df_input)
    setDT(df_target)

    # Ensure that the start_date_var is a Date object and create row_id and psu columns
    df_target[, `:=`(
        start_date = as.Date(get(start_date_var)),
        row_id = .I,
        psu = get(psu_var)
    )]
    
    # Calculate the end date of the period
    if (!is.null(add_days)) {
        df_target[,  start_date := as.Date(get(start_date_var))]
        df_target[,  end_date := start_date + add_days]
    } else if (!is.null(subtract_days)) {
        df_target[, end_date := as.Date(get(start_date_var))]
        df_target[, start_date := end_date - subtract_days]
    } else {
        stop("Either add_days or subtract_days must be provided")
    }

    # Identify 'exceed' and 'count' columns
    cols_ident_or_mag <- grep("^mag|^ident", names(df_input), value = TRUE)

    # Calculate the sum for each individual
    result <- df_input[df_target, 
        c(lapply(.SD, sum, na.rm = TRUE),
          .(row_id = row_id)),
        by = .EACHI, 
        on = .(psu, date >= start_date, date <= end_date),
        .SDcols = cols_ident_or_mag
    ][, !"date"]

    # Rename the sum columns
    setnames(result, cols_ident_or_mag, paste0("sum_", cols_ident_or_mag))

    # Merge result back to the target dataframe
    df_target[result, names(result) := mget(paste0("i.", names(result))), on = "row_id"]
    df_target[, row_id := NULL]
    
    return(df_target)
}

