#' @name func_calc_exceed_sum_count
#' @title Calculate the sum and count of exceedances for each individual
#' @description This function calculates the sum and count of exceedances for each individual based on absolute and percentile thresholds.
#' @param df A data.table object with columns: date, tmax, extreme_event, and exceedance.
#' @param vec_threshold_abs A vector of absolute thresholds.
#' @param vec_varnames_perc A vector of percentile thresholds.
#' @param clim_var The name of the climate variable.
#' @return A data.table object with sum and count variables for exceedances.
#' @import data.table dplyr lubridate
#' @export

func_calc_exceed_sum_count <- function(df, vec_threshold_abs, vec_varnames_perc, clim_var) {
  # Convert dataframes to data.tables if they aren't already 
  !is.data.table(df) && setDT(df)
    
  # Calculate exceedances and count crossings for each threshold 
  ## Based on absolute values
  for (threshold in vec_threshold_abs) {
    exceed_col <- paste0("exceed_abs_", threshold)
    count_col <- paste0("count_abs_", threshold)
    df[, c(exceed_col, count_col) := .(
      fifelse(get(clim_var) >= threshold, get(clim_var) - threshold, 0),
      fifelse(get(clim_var) >= threshold, 1, 0)
    )]
  }
  
  ## Based on percentile values
  for (var in vec_varnames_perc) {
    threshold_name <- sub("cutoff_", "", var)
    exceed_col <- paste0("exceed_perc_", threshold_name)
    count_col <- paste0("count_perc_", threshold_name)
    df[, c(exceed_col, count_col) := .(
      fifelse(get(clim_var) >= get(var), get(clim_var) - get(var), 0),
      fifelse(get(clim_var) >= get(var), 1, 0)
    )]
  }
  return(df)
}
#' @name function_calc_sum_count_exceed
#' @title Calculate the sum and count of exceedances for each individual
#' @export

function_calc_sum_count_exceed <- function(df_input, df_target, start_date_var, add_days, subtract_days, psu_var) {
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
    cols_exceed_or_count <- grep("^exceed|^count", names(df_input), value = TRUE)

    # Calculate the sum for each individual
    result <- df_input[df_target, 
        c(lapply(.SD, sum, na.rm = TRUE),
          .(row_id = row_id)),
        by = .EACHI, 
        on = .(psu, date >= start_date, date <= end_date),
        .SDcols = cols_exceed_or_count
    ][, !"date"]

    # Rename the sum columns
    setnames(result, cols_exceed_or_count, paste0("sum_", cols_exceed_or_count))

    # Merge result back to the target dataframe
    df_target[result, names(result) := mget(paste0("i.", names(result))), on = "row_id"]
    df_target[, row_id := NULL]

    # Identify columns starting with "sum"
    sum_cols <- grep("^sum", names(df_target), value = TRUE)

    # Scale the sum columns and create new columns with "_scale" suffix
    df_target[, (paste0(sum_cols, "_scale")) := lapply(.SD, scale), .SDcols = sum_cols]

    return(df_target)
}

