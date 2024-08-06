#' Calculate the number of instances for each bin of a climatic variable within a period
#' @name func_calc_bins_period
#' @description This function calculates the number of instances for each bin of a climatic variable within a period.
#' It takes as input a regular time series dataset with a climatic variable and a health dataset with a start date and calculates the number of instances for each bin of the climatic variable within a period.
#' @param df_health A data.table containing the health data
#' @param df_lt_clim A data.table containing the long-term climatic data
#' @param start_date_var A character string specifying the column name of the start date
#' @param add_days An integer specifying the number of days to add to the start date
#' @param subtract_days An integer specifying the number of days to subtract from the start date
#' @param psu_var A character string specifying the column name of the PSU
#' @param clim_var A character string specifying the column name of the climatic variable
#' @return A data.table containing the health data with the number of instances for each bin
#' @importFrom data.table setDT := .I .EACHI
#' @importFrom lubridate as.Date 
#' @export

func_calc_bins_period <- function(df_health, df_lt_clim,
                                  start_date_var, add_days, subtract_days, 
                                  psu_var = "psu", clim_var = "max_temp_wb") {
    # Ensure that the input and target dataframes are data.table
    setDT(df_lt_clim)
    setDT(df_health)
    
    # Ensure that the start_date_var is a Date object and create row_id and psu columns
    df_health[, `:=`(
        start_date = as.Date(get(start_date_var)),
        row_id = .I,
        psu = get(psu_var)
    )]
    
    # Calculate the end date of the period
    if (!is.null(add_days)) {
        df_health[, start_date := as.Date(get(start_date_var))]
        df_health[, end_date := start_date + add_days]
    } else if (!is.null(subtract_days)) {
        df_health[, end_date := as.Date(get(start_date_var))]
        df_health[, start_date := end_date - subtract_days]
    } else {
        stop("Either add_days or subtract_days must be provided")
    }
    
    # Calculate the number of instances for each bin
    result <- df_lt_clim[df_health, 
        .(
            bin_below_10 = sum(get(clim_var) < 10, na.rm = TRUE),
            bin_10_20 = sum(get(clim_var) >= 10 & get(clim_var) < 20, na.rm = TRUE),
            bin_20_30 = sum(get(clim_var) >= 20 & get(clim_var) < 30, na.rm = TRUE),
            bin_above_30 = sum(get(clim_var) >= 30, na.rm = TRUE)
        ),
        by = .EACHI,
        on = .(psu, date >= start_date, date <= end_date)
    ]
    
    # Merge result back to the target dataframe
    df_health <- cbind(df_health, result[, .(bin_below_10, bin_10_20, bin_20_30, bin_above_30)])
    
    return(df_health)
}

#' @name func_calc_sum_count_exceed_period
#' @title Calculate the sum and count of exceedances for each individual for a specific period
#' @description This function calculates the sum and count of exceedances for each individual for a specific period. 
#' It takes the outputs of the func_ident_mag_calc_mag function and calculates the sum and count of exceedances for each individual for a specific period.
#' @param df_lt_proc_exceed A data.table object with columns: date, tmax, extreme_event, and exceedance.
#' @param df_health A data.table object with columns: start_date_var, psu_var, add_days, and subtract_days.
#' @param start_date_var The name of the start date variable.
#' @param add_days The number of days to add to the start date. Cant be NULL if subtract_days is NULL.
#' @param subtract_days The number of days to subtract from the start date. Cant be NULL if add_days is NULL.
#' @param psu_var The name of the PSU variable.
#' @return A data.table object with sum and count variables for exceedances.
#' @import data.table
#' @importFrom data.table setDT := .I .EACHI
#' @importFrom lubridate as.Date 
#' @export

func_calc_sum_count_exceed_period <- function(df_lt_proc_exceed, df_health, start_date_var, add_days, subtract_days, psu_var) {
    # Ensure that the input and target dataframes are data.table
    setDT(df_lt_proc_exceed)
    setDT(df_health)

    # Ensure that the start_date_var is a Date object and create row_id and psu columns
    df_health[, `:=`(
        start_date = as.Date(get(start_date_var)),
        row_id = .I,
        psu = get(psu_var)
    )]
    
    # Calculate the end date of the period
    if (!is.null(add_days)) {
        df_health[,  start_date := as.Date(get(start_date_var))]
        df_health[,  end_date := start_date + add_days]
    } else if (!is.null(subtract_days)) {
        df_health[, end_date := as.Date(get(start_date_var))]
        df_health[, start_date := end_date - subtract_days]
    } else {
        stop("Either add_days or subtract_days must be provided")
    }

    # Identify 'exceed' and 'count' columns
    cols_ident_or_mag <- grep("^mag|^ident", names(df_lt_proc_exceed), value = TRUE)

    # Calculate the sum for each individual
    result <- df_lt_proc_exceed[df_health, 
        c(lapply(.SD, sum, na.rm = TRUE),
          .(row_id = row_id)),
        by = .EACHI, 
        on = .(psu, date >= start_date, date <= end_date),
        .SDcols = cols_ident_or_mag
    ][, !"date"]

    # Rename the sum columns
    setnames(result, cols_ident_or_mag, paste0("sum_", cols_ident_or_mag))

    # Merge result back to the target dataframe
    df_health[result, names(result) := mget(paste0("i.", names(result))), on = "row_id"]
    df_health[, row_id := NULL]
    
    return(df_health)
}

