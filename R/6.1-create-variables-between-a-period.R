#' Calculate the number of instances for each bin of a climatic variable within a period
#' @name func_calc_bins_period
#' @description This function calculates the number of instances for each bin of a climatic variable within a period.
#' Note: the psu vars need to have the same name in both data.tables and both need to be the same data type.
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
devtools::install_github("axdey/climExposuR")
#' @name func_calc_descr_period
#' @title Calculate various descriptive statistics for a climatic variable from a time series dataframe within a period for a corresponding health data
#' @description This function calculates various descriptive statistics for a climatic variable from a time series dataframe within a period for a corresponding health data.
#' Note: the psu vars need to have the same name in both data.tables and both need to be the same data type.
#' @param df_lt_clim A data.table containing the long-term climatic data
#' @param df_health A data.table containing the health data
#' @param start_date_var A character string specifying the column name of the start date
#' @param add_days An integer specifying the number of days to add to the start date
#' @param subtract_days An integer specifying the number of days to subtract from the start date
#' @param psu_var A character string specifying the column name of the PSU
#' @param sum A logical indicating whether to calculate the sum (default: TRUE), Can be set to FALSE to skip the calculation
#' @param count_non_zero A logical indicating whether to calculate the count of non-zero values (default: TRUE), Can be set to FALSE to skip the calculation
#' @param max A logical indicating whether to calculate the max (default: TRUE), Can be set to FALSE to skip the calculation
#' @param count_d A logical indicating whether to calculate the count of a specific value (default: FALSE), Can be set to TRUE to calculate the count of a specific value
#' @param d A numeric value specifying the value to count. Must only be provided if count_d is TRUE.
#' @param vec_identifiers A character vector containing the identifiers of the columns to calculate the sum, count, and max
#' @examples
#'  \dontrun{
#' vec_identifiers <- c("consec")
#' df_test <- func_calc_descr_period(
#'     df_lt_clim = df_lt_vars_2014, 
#'     df_health = df_anc_3mo, 
#'     start_date_var = "dob", 
#'     add_days = NULL,
#'     subtract_days = 7, 
#'     psu_var = "psu",
#'     vec_identifiers = vec_identifiers,
#'     sum = TRUE,
#'     count_non_zero = TRUE,
#'     max = FALSE
#'     cound_d = TRUE,
#'     d = 5)
#' }
#' @importFrom data.table setDT := .I .EACHI
#' @export

func_calc_descr_period <- function(
    df_lt_clim, 
    df_health, 
    start_date_var, 
    add_days, subtract_days, 
    psu_var,
    sum = TRUE,
    count_non_zero = TRUE,
    max = TRUE,
    count_d = FALSE,
    d = NULL,
    vec_identifiers) {
    
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
    
    # Identify columns of interest
    pattern <- paste(vec_identifiers, collapse = "|")
    cols_of_interest <- grep(pattern, names(df_lt_clim), value = TRUE)
    
    # Ensure consistent types in df_lt_clim
    df_lt_clim[, (cols_of_interest) := lapply(.SD, as.numeric), .SDcols = cols_of_interest]
    
    # Initialize result as NULL
    result <- NULL
    
    # Calculate the operations for each individual
    calculations <- list()
    
    ## Sum
    if (sum) {
        calculations$sum <- df_lt_clim[df_health, 
            c(lapply(.SD, function(x) {
                s <- sum(x, na.rm = TRUE)
                if (is.finite(s)) s else NA_real_
            }),
            .(row_id = row_id)),
            by = .EACHI, 
            on = .(psu, date >= start_date, date <= end_date),
            .SDcols = cols_of_interest
        ][, !"date"]
        setnames(calculations$sum, cols_of_interest, paste0("sum_", cols_of_interest))
    }
    
    ## Max
    if (max) {
        calculations$max <- df_lt_clim[df_health, 
            c(lapply(.SD, function(x) {
                mx <- max(x, na.rm = TRUE)
                if (is.finite(mx)) mx else NA_real_
            }),
            .(row_id = row_id)),
            by = .EACHI, 
            on = .(psu, date >= start_date, date <= end_date),
            .SDcols = cols_of_interest
        ][, !"date"]
        setnames(calculations$max, cols_of_interest, paste0("max_", cols_of_interest))
    }

    ## Count non-zero
    if (count_non_zero) {
        calculations$count <- df_lt_clim[df_health, 
            c(lapply(.SD, function(x) {
                count <- sum(x > 0, na.rm = TRUE)
                if (is.finite(count)) as.integer(count) else NA_integer_
            }),
            .(row_id = row_id)),
            by = .EACHI, 
            on = .(psu, date >= start_date, date <= end_date),
            .SDcols = cols_of_interest
        ][, !"date"]
        setnames(calculations$count, cols_of_interest, paste0("count_", cols_of_interest))
    }

    ## Count = x
    if (count_d) {
        if (is.null(d) || !is.numeric(d)) {
            stop("value of d must be provided and must be numeric")
        }
        calculations$count_d <- df_lt_clim[df_health, 
            c(lapply(.SD, function(x) {
                count_d <- sum(x == d, na.rm = TRUE)
                if (is.finite(count_d)) as.integer(count_d) else NA_integer_
            }),
            .(row_id = row_id)),
            by = .EACHI, 
            on = .(psu, date >= start_date, date <= end_date),
            .SDcols = cols_of_interest
        ][, !"date"]
        setnames(calculations$count_d, cols_of_interest, paste0("count_d_", cols_of_interest))
    }
    
    # Combine all results
    result <- Reduce(function(x, y) merge(x, y, by = "row_id", all = TRUE), calculations)
    
    # Merge result back to the target dataframe
    if (!is.null(result)) {
        df_health[result, names(result) := mget(paste0("i.", names(result))), on = "row_id"]
    }
    df_health[, row_id := NULL]
    
    return(df_health)
}
