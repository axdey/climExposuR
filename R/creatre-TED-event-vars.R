#' @name ted_events
#' @title Calculate temperature exceedance duration events
#' @param df A data.table object with columns: date, tmax, extreme_event, and exceedance.
#' @param var_raw The name of the raw variable (default: "tmax").
#' @param var_cutoff The name of the variable indicating the extreme event (default: "extreme_event").
#' @param var_exceed The name of the variable indicating the exceedance (default: "exceedance").
#' @return A data.table object with summarized extreme events.
#' @importFrom data.table setDT :=
#' @export

ted_events <- function(df,
                       var_raw = "tmax",
                       var_cutoff = "extreme_event",
                       var_exceed = "exceedance") {
  
    # Check if data.table is loaded
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("Please install and load the 'data.table' package.")
    }
  
    # Ensure the input is a data.table
    if (!inherits(df, "data.table")) {
        setDT(df)
    }
  
    # Order by date
    df <- df[order(date)]
    
    # Step-1: Create var for consecutive days above the threshold
    # Correctly use the variable name for dynamic column reference
    df[, consec_days_above_thresh := ifelse(get(var_cutoff) == 1, seq_len(.N), 0L), by = .(rleid(get(var_cutoff)))]
  
    # Step-2: Summarize extreme events by start and end dates, using dynamic variable names where necessary
    df[, run_id := as.integer(factor(rleid(consec_days_above_thresh != 0)))]
  
    # Step-3: Summarize to get start_date, end_date, and TED vars.
    df_summary <- df[consec_days_above_thresh > 0, 
                     .(start_date = min(date), 
                       end_date = max(date), 
                       duration = .N, 
                       peak_tot = max(get(var_raw)), 
                       peak_exceed = max(get(var_exceed)),
                       magnitude_tot = sum(get(var_raw)),
                       magnitude_exceed = sum(get(var_exceed)))
                    , by = run_id]

    # Step-4: Create variable for days since last event
    df_summary[, days_since_last_event := c(NA, diff(start_date))]

    # Step-5: Drop the run_id column
    df_summary[, run_id := NULL]

    return(df_summary)
}