#' @title Create heatwave variables
#' @description This function creates heatwave variables based on the specified threshold values and consecutive days. 
#' Note that threshold values need to be passed as variables, which makes this function more flexible to be used with absolute as well as relative threshold values.
#' @param df A data.table object containing the dataset.
#' @param threshold_vars A character vector containing the names of the threshold variables.
#' @param consec_days A numeric vector containing the number of consecutive days to define a heatwave.
#' @param date_var A character string specifying the name of the date variable.
#' @param clim_var A character string specifying the name of the climate variable.
#' @param psu_var A character string specifying the name of the PSU variable.
#' @return A data.table object with heatwave variables.
#' @examples
#' \dontrun{
#' df_lt_vars_2014 <- read_fst(here(path_project, "processed-data", "1.5.1-df_psu_wbgt_2014.fst"), as.data.table = T)
#' Note: This dataset is from my dissertation project dataset and is not available for public use.
#' df_lt_vars_2014$cutoff_abs_28 <- 28
#' df_lt_vars_2014$cutoff_abs_30 <- 30
#' df_lt_vars_2014$cutoff_abs_32 <- 32
#' vec_threshold_vars <- c("cutoff_abs_28", "cutoff_abs_30", "cutoff_abs_32")
#' vec_consec_days <- c(2, 3, 4, 5)
#' df_wbgt_hw_vars <- create_heatwave_vars(df_lt_vars_2014, vec_threshold_vars, vec_consec_days, date_var = "date", clim_var = "max_temp_wb", psu_var = "psu")
#' @export create_heatwave_vars
#' @importFrom data.table setDT setkeyv rleid

create_heatwave_vars <- function(df, threshold_vars, consec_days, date_var, clim_var, psu_var) {
  setDT(df)
  
  # Order the dataset by psu_var and date_var
  setkeyv(df, c(psu_var, date_var))
  
  for (var in threshold_vars) {
    # Extract threshold value from the data.table column
    threshold_col <- df[[var]]
    if (is.null(threshold_col)) {
      stop(paste("Column", var, "not found in the data.table"))
    }
    
    # Create hotday variable
    hotday_var <- paste0("hotday_", var)
    df[, (hotday_var) := ifelse(get(clim_var) >= threshold_col, 1, 0)]
    
    # Create consecutive days variable, resetting for each PSU
    consec_days_var <- paste0("consec_days_", var)
    df[, (consec_days_var) := ifelse(get(hotday_var) == 1, 
                                     seq_len(.N), 
                                     0L), 
       by = .(get(psu_var), rleid(get(hotday_var)))]
    
    # Create heatwave variables for specified consecutive days
    for (days in consec_days) {
      hw_var <- paste0("hw_", var, "_", days, "d")
      df[, (hw_var) := ifelse(get(consec_days_var) >= days, 1, 0)]
    }
  }
  
  return(df)
}