#' Title Time Series Set
#'
#' @param df A data frame. The data set that the function will operate on.
#' @param start A numeric value. The starting point or index
#' @param frequency A numeric value. The frequency of observations (e.g., for time series).
#'
#' @return The output of the function, such as a data frame, vector, or plot.
#' @export
#'
#'
tsset <-function(df, frequency = 1, start = c(1, 1)) {
  # Ensure the input is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe.")
  }
  
  # Extract numerical columns
  numerical_cols <- sapply(df, is.numeric)
  
  # Check if there are any numerical columns
  if (sum(numerical_cols) == 0) {
    stop("No numerical variables found in the dataframe.")
  }
  
  # Convert numerical columns to time series
  ts_list <- lapply(df[numerical_cols], function(x) ts(x, frequency = frequency, start = start))
  
  # Combine the time series into a dataframe
  ts_df <- as.data.frame(ts_list)
  
  # Create a time variable in decimal format
  time_var <- time(ts(1:nrow(df), frequency = frequency, start = start))
  
  # Convert decimal time to proper date format
  if (frequency == 4) {
    # Quarterly data
    dates <- sapply(time_var, function(t) {
      year <- floor(t)
      quarter <- (t - year) * 4 + 1
      month <- (quarter - 1) * 3 + 1
      as.Date(paste(year, month, "01", sep = "-"))
    })
  } else if (frequency == 12) {
    # Monthly data
    dates <- sapply(time_var, function(t) {
      year <- floor(t)
      month <- round((t - year) * 12) + 1
      as.Date(paste(year, month, "01", sep = "-"))
    })
  } else if (frequency == 1) {
    # Yearly data
    dates <- sapply(time_var, function(t) {
      year <- floor(t)
      as.Date(paste(year, "01", "01", sep = "-"))
    })
  } else {
    stop("Unsupported frequency. Please use 1 (yearly), 4 (quarterly), or 12 (monthly).")
  }
  
  # Add the date variable to the dataframe
  ts_df$time <- as.Date(dates, origin = "1970-01-01")
  
  return(ts_df)
}

