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
tsset <-  function(df, start, frequency) {
  # Keep only the numeric columns in the data frame
  numeric_cols <- sapply(df, is.numeric)
  df_numeric <- df[, numeric_cols, drop = FALSE]
  
  # Convert the numeric data frame to a multivariate time series.
  ts_data <- ts(df_numeric, start = start, frequency = frequency)
  
  # Extract the fractional time index from the time series object.
  time_index <- time(ts_data)
  
  # Convert the time index to a human-friendly date format based on frequency.
  if (frequency == 12) {
    # For monthly data: use zoo::as.yearmon then convert to Date (defaults to the first day of the month)
    regular_time <- as.Date(zoo::as.yearmon(time_index))
  } else if (frequency == 4) {
    # For quarterly data: use zoo::as.yearqtr then convert to Date (defaults to the first day of the quarter)
    regular_time <- as.Date(zoo::as.yearqtr(time_index))
  } else {
    # For other frequencies, issue a warning and return the numeric time index.
    warning("Frequency not specifically handled; returning numeric time index.")
    regular_time <- time_index
  }
  
  # Create and return a new data frame that includes the converted time variable along with the numeric data.
  result_df <- data.frame(time = regular_time, as.data.frame(ts_data))
  return(result_df)
}
