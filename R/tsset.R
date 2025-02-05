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
tsset <- function(df, start, frequency) {
  # Keep only numeric columns from the input data frame.
  numeric_cols <- sapply(df, is.numeric)
  df_numeric <- df[, numeric_cols, drop = FALSE]
  
  # Convert the numeric data frame to a multivariate time series.
  ts_data <- ts(df_numeric, start = start, frequency = frequency)
  
  # Determine the number of observations.
  n <- nrow(df_numeric)
  
  # Generate a proper Date sequence based on the frequency.
  if (frequency == 12) {
    # For monthly data: assume start[2] is the month (1-12).
    start_date <- as.Date(sprintf("%d-%02d-01", start[1], start[2]))
    date_seq <- seq.Date(from = start_date, by = "month", length.out = n)
  } else if (frequency == 4) {
    # For quarterly data: assume start[2] is the quarter (1-4).
    # The first month of the quarter is: (quarter - 1) * 3 + 1.
    start_month <- (start[2] - 1) * 3 + 1
    start_date <- as.Date(sprintf("%d-%02d-01", start[1], start_month))
    date_seq <- seq.Date(from = start_date, by = "quarter", length.out = n)
  } else if (frequency == 1) {
    # For annual data: use January 1st of the starting year.
    start_date <- as.Date(sprintf("%d-01-01", start[1]))
    date_seq <- seq.Date(from = start_date, by = "year", length.out = n)
  } else {
    warning("Frequency not specifically handled; returning numeric time index.")
    date_seq <- time(ts_data)
  }
  
  # Create and return a new data frame that includes the date variable and the time series data.
  result_df <- data.frame(time = date_seq, as.data.frame(ts_data))
  return(result_df)
}
