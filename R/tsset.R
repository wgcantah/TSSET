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
  # Check that df is a data frame
  df <- as.data.frame(df)
  
  # Convert the entire data frame into a multivariate time series
  # Note: This requires that all columns in df are numeric.
  ts_data <- ts(df, start = start, frequency = frequency)
  
  # Extract the time index from the time series object.
  # The 'time' function returns a vector of time points that correspond to the rows.
  time_index <- time(ts_data)
  
  # Create a new data frame that includes the time variable along with the original data.
  # We convert ts_data back to a data frame using as.data.frame().
  result_df <- data.frame(time = time_index, as.data.frame(ts_data))
  
  return(result_df)
}

