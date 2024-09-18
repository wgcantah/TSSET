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
tsset <- function(df, start = c(1, 1), frequency = "daily") {
  # Define frequency values based on the type of series
  freq_values <- list(
    daily = 365,       # daily data
    weekly = 52,       # weekly data
    monthly = 12,      # monthly data
    quarterly = 4,     # quarterly data
    annual = 1         # annual data
  )

  # Get the frequency number for the given frequency type
  freq <- freq_values[[frequency]]

  if (is.null(freq)) {
    stop("Invalid frequency type. Use 'daily', 'weekly', 'monthly', 'quarterly', or 'annual'.")
  }

  # Convert numeric columns to time series based on the frequency and start
  df_ts <- df
  df_ts[sapply(df_ts, is.numeric)] <- lapply(df_ts[sapply(df_ts, is.numeric)], function(col) {
    ts(col, start = start, frequency = freq)
  })

  return(df_ts)
}
