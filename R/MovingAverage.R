
#' A moving average calculator
#'
#' This function smooths the data (three year moving average).
#' @param df dataframe
#' @keywords moving average
#' @export
#' @examples
#' smoother3()
smoother3 <- function(df){
  smoothed_data <- df
  cols <- ncol(smoothed_data)
  for(i in 2:(cols-1)){
    smoothed_data[,i] <- rowMeans(df[,(i-1):(i+1)])
  }
  smooth_d <- smoothed_data[, -c(1,cols)]
  return(smooth_d)
}


#' A time series converter
#'
#' This function format the data into time series format.
#' @param df dataframe
#' @keywords Time Series
#' @export
#' @examples
#' ts_convert()
ts_convert <-function(df, begin = 1990, end = 2013){
  smooth_data <- smoother3(df)
  time_series  <- as.data.frame (t(smooth_data))
  time_series$Year <- seq(from=(begin + 1), to = (end-1), by = 1)
  return(time_series)
}
