
#' A function to convert nominal values to real values
#'
#' This function convert the estimated human capital premium into real values
#' @param df is the dataframe of estimated human capital premium
#' @param cpi is the vector of cpi index. The default is the cpi index of 1990 to 2013
#' @param index is the base year index. The default is 232.957 (cpi 2013 index)
#' @param begin and end takes the begining year, and the end year. 1990 is the default value for begin, and 2013 is the default value for end.
#' @keywords Human Capital Estimation
#' @export
#' @examples
#' RealValueConverter()
#load("data/cpi_Ind.RData")
RealValueConverter <- function(df, cpi = cpi_Ind, index=232.957, begin = 1990, end = 2013){
  beg = begin; end = end
  select <-as.data.frame(seq(beg,end,by=1))
  names(select) <-"Year"
  names(cpi) <- "CPI"
  cpi_data <- data.frame(select, cpi)
  cpi_data$coef<-cpi_data$cpi/index

  realvalues <- df # Initiate the real values dataframe
  for(i in 1:51){
    realvalues[i, ] <- df[i, ]/cpi_data$coef
  }
  return(realvalues)
}



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
