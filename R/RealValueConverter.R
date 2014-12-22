
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
  beg = begin
  end1 = end
  select <-as.data.frame(seq(beg,end1,by=1))
  names(select) <-"Year"
  names(cpi) <- "CPI"
  init <- beg-1989
  fin  <- end1+1 - begin
  cpi_data <- data.frame(select, cpi[init:fin])
  cpi_data$coef<-cpi_data$cpi/index

      realvalues <- df # Initiate the real values dataframe
      for(i in 1:51){
        realvalues[i, ] <- df[i, ]/cpi_data$coef
      }
      return(realvalues)
    }

