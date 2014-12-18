# A funtion for computing human capital
#library(checkpoint)
#checkpoint("2014-11-13")

#' A Human Capital Estimation Function
#'
#' This function allows you to compute the human capital value.
#' @param
#' @keywords Human Capital Estimation
#' @export
#' @examples
#' hum_cap()

library("quantreg")

hum_cap <- function(data, FUN, formula, tau = 0.5, weights = NULL){
  data$.weights <- weights
  if(is.null(weights)){
    data_m <-filter(data, sex == "Male")
    data_f <-filter(data, sex == "Female")
    est_m <- FUN(data = data_m, formula = formula, tau = tau)
    est_f <- FUN(data = data_f, formula = formula, tau = tau)
    intercept_m = est_m$coef[["(Intercept)"]]
    intercept_f = est_f$coef[["(Intercept)"]]
    zeroWorker_m <- exp(intercept_m)
    zeroWorker_f <- exp(intercept_f)
    hc.prem_m <- mean(exp(est_m[["model"]][[1]])-zeroWorker_m)
    hc.prem_f <- mean(exp(est_f[["model"]][[1]])-zeroWorker_f)
    hc.prem   <-mean(c(exp(est_m[["model"]][[1]])-zeroWorker_m,
                           exp(est_f[["model"]][[1]])-zeroWorker_f))
  }
  else {
    data_m <-filter(data, sex == "Male")
    est_m <- FUN(data = data_m, formula = formula, tau = tau, weights = .weights)
    intercept_m = est_m$coef[["(Intercept)"]]
    zeroWorker_m <- exp(intercept_m)
    hc.prem_m <- sum((exp(est_m[["model"]][[1]])-zeroWorker_m)*est_m[["model"]][["(weights)"]])/ sum(est_m[["model"]][["(weights)"]])

    data_f <-filter(data, sex == "Female")
    est_f <- FUN(data = data_f, formula = formula, tau = tau, weights = .weights)
    intercept_f = est_f$coef[["(Intercept)"]]
    zeroWorker_f <- exp(intercept_f)
    hc.prem_f <- sum((exp(est_f[["model"]][[1]])-zeroWorker_f)*est_f[["model"]][["(weights)"]])/ sum(est_f[["model"]][["(weights)"]])

    hc.prem   <- (sum((exp(est_m[["model"]][[1]])-zeroWorker_m)*est_m[["model"]][["(weights)"]]) +
                    sum((exp(est_f[["model"]][[1]])-zeroWorker_f)*est_f[["model"]][["(weights)"]]))/
      (sum(est_m[["model"]][["(weights)"]])+ sum(est_f[["model"]][["(weights)"]]))
 }

  out <- list(hc.prem = c(male = hc.prem_m, female = hc.prem_f, all = hc.prem),
              zeroWorker = c(male = zeroWorker_m, female = zeroWorker_f),
              intercept = c(male = intercept_m, female = intercept_f),
              coef = list(male = est_m$coef, female = est_f$coef),
              reg = list(male = summary(est_m),female = summary(est_f)))
}


#' A Muti-Period Human Capital Estimation Function
#'
#' This function allows you to compute the human capital value for each state, each year
#' @param
#' @keywords Human Capital Estimation
#' @export
#' @examples
#' mult_hum_cap()

mult_hum_cap <- function(data, FUN, formula, tau = 0.5, weights = NULL, begin , end ) {
  data = data
  beg = begin
  end = end
  l = 0
  k = 0
  # create matrices to collect the estimates
  results <- matrix(NA, ncol = end-(beg-1),
                    nrow = length(levels(data$state)))
  results_f <- results_m <- results
  # run the HC function for all states, from "begin" time to "end" time
  for(j in levels(data$state)){
    l = l +1
    for(i in beg:end){
      k = k+1
      mydata <- filter(data, state == j, year == i)
      mod <- hum_cap(data = mydata, formula = formula, tau = tau, FUN)
      results_m[l,k] <- mod$hc.prem["male"]
      results_f[l,k] <- mod$hc.prem["female"]
      results[l,k]   <- mod$hc.prem["all"]
      #print(mod$hc.prem); print(c(l,k))
    }
    k<-0
  }
  rownames(results) <- levels(data$state)
  rownames(results_m) <- levels(data$state)
  rownames(results_f) <- levels(data$state)
  colnames(results) <- as.character(seq(beg, end, by = 1))
  colnames(results_m) <- as.character(seq(beg, end, by = 1))
  colnames(results_f) <- as.character(seq(beg, end, by = 1))
  out <- list(All = results, Male = results_m, Female = results_f)
return(out)
}


