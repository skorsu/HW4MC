impSamp <- function(h, n, a, b, df, showCalc = FALSE){
  
  ## Generate the result from Rcpp: Importance Sampling
  impResult <- isRcpp(n, a, b, df)
  colnames(impResult) <- c("x", "fx", "qx", "W", "normW")
  
  ## Estimate the integral (or the expectation)
  est <- sum(h(impResult[, "x"]) * impResult[, "normW"])
  
  if(showCalc == FALSE){
    return(est)
  } else {
    return(list(est = est, calc = as.data.frame(impResult)))
  }
  
}

SIRSamp <- function(h, n, M, a, b, df){
  
  ## Generate the result from Rcpp: Importance Sampling
  impResult <- isRcpp(n, a, b, df)
  colnames(impResult) <- c("x", "fx", "qx", "W", "normW")
  
  ## Resampling Process
  samIndex <- sample(impResult[, "x"], M, replace = TRUE, prob = impResult[, "normW"])
  est <- mean(h(samIndex))
  
  est
  
}
