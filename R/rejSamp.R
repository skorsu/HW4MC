rejSamp <- function(h, n, alp, a, b, df, showSamp = FALSE){
  
  ## Perform a rejection sampling
  samp <- rsRcpp(n, alp, a, b, df)
  
  ## Calculate the estimate
  est <- mean(h(samp))
  
  if(showSamp == FALSE){
    return(est)
  } else {
    return(list(est = est, samp = samp))
  }
  
}

PhRoMethod <- function(h, n, alp, a, b, df){
  
  ## Perform a rejection sampling
  samp <- rsRcpp(n, alp, a, b, df)
  
  sort_samp <- sort(samp)
  qSort <- tg_dist(sort_samp, a, b)
  hSort <- h(sort_samp)
  
  sum((sort_samp[2:n] - sort_samp[1:(n-1)]) * hSort[1:(n-1)] * qSort[1:(n-1)])/
    sum((sort_samp[2:n] - sort_samp[1:(n-1)]) * qSort[1:(n-1)])
  
}
