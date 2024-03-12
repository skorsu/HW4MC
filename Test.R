library(tidyverse)
library(Rcpp)
library(RcppArmadillo)
library(foreach)
library(doParallel)
library(Deriv)

path <- "/Users/kevin-imac/Desktop/Github - Repo/"
if(! file.exists(path)){
  path <- "/Users/kevinkvp/Desktop/Github Repo/"
}

sourceCpp(paste0(path, "HW4MC/src/main.cpp"))

### User-defined functions
meanSD <- function(x, dplace = 5){
  mm <- round(mean(x), digits = dplace)
  ss <- round(sd(x), digits = dplace)
  paste0(mm, " (SD = ", ss, ")")
}


