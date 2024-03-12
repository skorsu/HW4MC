library(tidyverse)
library(Rcpp)
library(RcppArmadillo)
library(devtools)
library(foreach)
library(doParallel)
library(Dowd)

path <- "/Users/kevin-imac/Desktop/Github - Repo/"
if(! file.exists(path)){
  path <- "/Users/kevinkvp/Desktop/Github Repo/"
}

# sourceCpp(paste0(path, "HW4MC/src/main.cpp"))
uninstall()
compileAttributes()
build()
install()
library(HW4MC)

### User-defined functions
meanSD <- function(x, dplace = 5){
  mm <- round(mean(x), digits = dplace)
  ss <- round(sd(x), digits = dplace)
  paste0(mm, " (SD = ", ss, ")")
}

### Target distribution: f(x)
fx <- function(x){
  exp((-abs(x)^3)/3)
}

plot(fx(seq(-5, 5, 0.01)), col = "black", type = "l")
lines(dt(seq(-5, 5, 0.01), df = 3)/0.25, col = "red", type = 'l')

set.seed(1)
est1 <- rep(NA, 10)
for(i in 1:10){
  est1[i] <- impSamp(function(x){x^2}, 10000)
}

meanSD(est1)

SIRSamp(function(x){x^2}, 100, 10000)
hist(rsRcpp(n = 10000, alp = 0.25))
rejSamp(function(x){x^2}, n = 10000, alp = 0.1, showSamp = FALSE)

tt <- rejSamp(function(x){x^2}, n = 10, alp = 0.1, showSamp = TRUE)

tt$samp[2:10] - tt$samp[1:9]
sort(tt$samp)

PhRoMethod(h = function(x){x^2}, n = 10, alp = 0.1)

test <- impSampling(10000)
sum((test[, 1]^2) * test[, 5])

test$w <- test[, 3]/test[, 2]

plot(test[, 1], test[, 2], type = "p")
plot(test[, 1], test[, 3], type = "p")

x <- 5
eval("x^2")
