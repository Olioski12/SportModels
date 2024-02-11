library(dplyr)

ARR <- function(t1rr,t2o){
  target <- ceiling(t1rr*t2o + 1)
  return(target)
  
}

##MPO

Z0 <- function(w){
  a <- 300-30*w
  return(a)
}

B0 <- function(w){
  
  return(1)
}

Zruns <- function(o,w){
  
  Zruns <- Z0(w)*(1-exp(-B0(w)*o))
  return(Zruns)
}


ResourceApprox <- function(o,w){
  
}
