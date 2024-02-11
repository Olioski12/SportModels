source(here::here('DLSFunctions.R'))
library(dplyr)


overs <- 50
t1score <- 250 + ceiling(runif(1)*100)

#Average run rate method

t1rr <- t1score/overs

t2o <- ceiling(runif(1)*20)
t2w <- ceiling(runif(1)*8)
t2o
t2w

arrcalc <- ARR(t1rr,t2o)

Zruns(t2o,t2w)