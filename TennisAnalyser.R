library(dplyr)
load("uq.RData")
x = 1:300


d <- data.frame(x,uq)  ## need to use data in a data.frame for predict()
logEstimate <- lm(uq~log(x),data=d)

curve(-102.8+59*log(x),add=TRUE,col=2)
