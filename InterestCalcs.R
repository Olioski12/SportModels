library(dplyr)

##optimising reward saver

month_return <- function(start,rate){
  bal = start
  months = 0
  while(bal<100000){
    bal = bal*(rate/100+1)
    months = months +1
  }
  months = months + 1
  gains = bal - start
  mr = gains/months
  return(mr)
}

starts <- seq(50000,99000,1000)

mr <- c()
for(i in 1:length(starts)){
  mr <- c(mr,month_return(starts[i],20))
}

plot(starts,mr)

starts[which(mr==max(mr))]