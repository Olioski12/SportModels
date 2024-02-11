library(dplyr)
brackets <- c(0,19,45,90,200) *1000
rates <- c(0,20,30,37,45)/100

owed <- function(a){
  sum <- 0
  b <- 2
  while(a>brackets[b] & b<(length(brackets)+1)){
    sum <- sum + rates[b-1]*(brackets[b]-brackets[b-1])
    b <- b+1
  }
  sum <- sum + rates[b-1]*(a-brackets[b-1])
  
  return(sum)
}

earns <- 1:100 * 1000
paid <- c()
rate <- c()
for(i in earns){
  paid <- c(paid,owed(i))
  rate <- c(rate,owed(i)/i)
}

data <- data.frame(earns,paid,rate)
ggplot(data, aes(x=earns)) +
  geom_line(aes(y=paid)) +
  geom_line(aes(y=rate*10000))