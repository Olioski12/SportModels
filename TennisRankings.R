library(dplyr)

#tennis explorer website

GS <- c(10,50,100,200,400,800,1300,2000)
GSo <- c()
for(k in length(GS):1){
  GSo <- c(GSo,rep(GS[length(GS)+1-k],2^max(0,(k-2))))
}


n <- 128

players <- 1:n

points <- (1:n)/1000

rankings <- data.frame(players,points)

positions <- 1:n

finalrank <- data.frame(positions)

for(s in 1:2000){
  col <- sample(GSo,n,replace=FALSE)
  rankings <- rankings %>% mutate(points=points+col)
  rankings <- rankings[order(-rankings$points),]
  finalrank <- cbind(finalrank,rankings$players)
}

uq <- 1:n
for(r in 1:nrow(finalrank)){
  uq[r] <- length(unique(unlist(finalrank[r,2:100],use.names = FALSE)))
}

plot(uq,xlab='rank',ylab='count',xlim=c(1,128),ylim=c(1,128))
