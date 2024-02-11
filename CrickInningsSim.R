library(dplyr)
library(ggplot2)
source(here::here('CrickSimFunctions.R'))
hitprob = 0.1

#what is the value of a run? - 2 runs is great generally - derive from the back
#model with ability, setness, and aggression
Batability = seq(1,0,-0.1)
Faced <- 0*Batability

Bowlability = seq(1,0,-0.1)
Bowled <- 0*Bowlability
z = 0
o = 0
t = 0
th = 0
f = 0
s = 0
Probs <- data.frame(z,o,t,th,f,s)

overs <- 20

score <- 0;

Bat1 <- 1
Bat2 <- 2
Bowl <- 1
Strike <- 1
Outs <- 0
OnStrike <- c()
Runs <- c()
Bowler <- c()
OutProbs <- c()
total <- c()
Aggro <- c()

for(i in 1:(overs*6)){
  if(Strike==1){
    OS <- Bat1
  }else{
    OS <- Bat2
  }
  OnStrike <- c(OnStrike,OS)
  Faced[OS] = Faced[OS] + 1
  Bowler <- c(Bowler,Bowl)
  agg <- ((i-30)/30)^3
  outprob <- OP(Bowlability[Bowl],Batability[OS],Faced[OS],agg)
  Aggro <- c(Aggro,agg)
  OutProbs <- c(OutProbs,outprob)
  
  #Wicket 
  if(runif(1)<outprob){
    Outs <- Outs + 1
    Runs <- c(Runs,"W")
    if(Strike==1){
      Bat1 <- Outs+2
    }else{
      Bat2 <- Outs+2
    }
  }else{   #No wicket
    
    spin <- runif(1)
    outcome <- runVal(spin,Batability[OS],Bowlability[Bowl],Faced[OS],agg)
    if(outcome[1]%%2 == 1){
      Strike = 3-Strike
    }
    
    Runs <- c(Runs,outcome[1])
    Probs <- rbind(Probs,c(0,outcome[2:6]))
    score <- score + outcome[1]
  }
  if(i%%6 == 0){
    Strike = 3-Strike
    Bowled[Bowl] <- Bowled[Bowl] +1
    curBowl <- Bowl
    for(b in 1:length(Bowled)){
      if(b != curBowl && Bowled[b]<4){
        Bowl <- b
        break
      }
    }
  }
  total <- c(total,score)
  if(Outs == 10){
    break
    
  }
  
}

innings <- data.frame(OnStrike,Runs,Bowler,OutProbs,total)
print(score)
balls <- 1:nrow(innings)
data <- data.frame(balls,Runs)



ggplot(data, aes(x=balls)) +
  geom_line(aes(y=total)) +
  geom_line(aes(y=OutProbs*100))+
  geom_line(aes(y=Aggro))


BC <- battingCard(innings)
BoC <- bowlingCar
