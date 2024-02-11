library(dplyr)
OP <- function(Bowlability,Batability,Faced,Agg){
  outprobgen <- 0.09
  OPM <- 0.01
  FacedDec <- 0.005
  
  value <- max(outprobgen*Bowlability + 0.01*Agg - FacedDec * min(Faced,10) - 0.03*Batability,OPM)
  
  return(value)
}

runVal <- function(spin,Batability,Bowlability,Faced,Agg){
  ballQual <- Bowlability + runif(1)*10-runif(1)*10
  
  oneThres <- 0.5-Batability*0.05 + ballQual*0.01 - 0.005*min(Faced,10)
  twoThres <- 0.6-Batability*0.04 + ballQual*0.02 - 0.004*min(Faced,10)
  threeThres <- 0.78-Batability*0.022 + ballQual*0.03 - 0.0022*min(Faced,10)
  fourThres <- 0.8-Batability*0.02 + ballQual*0.04 - 0.002*min(Faced,10) - 0.02*Agg
  sixThres <- 1-Batability*0.005 + ballQual*0.05 - 0.005*min(Faced,10) - 0.01*Agg
  
  if(spin > sixThres){
    runVal <- 6
  }else if(spin> fourThres){
    runVal <- 4
  }else if(spin> threeThres){
    runVal <- 3
  }else if(spin> twoThres){
    runVal <- 2
  }else if(spin> oneThres){
    runVal <- 1
  }else{
    runVal <- 0
  }
  
  return(c(runVal,oneThres,twoThres,threeThres,fourThres,sixThres))
}

battingCard <- function(innings){
  Batters <- 1:11
  BattingCard <- data.frame()
  for(b in Batters){
    batInnings <- innings %>% filter(OnStrike == b)
    balls <- nrow(batInnings)
    runs <- sum(as.numeric((batInnings%>%filter(Runs!="W"))$Runs))
    Out <- nrow(batInnings%>%filter(Runs=="W")) > 0
    row <- data.frame(Batters = b,Runs=runs,Balls=balls,out=Out)
    BattingCard <-rbind(BattingCard,row)
  }
  return(BattingCard)
}

bowlingCard <- function(innings){
  bowlers <- 1:11
  BowlingCard <- data.frame()
  for(b in bowlers){
    bowlInnings <- innings %>% filter(Bowler == b)
    balls <- nrow(bowlInnings)
    overs <- floor(balls/6) + 0.1*balls%%6
    runs <- sum(as.numeric((bowlInnings%>%filter(Runs!="W"))$Runs))
    Wickets <- nrow(bowlInnings%>%filter(Runs=="W"))
    row <- data.frame(Bowlers = b,Overs = overs,Runs=runs,Wickets=Wickets)
    BowlingCard <-rbind(BowlingCard,row)
  }
  BowlingCard <- BowlingCard %>% filter(Overs>0)
  return(BowlingCard)
}
