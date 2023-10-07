library(fitzRoy)
library(dplyr)

##set up 
voters <- 3

team_size <- 23

rounds <- 22

votes <- 3


count <- data.frame()
for (t in 1:team_size){
  row <- data.frame(n = t, votes = 0)
  count <- rbind(count,row)
}

##vote giving procedure
game_rank <- data.frame()
for (r in 1:rounds){
  game_rank <- data.frame()
  
  ##performance rankings
  for (t in 1:team_size){
    s = runif(1)
    row <- data.frame(n = t, score = s)
    game_rank <- rbind(game_rank,row)
  }
  
  game_rank <- game_rank[order(-game_rank$score),]
  
  ##votes
  for (v in 1:voters){
    
  }
}



##random inputs