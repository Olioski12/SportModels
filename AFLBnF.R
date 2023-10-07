library(fitzRoy)
library(dplyr)

##set up 
voters <- 2

team_size <- 23

rounds <- 22

votes <- c(5,4,3,2,1)


count <- data.frame()
for (t in 1:team_size){
  row <- data.frame(n = t, votes = 0, skill = t/(4*team_size))
  count <- rbind(count,row)
}

##vote giving procedure
game_rank <- data.frame()
for (r in 1:rounds){
  game_rank <- data.frame()
  
  ##performance rankings
  for (t in 1:team_size){
    s = runif(1) + count$skill[t]
    row <- data.frame(n = t, score = s)
    game_rank <- rbind(game_rank,row)
  }
  
  game_rank <- game_rank[order(-game_rank$score),]
  
  ##votes
  for (v in 1:voters){
    vgr <- game_rank
    for(i in 1:team_size){
      voter_amend <- runif(1)*0.25
      vgr$score[i] <- vgr$score[i] + voter_amend
    }
    vgr <- vgr[order(-vgr$score),]
    c = 1
    for (n in votes){
      number = vgr$n[c]
      count$votes[which(count$n==number)] = count$votes[which(count$n==number)]+n
      c = c+1
      
    }
  }
}

count <- count[order(-count$votes),]

count


##random inputs