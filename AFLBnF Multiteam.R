library(fitzRoy)
library(dplyr)

# stats <- fetch_player_stats_afl(season="2023")
# stats <- stats %>% filter(round.roundNumber<=24)
# stats$Fullname = paste(stats$player.player.player.givenName,stats$player.player.player.surname,sep = " ")
# stats <- stats %>% select(providerId,team.name,Fullname)
# stats <- stats %>% rename(GameID = providerId)

players <- stats %>% select(team.name,Fullname)

players <- unique(players)

players <- players %>% mutate(votes = 0)


##set up 
voters <- 2

votes <- c(5,4,3,2,1)

games <- unique(stats$GameID)

##vote giving procedure
for (r in 1:length(games)){
  game_players <- stats %>% filter (GameID == games[r])
  game_players <- game_players %>% mutate(score = 0)
  
  ##performance rankings
  for (t in 1:nrow(game_players)){
    s = runif(1)*0.75
    game_players$score[t] = s
  }
  
  game_players <- game_players[order(-game_players$score),]
  
  ##votes
  for (v in 1:voters){
    vgr <- game_players
    for(i in 1:nrow(vgr)){
      voter_amend <- runif(1)*0.25
      vgr$score[i] <- vgr$score[i] + voter_amend
    }
    vgr <- vgr[order(-vgr$score),]
    c = 1
    for (n in votes){
      team = vgr$team.name[c]
      player = vgr$Fullname[c]
      players$votes[which(player==players$Fullname&team==players$team.name)] = players$votes[which(player==players$Fullname&team==players$team.name)]+n
      c = c+1
      
    }
  }
}

players <- players[order(-players$votes),]
players[1:10,]
