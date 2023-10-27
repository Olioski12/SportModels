#to do
# when innings ended short - filter out
# innings end for stoppage
# innings end for won
# modelling next ball outcome
#



library(dplyr)
library(cricketdata)
library(ggplot2)
balls <- fetch_cricsheet( type = c("bbb", "match", "player"), gender = c("male"), competition = "odis")
#matches <- fetch_cricsheet( type = c("match"), gender = c("male"), competition = "odis")

match_ids <- unique(balls$match_id)

####Generation####

ball_scores <- data.frame(ball=1:300) #Change to rows??
print(length(match_ids))
for (i in 1:length(match_ids)){
  print(i)
  for (j in 1:2){
    
    innings <- balls %>% filter(match_id==match_ids[i] & innings==j)
    if(nrow(innings)>0){
      innings <- innings[order(innings$ball),] 
      scores <- c()
      l = nrow(innings)
      b = 0
      score = 0
      a = 1
      while(b<300){
        ball <- innings[a,]
        score = score + ball$runs_off_bat + ball$extras
        if((trimws(ball$player_dismissed)!="") %in% TRUE){
          score = score +0.09
        }
        
        if(is.na(ball$wides) & is.na(ball$noballs)){
          b = b+1
          scores <- scores %>% append(score)
        }
        if(a==l){
          score <- floor(score) + 0.95
          if(b<300){
            for(k in (b+1):300){
              scores <- scores %>% append(score)
            }
          }
          break
        }
        a = a+1
      }
      ball_scores<-cbind(ball_scores,scores)
      ##check for complete innings
    }
  }
}

ball_scores_saved <- ball_scores

bsr <- floor(ball_scores)

##############Runs/Ball###########
inns <- length(ball_scores)

sums <- c(0,0,0,0,0,0)
for(i in 1:300){
  modb <- i %% 6
  if(modb == 0){modb<-6}
  
  rowsum <- sum(bsr[i,]) - i
  if(i==1){
    prevsum <- 0
  }else{
    prevsum <- sum(bsr[i-1,]) - (i-1)
  }
  
  sums[modb] <- sums[modb] + rowsum-prevsum
}

perball <- sums/inns/50

###########Runs/Over###########
sums <- 1:50
for(i in 1:50){
  rowsum <- sum(bsr[(i*6),]) - i*6
  if(i==1){
    prevsum <- 0
  }else{
    prevsum <- sum(bsr[((i-1)*6),]) - (i-1)*6
  }
  sums[i] <- rowsum-prevsum
}

perover <- sums/inns

########Plot#######
##thinning all out
bsp <- ball_scores
for(j in 1:inns){
  print(j)
  i=10
  while (i <= 300){
    k <- ball_scores[i,j]
    if((k-0.95) %% 1 == 0){
      for(p in i:300){
        bsp[p,j] = NA
      }
    }
    i = i + 1
  }
}


#matplot(ball_scores[,1],ball_scores[,2:innings],type = "l")
matplot(bsp[,1],bsp[,2:inns],type = "l")
grid()
