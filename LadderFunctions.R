library(dplyr)

makeLadder <- function(matches){
  teams <- c(matches$home,matches$away)
  teams <- unique(teams)
  ladder <- data.frame(team = teams)
  ladder <- ladder %>% mutate(P = 0, W = 0, L = 0, D = 0, PTS = 0, GF = 0, GA = 0,GD = 0)
  for (i in 1:length(teams)){
    team <- teams[i]
    W = 0;
    L = 0;
    D = 0;
    GF = 0;
    GA = 0;
    for(j in 1:nrow(matches)){
      if(matches$home[j] == team){
        hs <- matches$homescore[j]
        as <- matches$awayscore[j]
        GF <- GF + hs
        GA <- GA + as
        if(hs>as){
          W <- W+1
        }else if (hs==as){
          D <- D+1
        }else{
          L <- L+1
        }
      }
      if(matches$away[j] == team){
        hs <- matches$homescore[j]
        as <- matches$awayscore[j]
        GF <- GF + as
        GA <- GA + hs
        if(hs>as){
          L <- L+1
        }else if (hs==as){
          D <- D+1
        }else{
          W <- W+1
        }
      }
    }
    ladder[i,] <- c(team,W+L+D,W,L,D,W*3+D,GF,GA,GF-GA)
  }
  
  ladder <- ladder[order(ladder$PTS,decreasing=TRUE),]
  rownames(ladder) <- 1:nrow(ladder)
  return(ladder)
}

makeLadderAve <- function(matches){
  ladder <- makeLadder(matches)
  ladder <- ladder %>% mutate(PTSAVE = (as.numeric(ladder$PTS)/as.numeric(ladder$P)))
  ladder <- ladder[order(ladder$PTSAVE,decreasing=TRUE),]
  rownames(ladder) <- 1:nrow(ladder)
  return(ladder)
}