Decode_AFLCA_Votes <- function(vote_table){
  alloc <- c(1:10 %% 5) +1
  alloc <- alloc[order(-alloc)]
  vote_table <- vote_table[order(-vote_table)]
  final <- data.frame(votes = vote_table, v1 = 0, v2 = vote_table)
  for(j in 1:length(unique(alloc))){
    for (i in 1:nrow(final)){
      if(final$v1[i]==0){
        if(final$votes[i] >= max(alloc)*2-1){
            final$v1[i] = max(alloc)
          if(final$votes[i] == max(alloc)*2){
            final$v2[i] = max(alloc)
            alloc <- alloc[(alloc != max(alloc))]
          }else{
            final$v2[i] = max(alloc)-1
            m <- max(alloc)
            alloc <- alloc[order(-alloc)]
            alloc <- alloc[-1]
            if(alloc[1] == m-1){
              alloc <- alloc[-2]
            }else{
              alloc <- alloc[-1]
            }
          }
          
          
        }
        if(final$votes[i] == min(alloc)){
          final$v1[i] = min(alloc)
          final$v2[i] = 0
          alloc <- alloc[order(-alloc)]
          alloc <- alloc[-length(alloc)]
        }
        
        }
    }
  }
  return(final)
}

