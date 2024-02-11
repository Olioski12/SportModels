library(dplyr)

states1 <- c("PRC","FRA","RUS","UK","US","ALB","BRA","ECU","GAB","GHA","JAP","MAL","MZB","SUI","UAE")
states2 <- states1


dels <- c("ACT","NZ","WA","TAS","NT","VIC","SA","QLD","NSW","VIC","WA","SA","NSW")


extras <- c("VIC","NSW","QLD","WA")
ae <- extras[sample(1:length(extras),2)]

dels1 <- c(dels,ae)
dels2 <- dels

for(i in 1:length(extras)){
  if(!(extras[i] %in% ae)){
    dels2 <- c(dels2,extras[i])
  }
}

dels1 <- sample(dels1)
dels2 <- sample(dels2)

totaldels <- c(dels1,dels2)
totaldels <- totaldels[order(totaldels)]
uni <- unique(totaldels)

table <- data.frame(num = 1:30, state = c(states1,states2),delegation = c(dels1,dels2))
table <- table %>% mutate(chamber = ifelse(num<16,"Cisse","Makin"),team = "")

for(j in 1:length(uni)){
  d <- uni[j]
  num <- length(totaldels[totaldels==d])
  ord <- sample(LETTERS[1:num])
  for(k in 1:num){
    n=1
    while(n<31){
      if(d==table$delegation[n] & table$team[n]==""){
        table$team[n] <- ord[k]
        n=30
      }
      n = n+1
    }
  }
}

print(table)