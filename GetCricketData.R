library(dplyr)
library(cricketdata)


bblgames <- fetch_cricsheet('match','male','bbl')

bblballs <- fetch_cricsheet('bbb','male','bbl')


batters <- unique(bblballs$striker)

df <- data.frame("Balls","Runs","Outs")

for(b in 1:length(batters)){
  batdf <- bblballs %>% filter(striker==batters[b])
  balls <- nrow(batdf)
  runs <- sum(batdf$runs_off_bat)
  outs <- sum(batdf$wicket)
  bat <- data.frame(batters[b],balls,runs,outs)
  df <- rbind(df,bat)
  print(b)
}