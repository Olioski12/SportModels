library(dplyr)
library(rvest)
library(jsonlite)
document <- read_json("https://aleagues.com.au/ajax/get_ticker/a-leagues/xwnjb1az11zffwty3m6vn8y6,9p3nnxhdjahfn8qswpzy8oyc3/null/")
# ALWCId <- "9p3nnxhdjahfn8qswpzy8oyc3" 
# TourneyId <- "7p85tm8wjdbrab8yssm2ea9sk"
# url2 <- paste0("https://apl-middleware.keepup.football/v2/fixtures?competitionId=",ALWCId,"&tournamentId=",TourneyId)
# doc2 <- read_json(url)
data <- document$data
table <- data.frame()
for(i in 1:length(data)){
  id <- data[[i]]$id
  home <- data[[i]]$homeTeam$name
  away <- data[[i]]$awayTeam$name
  homescore <- data[[i]]$homeScore
  awayscore <- data[[i]]$awayScore
  startDate <- data[[i]]$startDate /1000
  startDate <- as.POSIXct(startDate,origin = "1970-01-01")
  comp <- data[[i]]$competition$name
  year <- data[[i]]$tournament$name
  row <- data.frame(id=id,home=home,away=away,homescore=homescore,awayscore=awayscore,startDate=startDate,comp=comp,year=year)
  table <- rbind(table,row)
}

ALW <- table %>% filter(comp == "A-League Women")

Wladder <- makeLadder(ALW)

ALM <- table %>% filter(comp == "A-League Men")

Mladder <- makeLadder(ALM)