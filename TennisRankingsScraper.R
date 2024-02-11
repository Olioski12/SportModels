library(dplyr)
library(rvest)
library(lubridate)

urlbase <- "https://www.tennisexplorer.com/ranking/atp-men/"
end= as.Date("2024-01-15")
n=300 #players
k=500 #weeks back

positions <- 1:n
finalrank <- data.frame(positions)



for(i in 0:k){
  print(i)
  week <- end-i*7
  
  if(year(week)<2024){
    url <- paste0(urlbase, year(week), "/?date=",week)
  }else{
    url <- paste0(urlbase, "?date=", week)
  }
  
  
  
  doc <- read_html(url)
  divs <- doc %>% html_elements(".t-name")
  
  
  m = ceiling(n/50)
  col <- c()
  for(rot in 1:m){
    if(rot>1){
      url2 <- paste0(url,"&page=",rot)
    }else{
      url2 <- url
    }
    doc <- read_html(url2)
    divs <- doc %>% html_elements(".t-name")
    if(rot<m){
      for(j in 1:50){
        col <- c(col,html_text(divs[j]))
      }
    }else{
      for(j in 1:(n-(m-1)*50)){
        col <- c(col,html_text(divs[j]))
      }
    }
    
  }
  
  finalrank <- cbind(finalrank,col)
  
}

finalweek <- week

uq <- 1:n
for(r in 1:nrow(finalrank)){
  uq[r] <- length(unique(unlist(finalrank[r,2:(2+k)],use.names = FALSE)))
}

plot(uq,xlab='rank',ylab='count',xlim=c(1,n),ylim=c(1,300))
title(paste0("Top ", n, " players back to ",finalweek ))

