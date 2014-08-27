library(lubridate)

## ------ PREPROCESSING -----------
preprocess <- TRUE
if(preprocess){
  v <- read.csv("data/col_30jun_24ago_full.csv")
  tam <- length(v$visits)
  ndays <- tam/1440 
  
## Parameters 
  
  hlfwin <- 30
  
## Timestmp
  v$tmstmp <- as.POSIXlt(paste(v$date,v$hits_hour,v$hits_minute), format = "%Y%m%d %H %M")


## Missing rows
## existingDF <- rbind(existingDF[1:r,],newrow,existingDF[-(1:r),])

tmseq <- seq(v$tmstmp[1],tail(v$tmstmp,n=1), by="min")

repeat {
  miss <- which.min(tmseq[1:length(v$tmstmp)] == v$tmstmp)
  if(miss==1) break
  newrow <- data.frame(date = v$date[miss-1],hits_hour = hour(tmseq[miss]), 
                       hits_minute = minute(tmseq[miss]), visits = 0, newvisits = 0, 
                       transactions = 0,brandvisits = 0, tmstmp = tmseq[miss])
  v <- rbind(v[1:miss-1,],newrow,v[miss:length(v$date),])
}
tam <- length(v$visits)

 ## Date

  v$date <- as.POSIXct(as.character(v$date), format = "%Y%m%d")
  v$wday <- wday(v$date)
  
## Smoothing

smooth <- function (x,hlfwin){
  len <- length(x)
  xsm <- numeric(len)
  for (t in 1:len){
    xsm[t]<-mean(x[max(1,t-hlfwin):min(len,t+hlfwin-1)], rm.na = TRUE)
  }
  return(xsm)
}

  v$v <- smooth(v$visits,hlfwin)
  v$nv <- smooth(v$newvisits,hlfwin)
  v$bv <- smooth(v$brandvisits,hlfwin)
}

## Averaging 
# xmat <- matrix(v$v,nrow=55)
# vpred <- smooth(rowMeans(xmat),hlfwin)
# matplot(cbind(xmat,vpred), type = c('l','l','l','l',"b"),pch=1,col = c(2:5,1), ylab = "Smoothed Visits")
# legend("topleft", legend = 1:4, col=2:5, pch=1)

## Save
# baseavg <- vpred
# save(baseavg, file = "baseline.rdata")

