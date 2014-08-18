install.packages('lubridate')

library(lubridate)




setwd("~/comercial_TV")



## ------ PREPROCESSING -----------
preprocess <- TRUE
if(preprocess){
  v <- read.csv("20140714_0810.csv")
  tam <- length(v$visits)
  ndays <- tam/1440 
  
  ## Parameters 
  
  hlfwin <- 15
  
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
                         transactions = 0, tmstmp = tmseq[miss])
    v <- rbind(v[1:miss-1,],newrow,v[miss:length(v$date),])
  }
  tam <- length(v$visits)
  
  ## Date
  
  v$date <- as.POSIXct(as.character(v$date), format = "%Y%m%d")
  v$wday <- wday(v$date)
  
  ## Smoothing
  v$v <- numeric(tam)
  v$nv <- numeric(tam)
  for (t in 1:tam){
    v$v[t]<-mean(v$visits[max(1,t-hlfwin):min(tam,t+hlfwin-1)])
    v$nv[t]<-mean(v$newvisits[max(1,t-hlfwin):min(tam,t+hlfwin-1)])
  }
}

plot(v$v, type='l')

xmat <- matrix(v$v,nrow=1440*7)
xmean <- rowMeans(xmat)
matplot(cbind(xmat,xmean), type = c('l','l','l','l',"b"),pch=1,col = c(2:5,1))
legend("topleft", legend = 1:4, col=2:5, pch=1)