

## ------ PREPROCESSING -----------
preprocess <- TRUE
if(preprocess){
  v <- read.csv("20140714_0810.csv")
  
  hlfwin <- 15
  v$x <- v$visits
  
  tam <- length(v$x)
  ndays <- tam/1440 
  
## Dates
  v$date <- as.POSIXct(as.character(v$date), format = "%Y%m%d")
  v$wday <- wday(v$date)
  
## Smoothing
  v$smx <- numeric(tam)
  for (t in 1:tam){
    v$smx[t]<-mean(v$x[max(1,t-hlfwin):min(tam,t+hlfwin-1)])
  }
}

plot(v$smx, type='l')
