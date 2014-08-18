
## ------ PREPROCESSING -----------
preprocess <- TRUE
if(preprocess){
  vh <- read.csv("20140714_0810_hour.csv")
  
  hlfwin <- 15
  vh$x <- vh$visits
  
  tam <- length(vh$x)
  ndays <- tam/1440 
  
## Dates
  vh$date <- as.POSIXct(as.character(vh$date), format = "%Y%m%d")
  vh$wday <- wday(vh$date)
  
## Smoothing
  vh$smx <- numeric(tam)
  for (t in 1:tam){
    vh$smx[t]<-mean(vh$x[max(1,t-hlfwin):min(tam,t+hlfwin-1)])
  }
}

plot(vh$smx, type='l')
