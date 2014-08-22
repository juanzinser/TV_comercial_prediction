##  --- Spot simulation

nspots <- 600
spot <- data.frame(rating=numeric(nspots),channel=character(nspots),length=numeric(nspots))

# random times
randia <- floor(runif(nspots,11,18))
randsec <- floor(runif(nspots, min=1,max=1440*7))
spot$tmstmp<- as.POSIXlt(paste("2014 08",randia, randsec%%24, randsec%%60),format="%Y %m %d %H %M")

# random channels
channs <- c("Discovery","Bandamax","C2","Fox")
randchan <- floor(runif(nspots,1,length(channs)+1))
spot$channel <- factor(channs[randchan])

# random ratings
spot$rating <- floor(runif(nspots,1,100))/100

# random cost
spot$cost <- rnorm(nspots,10000,1500)

# random lenghts
spot$length <- factor(floor(runif(nspots,2,4))*10)

spot <- spot[with(spot, order(tmstmp)), ]

save(spot,file="spots.rdata")

## -- Simulated Visits without accumulated effect

fint <- vpred/mean(vpred)
ftv <- rep(c(seq(0.7,0,length.out=60*5),seq(0,1,length.out=60*17),seq(1,0.7,length.out=60*2)),7); ftv<-ftv/mean(ftv) 
extra <- smooth(abs(cumsum(runif(length(vpred), -1, 1))*vpred/50),hlfwin)*fint*ftv
visits <- smooth(vpred + extra,hlfwin)
plot(extra, type='l')


tmp <- v
v <- data.frame(tmstmp = seq(as.POSIXlt("20140811 0 0 0",format="%Y%m%d %H %M %S"),as.POSIXlt("20140817 23 59 59",format="%Y%m%d %H %M %S"),by="min"),
                 v = visits)
save(v,file="visits.rdata")
v <- tmp; remove(tmp)
