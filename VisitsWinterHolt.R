<<<<<<< HEAD
# Look at https://www.otexts.org/fpp/7/5 for Holt-Winters Method

# First install.packages("fpp")

library(fpp)

visitsraw <- read.csv("WebTrekk Daily Visits.csv")
visitsraw$Days <- as.POSIXct(visitsraw$Days,format="%d/%m/%Y" )
start <- as.POSIXct("2014-05-26", format="%Y-%m-%d")
end <- as.POSIXct("2014-08-10", format="%Y-%m-%d")
v <- visitsraw[which(visitsraw$Days>=start & visitsraw$Days<=end),]

vts <- ts(v$Visits,frequency=7,start=1)
hwfit <- hw(vts,seasonal="multiplicative", initial="optimal")

plot(hwfit,ylab="Visits",
     plot.conf=TRUE, type="o", fcol="white", xlab="Week", main="Visits prediction")
lines(fitted(hwfit), col="blue", lty=2)
lines(hwfit$mean, type="o", col="blue")
legend("topleft",lty=1, pch=1, col=c("black","blue"),   c("Actual","Model"))

vpos <- visitsraw[which(visitsraw$Days>end),]$Visits
forpos <- hwfit$fitted[1:length(vpos)]

# The error for the two points we have data for (Aug 11th and 12th) is 3.8% and 2% 
error <- (forpos-vpos)/vpos
=======
# Look at https://www.otexts.org/fpp/7/5 for Holt-Winters Method

# First install.packages("fpp")

library(fpp)

visitsraw <- read.csv("WebTrekk Daily Visits.csv")
visitsraw$Days <- as.POSIXct(visitsraw$Days,format="%d/%m/%Y" )
start <- as.POSIXct("2014-05-26", format="%Y-%m-%d")
end <- as.POSIXct("2014-08-10", format="%Y-%m-%d")
v <- visitsraw[which(visitsraw$Days>=start & visitsraw$Days<=end),]

vts <- ts(v$Visits,frequency=7,start=1,)
hwfit <- hw(vts,seasonal="multiplicative", initial="optimal")

plot(hwfit,ylab="Visits",
     plot.conf=TRUE, type="o", fcol="white", xlab="Week", main="Visits prediction")
lines(fitted(hwfit), col="blue", lty=2)
lines(hwfit$mean, type="o", col="blue")
legend("topleft",lty=1, pch=1, col=c("black","blue"),   c("Actual","Model"))

vpos <- visitsraw[which(visitsraw$Days>end),]$Visits
forpos <- hwfit$fitted[1:length(vpos)]

# The error for the two points we have data for (Aug 11th and 12th) is 3.8% and 2% 
error <- (forpos-vpos)/vpos
>>>>>>> 4d7bd5513ca569e73bfe9b3f26d81d1c35bcefbf
