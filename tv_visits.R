# TV visits

setwd("~/linio-comercial")

install.packages('forecast')
install.packages('plyr')
install.packages('ggplot2')
install.packages('lubridate')
install.packages('reshape2')
install.packages('MTS')
install.packages('dse')
install.packages('TSA')
install.packages('lubridate')
install.packages('data.table')

library(forecast)
library(plyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(MTS)
library(dse)
library(TSA)
library(lubridate)
library(data.table)

#### Lectura de datos #####

data_ga_daily <- read.table("GA_daily_visits_7jul_13aug.csv",header=TRUE,sep=",")
data_ga_hourly <- read.table("GA_hourly_visits_7jul_13aug.csv",header=TRUE,sep=",")
data_wt_daily <- read.table("WebTrekk_daily_visits_14aug_13aug.csv",header=T,sep=",")

data_ga_hourly <- na.omit(data_ga_hourly)

# Fecha

data_ga_daily$date <- as.Date(data_ga_daily$Day.Index,format = "%m/%d/%Y")
data_wt_daily$date <- as.Date(data_wt_daily$Days, format="%d/%m/%Y")

#plots
ts_ga_daily <- ggplot(data_ga_daily,aes(x = date,y = Sessions)) + geom_line()  +labs(y="GA Sessions")
ts_ga_daily

ts_wt_daily <- ggplot(data_wt_daily,aes(x = date,y = Visits)) + geom_line()  +labs(y="WT visits")
ts_wt_daily

# Daily plot
i <- 7
inicio <- 1+(24*i)
final <- 24+(24*i)
ts_ga_hourly <- ggplot(data_ga_hourly[inicio:final,] , aes(x = Hour.Index,y = Sessions)) + geom_line()  +labs(y="GA hourly Sessions")
ts_ga_hourly

# Average Day
x <- data_ga_hourly$Sessions
m <- matrix(x,ncol=24,byrow=T)
mean(m[,1])
y <- matrix(nrow=1,ncol=24)

for(i in 1:24){
  y[1,i] <- print(mean(m[1:38,i]))
}

#Plot average day
qplot(y = y[1:24],x=seq(1:24),xlab="Hour",ylab=,main="Average hourly visits")+
  geom_line()

# add day number
m_dia <- as.data.frame(m)
aux7 <-seq(1:7) 
m_dia$nday <- c(aux7,aux7,aux7,aux7,aux7,1,2,3)

# Average monday
titulo <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

j <- 1 
y <- matrix(nrow=1,ncol=24)
for(i in 1:24){
  y[1,i] <- mean(m_dia[which(m_dia$nday == j),i])
}
qplot(y = y[1:24],x=seq(1:24),xlab = "Hour",ylab = "Visits",main=paste("Average",titulo[j]))+
  geom_line()
ggsave("plotmonday.png")

j <- 2 
y <- matrix(nrow=1,ncol=24)
for(i in 1:24){
  y[1,i] <- mean(m_dia[which(m_dia$nday == j),i])
}
qplot(y = y[1:24],x=seq(1:24),xlab = "Hour",ylab = "Visits",main=paste("Average",titulo[j]))+
  geom_line()
ggsave("plottuesday.png")

j <- 3 
y <- matrix(nrow=1,ncol=24)
for(i in 1:24){
  y[1,i] <- mean(m_dia[which(m_dia$nday == j),i])
}
qplot(y = y[1:24],x=seq(1:24),xlab = "Hour",ylab = "Visits",main=paste("Average",titulo[j]))+
  geom_line()
ggsave("plotwednesday.png")

j <- 4 
y <- matrix(nrow=1,ncol=24)
for(i in 1:24){
  y[1,i] <- mean(m_dia[which(m_dia$nday == j),i])
}
qplot(y = y[1:24],x=seq(1:24),xlab = "Hour",ylab = "Visits",main=paste("Average",titulo[j]))+
  geom_line()
ggsave("plotthursday.png")

j <- 5 
y <- matrix(nrow=1,ncol=24)
for(i in 1:24){
  y[1,i] <- mean(m_dia[which(m_dia$nday == j),i])
}
qplot(y = y[1:24],x=seq(1:24),xlab = "Hour",ylab = "Visits",main=paste("Average",titulo[j]))+
  geom_line()
ggsave("plotfriday.png")

j <- 6 
y <- matrix(nrow=1,ncol=24)
for(i in 1:24){
  y[1,i] <- mean(m_dia[which(m_dia$nday == j),i])
}
qplot(y = y[1:24],x=seq(1:24),xlab = "Hour",ylab = "Visits",main=paste("Average",titulo[j]))+
  geom_line()
ggsave("plotsaturday.png")

j <- 7 
y <- matrix(nrow=1,ncol=24)
for(i in 1:24){
  y[1,i] <- mean(m_dia[which(m_dia$nday == j),i])
}
qplot(y = y[1:24],x=seq(1:24),xlab = "Hour",ylab = "Visits",main=paste("Average",titulo[j]))+
  geom_line()
ggsave("plotsunday.png")

