# TV visits

setwd("~/TV_comercial_prediction")

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
library(tidyr)

#### Daily Data #####

# Lectura de datos
data_ga_daily <- read.table("data/GA_daily_visits_7jul_13aug.csv",header=TRUE,sep=",")
data_ga_hourly <- read.table("data/GA_hourly_visits_7jul_13aug.csv",header=TRUE,sep=",")
data_wt_daily <- read.table("data/WebTrekk_daily_visits_14aug_13aug.csv",header=T,sep=",")
data_minute <- read.table("data/minute_visits_14jul_10aug.csv",header=T,sep=",")

data_ga_hourly <- na.omit(data_ga_hourly)

# Fecha

data_ga_daily$date <- as.Date(data_ga_daily$Day.Index)
data_wt_daily$date <- as.Date(data_wt_daily$Days)

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

# Minute Data 
# qplot(data = data_minute[1:1440,], y = visits, x = seq(1:1440) , xlab = "Time" , ylab = "visits",main="Minute visits")+
  #geom_line()

#### Minute Data ####

source("preprocesamiento.R")
df_v <- data.frame(xmat)
df_v$avg <- xmean 
df_v$nday <- c(array(1,dim=1440) , array(2,dim=1440) , array(3,dim=1440) , array(4,dim=1440) , 
               array(5,dim=1440) , array(6,dim=1440) , array(0,dim=1440))
df_v$minute <- substr(v$tmstmp[1:1440],12,19)

qplot(data=df_v[which(df_v$nday == 1),], x=seq(1:1440), y=avg,main="Average Smooth Monday") + geom_line()

# soft hour data
m_dia <- m_dia[8:35,]
x <- rowMeans(t(m_dia[which(m_dia$nday == 1),])) # lunes
x <- x[1:24]
x_1440 <- c(array(x[1],dim = 60),array(x[2],dim = 60),array(x[3],dim = 60),array(x[4],dim = 60),
            array(x[5],dim = 60),array(x[6],dim = 60),array(x[7],dim = 60),array(x[8],dim = 60),
            array(x[9],dim = 60),array(x[10],dim = 60),array(x[11],dim = 60),array(x[12],dim = 60),
            array(x[13],dim = 60),array(x[14],dim = 60),array(x[15],dim = 60),array(x[16],dim = 60),
            array(x[17],dim = 60),array(x[18],dim = 60),array(x[19],dim = 60),array(x[20],dim = 60),
            array(x[21],dim = 60),array(x[22],dim = 60),array(x[23],dim = 60),array(x[24],dim = 60))
x_1440

qplot(x=seq(1:1440), y=x_1440 , main="Monday") + geom_line()

# Soften
tam <- 1440
hlfwin <- 30
sm_x_1440 <- numeric(tam)
for (t in 1:tam){
  sm_x_1440[t]<-mean(x_1440[max(1,t-hlfwin):min(tam,t+hlfwin-1)])
}

qplot(x=seq(1:1440), y= sm_x_1440 , main="Average Hour-minute Monday") + geom_line()


# Multilpe time series
aux_df <- data.frame(x = rep(1:1440,3),
           val = c(sm_x_1440/60,x_1440/60,df_v[which(df_v$nday == 1),"avg"]),
           variable = c(rep("smooth hour",1440),rep("hour",1440),rep("minute",1440)))
ggplot(data = aux_df, aes(x=x, y=val)) + geom_line(aes(colour=variable))


### Actualizando datos por hora
data_hour <- read.table("data/20140714_0810_hour.csv",header=TRUE,sep=",")
data_hour$nday <- rep(c(rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24)),4)

# lunes
y <- numeric(24)
for(i in 0:23){
y[i+1] <- mean(data_hour[which(data_hour$nday==1 & data_hour$hits_hour == i),"visits"])
}
# poner promedio de hora a todos los minutos
y_1440 <- c(array(y[1],dim = 60),array(y[2],dim = 60),array(y[3],dim = 60),array(y[4],dim = 60),
            array(y[5],dim = 60),array(y[6],dim = 60),array(y[7],dim = 60),array(y[8],dim = 60),
            array(y[9],dim = 60),array(y[10],dim = 60),array(y[11],dim = 60),array(y[12],dim = 60),
            array(y[13],dim = 60),array(y[14],dim = 60),array(y[15],dim = 60),array(y[16],dim = 60),
            array(y[17],dim = 60),array(y[18],dim = 60),array(y[19],dim = 60),array(y[20],dim = 60),
            array(y[21],dim = 60),array(y[22],dim = 60),array(y[23],dim = 60),array(y[24],dim = 60))

# Soften
tam <- 1440
hlfwin <- 30
sm_y_1440 <- numeric(tam)
for (t in 1:tam){
  sm_y_1440[t]<-mean(y_1440[max(1,t-hlfwin):min(tam,t+hlfwin-1)])
}


# Multilpe time series
aux_df <- data.frame(x = rep(1:1440,3),
                     val = c(sm_y_1440/60,y_1440/60,df_v[which(df_v$nday == 1),"avg"]),
                     variable = c(rep("smooth hour",1440),rep("hour",1440),rep("minute",1440)))
ggplot(data = aux_df, aes(x=x, y=val)) + geom_line(aes(colour=variable))

aux_df <- data.frame(x = rep(1:1440,2),
                     val = c(sm_y_1440/60 , df_v[which(df_v$nday == 1),"avg"]),
                     variable = c(rep("smooth hour",1440) , rep("minute",1440)))
ggplot(data = aux_df, aes(x=x, y=val)) + geom_line(aes(colour=variable))


