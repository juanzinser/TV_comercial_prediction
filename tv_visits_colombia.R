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

source("preprocesamiento_col.R")

v$wday  <- v$wday-1
v$wday[which(v$wday == 0)] <- 7
v <- v[which(v$date > "2014-07-13"),]

#### Visits ##### 
# m <- matrix(v$v,ncol = 42)
m_1 <- matrix(v$v[which(v$wday == 1)],ncol = 6)
m_2 <- matrix(v$v[which(v$wday == 2)],ncol = 6)
m_3 <- matrix(v$v[which(v$wday == 3)],ncol = 6)
m_4 <- matrix(v$v[which(v$wday == 4)],ncol = 6)
m_5 <- matrix(v$v[which(v$wday == 5)],ncol = 6)
m_6 <- matrix(v$v[which(v$wday == 6)],ncol = 6)
m_7 <- matrix(v$v[which(v$wday == 7)],ncol = 6)

#  plot(ts(rowMeans(m_3,na.rm=T)))
#  plot(ts(m_7[,1]))

col_prom <- data.frame(rowMeans(m_1,na.rm=T),rowMeans(m_2,na.rm=T),rowMeans(m_3,na.rm=T),rowMeans(m_4,na.rm=T),
                       rowMeans(m_5,na.rm=T),rowMeans(m_6,na.rm=T),rowMeans(m_7,na.rm=T))

names(col_prom) <- c("v_lunes","v_martes","v_miercoles","v_jueves","v_viernes","v_sabado","v_domingo")

# write.csv(col_prom,"output/promedios_colombia.csv")

# Calculo de sd 
x1 <- t(colwise(sd)(data.frame(t(m_1))))
x2 <- t(colwise(sd)(data.frame(t(m_2))))
x3 <- t(colwise(sd)(data.frame(t(m_3))))
x4 <- t(colwise(sd)(data.frame(t(m_4))))
x5 <- t(colwise(sd)(data.frame(t(m_5))))
x6 <- t(colwise(sd)(data.frame(t(m_6))))
x7 <- t(colwise(sd)(data.frame(t(m_7))))
x <- rowMeans(data.frame(x1,x2,x3,x4,x5,x6,x7),na.rm=T)

sd_v <- smooth(x,30)
col_prom$sd_v <- sd_v

# plot(ts(x_smooth))

# plots desviación estandar
plot(ts(x),ylim=c(0,30))
lines(x1,col="gray")
lines(x2,col="red")
lines(x3,col="blue")
lines(x4,col="orange")
lines(x5,col="green")
lines(x6,col="brown")
lines(x7,col="purple")

# serie +- sd
plot(ts(col_prom$v_lunes + 2*sd_v),col='lightblue',main="Lunes promedio +- 1 y 2 sd")
lines(ts(col_prom$v_lunes),lwd = 2)
lines(ts(col_prom$v_lunes - 2*sd_v),col='lightblue')
lines(ts(col_prom$v_lunes + sd_v),col='blue')
lines(ts(col_prom$v_lunes - sd_v),col='blue')

#### New Visits ####

# m <- matrix(v$v,ncol = 42)
m_1 <- matrix(v$nv[which(v$wday == 1)],ncol = 6)
m_2 <- matrix(v$nv[which(v$wday == 2)],ncol = 6)
m_3 <- matrix(v$nv[which(v$wday == 3)],ncol = 6)
m_4 <- matrix(v$nv[which(v$wday == 4)],ncol = 6)
m_5 <- matrix(v$nv[which(v$wday == 5)],ncol = 6)
m_6 <- matrix(v$nv[which(v$wday == 6)],ncol = 6)
m_7 <- matrix(v$nv[which(v$wday == 7)],ncol = 6)

#plot(ts(rowMeans(m_7,na.rm=T)))

col_prom2 <- data.frame(rowMeans(m_1,na.rm=T),rowMeans(m_2,na.rm=T),rowMeans(m_3,na.rm=T),rowMeans(m_4,na.rm=T),
                       rowMeans(m_5,na.rm=T),rowMeans(m_6,na.rm=T),rowMeans(m_7,na.rm=T))

names(col_prom2) <- c("nv_lunes","nv_martes","nv_miercoles","nv_jueves","nv_viernes","nv_sabado","nv_domingo")


# Calculo de sd 
x1 <- t(colwise(sd)(data.frame(t(m_1))))
x2 <- t(colwise(sd)(data.frame(t(m_2))))
x3 <- t(colwise(sd)(data.frame(t(m_3))))
x4 <- t(colwise(sd)(data.frame(t(m_4))))
x5 <- t(colwise(sd)(data.frame(t(m_5))))
x6 <- t(colwise(sd)(data.frame(t(m_6))))
x7 <- t(colwise(sd)(data.frame(t(m_7))))
x <- rowMeans(data.frame(x1,x2,x3,x4,x5,x6,x7),na.rm=T)

sd_nv <- smooth(x,30)
col_prom2$sd_nv <- sd_nv


#write.csv(col_prom,"output/baseline_colombia.csv")

# plot
plot(ts(col_prom$nv_lunes + 2*sd_nv),col='lightblue',main="Lunes promedio +- 1 y 2 sd",ylim=c(0,55))
lines(ts(col_prom$nv_lunes),lwd = 2)
lines(ts(col_prom$nv_lunes - 2*sd_nv),col='lightblue')
lines(ts(col_prom$nv_lunes + sd_nv),col='blue')
lines(ts(col_prom$nv_lunes - sd_nv),col='blue')

#### Brand visits ####

# m <- matrix(v$v,ncol = 42)
m_1 <- matrix(v$bv[which(v$wday == 1)],ncol = 6)
m_2 <- matrix(v$bv[which(v$wday == 2)],ncol = 6)
m_3 <- matrix(v$bv[which(v$wday == 3)],ncol = 6)
m_4 <- matrix(v$bv[which(v$wday == 4)],ncol = 6)
m_5 <- matrix(v$bv[which(v$wday == 5)],ncol = 6)
m_6 <- matrix(v$bv[which(v$wday == 6)],ncol = 6)
m_7 <- matrix(v$bv[which(v$wday == 7)],ncol = 6)

#plot(ts(rowMeans(m_7,na.rm=T)))

col_prom3 <- data.frame(rowMeans(m_1,na.rm=T),rowMeans(m_2,na.rm=T),rowMeans(m_3,na.rm=T),rowMeans(m_4,na.rm=T),
                        rowMeans(m_5,na.rm=T),rowMeans(m_6,na.rm=T),rowMeans(m_7,na.rm=T))

names(col_prom3) <- c("bv_lunes","bv_martes","bv_miercoles","bv_jueves","bv_viernes","bv_sabado","bv_domingo")


# Calculo de sd 
x1 <- t(colwise(sd)(data.frame(t(m_1))))
x2 <- t(colwise(sd)(data.frame(t(m_2))))
x3 <- t(colwise(sd)(data.frame(t(m_3))))
x4 <- t(colwise(sd)(data.frame(t(m_4))))
x5 <- t(colwise(sd)(data.frame(t(m_5))))
x6 <- t(colwise(sd)(data.frame(t(m_6))))
x7 <- t(colwise(sd)(data.frame(t(m_7))))
x <- rowMeans(data.frame(x1,x2,x3,x4,x5,x6,x7),na.rm=T)

sd_bv <- smooth(x,30)
col_prom3$sd_bv <- sd_bv

col_prom <- data.frame(col_prom,col_prom2,col_prom3)

 write.csv(col_prom,"output/baseline_colombia.csv")
#save(v,col_prom,file="output/baseline_colombia.Rdata")

# plot
plot(ts(col_prom$bv_lunes + 2*sd_bv),col='lightblue',main="Lunes promedio +- 1 y 2 sd",ylim=c(0,40))
lines(ts(col_prom$bv_lunes),lwd = 2)
lines(ts(col_prom$bv_lunes - 2*sd_bv),col='lightblue')
lines(ts(col_prom$bv_lunes + sd_bv),col='blue')
lines(ts(col_prom$bv_lunes - sd_bv),col='blue')

