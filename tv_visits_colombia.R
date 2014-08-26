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

m_1 <- matrix(v$v[which(v$wday == 1)],ncol = 8)
m_2 <- matrix(v$v[which(v$wday == 2)],ncol = 8)
m_3 <- matrix(v$v[which(v$wday == 3)],ncol = 8)
m_4 <- matrix(v$v[which(v$wday == 4)],ncol = 8)
m_5 <- matrix(v$v[which(v$wday == 5)],ncol = 8)
m_6 <- matrix(v$v[which(v$wday == 6)],ncol = 8)
m_7 <- matrix(v$v[which(v$wday == 7)],ncol = 8)

plot(ts(rowMeans(m_5,na.rm=T)))

col_prom <- data.frame(rowMeans(m_1,na.rm=T),rowMeans(m_2,na.rm=T),rowMeans(m_3,na.rm=T),rowMeans(m_4,na.rm=T),
                       rowMeans(m_5,na.rm=T),rowMeans(m_6,na.rm=T),rowMeans(m_7,na.rm=T))
names(col_prom) <- c("lunes","martes","miercoles","jueves","viernes","sabado","domingo")

write.csv(col_prom,"output/promedios_colombia.csv")



