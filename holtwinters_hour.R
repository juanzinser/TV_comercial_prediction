#holt winters por hora con todo y procesamiento de datos
install.packages('lubridate')

library(lubridate)

setwd("~/comercial_TV")

## ------ PREPROCESSING O LECTURA-----------
  v <- read.csv("20140714_0810_hour.csv")
  tam <- length(v$visits)
  ndays <- tam/24 
  
  ## Date
  v$date <- as.POSIXct(as.character(v$date), format = "%Y%m%d")
  v$wday <- wday(v$date)

plot(v$visits, type='l')

xmat <- matrix(v$visits,nrow=24*7)
xmean <- rowMeans(xmat)
matplot(cbind(xmat,xmean), type = c('l','l','l','l',"b"),pch=1,col = c(2:5,1))
legend("topleft", legend = 1:4, col=2:5, pch=1)



# HoltWinters para una hora -----------------------------------------------
serie_4sem_hr<-v

ga_hr_visits<-ts(serie_4sem_hr$visits,frequency=7*24,start=1)
hw_pred_hr_visits<-dshw(ga_hr_visits,24,24*7,24*7,alpha=NULL,beta=NULL,gamma=NULL,omega=NULL,phi=NULL,
                            lambda=NULL, armethod=TRUE,model=NULL)

hw_pred_hr_visits
plot(hw_pred_hr_visits)
plot(hw_pred_hr_visits$mean,type='l',main='visitas por hora con dshw')

lines(c(1:length(back_v_proces_4_sem_15$visits))/(min_semana),v$visits,col='blue',type='o')



# lectura de datos por hora de la ultima semana ---------------------------
v <- read.csv("20140811_0817_hour.csv")
tam <- length(v$visits)
ndays <- tam/24 

## Date
v$date <- as.POSIXct(as.character(v$date), format = "%Y%m%d")
v$wday <- wday(v$date)


# ultima semana -----------------------------------------------------------

serie_ult_semana<-v
plot(forecast(hw_pred_hr_visits,h=24*7))
pred_ult_sem<-forecast(hw_pred_hr_visits,h=24*7)

pred_ult_sem$mean
pred_ult_sem$lower

# comparacion entre holtwinters y promedios con los reales de la ultima semana
dif_prom<-sum(abs(serie_ult_semana$visits-xmean))
dif_prom
dif_hw<-sum(abs(serie_ult_semana$visits-pred_ult_sem$mean))
dif_hw


# comparacion entre holtwinters y promedios con los reales de primeras 4 semanas
dif_prom_fit<-0
for (i in 1:4)
{
  dif_prom_fit<-dif_prom_fit+sum(abs(xmean-xmat[,i]))
}
dif_prom_fit

dif_hw_fit<-sum(abs(serie_4sem_hr$visits-hw_pred_hr_visits$fitted))
dif_hw_fit

#errores relativos respecto a visitas reales de una y dos semanas
err_rel_prom_fit<-(dif_prom_fit/(24*28))/mean(serie_4sem_hr$visits)
err_rel_prom_fit
err_rel_prom<-(dif_prom/(24*7))/mean(serie_ult_semana$visits)
err_rel_prom
err_rel_hw_fit<-(dif_hw_fit/(24*28))/mean(serie_4sem_hr$visits)
err_rel_hw_fit
err_rel_hw<-(dif_hw/(24*7))/mean(serie_ult_semana$visits)
err_rel_hw

plot(abs(serie_4sem_hr$visits-hw_pred_hr_visits$fitted))
plot(abs(serie_ult_semana$visits-pred_ult_sem$mean))
plot(x=1:164,y=pred_ult_sem$mean)
lines(serie_ult_semana$visits,col='blue')


#porcentaje de error por hora a traves de las 5 semanas

#para holtwinters
dif_rel_hr_hw<-c(abs(serie_4sem_hr$visits-hw_pred_hr_visits$fitted)/serie_4sem_hr$visits,
                 abs(serie_ult_semana$visits-pred_ult_sem$mean)/serie_ult_semana$visits)

plot(dif_rel_hr_hw,type='l')


#para los promedios
dif_rel_hr_prom<-c(abs(serie_4sem_hr$visits-rep(xmean,4))/serie_4sem_hr$visits,
                 abs(serie_ult_semana$visits-xmean)/serie_ult_semana$visits)

plot(dif_rel_hr_prom,type='l')










