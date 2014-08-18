require('HoltWinters')
library('forecast')
install.packages('ggplot2')
library('ggplot2')
install.packages("fpp")
library(fpp)

setwd("~/linio-comercial")

ga_minut<-read.table('20140714_0810.csv',header=TRUE,sep=',')
ga_daily<-read.table('GA Daily Visits 7Jul-13Aug.csv',header=TRUE,sep=',')
ga_hourly<-read.csv('GA Hourly Visits 7Jul-13Aug.csv', sep=',')
wt_daily<-read.table('WebTrekk Daily Visits 14Aug - 13Aug.csv',header=TRUE,sep=',')

head(ga_minut)
head(ga_daily)
head(ga_hourly)
head(wt_daily)

#checar que tipo de datos son la fecha
ga_daily$Day.Index<-as.Date(ga_daily$Day.Index,format = "%m/%d/%Y")
wt_daily$Days<-as.Date(wt_daily$Days, format="%d/%m/%Y")
wt_daily<-wt_daily[which(wt_daily$Days>=as.Date("2014-07-07")),]

#convertir a numericas todas las de ga_hourly
ga_hourly$Sessions<-as.numeric(ga_hourly$Sessions)
head(is.numeric(ga_hourly$Sessions))

#checar que los datos de webtreck sean consistentes con los de ga
diferencia<-wt_daily[,3]-ga_daily[,2]
diferencia
wt_daily
ga_daily

#en termnos relativos
diferencia_r<-(wt_daily[,3]-ga_daily[,2])/wt_daily[,3]
diferencia_r

#hacer el analisis por hora y dia de la semana con ga_hourly
mat_sem<-matrix(ga_hourly$Sessions[1:912],ncol=24,byrow=TRUE)
mat_sem
mat_sem_df<-as.data.frame(mat_sem)

med_sem=c();
for (i in 1:24)
{
  med_sem=c(med_sem,mean(mat_sem_df[1:38,i]))
}
med_sem
qplot(1:24,med_sem,type='l',main='Promedio por hora del dia',xlab='hora del dia',ylab='sessions ga')

k=3
qplot(1:24*k,ga_hourly$Sessions[1:24*k],type='l')


#holt-winters double seasonal prediction method

ga_hrly<-ts(vh$visits,frequency=7*24,start=1)
hw_pred<-dshw(ga_hrly,24,24*7,alpha=NULL,beta=NULL,gamma=NULL,omega=NULL,phi=NULL,
     lambda=NULL, armethod=TRUE,model=NULL)

hw_pred
qplot(x=hw_pred$Forecasts[,1],y=hw_pred$Forecasts[,2],type='l')
plot(hw_pred$fitted,type='l',main='visitas por hora con dshw')
lines(c(1:length(v$visits))/(24*7),v$visits,col='blue',type='o')

#holt-winters double seasonal prediction method for minute

ga_min<-ts(v$visits,frequency=7*24*60,start=1)
hw_pred_min<-dshw(ga_min,24*60,24*60*7,alpha=NULL,beta=NULL,gamma=NULL,omega=NULL,phi=NULL,
              lambda=NULL, armethod=TRUE,model=NULL)

hw_pred_min
qplot(x=hw_pred_min$Forecasts[,1],y=hw_pred_min$Forecasts[,2],type='l')
plot(hw_pred_min$fitted,type='l',main='visitas por hora con dshw')
lines(c(1:length(v$visits))/(24*7),v$visits,col='blue',type='o')


#sacar el promedio de los 4 dias por dia de la semana
pred_min_prom=c();
min_semana<-60*24*7;
for(iw in 1:min_semana)
{
  4dias<-c();
  for (k in 1:4)
  {
    4dias<-c(4dias,v$visits[iw+min_semana*k]) 
  }
  pred_min_prom=c(pred_min_prom,mean(4dias))
}







