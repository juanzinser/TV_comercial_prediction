#Script IBOPE
setwd("~/linio-comercial")

# leer archivo de ibope ---------------------------------------------------

data_ibope_colombia<-read.table('chequeo agosto 25 Tv nal cable y city.csv',header=TRUE,sep=',')

names(data_ibope_colombia)


spots<-data_ibope_colombia[,c('Hora.Inicio','rat.porc.Prg','Canal','Duracion','Fecha')]
spots$Fecha<-as.factor(spots$Fecha)
spots$Hora.Inicio<-as.character(spots$Hora.Inicio)
spots$rat.porc.Prg<-as.numeric(spots$rat.porc.Prg)

for(i in 1:length(spots$Hora.Inicio)){ if(nchar(spots$Hora.Inicio[i],'chars')==5) {
  spots$Hora.Inicio[i]<-as.character(paste("0",as.character(spots$Hora.Inicio[i]),sep=""))}}
spots$Hora.Inicio

# cambiar el formato de la fecha ------------------------------------------

spots$tmstamp<-as.POSIXlt(paste(spots$Fecha,spots$Hora.Inicio),format="%Y%m%d %H%M%S")
spots<-spots[1:133,]
names(spots)
spots<-cbind(spots[,-5],spots[,5])
spots<-spots[,2:6]


# cambiar nombres de variables --------------------------------------------
names(spots)[2]<-paste("rating")
names(spots)[3]<-paste("channel")
names(spots)[4]<-paste("length")
names(spots)[5]<-paste("tmpstmp")
names(spots)[6]<-paste("cost")

# crear archivo csv -------------------------------------------------------

write.csv(spots,file='spots_ibope.csv')
save(spots,file='spots.rdata')

