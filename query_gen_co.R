# Query generator: ga visits colombia
library(lubridate)

# Inputs 

inicio <- as.Date("2014-06-30")
final <- as.Date("2014-08-24") 

seq <- seq(from = inicio, to=final,by = 1)
#paste(year(seq[20]),sprintf("%02d",month(seq[20])),sprintf("%02d",day(seq[20])),sep="")

query <- "SELECT date, hits.hour,hits.minute, visits, newvisits, transactions from"
p1 <- "(SELECT date, hits.hour,hits.minute, sum(totals.visits) visits, sum(totals.newVisits) newvisits, sum(totals.transactions) transactions FROM [golden-passkey-615:58093646.ga_sessions_"
p2 <- "] where trafficSource.source not like 'Postal%' and trafficSource.source not like 'Hermedia' and hits.time = 0 group by date, hits.hour, hits.minute)"

for(i in 1:length(seq)){
query <- paste(query,p1,year(seq[i]),sprintf("%02d",month(seq[i])),sprintf("%02d",day(seq[i])),p2,sep="")  
ifelse( i<length(seq) , query <- paste(query,",",sep="") , query <- paste(query," order by date,hits.hour, hits.minute;",sep=""))
}
