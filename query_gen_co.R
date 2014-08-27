# Query generator: ga visits colombia
library(lubridate)

# Inputs 

inicio <- as.Date("2014-06-30")
final <- as.Date("2014-08-24") 

seq <- seq(from = inicio, to=final,by = 1)
#paste(year(seq[20]),sprintf("%02d",month(seq[20])),sprintf("%02d",day(seq[20])),sep="")

query <- "SELECT date, hits.hour,hits.minute, visits, newvisits, transactions from"
query_dia <- "SELECT date, visits, newvisits, transactions from"
query_brand <- "SELECT date as fecha, hits.hour as hora,hits.minute as minute, visits as brandvisits, newvisits, transactions from"

p1 <- "(SELECT date, hits.hour,hits.minute, sum(totals.visits) visits, sum(totals.newVisits) newvisits, sum(totals.transactions) transactions FROM [golden-passkey-615:58093646.ga_sessions_"
p1_dia <- "(SELECT date, sum(totals.visits) visits, sum(totals.newVisits) newvisits, sum(totals.transactions) transactions FROM [golden-passkey-615:58093646.ga_sessions_"
p1_brand <- "(SELECT date, hits.hour,hits.minute, sum(totals.visits) visits, sum(totals.newVisits) newvisits, sum(totals.transactions) transactions FROM [golden-passkey-615:58093646.ga_sessions_"

p2 <- "] where trafficSource.source not like 'Postal%' and trafficSource.source not like 'Hermedia' and trafficSource.source not like 'Ingenious' and hits.time = 0 group by date, hits.hour, hits.minute)"
p2_dia <- "] where trafficSource.source not like 'Postal%' and trafficSource.source not like 'Hermedia' and trafficSource.source not like 'Ingenious' and hits.time = 0 group by date)"
p2_brand <- "] where trafficSource.source not like 'Postal%' and trafficSource.source not like 'Hermedia' and trafficSource.source not like 'Ingenious' and (trafficSource.medium like '%organic%' or trafficSource.source like '%(direct)%' or trafficSource.campaign like '%brand%') and hits.time = 0 group by date, hits.hour, hits.minute)"


for(i in 1:length(seq)){
query <- paste(query,p1,year(seq[i]),sprintf("%02d",month(seq[i])),sprintf("%02d",day(seq[i])),p2,sep="") 
query_dia <- paste(query_dia,p1_dia,year(seq[i]),sprintf("%02d",month(seq[i])),sprintf("%02d",day(seq[i])),p2_dia,sep="") 
query_brand <- paste(query_brand,p1_brand,year(seq[i]),sprintf("%02d",month(seq[i])),sprintf("%02d",day(seq[i])),p2_brand,sep="") 

ifelse( i<length(seq) , query <- paste(query,",",sep="") , query <- paste(query," order by date,hits.hour, hits.minute;",sep=""))
ifelse( i<length(seq) , query_dia <- paste(query_dia,",",sep="") , query_dia <- paste(query_dia," order by date;",sep=""))
ifelse( i<length(seq) , query_brand <- paste(query_brand,",",sep="") , query_brand <- paste(query_brand," order by date,hits.hour, hits.minute;",sep=""))
}

# Crear tabla full

# SELECT date, hits_hour,hits_minute, visits, t1.newvisits newvisits, t1.transactions transactions, t2.brandvisits as brandvisits
# from [golden-passkey-615:export.col_30jun_24ago_act] as t1
# join [golden-passkey-615:export.col_30jun_24ago_brand] as t2
# on t1.date = t2.fecha and t1.hits_hour = t2.hora and t1.hits_minute = t2.minute
# order by date,hits_hour, hits_minute;
