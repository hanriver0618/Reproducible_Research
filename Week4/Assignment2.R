
## download & unzip file
if (!"data.csv.bz2" %in% dir("./")) {
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "data/stormData.csv.bz2")
        bunzip2("data/data.csv.bz2", overwrite=T, remove=F)
}

## read file
df<-read.csv("data.csv")
dim(df)

## extract year information
df$year<-as.numeric(format(as.Date(df$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
hist(df$year)

## More recent years should be considered more complete. -->1990-2011
index<-df$year>=1990
final<-df[index,]
dim(final)

## 1. Across the United States, which types of events (as indicated in the 𝙴𝚅𝚃𝚈𝙿𝙴 vari
## able) are most harmful with respect to population health?
library(plyr)
library(ggplot2)
fat<-ddply(final,.(EVTYPE),summarize,sum=sum(FATALITIES))
fat_order<-fat[order(fat$sum,decreasing = TRUE),]
fat_order<-head(fat_order[fat_order$sum!=0,],10)
fat_order$EVTYPE <- factor(fat_order$EVTYPE, levels = fat_order$EVTYPE[order(fat_order$sum,decreasing = TRUE)])
ggplot(fat_order,aes(EVTYPE,sum))+geom_bar(stat="identity")

inj<-ddply(final,.(EVTYPE),summarize,sum=sum(INJURIES))
inj_order<-inj[order(inj$sum,decreasing = TRUE),]
inj_order<-head(inj_order[inj_order$sum!=0,],10)
inj_order$EVTYPE <- factor(inj_order$EVTYPE, levels = inj_order$EVTYPE[order(inj_order$sum,decreasing = TRUE)])
ggplot(inj_order,aes(EVTYPE,sum))+geom_bar(stat="identity")


## 2. Across the United States, which types of events have the greatest economic 
## consequences?

index1<-final$PROPDMGEXP=="H"
index2<-final$PROPDMGEXP=="K"
index3<-final$PROPDMGEXP=="M"
index4<-final$PROPDMGEXP=="B"

final[index1,]$PROPDMG<-final[index1,]$PROPDMG*10^2
final[index2,]$PROPDMG<-final[index2,]$PROPDMG*10^3
final[index3,]$PROPDMG<-final[index3,]$PROPDMG*10^6
final[index4,]$PROPDMG<-final[index4,]$PROPDMG*10^9

index1<-final$CROPDMGEXP=="H"
index2<-final$CROPDMGEXP=="K"
index3<-final$CROPDMGEXP=="M"
index4<-final$CROPDMGEXP=="B"

final[index1,]$CROPDMG<-final[index1,]$CROPDMG*10^2
final[index2,]$CROPDMG<-final[index2,]$CROPDMG*10^3
final[index3,]$CROPDMG<-final[index3,]$CROPDMG*10^6
final[index4,]$CROPDMG<-final[index4,]$CROPDMG*10^9

pro<-ddply(final,.(EVTYPE),summarize,sum=sum(PROPDMG))
pro_order<-pro[order(pro$sum,decreasing = TRUE),]
pro_order<-head(pro_order[pro_order$sum!=0,],10)
pro_order$EVTYPE <- factor(pro_order$EVTYPE, levels = pro_order$EVTYPE[order(pro_order$sum,decreasing = TRUE)])
ggplot(pro_order,aes(EVTYPE,sum))+geom_bar(stat="identity")


cro<-ddply(final,.(EVTYPE),summarize,sum=sum(CROPDMG))
cro_order<-cro[order(cro$sum,decreasing = TRUE),]
cro_order<-head(cro_order[cro_order$sum!=0,],10)
cro_order$EVTYPE <- factor(cro_order$EVTYPE, levels = cro_order$EVTYPE[order(cro_order$sum,decreasing = TRUE)])
ggplot(cro_order,aes(EVTYPE,sum))+geom_bar(stat="identity")
