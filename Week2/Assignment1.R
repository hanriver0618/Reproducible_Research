
## download & unzip file
if (!"data.zip" %in% dir("./")) {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
                      destfile = "data.zip")
        unzip("data.zip")
}

## Loading and preprocessing the data
df<-read.csv("activity.csv")
dim(df)
df$date<-(as.Date(df$date, format = "%Y-%m-%d"))
df1<-df[!is.na(df$steps),]
dim(df1)

## What is mean total number of steps taken per day?
library(plyr)
library(ggplot2)

final<-tapply(df1$steps,df1$date,sum)
hist(final,main="Total steps per day",xlab="Date")
mean(final,na.rm=TRUE)
median(final,na.rm=TRUE)

## What is the average daily activity pattern?
final2<-ddply(df1,.(interval),summarize,mean=mean(steps))
plot(final2$interval,final2$sum,type='l',main="Average daily active pattern",xlab="Interval",ylab="Average steps")
final2[final2$mean==max(final2$mean),]

## Imputing missing values
dim(df[is.na(df$steps),])
index<-is.na(df$steps)

for (i in final2$interval){
        index2<-df$interval[index]==i
        df$steps[index][index2]<-final2[final2$interval==i,]$mean
}
final3<-tapply(df$steps,df$date,sum)
hist(final3,main="Total steps per day",xlab="Date")
mean(final3,na.rm=TRUE)
median(final3,na.rm=TRUE)

## Are there differences in activity patterns between weekdays and weekends?
df$weekday<-weekdays(df$date)
final4<-ddply(df,.(interval,weekday),summarize,mean=mean(steps))

index3<-(final4$weekday=="Sunday" | final4$weekday=="Saturday")
index4<-(final4$weekday!="Sunday" & final4$weekday!="Saturday")

plot(final4$interval[index3],final4$mean[index3],type='l',col="blue",xlab="Interval",ylab="Average")
points(final4$interval[index4],final4$mean[index4],type='l',col="red")
title("Weekday vs. Weekend")
legend("topleft",legend=c("Weekend","Weekday"),pch=20,col=c("blue","red"))