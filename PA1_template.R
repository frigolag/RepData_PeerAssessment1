## Loading and preprocessing the data
## Download data and unzip
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipfile<- "repdata_data_activity.zip"
file<- "activity.csv"
if (!file.exists(zipfile)){
        download.file(url, zipfile, mode = "wb")
}
if (!file.exists(file)){
        unzip(zipfile)
}

## 1.Load the data
library(tidyverse)
dataset<-read.csv(file)
## 2.Process data (remove NA's)
df<-na.omit(dataset)

## What is mean total number of steps taken per day?
## 1.Make a histogram of the total number of steps taken each day
stepxday<-df%>%group_by(date)%>%summarise(steps=sum(steps))
g_stepxday<- ggplot(stepxday, aes(date,steps)) + geom_col()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
print(g_stepxday)
## 2.Calculate and report the mean and median total number of steps taken per day
mean(stepxday$steps)
median(stepxday$steps)

## What is the average daily activity pattern?
## 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
stepxint<-with(df,tapply(steps, interval, mean))
stepxint<-as.tibble(as.data.frame(as.table(stepxint)))
colnames(stepxint)<-c("interval","mean_steps")
g_stepxint<-ggplot(stepxint,aes(x=interval,y=mean_steps,group=1))+geom_line()
print(g_stepxint)

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_stp_int<-stepxint%>%arrange(desc(mean_steps))
max_stp_int[1,]

## Imputing missing values
##1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs
sum(is.na(dataset))
##2.Filling in all of the missing values in the dataset: the mean of that 5-minute interval
narows<-is.na(dataset$steps)
df2<-dataset
for (i in which(narows==TRUE)){
        int<- dataset[i,3]
        mean_steps<-stepxint[stepxint$interval==int,2]
        df2[i,1]<-mean_steps
}
## 4.Make a histogram of the total number of steps taken each day
stepxday2<-df2%>%group_by(date)%>%summarise(steps=sum(steps))
g_stepxday2<- ggplot(stepxday2, aes(date,steps)) + geom_col()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
print(g_stepxday2)
## 4.Calculate and report the mean and median total number of steps taken per day
mean(stepxday2$steps)
median(stepxday2$steps)

##Are there differences in activity patterns between weekdays and weekends?
library(gridExtra)
grid.arrange(g_stepxday,g_stepxday2,ncol=2)
