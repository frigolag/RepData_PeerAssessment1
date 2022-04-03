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
df<-read_csv(file)
## 2.Process data (remove NA's)
df<-na.omit(df)

## What is mean total number of steps taken per day?
## 1.Make a histogram of the total number of steps taken each day
stepxday<-df%>%group_by(date)%>%summarise(steps=sum(steps))
g_stepxday<- ggplot(stepxday, aes(date,steps)) + geom_col()
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

