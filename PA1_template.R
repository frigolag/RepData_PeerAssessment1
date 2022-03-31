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

## What is mean total number of steps taken per day?
## Load packages
library(tidyverse)
mstep<-dataset%>%group_by(date)%>%summarise(mean=mean(steps),n=n())
hist(mstep$mean)
stpday<- with(dataset,tapply(steps, date, mean))

hist<-ggplot(data=dataset)
hist+aes(mean(steps))
