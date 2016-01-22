#---------------------------------------------------------------------------------------------------------
#Reproducible Research,Assignment 1
#---------------------------------------------------------------------------------------------------------
setwd('E:/online courses/reproducible research/assignment')
activity <- read.csv('activity.csv')

#data cleaning
activity$date <- as.Date(activity$date)
activity$steps <- as.numeric(activity$steps)

#---------------------------------------------------------------------------------------------------------
#mean total number of steps taken per day
#---------------------------------------------------------------------------------------------------------
aggdata <- aggregate(activity[,-2],by =list(activity$date), FUN=sum,na.rm=TRUE)
names(aggdata) <- c("id","steps","min")
summary(aggdata$steps)
library(ggplot2)
h <- ggplot(aggdata,aes(x=steps))
h + geom_histogram(binwidth =1000, fill = "steelblue", colour = "black")

mean_steps <- mean(aggdata$steps,na.rm=TRUE)
median_steps <- median(aggdata$steps,na.rm=TRUE)
print(paste("The mean total number of steps taken per day is",as.character(mean_steps),"steps."))
print(paste("The median total number of steps taken per day is",as.character(median_steps),"steps."))

#---------------------------------------------------------------------------------------------------------
#average daily activity pattern
#---------------------------------------------------------------------------------------------------------
agg_mean <- aggregate(activity[,-2],by = list(activity$interval), FUN=mean,na.rm=TRUE)
l <- ggplot(agg_mean,aes(x=interval,y=steps))
l + geom_line(color='steelblue')

max_interval <- agg_mean$interval[which.max(agg_mean$steps)]
print(paste("The",as.character(max_interval),"th 5-minute interval contains the maximum number of steps."))

#---------------------------------------------------------------------------------------------------------
#imputing missing values
#---------------------------------------------------------------------------------------------------------
step_na <- is.na(activity$steps)
na_num <- sum(as.numeric(step_na))
print(paste("There are",as.character(na_num),"missing values in steps."))

newdata <- cbind(activity,step_na)
summary(unclass(newdata$date))
for (i in 15614:15674){
  for (j in seq(0,2355,5)){
    newdata$steps[unclass(newdata$date)==i & newdata$interval==j & newdata$step_na[unclass(newdata$date)==i & newdata$interval==j]] = agg_mean$steps[agg_mean$interval==j]
  }
}

aggdata2 <- aggregate(newdata[,-4][,-2],by =list(newdata$date), FUN=sum,na.rm=TRUE)
h2 <- ggplot(aggdata2,aes(x=steps))
h2 + geom_histogram(binwidth =1000, fill = "steelblue", colour = "black")

mean_steps2 <- mean(aggdata2$steps,na.rm=TRUE)
median_steps2 <- median(aggdata2$steps,na.rm=TRUE)
print(paste("The mean total number of steps taken per day is",as.character(mean_steps2),"steps."))
print(paste("The median total number of steps taken per day is",as.character(median_steps2),"steps."))

#---------------------------------------------------------------------------------------------------------
#weekdays and weekends
#---------------------------------------------------------------------------------------------------------
isweekends <- weekdays(activity$date)==weekdays(as.Date("2016-01-23")) |weekdays(activity$date)==weekdays(as.Date("2016-01-24"))
newdata2 <- cbind(newdata,isweekends)
agg_mean2 <- aggregate(newdata2[,-2],by = list(newdata2$interval,newdata2$isweekends), FUN=mean,na.rm=TRUE)
Weekdays <- agg_mean2$Group.2
Weekdays[agg_mean2$Group.2] <- "weekends"
Weekdays[!agg_mean2$Group.2] <- "weekdays"
agg_mean2 <- cbind(agg_mean2,Weekdays)
l2 <- ggplot(agg_mean2,aes(x=interval,y=steps,color=Weekdays))
l2 + geom_line() + facet_grid(Weekdays ~ .)
