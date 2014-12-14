---
title: "Reproducible Research Project 1"
author: "RR Student"
date: "Saturday, December 13, 2014"
output: html_document
---
#**Insights to Step Activity**

##Introduction

Activity monitoring devices are now becoming popular among sports enthusiasts.  This report summarizes data taken over a two month period (61 days) in late 2012.  The data samples are the total number of steps taken in each 5-minute increment of each day for the 61 days. This results in 288 observations per day (5 * 288 = 1440 minutes/day) and 17,568 observations over the 61 days.

These data are analyzed to find total steps, average total steps over each 5 minute increment (averaged over the 61 days), assessing the impact of missing data (NAs) by replacing those missing observations with the mean for the 5-minute increment, and comparing weekday versus weekend activity.

##Loading and Preprocessing the Data
The overall code for the analysis is as follows:
```{r, echo=TRUE}
# RR PROJECT - Step Exercise Data - Version 6
# Project Data:  https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
# Dataset: The variables included in this dataset are:
# steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# date: The date on which the measurement was taken in YYYY-MM-DD format
# interval: Identifier for the 5-minute interval in which measurement was taken (hour-min, HHMM)
# Tasks:  1) Loading and preprocessing the data, 2) What is mean total number of steps taken per day?
# 3) What is the average daily activity pattern?  4) Imputing missing values
# 5) Differences in activity patterns between weekdays and weekends?
#
    #  rm(list=ls())  # clears all variables in workspace

# set working directory, read data file
wrkdr <- "D:/MLO_Education_Coursera_8Sep14/JH_Reproducible Research/RR_Projects/RR_Project 1"  # Note "/"
setwd(wrkdr) # ; getwd() # checks for proper working directory for this project
actStepsOrig <- read.table(file="./repdata-data-activity/activity.csv", header = TRUE, sep = ",")  #; activity_steps
actSteps <- actStepsOrig  # copy data file  head(actSteps); dim(actStepsOrig); summary(actStepsOrig); str(actStepsOrig)

# adds index count (int_count) to avoid HHMM conversion to total minutes
actSteps$int_count <- c(1:288)  # converst HHMM from 5 min to integer index: 288 five-minute increments in a day
    #  dim(actSteps); summary(actSteps); str(actSteps)  # summarize file

#  Identify number of NAs and finds number of unique dates
sum(is.na(actSteps$steps))   # counts NA in steps column
sum(is.na(actSteps$steps)) * 100 / dim(actStepsOrig)[1]
    # actDate <- unique(actSteps$date); actDate; class(actDate)  # counts unique dates
```
There are 2304 missing data observations out of 17,568 observcations in the 61 days (NAs). That is approximately 13 per cent.

##What is the mean total number of steps taken per day?
```{r, echo=TRUE}
#  Q1: Histogram of total steps in each day. Find mean and median of total steps per day across all days
totalStepsDay <- aggregate(steps ~ date, data=actSteps, FUN=sum)   # finds total steps per day ;class(totalStepsDay)
plot(totalStepsDay$date, totalStepsDay$steps, type = "h", main = "Steps / Day Histogram - NA=0", xlab = "Date",
     ylab="Total Steps", sub="missing data equals zero")
summary(totalStepsDay)[3, 2]; summary(totalStepsDay)[4,2]  # finds mean and median of steps per day  head(totalStepsDay)
```
The total steps per day are shown in the first histogram Steps / Day Histogram - NA = 0 with missing data treated as zero steps
The median steps per day is 10765 with the mean steps per day of 10766
    
##What is the average daily acitivty pattern?
```{r, echo=TRUE}
# Q2:  Average Daily Activity Pattern - time series plot, time interval of maximum "average steps"
totalStepsInterval <- aggregate(steps ~ int_count + interval, data=actSteps, FUN=sum) # finds total steps per interval
meanStepsInterval <- aggregate(steps ~ int_count, data=actSteps, FUN=mean)  # finds mean steps / interval
plot(meanStepsInterval$int_count, meanStepsInterval$steps, type = "l", 
     main = "Average Steps per Interval", xlab="Interval", ylab="Average Steps",
     sub="288 5-minute invervals, 1440 minutes / day")
# total steps per interval
# plot(totalStepsInterval$interval, totalStepsInterval$steps, type = "l", 
#      main = "Total Steps per Interval", xlab="Interval", ylab="Total Steps",
#      sub="288 5-minute invervals, 1440 minutes / day")
# totalMeanStepsInterval[100:125, 2]  # looks at mean steps over intervals 100 to 125 

# Q2: maximum average steps; interval number, hour and minute
maxStepInterval <- with(meanStepsInterval, int_count[steps==max(steps)])  # finds maximum time interval
maxStepInterval; maxStepMin <- (maxStepInterval * 5) %% 60 ; maxStepHr <- (maxStepInterval * 5) %/% 60
c(maxStepHr, maxStepMin)
```
The average steps in each time interval is shown in "Average Steps per Interval" graph.  Again NAs are assumed zero.
The daily time at which the average of the time window over the 61 days is maximum is interval 104 which is 08:40.

##Inputing the missing values
```{r, echo=TRUE}
# Q3:  convert NAs to something - mean of that interval
actSteps1 <- actSteps # ; head(actSteps1)
actSteps1$means <- meanStepsInterval$steps  # ; head(actSteps1); dim(actSteps1)

# Q3: Assign mean of corresponding time interval (mean over all days) to each step == NA
for (i in 1:dim(actSteps1)[1])  
    if (is.na(actSteps1$steps[i])) 
        actSteps1$stepsNA[i] = actSteps1$means[i] else actSteps1$stepsNA[i] = actSteps1$steps[i]

# Q3: Histogram of total number of steps per day. Find mean and median of total steps per day across all days
totalStepsDay1 <- aggregate(stepsNA ~ date, data=actSteps1, FUN=sum)   # finds total steps per day
    # head(totalStepsDay1, 300)
plot(totalStepsDay1$date, totalStepsDay1$stepsNA, type = "h", main = "Steps / Day Histogram - NA=mean", 
     sub="NAs replaced by mean of that time interval", xlab = "Date", ylab="Total Steps")
summary(totalStepsDay1)[3, 2]; summary(totalStepsDay1)[4,2]  # finds mean and median of steps per day
```
The total steps per day are shown in the second histogram "Steps / Day Histogram - NA = mean" with NA replaced by the mean for that period.
The median steps per day is 10766 with the mean steps per day of 10766.

##Are there differences in activity patterns between weekdays and weekends?
```{r, echo=TRUE}
# Q4: Weekday versus Weekend Activity Differences
actSteps2 <- actSteps1
    # Identify weekdays and weekends and assign as new variable for each date entry
for (i in 1:dim(actSteps2)[1]) { 
    actSteps2$wkDay[i] <- weekdays(as.Date(actSteps2$date[i], format="%Y-%m-%d"))
    if(any(grepl(actSteps2$wkDay[i], c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")))) 
        actSteps2$day_type[i] <- "weekday" else actSteps2$day_type[i] <- "weekend"
    }
    # for (j in 1:61) print(c(j, actSteps2$day_type[1 + (j-1)*288]))  # confirms day assignment to weekday / weekend
    # head(actSteps2)  # confirms data frame structure

# Q4: Subsets by weekday or weekend.  Finds average steps in 
actSteps2wkD <- subset(actSteps2, day_type == "weekday") # ; str(actSteps2wkD); summary(actSteps2wkD); head(actSteps2wkD)
actSteps2wkE <- subset(actSteps2, day_type == "weekend") # ; str(actSteps2wkE); summary(actSteps2wkE); head(actSteps2wkE)
meanwkD <- aggregate(stepsNA ~ int_count, data=actSteps2wkD, FUN=mean) # ; head(meanwkD)
meanwkE <- aggregate(stepsNA ~ int_count, data=actSteps2wkE, FUN=mean) # ; head(meanwkE)

    # Two plots arranged in two rows
# par(mfrow=c(2,1)
plot(stepsNA ~ int_count, meanwkD, main="Average Steps over Weekdays", ylab="Average Steps",
     sub=NULL, type="l", asp=0.5)
plot(stepsNA ~ int_count, meanwkE, main="Average Steps over Weekends", xlab="Interval", ylab="Average Steps",
     sub="288 5-minute invervals, 1440 minutes / day", type="l", asp=0.5)
```

The final two-graph chart compares the average steps in each time interval between weekdays and weekends.  It is clear that the weekends have much more stepping activity than the weekdays. 


###Primary results are repeated as follows:

1.  There are 2304 missing data observations out of 17,568 observcations in the 61 days (NAs). That is approximately 13 per cent.
2.  The total steps per day are shown in the first histogram Steps / Day Histogram - NA = 0 with missing data treated as zero steps
    The median steps per day is 10765 with the mean steps per day of 10766
3.  The average steps in each time interval is shown in "Average Steps per Interval" graph.  Again NAs are assumed zero.
    The daily time at which the average of the time window over the 61 days is maximum is interval 104 which is 08:40.
4.  The total steps per day are shown in the second histogram "Steps / Day Histogram - NA = mean" with NA replaced by the mean for that period.
    The median steps per day is 10766 with the mean steps per day of 10766.
5.  The final two-graph chart compares the average steps in each time interval between weekdays and weekends.  It is clear that the weekends have much more stepping activity than the weekdays. 

