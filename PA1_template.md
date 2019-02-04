---
title: "Reproducible Research: Peer Assessment 1"
author: "dp"
date: "3 de febrero de 2019"
output: html_document
keep_md: true
---
Loading and preprocessing the data
-----------------------------------

Before starting with the loading and information processing stage, it is necessary to load certain packages for the correct operation, including dplyr, ggplot2.

```r
library(dplyr)
library(ggplot2)
library(lubridate)
library(knitr)
```
The first step in order to perform the assigned task is to load the previously downloaded data, this is done with the following command:

```r
setwd("E:/DataScience/assigment")
activity <- read.csv("activity.csv")
```
* Process the data into a format suitable for your analysis

```r
r_steps <- activity%>%select(date,steps)%>%
    group_by(date)%>%summarize(Total=sum(steps,na.rm=TRUE))%>%
    filter(Total>0)
```

What is mean total number of steps taken per day?
--------------------------------------------------

To know the total of steps that were carried out in the different days, a table and a histogram were generated where the quantity per day and the concentration of the values can be identified.

```r
m_steps <- activity%>%select(date,steps)%>%
    group_by(date)%>%summarize(Medio=mean(steps,na.rm = TRUE))%>%
    filter(Medio!="NaN")
m_steps$date <- ymd(m_steps$date)
tapply(r_steps$Total,r_steps$date,sum,na.rm=TRUE)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

```r
hist(r_steps$Total)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

* Calculate and report the **mean** and **median** total number of steps taken 
per day 

To obtain these values, the calculation is made and stored in variables that are then presented.

```r
me <- mean(r_steps$Total)
med <- median(r_steps$Total)
```
* The **mean** total number of steps taken per day is 
    1.0766189 &times; 10<sup>4</sup> steps.
* The **median** total number of steps taken per day is 
    10765 steps.
    
What is the average daily activity pattern?
--------------------------------------------

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsInterval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
plot(steps~interval,data=stepsInterval,type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 

```r
stepsInterval[which.max(stepsInterval$steps),]$interval
```

```
## [1] 835
```

It is the **835th** interval.

Imputing missing values
-----------------------

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
num_per <- sum(is.na(activity$steps))
```
Total 2304 rows are missing.

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy to be used is to fill in the missing data with the average of the day and to be able to complete all the data. 

```r
interval2steps<-function(interval){
    stepsInterval[stepsInterval$interval==interval,]$steps
}
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityFilled<-activity 
count=0           
for(i in 1:nrow(activityFilled)){
    if(is.na(activityFilled[i,]$steps)){
        activityFilled[i,]$steps<-interval2steps(activityFilled[i,]$interval)
        count=count+1
    }
}
cat("Total ",count, "NA values were filled.\n\r")  
```

```
## Total  2304 NA values were filled.
## 
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
totalSteps2<-aggregate(steps~date,data=activityFilled,sum)
hist(totalSteps2$steps)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

```r
me2 <- mean(totalSteps2$steps)
med2 <- median(totalSteps2$steps)
```
* The **mean** total number of steps taken per day is 
1.0766189 &times; 10<sup>4</sup> steps.
* The **median** total number of steps taken per day is 
1.0766189 &times; 10<sup>4</sup> steps.

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The **mean** value is the **same** as the value before imputing missing data because we put the mean value for that particular 5-min interval. The median value shows **a little** difference : but it depends on **where the missing values are**.

Are there differences in activity patterns between weekdays and weekends?
---------------------------------------------------------------------------

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
activityFilled$day=ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6==0,
                          "weekend","weekday")
# For Sunday and Saturday : weekend, Other days : weekday 
activityFilled$day=factor(activityFilled$day,levels=c("weekday","weekend"))
```


* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
stepsInterval2=aggregate(steps~interval+day,activityFilled,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=stepsInterval2,aspect=1/2,type="l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

knit2html("PA1_template.Rmd")
