---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(utils)
library(lattice)
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.6.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
knitr::opts_chunk$set(echo = TRUE)

unzip(zipfile = "activity.zip", exdir = "./foo")
readCsv <- read.csv("./foo/activity.csv", sep = ",", stringsAsFactors = F)
readCsv$date <- ymd(readCsv$date)
```


## What is mean total number of steps taken per day?

```r
stepDay <- aggregate(steps ~ date, readCsv, sum, na.action = na.omit)
hist(stepDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
sprintf("Mean of steps per day: %f, Median of steps per day: %f", mean(stepDay$steps), median(stepDay$steps))
```

```
## [1] "Mean of steps per day: 10766.188679, Median of steps per day: 10765.000000"
```


## What is the average daily activity pattern?

```r
stepInterval <- aggregate(steps ~ interval, readCsv, mean, na.action = na.omit)
plot(steps ~ interval, data = stepInterval, type = 'l')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
sprintf("The interval which has maximum steps : %d", stepInterval$interval[which(stepInterval$steps == max(stepInterval$steps))])
```

```
## [1] "The interval which has maximum steps : 835"
```


## Imputing missing values

```r
sprintf("The number of observations which have missing values : %d", 
sum(!complete.cases(readCsv)))
```

```
## [1] "The number of observations which have missing values : 2304"
```

```r
naFilled <- data.frame(readCsv)
naFilled$steps[is.na(naFilled$steps)] <- mean(naFilled$steps, na.rm = TRUE) # Fill with average total steps
stepDay2 <- aggregate(steps ~ date, naFilled, sum)
hist(stepDay2$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
sprintf("Mean of steps per day: %f, Median of steps per day: %f", mean(stepDay2$steps), median(stepDay2$steps))
```

```
## [1] "Mean of steps per day: 10766.188679, Median of steps per day: 10766.188679"
```

```r
print("The result is differ from the first part of the assignment. Latter has larger frequency at middle rank.")
```

```
## [1] "The result is differ from the first part of the assignment. Latter has larger frequency at middle rank."
```


## Are there differences in activity patterns between weekdays and weekends?

```r
weekendWeekday <- ifelse(wday(naFilled$date) == 1 | wday(naFilled$date) == 7, 'weekend', 'weekday')
weekendWeekday <- as.factor(weekendWeekday)
naFilled2 <- cbind(naFilled, weekendWeekday)

stepInterval2 <- aggregate(steps ~ interval + weekendWeekday, naFilled2, mean)
xyplot(steps ~ interval | weekendWeekday, data = stepInterval2, layout = c(1,2), type = 'l')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# plot(steps ~ interval + weekendWeekday, data = stepInterval2, type = 'l')
```
