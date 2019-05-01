---
title: "Reproducible Research: Peer Assessment 1"

output: 

  html_document:

    keep_md: true

---
1.loading and preprossessing the data

```r
activity <- read.csv("activity.csv", head = TRUE, colClasses =c("integer", "character", "integer"), na.strings="NA")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

change the format of date from character to date 

```r
activity$date <- as.Date(activity$date)
## subset and keep only rows with non "NA" steps
activity2 <- subset(activity, !is.na(activity$steps))
```

2.what is the mean total number of steps taken per day
create a histogram of the total no of steps taken every day

```r
sumofsteps <- tapply(activity2$steps, activity2$date, sum, na.rm=TRUE, simplify=T)
sumofsteps <- sumofsteps[!is.na(sumofsteps)]
```


```r
hist(x=sumofsteps,
     main="Histogram - Number of Steps Taken Each Day",
     xlab="Number of Steps Daily",
     ylab="Frequency",
     col="blue",
     breaks=10)
```

![](PA1_template_-_Copy_files/figure-html/plot1-1.png)<!-- -->

3.calculate and report the mean and median no of steps taken per day

```r
mean(sumofsteps)
```

```
## [1] 10766.19
```

```r
median(sumofsteps)
```

```
## [1] 10765
```

what is the average daily activity pattern
3.calculate and report the mean and median no of steps taken per day

```r
mean(sumofsteps)
```

```
## [1] 10766.19
```

```r
median(sumofsteps)
```

```
## [1] 10765
```
what is the average daily activity pattern
3.calculate and report the mean and median no of steps taken per day

```r
avestep <- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify=T)
avestep2 <- data.frame(interval=as.integer(names(avestep)), ave=avestep)
```


```r
with(avestep2,
     plot(interval,
          ave,
          type="l",
          xlab="intervals",
          ylab="average num of steps"))
```

![](PA1_template_-_Copy_files/figure-html/plot2-1.png)<!-- -->



