# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The data file is unzipped and read into a data frame.


```r
data <- read.csv(unzip("activity.zip"))
```

## What is mean total number of steps taken per day?

First, all missing values in the dataset are removed, and the missing factor levels in the subsetted data frame are dropped.  This ensures that only days with at least one non-missing value are taken into account.


```r
cleansedData <- na.omit(data)
cleansedData$date <- factor(cleansedData$date)
```

We then obtain the total number of steps taken each day.  This data is used to plot a histogram to visualize the results.


```r
stepsPerDay <- tapply(cleansedData$steps, cleansedData$date, FUN=sum)
hist(stepsPerDay,
     breaks = 10, 
     main="Histogram of Total Number of Steps Taken Per Day",
     xlab="Number of Steps Taken Per Day",
     ylab="Number of Days")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The median and mean total number of steps taken per day is computed.


```r
stepsPerDayMedian = median(stepsPerDay)
stepsPerDayMean = mean(stepsPerDay)
```

The median is **10765** and the mean is **1.0766 &times; 10<sup>4</sup>**.

## What is the average daily activity pattern?

The average number of steps taken, averaged across all days, across each 5-minute interval is computed.


```r
stepsPerInterval <- tapply(cleansedData$steps, cleansedData$interval, FUN=mean)
```

A time-series plot of the average number of steps taken at each 5-minute interval is drawn to visualise the results.


```r
plot(names(stepsPerInterval), stepsPerInterval, type="l",
     main="Time-Series Plot of Average Daily Activity Pattern",
     xlab="Interval",
     ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

We then find the 5-minute interval which contains the maximum number of steps on average across all days in the dataset.


```r
maxInterval <- names(which.max(stepsPerInterval))
```

The 5-minute interval starting at **835 mins** contains the maximum number of steps on average across all days in the dataset.

## Imputing missing values

The presence of missing days may introduce bias into some calculations or summaries of the data.  Thus, we will first compute the total number of missing values in the dataset.


```r
numMissingValues <- sum(is.na(data$steps))
```

There are a total of **2304** missing values in the dataset.

To reduce the bias introduced by the presence of missing days, we will fill in each missing value using the corresponding mean of the 5-minute interval.


```r
imputedData <- data
naIntervals <- imputedData$interval[is.na(imputedData$steps)]
stepsToImpute <- stepsPerInterval[as.character(naIntervals)]
imputedData$steps[is.na(imputedData$steps)] <- stepsToImpute
```

To evaluate how removing the missing data affects the results, we plot the histogram for the total number of steps taken each day and compute the median and mean using the same methodology as before.


```r
stepsPerDayImputed <- tapply(imputedData$steps, imputedData$date, FUN=sum)
hist(stepsPerDayImputed,
     breaks=10,
     main="Histogram of Total Number of Steps Taken Per Day",
     xlab="Number of Steps Taken Per Day",
     ylab="Number of Days")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
stepsPerDayMedianImputed = median(stepsPerDayImputed)
stepsPerDayMeanImputed = mean(stepsPerDayImputed)
```

The median is **1.0766 &times; 10<sup>4</sup>** and the mean is **1.0766 &times; 10<sup>4</sup>**.

Therefore, the plot, median and mean do not differ much from the estimates in the first part of this report.  Imputing the missing data has very little impact on the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

In order to surface any differences in activity patterns between weekdays and weekends,  new factor variable is created with two levels – “weekday” and “weekend” indicating whether a given date is a weekday day or a weekend day.


```r
imputedData$datetype <- factor(ifelse(weekdays(as.Date(data$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
```


The average number of steps taken, averaged across all days, across each 5-minute interval is computed separately for weekday days and weekend days.


```r
stepsPerIntervalWeekend <- tapply(imputedData$steps[imputedData$datetype=="weekend"], 
                                  imputedData$interval[imputedData$datetype=="weekend"], 
                                  FUN=mean)
stepsPerIntervalWeekday <- tapply(imputedData$steps[imputedData$datetype=="weekday"], 
                                  imputedData$interval[imputedData$datetype=="weekday"], 
                                  FUN=mean)
```

We then generate a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
attach(mtcars)
```

```
## The following objects are masked from mtcars (pos = 3):
## 
##     am, carb, cyl, disp, drat, gear, hp, mpg, qsec, vs, wt
```

```r
par(mfrow=c(2,1))
plot(names(stepsPerIntervalWeekend), stepsPerIntervalWeekend, type="l",
     main="weekend",
     xlab="Interval",
     ylab="Number of Steps")
plot(names(stepsPerIntervalWeekday), stepsPerIntervalWeekday, type="l",
     main="weekday",
     xlab="Interval",
     ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
