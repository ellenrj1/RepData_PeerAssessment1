# PA1_template.Rmd


## Loading and Preprocessing the Data

1. Load the data (i.e. read.csv())


```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) 
  {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
  }

data = read.csv("activity.csv", sep=",", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(lubridate)
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
data[,2] = ymd(data[,2])
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
fulldata = na.omit(data)
aggdata = aggregate(steps ~ date, FUN = sum, data =data)
aggsteps = aggdata$steps
names(aggsteps) = aggdata$date 
```

2. Make a histogram of the total number of steps taken each day


```r
hist(aggsteps, xlab = "Number of Steps per Day", main = "Histogram of Average Total Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
averagesteps = mean(aggsteps)
averagesteps
```

```
## [1] 10766.19
```


```r
mediansteps = median(aggsteps)
mediansteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
aggintdata = aggregate(steps ~ interval, FUN = mean, data = data)
plot(aggintdata$interval, aggintdata$steps, type = "l",
     main = "Time Series: Average Number of Steps", 
     xlab = "5-minute interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
indexofmax = which.max(aggintdata$steps)
intervalofmax = aggintdata[indexofmax,1]
```

## Inputting Missing Values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
naents = nrow(data) - nrow(fulldata)
naents
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I'm going to fill the missing values with the average for that 5-minute interval. I already have a dataframe with the averages per interval, so I just have to match them up.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filleddata = data
filleddata[,2] = ymd(filleddata[,2])
for(i in 1:nrow(filleddata))
{
  if(is.na(filleddata[i,1]))
  {
    theint = filleddata[i,3]
    indexofint = match(theint, aggintdata$interval)
    stepsofint = aggintdata[indexofint,2]
    filleddata[i,1] = stepsofint
  }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
aggdata2 = aggregate(steps ~ date, FUN = sum, data = filleddata)
aggsteps2 = aggdata2$steps
names(aggsteps2) = aggdata2$date 
hist(aggsteps2, xlab = "Number of Steps per Day", main = "Histogram of Average Total Number of Steps: Filled")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Mean and median:

```r
averagesteps2 = mean(aggsteps2)
averagesteps2
```

```
## [1] 10766.19
```

```r
mediansteps2 = median(aggsteps2)
mediansteps2
```

```
## [1] 10766.19
```

These values have the same trend, but more biased towards the middle. This makes sense, since I filled it with averages.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
filleddata$weekend = weekdays(filleddata[,2])
for(i in 1:nrow(filleddata))
{
  if(filleddata[i,4] == "Saturday" | filleddata[i,4] == "Sunday")
  {
    filleddata[i,4] = "Weekend"
  }
  else
  {
    filleddata[i,4] = "Weekday"
  }
}

filledlist = split(filleddata, filleddata$weekend)
weekdaydata = data.frame(filledlist[1])
weekenddata = data.frame(filledlist[2])

aggweekend = aggregate(Weekend.steps ~ Weekend.interval, FUN = mean, data = weekenddata)
aggweekday = aggregate(Weekday.steps ~ Weekday.interval, FUN = mean, data = weekdaydata)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
par(mfcol = c(1,2))

plot(aggweekend$Weekend.interval, aggweekend$Weekend.steps, type = "l",
     xlab = "5-minute interval", ylab = "Average number of steps", main = "Weekends")
plot(aggweekday$Weekday.interval, aggweekday$Weekday.steps, type = "l",
     xlab = "5-minute interval", ylab = "Average number of steps", main = "Weekdays")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
