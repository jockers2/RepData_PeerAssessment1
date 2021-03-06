# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
# Fetch the data file and unzip it into the './data' directory (if required).
# Then read the 'activity.csv' file.  


fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
localFilename <- "activity.zip"

if (!file.exists(localFilename)) {
    download.file(fileUrl, destfile = localFilename, mode = "wb")
} 

dataFileList <- unzip(localFilename,list = TRUE)

if (!file.exists("data")) {  
  unzip(localFilename, overwrite=FALSE, exdir="data")
}

activityData <- read.csv("./data/activity.csv")
```

## What is mean, median total number of steps taken per day?


```r
# 1. Summarize the activity data by day using ddply (see package plyr)

require(plyr)
summ1 <- ddply(activityData, "date", summarize,
               totalSteps = sum(steps),
               medianSteps = median(steps, na.rm=TRUE))

# 2. Plot a histogram of the *total* steps per day

hist(summ1$totalSteps, 
     xlab = "Steps",
     main = "Total Steps per Day")
```

![plot of chunk dailysummary](figure/dailysummary.png) 

```r
# 3. Report the mean and median total steps per day (ignoring missing values)

dailyMean <- mean(summ1$totalSteps, na.rm=TRUE)
dailyMedian <- median(summ1$totalSteps, na.rm=TRUE)
print(paste("The mean number of total steps in a day: ",
            dailyMean))
```

```
## [1] "The mean number of total steps in a day:  10766.1886792453"
```

```r
print(paste("The median number of total steps in a day: ",
            dailyMedian))
```

```
## [1] "The median number of total steps in a day:  10765"
```

## What is the average daily activity pattern?



```r
# 1. Summarize the activity data by interval using ddply

summ2 <- ddply(activityData, "interval", summarize,
                meanSteps = mean(steps, na.rm=TRUE),
                medianSteps = median(steps, na.rm=TRUE))
summ2$interval <- as.factor(summ2$interval)

# 2. Output a time series plot of average number of steps taken vs. interval

with(summ2,
     plot(as.character(interval), meanSteps, type = "l",
          xlab = "Interval", ylab = "mean steps",
          main = "Average Daily Activity Pattern"))
```

![plot of chunk dailyaverage](figure/dailyaverage.png) 

```r
# 3. Report the 5-minute interval containing on average the max number of steps

peakInterval <- summ2$interval[order(summ2$meanSteps, decreasing=TRUE)[1]]
print(paste("The interval with the maximum average number of steps: ", peakInterval))
```

```
## [1] "The interval with the maximum average number of steps:  835"
```

## Imputing missing values


```r
# 1. Find and report number of missing values in activity data

miss <- is.na(activityData$steps)
print(paste(sum(miss), " missing values in data set"))
```

```
## [1] "2304  missing values in data set"
```

```r
# 2. fill in missing data with simple strategy:

# First check if there are insufficient past points to fill in the data using
# a moving average. In which case use the mean value for that time interval,
# else fill in the missing data point with the mean of the preceeding values
# within a specified window size.
#
# The rationale used is that if they day shows cyclical patterns (for example,
# high level of activity in the morning and very low activity in the evening)
# by using a moving average a missing value will be similar to the activity of
# the nearest past neighbors.
#
# Note that the floor() function is used on the mean values to keep number of
# steps an integer value

meanWindow = 12
missIndexes <- which(miss)
mungedActivityData <- activityData
replaceByMean <- rep(FALSE, length(missIndexes))
replaceByMovingAverage <- rep(FALSE, length(missIndexes))
for (j in 1:length(missIndexes)) {
    i <- missIndexes[j]
    countMissPast <- sum(missIndexes >= i-meanWindow & missIndexes <= i-1)
    if ((i < meanWindow) || (countMissPast > 0)) {
        mungedActivityData$steps[i] <- with(summ2, floor(
                meanSteps[interval == as.character(mungedActivityData$interval[i])]))
        replaceByMean[j] <- TRUE
    } else {
        mungedActivityData$steps[i] <- floor(
                mean(mungedActivityData$steps[(i-meanWindow):(i-1)]))
        replaceByMovingAverage[j] <- TRUE
    }
}
print(paste(sum(replaceByMean),
            " missing values replaced using mean at corresponding interval"))
```

```
## [1] "2298  missing values replaced using mean at corresponding interval"
```

```r
print(paste(sum(replaceByMovingAverage),
            " missing values replaced using moving average of past ",
            meanWindow, " data points"))
```

```
## [1] "6  missing values replaced using moving average of past  12  data points"
```

## What is mean, median total number of steps taken per day after filling missing values?


```r
# 1. Summarize the new activity data set by day using ddply

require(plyr)
summ4 <- ddply(mungedActivityData, "date", summarize,
               totalSteps = sum(steps),
               medianSteps = median(steps, na.rm=TRUE))

# 2. Plot a histogram of the *total* steps per day

hist(summ4$totalSteps, 
     xlab = "Steps",
     main = "Total Steps per Day (after filling missing values)")
```

![plot of chunk newdailysummary](figure/newdailysummary.png) 

```r
# 3. Report the mean and median total steps per day (ignoring missing values)

dailyMean <- mean(summ4$totalSteps, na.rm=TRUE)
dailyMedian <- median(summ4$totalSteps, na.rm=TRUE)
print(paste("The mean number of total steps in a day: ",
            dailyMean))
```

```
## [1] "The mean number of total steps in a day:  10749.6721311475"
```

```r
print(paste("The median number of total steps in a day: ",
            dailyMedian))
```

```
## [1] "The median number of total steps in a day:  10640"
```

Using the median to fill in the data was of no use: run a summary on
`activityData' and it is easy to see that the median steps will be 0. The
histogram shows higher frequency near the center of the data (note the y-axis
label of 35 instead of 25). The mean number is fairly similar, while the median
number has dropped more noticeably.  

The choice of `ceiling()` or `floor()` can have a pretty dramatic effect
on the results. The results are very close after skipping those functions when
filling in missing values.  Rounding down seems more reasonable as it tends to
lower the the mean activity level which is consistent with the assuming the
activity level is sedentary. It is pretty surprising the frequency of missing
values that appear and how very few of the missing data points were able to
exploit the use of a moving average.

## Are there differences in activity patterns between weekdays and weekends?


```r
# add factor column to identify weekdays vs. weekends. Note that when performing
# the substitution, R capitalizes the first letter in the character string

mungedActivityData$date <- as.Date(mungedActivityData$date)
mungedActivityData$dayType <- weekdays(mungedActivityData$date)
mungedActivityData$dayType <- gsub("Saturday|Sunday","weekend",mungedActivityData$dayType)
wkdyIndices <- grep("[Ww]eekend", mungedActivityData$dayType, invert = TRUE)
mungedActivityData$dayType[wkdyIndices] <- "weekday"

require(plyr)
summ5 <- ddply(mungedActivityData, c("interval","dayType") , summarize,
               totalSteps = sum(steps),
               medianSteps = median(steps),
               meanSteps = mean(steps))

require(lattice)
plt5 <- xyplot(meanSteps ~ interval | dayType, summ5, layout = c(1,2),
                           type = "l", xlab = "Interval", ylab = "Number of Steps")
print(plt5)
```

![plot of chunk wkdaywkend](figure/wkdaywkend.png) 

```r
# compare the weekend to weekday

summ5 <- ddply(summ5, "dayType", summarize,
               sumTotalSteps = sum(totalSteps),
               meanMeanSteps = mean(meanSteps),
               meanMedianSteps = mean(medianSteps))
print(summ5)
```

```
##   dayType sumTotalSteps meanMeanSteps meanMedianSteps
## 1 weekday        460757         35.55           7.319
## 2 weekend        194973         42.31          12.762
```
The weekday shows a higher peak and more total steps than the weekend. However,
activity begins much earlier and spikes in the first third of the weekday
(perhaps morning?) then the activity level is lower through the rest of the day.
Weekends are for sleeping in but do show steadier activity levels throughout
the day.
