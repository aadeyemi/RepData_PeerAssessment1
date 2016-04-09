---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Data loaded using read.csv after which it is filtered by removing NA rows. hist is used to plot a histogram

```r
data <- read.csv("activity.csv",colClasses = "character")
stepsVector  <- data$steps
filteredData <- data[!is.na(stepsVector),]

filteredData[, 1] <- as.numeric(filteredData[, 1])
filteredData[, 2] <- as.Date(filteredData[, 2],"%Y-%m-%d")

hist(filteredData$steps,xlab = "Steps", ylab = "Frequency", main = "Data Histogram")
```

![plot of chunk load](figure/load-1.png)



## What is mean total number of steps taken per day?
Mean steps calculated using mean function and median calculated using median function

```r
meanSteps <- mean(filteredData$steps)
medianSteps <- median(filteredData$steps)
print(meanSteps)
```

```
## [1] 37.3826
```

```r
print(medianSteps)
```

```
## [1] 0
```



## What is the average daily activity pattern?
for this question, we will need to write a helper function to compute daily steps. The code is as follows

```r
calcTotalForDay <- function (reqDay,data) {
    dateVector <- data[, 2]
    dayData <- data[dateVector == reqDay,]
    
    d <- c(sum(dayData$steps),reqDay)
    
    d
    
}
```

Once the helper function is done, we can compute daily steps with the following code

```r
days <- unique(sort(filteredData[,2]))
dailyData <- sapply(days,calcTotalForDay,data = filteredData)

dailyData <- data.frame(t(dailyData))

cnames <- c("steps", "day")
colnames(dailyData) <- cnames

dailyData[, 1] <- as.numeric(as.character(dailyData[, 1]))
```

And then compute daily mean and median using

```r
hist(dailyData$steps,xlab = "Daily Steps", ylab = "Frequency", main = "Daily Histogram")
```

![plot of chunk daily4](figure/daily4-1.png)

```r
totalDailySteps  <- dailyData$steps
meanDailySteps   <- mean(totalDailySteps)
medianDailySteps <- median(totalDailySteps)
```

The mean daily steps is 10766
The median daily steps is 10765

A histogram is shown below

![Histogram of daily steps] (./figure/daily_steps_histogram.png)


To compute the most active 5 minute interval, we need another helper function


```r
fiveMinsIntervalForDay <- function (reqDay,data) {
    dateVector <- data[, 2]
    dayData <- data[dateVector == reqDay,]
    
    d <- dayData$steps
    
    d
    
}
```


With this helper function, we can proceed. First we compute a table with all 5 minute interval accross all days and then name the columns


```r
dailyPer5MinsData <- sapply(days,fiveMinsIntervalForDay,data = filteredData)
dailyPer5MinsData <- data.frame(t(dailyPer5MinsData))
cnames <- (c(1:length(dailyPer5MinsData))-1)*5
cnames <- c(1:length(dailyPer5MinsData))-1
cnames <- as.character(cnames)
colnames(dailyPer5MinsData) <- cnames

per5MinuteSum <- colSums (dailyPer5MinsData)
print(dim(dailyPer5MinsData))
```

```
## [1]  53 288
```

```r
print(length(per5MinSum))
```

```
## Error in print(length(per5MinSum)): object 'per5MinSum' not found
```

```r
#plot
plot(per5MinuteSum, type="l", col="blue",xlab = "5 Minute Interval", ylab = "Frequency", main = "Daily Steps per 5 Minute")
```

![plot of chunk daily6](figure/daily6-1.png)

```r
# which 5 min interval is max
print(max(per5MinuteSum))
```

```
## [1] 10927
```

```r
mostActive5Minute <- per5MinuteSum[per5MinuteSum == max(per5MinuteSum)]
print(mostActive5Minute)
```

```
##   103 
## 10927
```
The most active 5 minute interval is  510 (the 103rd 5 minute) reading and its value is 10927


![ Time series plot of the 5-minute interval and the average number of steps taken] (./figure/per_5minute_sum2.png)

## Imputing missing values
The total number of missing values is obtained with the code

```r
stepsVector  <- data$steps
numberOfNaRows <-nrow(data[is.na(stepsVector),])
```
The number of rows with missing data is 2304

To fill in missing data, I replaced NA values with the mean daily value divided by total number of readings (288) as follows


```r
dataWithFixedNa <- data
dataWithFixedNa$steps[is.na(dataWithFixedNa$steps)] = meanDailySteps/288
```
The original data with NA is the variable "data" and the updated data with average values replacing the NA values is the variable "dataWithFixedNa".

Now I calculate new daily steps and store them is the variable "dataWithFixedNaDailyData".

```r
# data with fixedNa to daily
days <- unique(sort(filteredData[,2]))
dataWithFixedNa[, 1] <- as.numeric(as.character(dataWithFixedNa[, 1]))
dataWithFixedNaDailyData <- sapply(days,calcTotalForDay,data = dataWithFixedNa)
dataWithFixedNaDailyData <- data.frame(t(dataWithFixedNaDailyData))
dataWithFixedNaDailyData[, 1] <- as.numeric(as.character(dataWithFixedNaDailyData[, 1]))

cnames <- c("steps", "day")
colnames(dataWithFixedNaDailyData) <- cnames
```

To plot the new histograms, I use


```r
hist(dataWithFixedNaDailyData$steps,xlab = "Daily Steps", ylab = "Frequency", main = "Daily Histogram")
```

![plot of chunk missing data4](figure/missing data4-1.png)

A histogram of the new plots is shown below
![Histogram of daily steps after fixing NA values] (./figure/fixed_na_daily_steps_histogram.png)

New mean and mode are 10766 and 10766 respectively.

Since I used daily averages, the mean is not changed, but the median changes because of the added entries.

## Are there differences in activity patterns between weekdays and weekends?
A helper function to help determine day type which will be called from sapply is defined as


```r
weekdayOrWeekend <- function (testDay) {
    if (grepl(testDay,c("Saturday")) | grepl(testDay,c("Sunday")) ) {
        d <- 0
    } else {
        d <- 1
    }    
    d
}
```

After defining this function I created a vector containg all dates and pass that vector with the function "weekdayOrWeekend" to sapply and then append the result to "filteredData" using cbind.


```r
daysData <- filteredData$date
daysData <- as.Date(as.character(filteredData$date))
stepDays <- weekdays(daysData)
stepDaysBinom <- sapply(stepDays,weekdayOrWeekend)
stepDaysBinomFactor <- factor(stepDaysBinom, labels = c("weekend", "weekday"))
dailyDataWithWeekendOrWeekday <- cbind(filteredData, stepDaysBinomFactor)

cnames <- c("steps", "date", "interval", "day_type")
colnames(dailyDataWithWeekendOrWeekday) <- cnames
```

To create weekend and weekds plots I use a modified version of the helper function described earlier


```r
fiveMinsIntervalForWeekDayType <- function (reqDay,data,type) {
    dateVector <- data[, 2]
    daytypeVector <- data[, 4]
    
    dayData <- data[dateVector == reqDay & daytypeVector == type,]
    
    d <- NULL
    
    if (nrow(dayData)>0) {
        d <- dayData$steps
    } else {
        d <- as.numeric(rep(NA,288))
    }
    d
}
```


Then I run the following code

```r
# daily per 5 mins
days <- unique(sort(dailyDataWithWeekendOrWeekday$date))
dailyPer5MinsDataWeekDay <- sapply(days,fiveMinsIntervalForWeekDayType,data = dailyDataWithWeekendOrWeekday, type="weekday")
dailyPer5MinsDataWeekEnd <- sapply(days,fiveMinsIntervalForWeekDayType,data = dailyDataWithWeekendOrWeekday, type="weekend")
dailyPer5MinsDataWeekDay <- data.frame(t(dailyPer5MinsDataWeekDay))
dailyPer5MinsDataWeekEnd <- data.frame(t(dailyPer5MinsDataWeekEnd))

vecWeekday  <- dailyPer5MinsDataWeekDay[1]
vecWeekend  <- dailyPer5MinsDataWeekEnd[1]

dailyPer5MinsDataWeekDay <- dailyPer5MinsDataWeekDay[!is.na(vecWeekday),]
dailyPer5MinsDataWeekEnd <- dailyPer5MinsDataWeekEnd[!is.na(vecWeekend),]

cnames <- (c(1:length(dailyPer5MinsDataWeekDay))-1)*5
cnames <- as.character(cnames)
colnames(dailyPer5MinsDataWeekDay) <- cnames
dailyPer5MinsDataWeekDaySum <- colSums (dailyPer5MinsDataWeekDay)/nrow(dailyPer5MinsDataWeekDay)

cnames <- (c(1:length(dailyPer5MinsDataWeekEnd))-1)*5
cnames <- as.character(cnames)
colnames(dailyPer5MinsDataWeekEnd) <- cnames
dailyPer5MinsDataWeekEndSum <- colSums (dailyPer5MinsDataWeekEnd)/nrow(dailyPer5MinsDataWeekEnd)

#plot
plot(dailyPer5MinsDataWeekDaySum, type="l", col="blue",xlab = "5 Minute Interval For Weekdays", ylab = "Number of steps", main = "Daily Steps per 5 Minute")
```

![plot of chunk daytype4](figure/daytype4-1.png)

```r
plot(dailyPer5MinsDataWeekEndSum, type="l", col="blue",xlab = "5 Minute Interval For Weekends", ylab = "Number of steps", main = "Daily Steps per 5 Minute")
```

![plot of chunk daytype4](figure/daytype4-2.png)

My resulting time series plots are shown below

![ Time series plot of the 5-minute interval and the average number of steps taken on Weekdays] (./figure/per_5minute_weekdays_avg.png)



![ Time series plot of the 5-minute interval and the average number of steps taken on Weekends] (./figure/per_5minute_weekends_avg.png)
