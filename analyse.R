calcTotalForDay <- function (reqDay,data) {
    dateVector <- data[, 2]
    dayData <- data[dateVector == reqDay,]
    
    d <- c(sum(dayData$steps),reqDay)
    
    d
    
}

fiveMinsIntervalForDay <- function (reqDay,data) {
    dateVector <- data[, 2]
    dayData <- data[dateVector == reqDay,]
    
    d <- dayData$steps
    
    d
    
}

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

weekdayOrWeekend <- function (testDay) {
    
    if (grepl(testDay,c("Saturday")) | grepl(testDay,c("Sunday")) ) {
        d <- 0
    } else {
        d <- 1
    }
           
    d
    
}


data <- read.csv("activity.csv",colClasses = "character")
stepsVector  <- data$steps
filteredData <- data[!is.na(stepsVector),]

filteredData[, 1] <- as.numeric(filteredData[, 1])

#filteredData[, 2] <- as.Date(filteredData[, 2],"%Y-%m-%d")

# daily
days <- unique(sort(filteredData[,2]))
dailyData <- sapply(days,calcTotalForDay,data = filteredData)

dailyData <- data.frame(t(dailyData))

cnames <- c("steps", "day")
colnames(dailyData) <- cnames

dailyData[, 1] <- as.numeric(as.character(dailyData[, 1]))

# Daily Mean, Median, Hist 
hist(dailyData$steps,xlab = "Daily Steps", ylab = "Frequency", main = "Daily Histogram")

png(filename="./figure/daily_steps_histogram.png")
hist(dailyData$steps,xlab = "Daily Steps", ylab = "Frequency", main = "Daily Histogram")
dev.off()

totalDailySteps  <- dailyData$steps
meanDailySteps   <- mean(totalDailySteps)
medianDailySteps <- median(totalDailySteps)

print(c("mean",meanDailySteps))
print(c("median",medianDailySteps))


# daily per 5 mins
dailyPer5MinsData <- sapply(days,fiveMinsIntervalForDay,data = filteredData)
dailyPer5MinsData <- data.frame(t(dailyPer5MinsData))
cnames <- (c(1:length(dailyPer5MinsData))-1)*5
cnames <- c(1:length(dailyPer5MinsData))-1
cnames <- as.character(cnames)
colnames(dailyPer5MinsData) <- cnames

per5MinuteSum <- colSums (dailyPer5MinsData)/nrow(dailyPer5MinsData)
print(dim(dailyPer5MinsData))

#plot
plot(per5MinuteSum, type="l", col="blue",xlab = "5 Minute Interval", ylab = "Number of steps", main = "Daily Steps per 5 Minute")

png(filename="./figure/per_5minute_sum2.png")
plot(per5MinuteSum, type="l", col="blue",xlab = "5 Minute Interval", ylab = "Number of steps", main = "Daily Steps per 5 Minute")
dev.off()

# which 5 min interval is max
print(c("max(per5MinuteSum)", max(per5MinuteSum)))
mostActive5Minute <- per5MinuteSum[per5MinuteSum == max(per5MinuteSum)]
print(mostActive5Minute)

#print(dailyData)
print(meanDailySteps)
print(medianDailySteps)





## NA
#calculate total number of NA
stepsVector  <- data$steps
numberOfNaRows <-nrow(data[is.na(stepsVector),])
print(c("numberOfNaRows",numberOfNaRows))

dataWithFixedNa <- data
dataWithFixedNa$steps[is.na(dataWithFixedNa$steps)] <- meanDailySteps/288
# data with fixedNa to daily
days <- unique(sort(dataWithFixedNa[,2]))
dataWithFixedNa[, 1] <- as.numeric(as.character(dataWithFixedNa[, 1]))
dataWithFixedNaDailyData <- sapply(days,calcTotalForDay,data = dataWithFixedNa)
dataWithFixedNaDailyData <- data.frame(t(dataWithFixedNaDailyData))
dataWithFixedNaDailyData[, 1] <- as.numeric(as.character(dataWithFixedNaDailyData[, 1]))

cnames <- c("steps", "day")
colnames(dataWithFixedNaDailyData) <- cnames

# New Daily Mean, Median, Hist 
hist(dataWithFixedNaDailyData$steps,xlab = "Daily Steps", ylab = "Frequency", main = "Daily Histogram")

png(filename="./figure/fixed_na_daily_steps_histogram.png")
hist(dataWithFixedNaDailyData$steps,xlab = "Daily Steps", ylab = "Frequency", main = "Daily Steps After NA is Replaced with Daily Average")
dev.off()

fixedTotalDailySteps  <- dataWithFixedNaDailyData$steps
meanFixedDailySteps   <- mean(fixedTotalDailySteps)
medianFixedDailySteps <- median(fixedTotalDailySteps)

print(c("meanFixedDailySteps",meanFixedDailySteps))
print(c("medianFixedDailySteps",medianFixedDailySteps))


# weekdays
daysData <- filteredData$date
daysData <- as.Date(as.character(filteredData$date))
stepDays <- weekdays(daysData)

stepDaysBinom <- sapply(stepDays,weekdayOrWeekend)
#print(class(stepDays))
#print(stepDaysBinom)
stepDaysBinomFactor <- factor(stepDaysBinom, labels = c("weekend", "weekday"))
dailyDataWithWeekendOrWeekday <- cbind(filteredData, stepDaysBinomFactor)

cnames <- c("steps", "date", "interval", "daytype")
colnames(dailyDataWithWeekendOrWeekday) <- cnames

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
#print(dim(dailyPer5MinsDataWeekDaySum))

cnames <- (c(1:length(dailyPer5MinsDataWeekEnd))-1)*5
cnames <- as.character(cnames)
colnames(dailyPer5MinsDataWeekEnd) <- cnames
dailyPer5MinsDataWeekEndSum <- colSums (dailyPer5MinsDataWeekEnd)/nrow(dailyPer5MinsDataWeekEnd)
#print(dim(dailyPer5MinsDataWeekEndSum))

#plot
plot(dailyPer5MinsDataWeekDaySum, type="l", col="blue",xlab = "5 Minute Interval For Weekdays", ylab = "Number of steps", main = "Daily Steps per 5 Minute")

png(filename="./figure/per_5minute_weekdays_avg.png")
plot(dailyPer5MinsDataWeekDaySum, type="l", col="blue",xlab = "5 Minute Interval For Weekdays", ylab = "Number of steps", main = "Daily Steps per 5 Minute")
dev.off()

plot(dailyPer5MinsDataWeekEndSum, type="l", col="blue",xlab = "5 Minute Interval For Weekends", ylab = "Number of steps", main = "Daily Steps per 5 Minute")

png(filename="./figure/per_5minute_weekends_avg.png")
plot(dailyPer5MinsDataWeekEndSum, type="l", col="blue",xlab = "5 Minute Interval For Weekends", ylab = "Number of steps", main = "Daily Steps per 5 Minute")
dev.off()

#print(class(stepDaysBinom))
#print(class(stepDaysBinomFactor))

#print(dailyDataWithWeekendOrWeekday)








#g <- sapply(filteredData,calcAvgForDay,days)

#meanSteps <- mean(filteredData$steps)

#hist(filteredData$steps,xlab = "Steps", ylab = "Frequency", main = "Data Histogram")

#print(names(data))
#print(nrow(data))
#print(nrow(filteredData))
#print(mean(data[,1]))
#print(meanSteps)
