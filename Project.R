activity<-read.csv("activity.csv")

## Summary of unprocessed data
summary(activity)

## Convert Date Column to Date type
activity$date <- as.Date(activity$date)


## Step Histogram
## Load GGPLOT2 Library
library(ggplot2)

## Extract data for Steps Per Day Histogram
stepsperday<-tapply(activity$steps,activity$date,sum)

## Summary of Summed data
summary(stepsperday)


## Save Histogram as PNG
png(filename="figure/001.png")
## Print Histogram of Steps Per Day
hist(stepsperday,xlab="Steps",main="Steps Per Day Histogram",labels=T)
## Close Device
dev.off()

## Calculate Mean Steps Per Day
meanOrg <- mean(stepsperday, na.rm=T)
meanOrg
## Calculate Median Steps Per Day
medianOrg <- median(stepsperday, na.rm=T)
medianOrg

## Extract data for Steps Per 5 Minute Time Series Plot
stepsPer5MinutesAverage<-tapply(activity$steps,activity$interval,mean,na.rm=T)
intervals <- sort(unique(activity$interval,na.rm=T))

## Save Time Series Plot as PNG
png(filename="figure/002.png")
## Print Steps Per 5 Minute Time Series Plot
plot(intervals,stepsPer5MinutesAverage,type="l")
## Close Device
dev.off()


## Interval with biggest mean
highestMeanInterval <- as.numeric(names(which.max(stepsPer5MinutesAverage)) )
highestMeanInterval

##Missing Data

# Total Missing Data
missingDataCountSteps <- sum(is.na(activity$steps))
missingDataCountDate <- sum(is.na(activity$date))
missingDataCountInterval <- sum(is.na(activity$interval))
missingDataCountSteps
missingDataCountDate
missingDataCountInterval

# Initialize Clean Data
cleanData <- activity


## Replace NA's with mean
for(i in 1:nrow(cleanData)){
  if(is.na(cleanData[i,]$steps)){
    cleanData[i,]$steps<-stepsPer5MinutesAverage[i]
  }
}

## Create Average Clean Data
stepsperdayClean<-tapply(cleanData$steps,cleanData$date,sum)

## Save Histogram as PNG
png(filename="figure/003.png")
## Print Histogram of Steps Per Day (Clean)
hist(stepsperdayClean,xlab="Steps",main="Steps Per Day Histogram",labels=T)
## Close Device
dev.off()

## Calculate Mean Steps Per Day
meanClean <- mean(stepsperdayClean, na.rm=T)
meanClean

## Calculate Median Steps Per Day
medianClean <- median(stepsperdayClean, na.rm=T)
medianClean


## Find Weekdays and Weekends
cleanData$day <- format(cleanData$date, "%A")
cleanData$day[cleanData$day == "Monday"] <- "weekday"
cleanData$day[cleanData$day == "Tuesday"] <- "weekday"
cleanData$day[cleanData$day == "Wednesday"] <- "weekday"
cleanData$day[cleanData$day == "Thursday"] <- "weekday"
cleanData$day[cleanData$day == "Friday"] <- "weekday"
cleanData$day[cleanData$day == "Saturday"] <- "weekend"
cleanData$day[cleanData$day == "Sunday"] <- "weekend"

## Create Weekday Data
cleanWDData <- cleanData[which(cleanData$day == "weekday"),]

## Create Weekend Data
cleanWEData <- cleanData[which(cleanData$day == "weekend"),]

## Weekday-Weekend Difference Graph
library(lattice)

meanStepsByWEWD <- aggregate(steps ~ interval + day, data = cleanData, mean)

## Save Time Series Plot as PNG
png(filename="figure/004.png")
## Print Steps Per 5 Minute Time Series Plot
xyplot(steps ~ interval | factor(day), meanStepsByWEWD, type = "l", layout = c(1, 2))
## Close Device
dev.off()

