## Loading and preprocessing the data

### Download and unzip the data
setwd("~/Coursera/Reproducible Research/Week 2")
url1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile1 <- "repdata_data_activity.zip"

if(!file.exists(destfile1)) {
    download.file(url1, 
                  destfile = destfile1, 
                  method = "curl")
    unzip(destfile1, exdir = "./Data")
}


activity <- read.csv("./Data/activity.csv")
str(activity)
names(activity)
head(activity)


activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
#activity$DateOnly <- as.Date(activity$date, format="%Y-%m-%d")

#Data with no NAs
activityNoNA <- activity[!is.na(activity$steps),]
head(activityNoNA)


## What is mean total number of steps taken per day?
#Method 1

library(dplyr)
tot.steps.per.day <- activity[!is.na(activity$steps),] %>% group_by(date) %>%
   summarize(totalSteps = sum(steps, na.rm = TRUE))

#This keeps rows for all dates even ones with NAs with total 0
#tot.steps.per.day <- activity %>% group_by(date) %>%
#    summarize(totalSteps = sum(steps, na.rm = TRUE))

#Method 2
#tot.steps.per.day <- aggregate(activity$steps ~ activity$date, FUN=sum, )
#colnames(tot.steps.per.day)<- c("Date", "Steps")

#Histogram
hist(tot.steps.per.day$totalSteps, breaks=5, xlab="Steps", main = "Total Steps per Day")

#Mean Steps
as.integer(mean(tot.steps.per.day$totalSteps))

#Median Steps
as.integer(median(tot.steps.per.day$totalSteps))

## What is the average daily activity pattern?

library(ggplot2)

##create average number of steps per interval
avg.steps.per.interval <- activity[!is.na(activity$steps),] %>% group_by(interval) %>%
    summarize(avgSteps = mean(steps, na.rm = TRUE))

#Method 2
#library(plyr)
#avg.steps.per.interval <- ddply(activityNoNA, .(interval), summarize, avgSteps = mean(steps))

##Create line plot of average number of steps per interval
p <- ggplot(avg.steps.per.interval, aes(x=interval, y=avgSteps), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

#Maximum steps by interval
maxSteps <- max(avg.steps.per.interval$avgSteps)

#Which interval contains the maximum average number of steps
avg.steps.per.interval[avg.steps.per.interval$avgSteps==maxSteps,1]

## Imputing missing values

#Number of NAs in original data set
nrow(activity[is.na(activity$steps),])

# Create the average number of steps per weekday and interval
avgTable <- activity[!is.na(activity$steps),] %>% group_by(interval, day) %>%
    summarize(avgStepsComputed = mean(steps, na.rm = TRUE))

#Method 2
#library(plyr)
#avgTable <- ddply(activityNoNA, .(interval, day), summarize, Avg = mean(steps))

# Create dataset with all NAs for substitution
nadata<- activity[is.na(activity$steps),]
# Merge NA data with average weekday interval for substitution
newdata<-merge(nadata, avgTable, by=c("interval", "day"))

# Reorder the new substituded data in the same format as clean data set
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")

#Merge the NA averages and non NA data together
mergeData <- rbind(activityNoNA, newdata2)

##Create sum of steps per date to compare with step 1
tot.steps.per.day2 <- mergeData %>% group_by(date) %>%
    summarize(totalSteps = sum(steps, na.rm = TRUE))

#tot.steps.per.day2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
#colnames(tot.steps.per.day2)<- c("Date", "totalSteps")

# Mean of Steps with NA data taken care of
as.integer(mean(tot.steps.per.day2$totalSteps))

# Median of Steps with NA data taken care of
as.integer(median(tot.steps.per.day2$totalSteps))

# Creating the histogram of total steps per day, categorized by data set to show impact
hist(tot.steps.per.day2$totalSteps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(tot.steps.per.day$totalSteps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )

## Are there differences in activity patterns between weekdays and weekends?

# Create new category based on the days of the week
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

library(lattice) 

# Summarize data by interval and type of day
avg.steps.per.interval2 <- mergeData %>% group_by(interval, DayCategory) %>%
    summarize(avgSteps = mean(steps, na.rm = TRUE))

#Method 2 using plyr
#avg.steps.per.interval2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

#Plot data in a panel plot
xyplot(avgSteps~interval|DayCategory, data=avg.steps.per.interval2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")

