# Reproducible Research: Peer Assessment 1
setwd("C:/Vasu/R_Coursera/ReproducibleResearch/repdata-data-activity")
list.files()
# Read the file
data <- read.csv("activity.csv")
head(data)
# Data has three variavles. 
# *steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# *date: The date on which the measurement was taken in YYYY-MM-DD format
# *interval: Identifier for the 5-minute interval in which measurement was taken

## What is mean total number of steps taken per day?

library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
#total.steps

# Histogram for total number of steps taken each day
qplot(total.steps, binwidth = 500, xlab = "total steps per day", ylab = "frequency")

# Mean and median total number of steps taken per day
mean(total.steps, na.rm = TRUE)
# 9354.23
median(total.steps, na.rm = TRUE)
# 10395

## What is the average daily activity pattern?
daily.average <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval),FUN = mean, na.rm = TRUE)
daily.average

# Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of 
# steps taken, averaged across all days (y-axis)

ggplot(data = daily.average, aes(x = interval, y = steps)) +
  geom_line() + 
  ggtitle("Average number of steps across all days") + 
  xlab("5-minute interval") +  
  ylab("average number of steps taken")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
daily.average[which.max(daily.average$steps),]
# interval    steps
# 104      835 206.1698

# MISSING VALUES
# total number of missing values in the dataset 
missing.values <- length(which(is.na(data$steps)))
missing.values
#2304 rows have missing values

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# Replace each missing value with the mean value of its 5-minute interval
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
#install.packages("e1071")
#library(e1071)

fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (daily.average[daily.average$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)


# Histogram of the total number of steps taken each day
stepsByDay.filled <- tapply(filled.data$steps, filled.data$date, sum)
qplot(stepsByDay.filled, xlab = 'Total steps per day (filled)', ylab = 'Frequency', binwidth = 500)


# mean and median total number of steps taken per day
stepsByDayMean.filled <- mean(stepsByDay.filled)
stepsByDayMedian.filled <- median(stepsByDay.filled)

# Are there differences in activity patterns between weekdays and weekends?
#1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.
filled.data$datetype <-  ifelse(as.POSIXlt(filled.data$date)$wday %in% c(0,6), 'weekend', 'weekday')

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README 
# file in the GitHub repository to see an example of what this plot should look like using simulated data.


average.filled.data <- aggregate(steps ~ interval + datetype, data = filled.data, mean)
ggplot(average.filled.data, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(datetype ~ .) +
  xlab("5-minute interval") + 
  ylab("avarage number of steps")


