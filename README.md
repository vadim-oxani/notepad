# notepad

# Remember to always use  echo = TRUE
download.file('http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', 'factivity.zip')
unzip('factivity.zip')
factivity <- read.csv('activity.csv', colClasses = c('integer','Date','integer'), na.strings = 'NA')

# Summarising data
library(dplyr)
sumSteps <- group_by(factivity, date)
summarData <- summarize(sumSteps, s = sum(steps, na.rm = T), mu = mean(steps, na.rm = T), md = median(steps, na.rm = T))

# Making the histogram
hist(summarData$s, xlab = 'Steps', main = 'Histogram of Total # Steps per Day', breaks = 10)

# Calculating the mean and median of total  number of steps taken per day
mean(summarData$mu, na.rm = T)
median(summarData$mu, na.rm = T)

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
sumSteps2 <- group_by(factivity, interval)
summarData2 <- summarize(sumSteps2, mu = mean(steps, na.rm = T))
with (summarData2, plot(interval, mu, type = 'l'))

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
summarData2[round(summarData2$mu,4) == round(max(summarData2$mu),4),1]

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
colSums(is.na(factivity))
sum(is.na(factivity$steps)) # optional

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# The mean of the interval avergaed across all days will be used.

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
for (i in unique(factivity$interval)){ 
factivity[factivity$interval == i & is.na(factivity$steps), 1] =  
summarData2[summarData2$interval == i, 2]}

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
sumSteps3 <- group_by(factivity, date)
summarData3 <- summarize(sumSteps3, s = sum(steps), mu = mean(steps), md = median(steps))

hist(summarData3$s, xlab = 'Steps', main = 'Histogram of Total # Steps per Day', breaks = 10)
mean(summarData3$mu, na.rm = T)
median(summarData3$mu, na.rm = T)
# Compare and report that median has changed, but the mean hasn't
