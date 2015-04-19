---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
# Load provided data (use relative paths)
activity <- read.csv(file = './data/activity.csv', stringsAsFactors = FALSE)

# Change class for the date variable
activity$date <- as.Date(activity$date)


## What is mean total number of steps taken per day?
#Summarize the data by day
daily_activity <- aggregate(formula = steps~date, data=activity, 
                            FUN = sum, na.rm=TRUE)
# Calculate summary statistics
mean_steps <- round(mean(daily_activity$steps),2) #Mean
median_steps <- quantile(x = daily_activity$steps, probs = 0.5) #Median, 50%Q

# Plot a histogram of the total number of steps per day.
histogram <- qplot(x=steps, data=daily_activity) + 
  labs(y='Count of each total steps per day', x='Number of steps')
plot(histogram)


## What is the average daily activity pattern?
# Aggregate the steps per interval, calculating the mean across the days
interval_activity <- 
  aggregate(formula=steps~interval, data=activity,
            FUN=mean, na.rm=TRUE)
# Get the data for the interval with the most average activity across the days
max_steps <- interval_activity[which(interval_activity$steps==max(interval_activity$steps)),]

# Function to calculate the mean and normal
# 95% confidence interval around it
mean_ci <- function(data){
  m <- mean(data)
  data.frame(y=m,
             ymin = m-(1.96*sd(data)/sqrt(length(data))),
             ymax = m+(1.96*sd(data)/sqrt(length(data))))
}

# Plot the average number of steps per interval
# Use ggplot2 to summarize the data, to 
# find inconsistencies with the analysis.
# Geom 'line' is equivalent to 'type="l"' in plot.
steps_per_interval <- 
  qplot(x=interval, y=steps,
        data=subset(activity, complete.cases(activity)),
        geom='smooth', stat='summary', fun.data=mean_ci) +
  labs(y='Average steps per interval', x='Interval')

steps_per_interval


## Imputing missing values
# Count the number of NAs
total_NAs <- sum(!complete.cases(activity))
step_NAs <- sum(is.na(activity$steps))

# Calculating the number of missing dates
dates_in_range <- seq.Date(from = min(activity$date),
                           to = max(activity$date),
                           by='1 day')
date_NAs <- sum(!activity$date[complete.cases(activity)] %in% dates_in_range)

# Imputation strategy
# Use previously calculated means
interval_activity$imputed_steps <- floor(interval_activity$steps)

#Merge the replacement values
imputed_activity <- merge(activity,
                          interval_activity[,c('interval', 'imputed_steps')],
                          by='interval')

# Replace the missing values
imputed_activity$steps <- ifelse(is.na(imputed_activity$steps),
                                 imputed_activity$imputed_steps,
                                 imputed_activity$steps)

# Remove unneccessary data
imputed_activity$imputed_steps <- NULL

# Summarize the data by day
daily_imputed_activity <-
  aggregate(formula = steps~date, data = imputed_activity,
            FUN = sum, na.rm=TRUE)

#calculate summary statistics
mean_imputed_steps <- round(mean(daily_imputed_activity$steps),2)
median_imputed_steps <- quantile(x = daily_imputed_activity$steps, probs = 0.5)

#Replace the data in the original histogram with the imputed data
histogram %+% daily_imputed_activity


## Are there differences in activity patterns between weekdays and weekends?
#Label each date as weekday/weekend(1:5 are weekdays, 6:7 are weekends)
imputed_activity$week_part <- factor(
  ifelse(as.integer(format(imputed_activity$date, format = '%u')) %in% c(1:5),
         'weekday', 'weekend'))

# Plot the average steps per interval, given the week_part
steps_per_interval %+% imputed_activity + facet_grid(week_part~.)
