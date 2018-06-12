---
title: "Reproducible Research"
output:
  html_document:
    keep_md: true
---



# Reproducible Research Week 2 Assignment
# Activity monitoring data analysis
### Author:  Fred Jensen
### Date:  April 15, 2018
## 1. Code for reading in the dataset and/or processing the data

```r
	activity_data <- read.csv("C:/Users/TheJensen5/Documents/R/download/Replicated Results - Week 2/activity.csv")
	activity_data$date <- as.Date(activity_data$date,"%Y-%m-%d")
	head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
## 2. Histogram of the total number of steps taken each day	
### 	Determine the mean total number of steps taken per day

```r
	activity_data_per_day <- aggregate(activity_data$steps, by = list(activity_data$date), FUN = sum, na.rm = TRUE)
	## Rename columns of activity_data_per_day
	names(activity_data_per_day)[names(activity_data_per_day)=="Group.1"] <- "date"
	names(activity_data_per_day)[names(activity_data_per_day)=="x"] <- "steps"
	summary(activity_data_per_day)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :    0  
##  1st Qu.:2012-10-16   1st Qu.: 6778  
##  Median :2012-10-31   Median :10395  
##  Mean   :2012-10-31   Mean   : 9354  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```
###  	Create Histogram graph

```r
	hist(subset(activity_data_per_day$steps, !is.na(activity_data_per_day$steps)), xlab = "Total daily Steps", main="Histogram of Total Steps by day")
```

![](Reproducible_Research_-_Week_2_assignment_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
  
###   Mean and median number of steps taken each day

```r
	## Calculate and report the mean and median of the total number of steps taken per day
	mean(activity_data_per_day$steps)
```

```
## [1] 9354.23
```

```r
	median(activity_data_per_day$steps)
```

```
## [1] 10395
```
## 3. Time series plot of the average numbers of steps taken
### Determine the average daily activity pattern

```r
	activity_data_per_interval <- aggregate(activity_data$steps, by = list(activity_data$interval), FUN = mean, na.rm = TRUE)
	## Rename columns of activity_data_per_interval
	names(activity_data_per_interval)[names(activity_data_per_interval)=="Group.1"] <- "interval"
	names(activity_data_per_interval)[names(activity_data_per_interval)=="x"] <- "steps"
	summary(activity_data_per_interval)
```

```
##     interval          steps        
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```
### Plot time series of 5 minute intervals

```r
	plot(activity_data_per_interval$interval,activity_data_per_interval$steps, xlab = "Total Interval Steps", main = "Times series plot of Total Steps by Interval", type ="l")
```

![](Reproducible_Research_-_Week_2_assignment_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
  
## 4. Determine which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
	subset(activity_data_per_interval, activity_data_per_interval$steps == max(activity_data_per_interval$steps))[1,1]
```

```
## [1] 835
```
## 5. Code to describe and show a strategy for imputing missing data
### Impute missing values
#### Imputed values were equal to the average numbers of steps taken during each 5 minute interval
##### Calculate the total number of missing values in the dataset

```r
  activity_data_imput <- activity_data
  ## calculate total number of missing values
  missing_values <- nrow(subset(activity_data_imput, is.na(steps)))
  missing_values
```

```
## [1] 2304
```

```r
	## replace missing values with the mean value of the available data by interval
	activity_data_imput$steps <- with(activity_data_imput, ave(steps, interval, FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
	head(activity_data_imput)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
	summary(activity_data_imput)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```
## 6. Histogram of the total number of steps taken each day after missing values are imputed
### 	Determine the mean total number of steps taken per day using imputed values in data

```r
  activity_data_per_day_imput <- aggregate(activity_data_imput$steps, by = list(activity_data_imput$date), FUN = sum, na.rm = TRUE)
	## Rename columns of activity_data_per_day_imput
	names(activity_data_per_day_imput)[names(activity_data_per_day_imput)=="Group.1"] <- "date"
	names(activity_data_per_day_imput)[names(activity_data_per_day_imput)=="x"] <- "steps"
	summary(activity_data_per_day_imput)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```
###  	Create Histogram graph

```r
	hist(subset(activity_data_per_day_imput$steps,!is.na(activity_data_per_day_imput$steps)), xlab = "Total daily Steps", main="Histogram of Total Steps by day")
```

![](Reproducible_Research_-_Week_2_assignment_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
###   Mean and median number of steps taken each day including imputed values

```r
	## Calculate and report the mean total number of steps taken per day including imputed values
	mean(activity_data_per_day_imput$steps)
```

```
## [1] 10766.19
```

```r
  median(activity_data_per_day_imput$steps)
```

```
## [1] 10766.19
```

```r
  ## compare mean and median from original data file
  mean(activity_data_per_day_imput$steps) - mean(activity_data_per_day$steps)
```

```
## [1] 1411.959
```

```r
	median(activity_data_per_day_imput$steps) - median(activity_data_per_day$steps)
```

```
## [1] 371.1887
```
## 7. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
###	Calculate and report the mean and median of the total number of steps taken per day

### Assign days to either weekday or weekend

```r
  activity_data_type_of_day <- activity_data
	activity_data_type_of_day$day <- weekdays(activity_data_type_of_day$date)
	## create table that defines days of the week as either "Weekend" or "Weekday"
	day <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
	type <- c("Weekend","Weekday","Weekday","Weekday","Weekday","Weekday","Weekend")
	type_of_day <- data.frame(day, type)
	## merge activity_data_type_of_day and type_of_day
	activity_data_type_of_day <- merge(activity_data_type_of_day, type_of_day, by.Exposuredata_consol = "day", by.Withdata_consol = "day", all.x = T)
	head(activity_data_type_of_day)
```

```
##      day steps       date interval    type
## 1 Friday     0 2012-10-05     2220 Weekday
## 2 Friday     0 2012-11-16      430 Weekday
## 3 Friday     0 2012-10-05     2235 Weekday
## 4 Friday     0 2012-11-16      435 Weekday
## 5 Friday     0 2012-10-05     2315 Weekday
## 6 Friday     0 2012-11-16      445 Weekday
```
###aggregate by interval and type

```r
  activity_data_type_of_day_interval <- aggregate(activity_data_type_of_day$steps, by = list(activity_data_type_of_day$type, activity_data_type_of_day$interval), FUN = sum, na.rm = TRUE)
  	## Rename columns of activity_data_per_day_imput
	names(activity_data_type_of_day_interval)[names(activity_data_type_of_day_interval)=="Group.1"] <- "type"
	names(activity_data_type_of_day_interval)[names(activity_data_type_of_day_interval)=="Group.2"] <- "interval"
	names(activity_data_type_of_day_interval)[names(activity_data_type_of_day_interval)=="x"] <- "steps"
  head(activity_data_type_of_day_interval)
```

```
##      type interval steps
## 1 Weekday        0    91
## 2 Weekend        0     0
## 3 Weekday        5    18
## 4 Weekend        5     0
## 5 Weekday       10     7
## 6 Weekend       10     0
```
### Create the panel plot graph

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
ggplot(activity_data_type_of_day_interval, aes(x=interval, y = steps, colour = type)) + geom_line() +
	facet_grid(type ~.) + xlab("Interval") + ylab("Mean of Steps") +
	ggtitle("Comparison of Average Steps in Each Interval")
```

![](Reproducible_Research_-_Week_2_assignment_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
