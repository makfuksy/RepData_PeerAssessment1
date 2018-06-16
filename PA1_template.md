Introduction
------------

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

-   Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA) </br> date: The date on which the measurement was taken in YYYY-MM-DD format </br> interval: Identifier for the 5-minute interval in which measurement was taken </br> The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Loading and preprocessing the data
----------------------------------

Unzip data to obtain a csv file.

Loading data
------------

``` r
dataset1 <- read.csv("activity.csv")
```

What is mean total number of steps taken per day?
-------------------------------------------------

1.  Calculate the total number of steps taken per day

``` r
 Total_steps_taken_per_days1 <- dataset1 %>% select(steps, date) %>% group_by(date) %>% summarise(totalSteps=sum(steps))
head(Total_steps_taken_per_days1, 10)
```

    ## # A tibble: 10 x 2
    ##    date       totalSteps
    ##    <fct>           <int>
    ##  1 2012-10-01         NA
    ##  2 2012-10-02        126
    ##  3 2012-10-03      11352
    ##  4 2012-10-04      12116
    ##  5 2012-10-05      13294
    ##  6 2012-10-06      15420
    ##  7 2012-10-07      11015
    ##  8 2012-10-08         NA
    ##  9 2012-10-09      12811
    ## 10 2012-10-10       9900

1.  If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

``` r
ggplot(Total_steps_taken_per_days1, aes(x = totalSteps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Number of steps", y = "Frequency")
```

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](master/PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

1.  Calculate and report the mean and median of the total number of steps taken per day

``` r
Mean_Median1 <- na.omit(Total_steps_taken_per_days1) %>% summarise(Mean_Steps=mean(totalSteps), Median_Steps=median(totalSteps))
print(Mean_Median1)
```

    ## # A tibble: 1 x 2
    ##   Mean_Steps Median_Steps
    ##        <dbl>        <int>
    ## 1     10766.        10765

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e Type=="1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
day_averaged_data <- na.omit(dataset1) %>% group_by(interval) %>% summarise(MeanSteps=mean(steps))
ggplot(day_averaged_data, aes(x=interval, y=MeanSteps)) + geom_line(color="red", size=1) + labs(title="Avarage Daily Steps", x="Interval", y="Average steps per day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
day_averaged_data[which.max(day_averaged_data$MeanSteps),]
```

    ## # A tibble: 1 x 2
    ##   interval MeanSteps
    ##      <int>     <dbl>
    ## 1      835      206.

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
nrow(dataset1[is.na(dataset1$steps),])
```

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` r
# Filling in missing values with mean of steps by interval. 
dataset1[is.na(dataset1$steps), "steps"] <- sapply(dataset1[is.na(dataset1$steps), "interval"], function(X){day_averaged_data[which(day_averaged_data$interval==X),]$MeanSteps})
head(dataset1, 10)
```

    ##        steps       date interval
    ## 1  1.7169811 2012-10-01        0
    ## 2  0.3396226 2012-10-01        5
    ## 3  0.1320755 2012-10-01       10
    ## 4  0.1509434 2012-10-01       15
    ## 5  0.0754717 2012-10-01       20
    ## 6  2.0943396 2012-10-01       25
    ## 7  0.5283019 2012-10-01       30
    ## 8  0.8679245 2012-10-01       35
    ## 9  0.0000000 2012-10-01       40
    ## 10 1.4716981 2012-10-01       45

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
 write.csv(dataset1, file = "newActivityDataset.csv", row.names = TRUE)
```

1.  Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
dataset2 <- read.csv("newActivityDataset.csv")
  Total_steps_taken_per_days2 <- dataset2 %>% select(steps, date) %>% group_by(date) %>% summarise(totalSteps=sum(steps))
  
  
  
  ggplot(Total_steps_taken_per_days2, aes(x = totalSteps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
  Mean_Median2 <- Total_steps_taken_per_days2 %>% summarise(Mean_Steps=mean(totalSteps), Median_Steps=median(totalSteps))
  
  head(Mean_Median2)
```

    ## # A tibble: 1 x 2
    ##   Mean_Steps Median_Steps
    ##        <dbl>        <dbl>
    ## 1     10766.       10766.

| Estimate                                  | Mean  | Median |
|-------------------------------------------|-------|--------|
| With NAs                                  | 10766 | 10765  |
| With NAs filled with average of intervals | 10766 | 10766  |

Conclusion: These values don't differ from the first part of the assignment. Imputing missing data has no impact.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
dataset2 <- data.table::fread(input = "activity.csv")
  dataset2[, date := as.POSIXct(date, format = "%Y-%m-%d")]
  dataset2[, `Day of Week`:= weekdays(x = date)]
  dataset2[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
  dataset2[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
  dataset2[, `weekday or weekend` := as.factor(`weekday or weekend`)]
  head(dataset2)
```

    ##    steps       date interval Day of Week weekday or weekend
    ## 1:    NA 2012-10-01        0      Monday            weekday
    ## 2:    NA 2012-10-01        5      Monday            weekday
    ## 3:    NA 2012-10-01       10      Monday            weekday
    ## 4:    NA 2012-10-01       15      Monday            weekday
    ## 5:    NA 2012-10-01       20      Monday            weekday
    ## 6:    NA 2012-10-01       25      Monday            weekday

1.  Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
 dataset2[is.na(steps), "steps"] <- dataset2[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
  dataset2_intervall <- dataset2[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)]
  
  ggplot(dataset2_intervall , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg.Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-13-1.png)
