# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# Load data
activity <- read.csv("activity.csv", header = TRUE)

# Convert dates to proper format
activity$date <- as.Date(activity$date, "%Y-%m-%d")

# Verify changes to data
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity)
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


## What is mean total number of steps taken per day?
**1. Calculate the total number of steps taken per day**

```r
daily_steps <- aggregate(activity$steps, list(activity$date), sum, na.rm = TRUE)
names(daily_steps) <- c("date", "steps")
str(daily_steps)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ date : Date, format: "2012-10-01" "2012-10-02" ...
##  $ steps: int  0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
```

```r
head(daily_steps)
```

```
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

**2. Make a histogram of the total number of steps taken each day**

```r
hist(daily_steps$steps,
  xlab = "Total Steps",
  main = "Total Number of Steps Taking Daily",
  breaks=seq(from=0, to=25000, by=2500),
  col = "aquamarine"
)
```

![](PA1_template_files/figure-html/stepsHistogram-1.png)<!-- -->

**3. Calculate and report the mean and median of the total number of steps taken per day**

```r
mean(daily_steps$steps)
```

```
## [1] 9354.23
```

```r
median(daily_steps$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
**1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```r
interval_means <- aggregate(activity$steps,list(activity$interval), mean, na.rm = TRUE)
names(interval_means) <- c("interval", "mean.steps")
str(interval_means)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval  : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ mean.steps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
head(interval_means)
```

```
##   interval mean.steps
## 1        0  1.7169811
## 2        5  0.3396226
## 3       10  0.1320755
## 4       15  0.1509434
## 5       20  0.0754717
## 6       25  2.0943396
```

```r
ggplot(interval_means, aes(x=interval, y=mean.steps)) +
  geom_line(color = "slategrey") + xlab("Interval") + ylab("Average Steps") + ggtitle("Average Steps per 5 Minute Intervals")
```

![](PA1_template_files/figure-html/timeSeries-1.png)<!-- -->

**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
(subset(interval_means, interval_means$mean.steps==max(interval_means$mean.steps)))
```

```
##     interval mean.steps
## 104      835   206.1698
```

## Imputing missing values
**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)**

```r
sum(is.na(activity))
```

```
## [1] 2304
```

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

```r
activity_filled <- activity
na_steps <- is.na(activity_filled$steps)
mean_interval <- tapply(activity_filled$steps, activity_filled$interval, mean, na.rm=TRUE, simplify = TRUE)
```

**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**

```r
activity_filled$steps[na_steps] <- mean_interval[as.character(activity_filled$interval[na_steps])]
head(activity_filled)
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

**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

```r
steps_full <- activity_filled %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
```

```
## # A tibble: 61 × 2
##          date    steps
##        <date>    <dbl>
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## # ... with 51 more rows
```

```r
ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "steelblue1", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
```

![](PA1_template_files/figure-html/FilledStepsPerDay-1.png)<!-- -->

```r
(mean(steps_full$steps, na.rm = TRUE))
```

```
## [1] 10766.19
```

```r
(median(steps_full$steps, na.rm = TRUE))
```

```
## [1] 10766.19
```

The impact of imputing missing data is that the _mean_ increses slightly, probably due to the addition of values higher than the original mean for missing values. Interstingly the _median_ stays the same.

## Are there differences in activity patterns between weekdays and weekends?
**1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**

```r
weekday_data <- mutate(activity_filled, weektype = ifelse(weekdays(activity_filled$date) == "Saturday" | weekdays(activity_filled$date) == "Sunday", "weekend", "weekday"))
weekday_data$weektype <- as.factor(weekday_data$weektype)
head(weekday_data)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

**2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.**

```r
interval_full <- weekday_data %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
weekday_plot <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(weekday_plot)
```

![](PA1_template_files/figure-html/weekDayTimeSeries-1.png)<!-- -->
