# Project Assignment 1 - Reproducible Research
### Loading and preprocessing the data

```r
library(knitr)
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
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)
data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))
data$date <- ymd(data$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
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

### What is mean total number of steps taken per day?
####1. Calculate the total number of steps per day using dplyr and group by date:

```r
steps <- data %>%
         filter(!is.na(steps)) %>%
         group_by(date) %>%
         summarize(steps = sum(steps)) %>%
         print
```

```
## # A tibble: 53 × 2
##          date steps
##        <date> <dbl>
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## # ... with 43 more rows
```

####2. Use ggplot for making the histogram

```r
ggplot(steps, aes(x = steps)) + geom_histogram(fill = "firebrick", 
binwidth = 1000) + labs(title = "Histogram of Steps per day", 
x = "Steps per day", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

####3. Calculate the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```

### What is the average daily activity pattern?
####1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval <- data %>%
filter(!is.na(steps)) %>%
group_by(interval) %>%
summarize(steps = mean(steps))

ggplot(interval, aes(x=interval, y=steps)) + geom_line(color = "firebrick")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

####2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval[which.max(interval$steps),]
```

```
## # A tibble: 1 × 2
##   interval    steps
##      <int>    <dbl>
## 1      835 206.1698
```

### Imputing missing values
####1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

####2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
data_full <- data

nas <- is.na(data_full$steps)

avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)

data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
```

####3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
sum(is.na(data_full$steps))
```

```
## [1] 0
```

```r
steps_full <- data_full %>%
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

####4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
ggplot(steps_full, aes(x = steps)) +
geom_histogram(fill = "firebrick", binwidth = 1000) +
labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)
mean_steps_full
```

```
## [1] 10766.19
```

```r
median_steps_full
```

```
## [1] 10766.19
```

### Are there differences in activity patterns between weekdays and weekends?
####1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "sabado" | weekdays(data_full$date) == "domingo", "weekend", "weekday"))

data_full$weektype <- as.factor(data_full$weektype)

head(data_full)
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

####2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
interval_full <- data_full %>%
group_by(interval, weektype) %>%
summarise(steps = mean(steps))
interval_full
```

```
## Source: local data frame [576 x 3]
## Groups: interval [?]
## 
##    interval weektype       steps
##       <int>   <fctr>       <dbl>
## 1         0  weekday 1.943752225
## 2         0  weekend 0.214622642
## 3         5  weekday 0.384478462
## 4         5  weekend 0.042452830
## 5        10  weekday 0.149519402
## 6        10  weekend 0.016509434
## 7        15  weekday 0.170879316
## 8        15  weekend 0.018867925
## 9        20  weekday 0.085439658
## 10       20  weekend 0.009433962
## # ... with 566 more rows
```

```r
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
geom_line() + facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
