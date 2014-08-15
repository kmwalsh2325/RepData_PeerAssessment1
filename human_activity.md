Reproducible Research: Peer Assignment 1
===============


### Loading and pre-processing data

This is a markdown file explains the first peer assignment. The data is stored with this Rmd in the same directory. I prefer to work with data tables as opposed to data frames; let's load data.table and read in some data.


```r
library(data.table)

dataset <- fread("~/Coursera/Data Science Specialization/Reproducible Research/Project 1/activity.csv")
```


Let's take a look at what structure this data is in:

```r
str(dataset)
```

```
## Classes 'data.table' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

We notice the date column isn't truly the Date class but instead is a character. Let's change that.

```r
dataset[, date := as.Date(date)]
```

```r
str(dataset)
```

```
## Classes 'data.table' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

That's better. We also notice there are some blocks of NAs in the steps column but we were warned that might be the case as devices do not always record well. We will leave those in for now.


### What is the mean total number of steps taken per day?

Sounds like we are going to want to add up the number of steps per day (ignoring NAs by the na.rm feature) and then use this data to generate a histogram.

```r
stepsstats <- dataset[, list(total_steps = sum(steps, na.rm=TRUE)), by = date]
hist(stepsstats[, total_steps], xlab = "Number of Steps", main = "Histogram of Total Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

We should probably get the mean and median of total steps taken per day from this data as well.

```r
stepsstats[, mean(total_steps)]
```

```
## [1] 9354
```

```r
stepsstats[, median(total_steps)]
```

```
## [1] 10395
```

These values make sense as the image we have produced is skewed right which will shift the mean to the left. I would like to take a moment to explain why I did not find each mean and median for each day as English can be confusing and to confess, that was my first interpretation of this question. I am going to go ahead and calculate one of these (mean) just for kicks to show my point.

```r
dataset[, list(mean_steps = mean(steps, na.rm=TRUE)), by = date]
```

```
##           date mean_steps
##  1: 2012-10-01        NaN
##  2: 2012-10-02     0.4375
##  3: 2012-10-03    39.4167
##  4: 2012-10-04    42.0694
##  5: 2012-10-05    46.1597
##  6: 2012-10-06    53.5417
##  7: 2012-10-07    38.2465
##  8: 2012-10-08        NaN
##  9: 2012-10-09    44.4826
## 10: 2012-10-10    34.3750
## 11: 2012-10-11    35.7778
## 12: 2012-10-12    60.3542
## 13: 2012-10-13    43.1458
## 14: 2012-10-14    52.4236
## 15: 2012-10-15    35.2049
## 16: 2012-10-16    52.3750
## 17: 2012-10-17    46.7083
## 18: 2012-10-18    34.9167
## 19: 2012-10-19    41.0729
## 20: 2012-10-20    36.0938
## 21: 2012-10-21    30.6285
## 22: 2012-10-22    46.7361
## 23: 2012-10-23    30.9653
## 24: 2012-10-24    29.0104
## 25: 2012-10-25     8.6528
## 26: 2012-10-26    23.5347
## 27: 2012-10-27    35.1354
## 28: 2012-10-28    39.7847
## 29: 2012-10-29    17.4236
## 30: 2012-10-30    34.0938
## 31: 2012-10-31    53.5208
## 32: 2012-11-01        NaN
## 33: 2012-11-02    36.8056
## 34: 2012-11-03    36.7049
## 35: 2012-11-04        NaN
## 36: 2012-11-05    36.2465
## 37: 2012-11-06    28.9375
## 38: 2012-11-07    44.7326
## 39: 2012-11-08    11.1771
## 40: 2012-11-09        NaN
## 41: 2012-11-10        NaN
## 42: 2012-11-11    43.7778
## 43: 2012-11-12    37.3785
## 44: 2012-11-13    25.4722
## 45: 2012-11-14        NaN
## 46: 2012-11-15     0.1424
## 47: 2012-11-16    18.8924
## 48: 2012-11-17    49.7882
## 49: 2012-11-18    52.4653
## 50: 2012-11-19    30.6979
## 51: 2012-11-20    15.5278
## 52: 2012-11-21    44.3993
## 53: 2012-11-22    70.9271
## 54: 2012-11-23    73.5903
## 55: 2012-11-24    50.2708
## 56: 2012-11-25    41.0903
## 57: 2012-11-26    38.7569
## 58: 2012-11-27    47.3819
## 59: 2012-11-28    35.3576
## 60: 2012-11-29    24.4688
## 61: 2012-11-30        NaN
##           date mean_steps
```

This result is overwhelming. Where does one begin with understanding the mean for each day? This way of thinking is not very useful so we choose to find the mean of the sum of total number of steps per day. Let us continue.

### What is the average daily activity pattern?

Here we're interested in looking at the daily average pattern, that is, averaging across all days what does a typical day look like with regard to activity pattern?


```r
dailypattern <- dataset[, list(avg_steps = mean(steps, na.rm=TRUE)), by = interval]
plot(x = dailypattern[, interval], y = dailypattern[, avg_steps], type = "l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

The maximum number of steps for average daily activity

```r
dailypattern[avg_steps == max(avg_steps)]
```

```
##    interval avg_steps
## 1:      835     206.2
```
occurs on the 835th minute with 206.1698 steps.


### Imputing missing values

1. Calculate the total number of rows with an NA value for steps. 
This question is easy, we need only filter for the NA steps and count the number of rows using the .N function. 


```r
dataset[is.na(steps), .N]
```

Looks like there are 2304 rows with null value.


2. Devise a strategy for filling in NA values. To make things simple I am choosing to assign NA values the overall mean number of steps, 37.3826. The first note that should be made is that this value is not an integer which is the class of the steps column.

3. Create a new dataset with NA values filled in.

```r
completedata <- fread("~/Coursera/Data Science Specialization/Reproducible Research/Project 1/activity.csv")
avg_steps <- completedata[, mean(steps, na.rm=TRUE)]
completedata[is.na(steps), steps:=avg_steps]
```

```
##        steps       date interval
##     1:    37 2012-10-01        0
##     2:    37 2012-10-01        5
##     3:    37 2012-10-01       10
##     4:    37 2012-10-01       15
##     5:    37 2012-10-01       20
##    ---                          
## 17564:    37 2012-11-30     2335
## 17565:    37 2012-11-30     2340
## 17566:    37 2012-11-30     2345
## 17567:    37 2012-11-30     2350
## 17568:    37 2012-11-30     2355
```

To clean up the output I hid a warning about coersion. This warning was about the steps columns casting the double value to the integer class to match the original steps column. That is, 37.3826 was rounded to integer value 37.

4. Make a pretty histogram and report the **mean** and **median** values as before.


```r
stepsstats2 <- completedata[, list(total_steps = sum(steps)), by = date]
hist(stepsstats2[, total_steps], xlab = "Number of Steps", main = "Histogram of Total Steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

The new **mean** value is 

```r
options(scipen = 100)
stepsstats2[, mean(total_steps)]
```

```
## [1] 10752
```

and the new **median** value is 

```r
stepsstats2[, median(total_steps)]
```

```
## [1] 10656
```

Do these values differ from the data at the beginning of the assignment? And how does this affect estimating the daily total number of steps?

Incredibly so! With regards to the mean and median overall, the NAs were originally regarded as 0s as they did not add value to the days missing values. By adding in a non-zero quantity, there is no surprise that the overall mean and median increased as more steps were being added to the total daily sums.


### Are there differences in activity patterns between weekdays and weekends?

We begin by declaring a weekday column with two levels: "weekday", "weekend". 

```r
completedata[, date := as.Date(date)]
completedata[, weekdays(date)]
completedata[, weekday := ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday")]
```


Using the lattice package we create a two panel xyplot.

```r
library(lattice)
wkdata <- completedata[, list(avg_steps = mean(steps)), keyby = c("weekday", "interval")]
```

```r
xyplot(avg_steps ~ interval | weekday, data = wkdata, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of Steps", main = "Week Activity Patterns")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 

It appears that over the weekend there tends to be more constant activity than during the average weekday.
