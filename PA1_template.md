Peer Assessment Project 1 : Reproducible Research: JH May 2014
==============================================================

This is the R Markdown file for Peer Assessment 1 in the John Hopkins
Reproducible Research course.

The dataset used in this exercise is avaiable at the following address:

```r
u <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

```


Or at this Github address: github.com/rdpeng/RepData_PeerAssessment1

### Download

The code is downloaded automatically:


```r
download.file(u, destfile = "activity.zip", mode = "wb")
```

```
## Error: unsupported URL scheme
```

```r

uu <- unzip(zipfile = "activity.zip")
data <- read.csv(file = "activity.csv", sep = ",", header = T)
```


The data file is formatted to assist in data analysis:


```r
data$date <- as.Date(as.character(data$date), format = "%Y-%m-%d", tz = "")
```

### Exploration

A histogram of the number of steps taken each day is generated. Mean & Median 
are reported :


```r
hist(data$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
m1 <- mean(data$steps, na.rm = T)
m2 <- median(data$steps, na.rm = T)

print(matrix(data = c("mean", "median", round(m1, digits = 2), m2), nrow = 2, 
    ncol = 2, byrow = T), quote = F)
```

```
##      [,1]  [,2]  
## [1,] mean  median
## [2,] 37.38 0
```


### Category Averages - Intervals

A time-series plot is provided of average number of steps / day against
the 5 minute interval.


```r
m3 <- with(data, tapply(steps, INDEX = interval, FUN = mean, na.rm = T))

plot(m3, type = "l", main = "Average number of Steps at each 5 minute Interval", 
    xlab = "Interval", ylab = "Average number of Steps", sub = paste("Maximum Step Average =", 
        round(max(m3), digits = 2), "steps: At Interval", names(subset(m3, m3 == 
            max(m3)))), axes = F)
axis(side = 1, at = seq(along.with = m3), labels = names(m3), tick = F)
axis(side = 2, tick = F)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


Next we will deal with missing step measurements. In the data these are listed as NA values.


```r
print(paste("The number of missing step measurements is", table(is.na(data$steps))[[2]], 
    "out of", length(data$steps), "measurements"), quote = F)
```

```
## [1] The number of missing step measurements is 2304 out of 17568 measurements
```


### Replacement

We will now replace missing step data with random samples from the existing step measures. One might consider this a "bootstrap-lite" replacement strategy.


```r
n1 <- table(is.na(data$steps))[[2]]
d1 <- subset(data$steps, is.na(data$steps) == F)
r1 <- sample(d1, size = n1, replace = T)
```


r1 becomes a sample set to replace NA values from the original values in the measurements.

Sampled values are now inserted into the original data replacing NAs.

```r
data1 <- data.frame(replace(data$steps, is.na(data$steps), r1), data$date, data$interval)

names(data1) <- c("steps.replace", "date", "interval")
```





Lets take a look at our altered data set with statistics:


```r
hist(data1$steps.replace)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
m4 <- mean(data1$steps.replace, na.rm = T)
m5 <- median(data1$steps.replace, na.rm = T)

print((matrix(data = c("mean", "median", round(m4, digits = 2), m5), nrow = 2, 
    ncol = 2, byrow = T)), quote = F)
```

```
##      [,1]  [,2]  
## [1,] mean  median
## [2,] 36.76 0
```


There isn't much difference in the statistics. Total number of steps rises of course (at minimum it should be the same assuming all NAs were replaced by zeros), but the histogram looks familiar & stats look familiar.


```r
print(paste("Original total of steps (with NAs):", sum(data$steps, na.rm = T)))
```

```
## [1] "Original total of steps (with NAs): 570608"
```

```r

print(paste("Revised total of steps (bootstrap replacement):", sum(data1$steps.replace, 
    na.rm = T)))
```

```
## [1] "Revised total of steps (bootstrap replacement): 645786"
```


### Category Averages - Weekends!

Finally we will look at averages in weekdays vs weekends. This is where there may be some weakness in the bootstrapping method. If there is a pattern of difference between weekdays & weekends then the simple bootstrapping method may 'pave over' those differences with indiscriminant replacement.A proper bootstrap method may want to create sample sets only from relevant factors:
i.e. bootstrap weekend values & weekday values separately, in order to attempt to preserve trends. Given the purview of this course we will simply move on at this point.

Let's determine which days are in the weekend and a factor column called 'week'


```r
wd <- NULL
# Factor into weekends & weekdays
for (i in 1:length(data1$steps.replace)) {
    if (weekdays(data1$date[i]) %in% c("Saturday", "Sunday")) {
        wd[i] <- "Weekend"
    } else {
        wd[i] <- "Weekday"
    }
}
data2 <- data1
data2[, "week"] <- wd
```


Now we will find the averages by 2 factors: interval level & status of week.
The modified data set with weekday/weekends' declared split into an array.


```r
d2 <- with(data2, split(data2, week))
m6d <- with(d2$Weekday, tapply(steps.replace, INDEX = interval, FUN = mean, 
    na.rm = T))
m6w <- with(d2$Weekend, tapply(steps.replace, INDEX = interval, FUN = mean, 
    na.rm = T))
```


And we plot both time series:


```r
par(mfrow = c(2, 1))

plot(m6w, type = "l", main = "Weekend", xlab = "Interval", ylab = "Average number of Steps", 
    sub = paste("Maximum Step Average =", round(max(m6w), digits = 2), "steps: At Interval", 
        names(subset(m6w, m6w == max(m6w)))), axes = F)
axis(side = 1, at = seq(along.with = m6w), labels = names(m6w), tick = F)
axis(side = 2, tick = F)

plot(m6d, type = "l", main = "Weekday", xlab = "Interval", ylab = "Average number of Steps", 
    sub = paste("Maximum Step Average =", round(max(m6d), digits = 2), "steps: At Interval", 
        names(subset(m6d, m6d == max(m6d)))), axes = F)
axis(side = 1, at = seq(along.with = m6d), labels = names(m6d), tick = F)
axis(side = 2, tick = F)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


That is it for this week's peer assignment. See you next week!
