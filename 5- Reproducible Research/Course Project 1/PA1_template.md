# <span style="color:#19A972"><b><u>Reproducible Research Week 2 Project 1</u></b></span>

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this data set are:

<b>steps:</b> Number of steps taking in a 5-minute interval (missing values are coded as NA)

<b>date:</b> The date on which the measurement was taken in YYYY-MM-DD format

<b>interval:</b> Identifier for the 5-minute interval in which measurement was taken

The data set is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this data set.

### <span style="color:#D50D47"><i>Load necessary libraries.</i></span>


```r
library("ggplot2")
```

### <span style="color:#D50D47"><i>Loading and preprocessing the data</i></span>

Check whether data file is in working directory, if not download the data. If the file is in working directory check whether it is unzipped if not unzip the file if it is load the data.


```r
# Download data set.

filename <- "repdata_data_activity.zip"
path <- getwd()

# Checking if file already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists.
if (!file.exists("repdata_data_activity")) { 
  unzip(filename) 
}

# Load data.

df <- read.csv("repdata_data_activity/activity.csv")

# Check classes.

sapply(df, class)
```

```
##       steps        date    interval 
##   "integer" "character"   "integer"
```
Convert "date" column to date class.


```r
df$date <- as.Date(df$date)

# Check classes again.

sapply(df, class)
```

```
##     steps      date  interval 
## "integer"    "Date" "integer"
```

### <span style="color:#D50D47"><i>What is mean total number of steps taken per day?
</i></span>

<b>1. Calculate the total number of steps taken per day.</b>


```r
steps <- aggregate(df$steps, by=list(Steps.Date = df$date), FUN = sum, na.rm=TRUE)
```

<b>2. Make a histogram of the total number of steps taken each day.</b>


```r
hst <- hist(steps$x, main = "Total Number of Steps per Day", 
     col="springgreen2", xlab="Number of Steps", ylim = c(0,20), breaks = seq(0,22500,2500), xaxt="n", labels = TRUE)
axis(1, at = seq(0, 22500, by = 2500), las=0, cex = 0.7)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)

<b>3. Calculate and report the mean and median of the 
total number of steps taken per day.</b>


```r
mean_steps <- round(mean(steps$x))
median_steps <- round(median(steps$x))

print(paste("Mean is:",format(round(as.numeric(mean_steps)),big.mark = ".")))
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, : 'big.mark' and
## 'decimal.mark' are both '.', which could be confusing
```

```
## [1] "Mean is: 9.354"
```

```r
print(paste("Median is:",format(round(as.numeric(median_steps)),big.mark = ".")))
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, : 'big.mark' and
## 'decimal.mark' are both '.', which could be confusing
```

```
## [1] "Median is: 10.395"
```

### <span style="color:#D50D47"><i>What is the average daily activity pattern?
</i></span>

<b>1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)</b>


```r
average_steps <- aggregate(df$steps, by = list(Interval = df$interval), 
                          FUN = "mean", na.rm=TRUE)

plot(average_steps$x ~ average_steps$Interval,
     col="deepskyblue4", type="l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps Taken", main = "Average Number of Steps By Time Interval")
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)

<b>2. Which 5-minute interval, on average across all the days in the data set, contains the maximum number of steps?</b>


```r
print(paste("Interval containing the max number of steps on average:"
            ,average_steps$Interval[which.max(average_steps$x)]))
```

```
## [1] "Interval containing the max number of steps on average: 835"
```

### <span style="color:#D50D47"><i>Imputing missing values
</i></span>

<b>1. Calculate and report the total number of missing values in the data set.</b>


```r
na_values <- colSums(is.na(df))

print(paste("Total number of NA values in column",colnames(df),":",na_values))
```

```
## [1] "Total number of NA values in column steps : 2304"
## [2] "Total number of NA values in column date : 0"    
## [3] "Total number of NA values in column interval : 0"
```

<b>2. Devise a strategy for filling in all of the missing values in the data set.</b>
<b>3. Create a new data set that is equal to the original data set but with the missing data filled in.</b>


```r
df_new <- df
for (i in 1:nrow(df)){
        if(is.na(df$steps[i])){
                df_new$steps[i]<- average_steps$x[df_new$interval[i] == average_steps$Interval]
        }
}
```

<b>4. Make a histogram of the total number of steps taken each day.</b>


```r
steps_new <- aggregate(df_new$steps, by=list(Steps.Date = df_new$date), FUN = sum, na.rm=TRUE)
hst <- hist(steps_new$x, main = "Total Number of Steps per Day", 
     col="springgreen2", xlab="Number of Steps", ylim = c(0,30), breaks = seq(0,22500,2500), xaxt="n", labels = TRUE)
axis(1, at = seq(0, 22500, by = 2500), las=0, cex = 0.7)
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)

<b>5. Calculate and report the mean and median total number of steps taken per day.</b>


```r
mean_steps_new <- round(mean(steps_new$x))
median_steps_new <- round(median(steps_new$x))

print(paste("Mean is:",format(round(as.numeric(mean_steps_new)),big.mark = ".")))
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, : 'big.mark' and
## 'decimal.mark' are both '.', which could be confusing
```

```
## [1] "Mean is: 10.766"
```

```r
print(paste("Median is:",format(round(as.numeric(median_steps_new)),big.mark = ".")))
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, : 'big.mark' and
## 'decimal.mark' are both '.', which could be confusing
```

```
## [1] "Median is: 10.766"
```
Filling in with the mean for that 5 minute interval caused an increase in both the mean and the median.

### <span style="color:#D50D47"><i>Are there differences in activity patterns between weekdays and weekends?
</i></span>

<b>1. Create a new factor variable in the data set with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.</b>


```r
library(chron)

df_new$day <- chron::is.weekend(df_new$date)
```

<b>2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).</b>


```r
df_day_mean <- aggregate(steps ~ interval + day, data=df_new, mean)

day_labels <- c("Weekend", "Weekday")
names(day_labels) <- c("TRUE", "FALSE")


ggplot(df_day_mean, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(day ~ ., labeller = labeller(day=day_labels)) +
        xlab("5-Minute Intervals") + 
        ylab("Average Number of Steps Taken") +
        ggtitle("Weekdays and Weekends Average Number of Steps Taken")+
        theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-29-1.png)


```r
library(knitr)
knit2html("PA1_template.Rmd")
```

```
## 
## 
## processing file: PA1_template.Rmd
```

```
## 
  |                                                                                      
  |                                                                                |   0%
  |                                                                                      
  |...                                                                             |   3%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |.....                                                                           |   7%
## label: unnamed-chunk-31
## 
  |                                                                                      
  |........                                                                        |  10%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |...........                                                                     |  13%
## label: unnamed-chunk-32
## 
  |                                                                                      
  |.............                                                                   |  17%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |................                                                                |  20%
## label: unnamed-chunk-33
## 
  |                                                                                      
  |...................                                                             |  23%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |.....................                                                           |  27%
## label: unnamed-chunk-34
## 
  |                                                                                      
  |........................                                                        |  30%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |...........................                                                     |  33%
## label: unnamed-chunk-35
```

```
## 
  |                                                                                      
  |.............................                                                   |  37%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |................................                                                |  40%
## label: unnamed-chunk-36
## 
  |                                                                                      
  |...................................                                             |  43%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |.....................................                                           |  47%
## label: unnamed-chunk-37
```

```
## 
  |                                                                                      
  |........................................                                        |  50%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |...........................................                                     |  53%
## label: unnamed-chunk-38
## 
  |                                                                                      
  |.............................................                                   |  57%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |................................................                                |  60%
## label: unnamed-chunk-39
## 
  |                                                                                      
  |...................................................                             |  63%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |.....................................................                           |  67%
## label: unnamed-chunk-40
## 
  |                                                                                      
  |........................................................                        |  70%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |...........................................................                     |  73%
## label: unnamed-chunk-41
```

```
## 
  |                                                                                      
  |.............................................................                   |  77%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |................................................................                |  80%
## label: unnamed-chunk-42
## 
  |                                                                                      
  |...................................................................             |  83%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |.....................................................................           |  87%
## label: unnamed-chunk-43
## 
  |                                                                                      
  |........................................................................        |  90%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |...........................................................................     |  93%
## label: unnamed-chunk-44
```

```
## 
  |                                                                                      
  |.............................................................................   |  97%
##   ordinary text without R code
## 
## 
  |                                                                                      
  |................................................................................| 100%
## label: unnamed-chunk-45
```

```
## output file: PA1_template.md
```
