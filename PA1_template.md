Loading and preprocessing the data

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    if (!file.exists("./data")){dir.create("./data")}
    fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileurl, destfile = "11.zip")

    unzip(zipfile = "11.zip")
    activity <- read.csv("activity.csv")
    activity_clear <- filter(activity, !is.na(steps))

What is mean total number of steps taken per day?

    #1.Calculate the total number of steps taken per day
    group_ac <- group_by( activity_clear, date)
    sumteps <- summarise(group_ac, steps= sum(steps))

    #2.Make a histogram of the total number of steps taken each day
    qplot(steps, data = sumteps, binwidth = 4000, main = "total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    hist(sumteps$steps,
         main = "Total Steps per Day",
         xlab = "Number of Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-2.png)

    #3.Calculate the mean and median of the total number of steps taken per day
    mean <- mean(sumteps$steps)
    median <- median(sumteps$steps)

What is the average daily activity pattern?

    group_ac2 <- group_by(activity_clear, interval)
    data <- summarise(group_ac2, steps = mean(steps))
    ggplot(data, aes(x = interval, y = steps)) +
            geom_line() +
            ggtitle("Average Daily Activity Pattern") +
            xlab("5-minute Interval") +
            ylab("Average Number of Steps") 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    # Which 5-minute interval, on average across all the days in the dataset, 
    #contains the maximum number of steps?
    max_steps <- data[which(data$steps == max(data$steps)),]

Imputing missing values

    #1.alculate and report the total number of missing values in the dataset
    sum_na <- sum(is.na(activity$steps))

    #2.Devise a strategy for filling in all of the missing values in the dataset.
    #3.Create a new dataset that is equal to the original dataset 
    #but with the missing data filled in.
    activity_new <- transform(activity,
                              steps= ifelse(is.na(activity$steps),
                              data$steps[match(activity$interval,data$interval)],
                              activity$steps)
                              )
    #4.Make a histogram of the total number of steps taken each day and
    #Calculate and report the mean and median total number of steps taken per day.
    group_ac3 <- group_by(activity_new, date)
    sumsteps2 <- summarise(group_ac3, steps = sum(steps))
    qplot(steps, data = sumsteps2, binwidth = 4000)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    mean_new <- mean(sumsteps2$steps)
    median_new <- median(sumsteps2$steps)
    diffmean <- mean_new - mean
    diffmedian <- median_new - median 

The difference of mean is 0 and of median is diffmedian 1.1886792.

Are there differences in activity patterns between weekdays and
weekends?

    activity_new$date <- as.Date(activity_new$date)
    dayInx <- function(date){
            date1 <- weekdays(date)
            if (date1 == "Sunday" | date1 == "Saturday") 
            return("Weekend")
            else return("Weekdays")
    }
    activity_new <- mutate(activity_new, daytype = sapply(activity_new$date, dayInx))

    bb <- group_by(activity_new, interval, daytype)
    cc <- summarise(bb, steps = mean(steps))

    ggplot(data = cc, aes(x = interval, y= steps, color = daytype)) + 
            geom_line() +
            ggtitle("Average Daily Activity Pattern") +
            xlab("5-minute Interval") +
            ylab("Average Number of Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)
