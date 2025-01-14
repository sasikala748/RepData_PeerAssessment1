---
title: "Untitled"
author: "Sasikala"
date: "02/04/2021"
output: html_document
---

As per project instructions in the course project we need to load the data and preprocess it if necessary.
Since it is a comma seperated file(csv) we need to call read.csv() to load the activity data.The data has been stored in my local machine.To check our working directory call getwd()

```{r,echo=TRUE}
getwd()
ActivityData <- read.csv("C:/Users/GuruJithin/Desktop/Coursera/UCI HAR Dataset/RepData_PeerAssessment1/activity.csv")

```


The data has been loaded.Now we have to preprocess for our analysis.


```{r,echo=TRUE}

head(ActivityData)
tail(ActivityData)
dim(ActivityData)
sum(is.na(ActivityData))


```

The first part of the question is to calculate the  total number of steps taken per day

```{r,echo=TRUE}

stepsperday <- ActivityData%>%
        group_by(date)%>%
        summarise(steps=sum(steps,na.rm = TRUE)) 
head(stepsperday)
n1 <- filter(stepsperday,steps!=0)
head(n1)
```
1.2.To plot the histogram for the number of steps taken per day we need to call hist().Also copy the histogram plot
  as png file and save it to the local device

```{r,echo=TRUE}

hist(n1$steps,main="Steps taken per day")  


1.3. Calculate the mean and median of the total number of steps taken per day.To calculate mean call mean()

```{r,echo=TRUE}
mean1 <- mean(n1$steps)
mean1 
```

To calculate the median use median ()

```{r,echo=TRUE}
median1 <- median(n1$steps)
median1  

```
The second part of the question is to calculate the Average daily activity pattern
2.1.To plot the time series plot we need to calculate the average number of steps taken per interval.In the main data we need to calculate the mean steps by grouping the intervals now.


```{r,echo=TRUE}
library(dplyr)
second <-ActivityData%>%
        group_by(interval)%>%
        
        summarise(steps=mean(steps,na.rm = TRUE))
head(second)      
``````

To do a time series we should call a plot with type="l"

```{r,echo=TRUE}
dev.copy(png,file="Plot2.png")
with(second,plot(interval,steps,type="l",xlab="5-minute interval",
                             ylab="Average no.of steps taken",main="Average daily activity pattern"))
dev.off()

```
2.2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  For this we have to calculate by using which.max()

```{r,echo=TRUE}
valuemax<- second[which.max(second$steps),]$interval                                    #2.2
valuemax
```

The third part of the question is imputing the missing values.There are a number of days/intervals where there are  missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
3.1.To calculate and report the total number of missing values as NA,we have to use sum of is.na() mv is a missing variable

```{r,echo=TRUE}
mv <- sum(is.na(ActivityData))
mv   
```

3.2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval.
So we could use mean to fill the missing values in the steps.then zero values are removed by filtering the ActivityData

```{r,echo=TRUE}
ActivityData$steps[is.na(ActivityData$steps)] <- mean(ActivityData$steps,na.rm = TRUE)
n2 <- filter(ActivityData,steps!=0)
View(n2)

```

3.3 To create a filled new data set with the original data set with a missing values as mean value,here(NA_filling).The fd(filled data) is created by aggregating steps by groupng dates.

```{r,echo=TRUE}
fd <- aggregate(steps~date,n2,sum)
head(fd)
```
3.4.To create a histogram,total number of steps per day ,mean and meadian values after imputing  and replaced with missing values.


```{r,echo=TRUE}
dev.copy(png,file="Plot3.png")
hist(fd$steps,main="steps per day after replacing NA")
dev.off()

```
Now mean and median has to be calculated after replacing na values with mean values
```{r,echo=TRUE}

mean2 <- mean(fd$steps)
mean2
median2 <- median(fd$steps)
median2



```


Both median and median of n1 is comparitively same as in the mean and median of n2 is 10766.19

4.1 Differences in activity patterns between weekdays and weekends? To work in dates we load the lubridate package.
we can use weekdays() to find a day is weekday or weekend.I have added one more column called whichday indicates weekday or weekend

```{r,echo=TRUE}
library(lubridate)
datend <- as.Date(n2$date)               #4.1
chechweek <- n2%>%
        mutate(whichday= case_when(wday(datend) %in% 2:6 ~ "Weekday",
                                   wday(datend) %in% c(1,7) ~ "Weekend"))
head(chechweek)

```
4.2 To make a panel plot for interval and steps with whichday,we have to groupby interval and whichday,then calculate the total number of steps and summararise it

```{r,echo=TRUE}
avg <- chechweek%>%
        select(-date)%>%
        group_by(whichday,interval)%>%
        summarise(steps=sum(steps))
head(avg)
```
4.2.After grouping and summarising we can make a panel plot by loading lattice package and make a time series plot with the layout as 2 row and 1 column as mentioned in the assignment

```{r,echo=TRUE}
library(lattice)

xyplot(steps~interval|whichday,data=avg,type="l",layout=c(1,2),facet=.~whichday,xlab="5min interval"
       ,ylab="Number of steps",main="Differences in pattern")



```



