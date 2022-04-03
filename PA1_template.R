library(dplyr)
library(ggplot2)
library(lattice)
Sys.setlocale(category = "LC_ALL", locale = "english")

# Task 1: loading and preprocessing the data
mydf <- read.csv("./activity.csv", header = TRUE, 
                 colClasses = c("numeric", "character", "numeric"),
                 na.strings = "NA")
mydf <- mutate(mydf, date=strptime(date, "%Y-%m-%d"))

# Task 2: what is mean total number of steps taken per day?
StepDay <- mydf %>% na.omit %>% group_by(date) %>% summarise(TotalStep=sum(steps))
mean(StepDay$TotalStep)
median(StepDay$TotalStep)
g1 <- ggplot(data=StepDay, aes(x=TotalStep, fill=..count..)) + 
     geom_histogram(binwidth = 1000) + 
     labs(title="Histogram of Total Number of Steps Taken Each Day") + 
     labs(x="Total Number of Steps Taken Each Day") + 
     labs(y="Count")
print(g1)

# Task 3: what is the average daily activity pattern?
AvgStep <- mydf %>% na.omit %>% group_by(interval) %>% summarise(avgstep=mean(steps))
g2 <- ggplot(data=AvgStep, aes(x=interval, y=avgstep)) + 
      geom_line() + 
      labs(title="Time Series of 5-Minute Interval and Average Number of Steps across All Days") + 
      labs(x="Interval") + 
      labs(y="Average Steps across All Days")
print(g2)
MaxAvgStepInterval<- with(AvgStep, AvgStep[avgstep==max(avgstep), "interval", drop=TRUE])

# Task 4: imputing missing values
is.na(mydf) %>% sum()
imputeMydf_1 <- merge(mydf, AvgStep, by="interval", all=TRUE)
imputeMydf_2 <- mutate(imputeMydf_1, steps=ifelse(is.na(steps), avgstep, steps), avgstep=NULL)
StepDayImpute <- imputeMydf_2 %>% group_by(date) %>% summarise(TotalStep=sum(steps))
mean(StepDayImpute$TotalStep)
median(StepDayImpute$TotalStep)
g3 <- ggplot(data=StepDayImpute, aes(x=TotalStep, fill=..count..)) + 
    geom_histogram(binwidth = 1000) + 
    labs(title="Histogram of Total Number of Steps Taken Each Day After Imputation") + 
    labs(x="Total Number of Steps Taken Each Day") + 
    labs(y="Count")
print(g3)

# Task 5: are there differences in activity patterns between weekdays and weekends?
imputeMydf_3 <- mutate(imputeMydf_2, dayType=ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
AvgStepImpute <- imputeMydf_3 %>% group_by(interval, dayType) %>% summarise(avgstep=mean(steps))
xyplot(avgstep ~ interval|dayType, data=AvgStepImpute, type="l", layout=c(1,2),
       main="Time Series of 5-Min Interval and Average Number of Steps on Weekend and Weekday",
       xlab="Interval",
       ylab="Average Steps across All Days")
