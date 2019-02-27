##Loading and preprocessing the data

setwd("~//Desktop/datasciencecoursera/RepRes project 1")
main0 <- read.csv("activity.csv", header = TRUE)

##mean total number of steps taken per day 
sums <- aggregate(main0$steps, by = list(main0$date), sum, na.rm = TRUE)
colnames(sums) = c("date", "steps")

##hist of total steps taken each day 
hist(sums$steps, main = "Total number of steps taken per day", 
     xlab = "Total steps taken per day", col = "green", ylim = c(0,25), 
     breaks = seq(0,25000, 2500))

##Calculate and report the mean and median of the total number of steps taken per day
means <- mean(sums$steps, na.rm = TRUE)
medians <- median(sums$steps, na.rm = TRUE)

##What is the average daily activity pattern?
##Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) 
##and the average number of steps taken, averaged across all days (y-axis)

imeans <- aggregate(main0$steps, by = list(main0$interval), mean, na.rm = TRUE)
plot(imeans$Group.1, imeans$x, type = "l", xlab = "Time interval", ylab ="Mean steps during interval")

##Which 5-minute interval, on average across all the days in the dataset, 
##contains the maximum number of steps?
imax <- imeans[which.max(imeans$x), ]$Group.1

##NANS
##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
nans <- sum(is.na(main0$steps))

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##Create a new dataset that is equal to the original dataset but with the missing data filled in
main0$stepsi <- main0$steps
impsteps <- imeans$x[match(main0$interval, imeans$Group.1)]
impMain <- transform(main0, stepsi = ifelse(is.na(main0$stepsi), yes = impsteps, no = main0$stepsi))

##Make a histogram of the total number of steps taken each day 
isteps <- aggregate(impMain$stepsi,by = list(impMain$date), sum)
hist(isteps$x, main = "Total number of steps taken per day", 
     xlab = "Total steps taken per day", col = "yellow", ylim = c(0,30), xlim = c(0,25000), 
     breaks = seq(0,25000, 2500))
##Calculate and report the mean and median total number of steps taken per day. 

impmeans <- mean(isteps$x)
impmedian <- median(isteps$x)

##Do these values differ from the estimates from the first part of the assignment? 
##What is the impact of imputing missing data on the estimates of the total daily number of steps?

Means <- data.frame(means, impmeans)
Medians <- data.frame(medians, impmedian)
MM <- cbind(Means, Medians)
colnames(MM) <- c("Mean", "Mean (Imputed)", "Median", "Median (Imputed)")


##Weekends Vs. Weekdays
impMain$date <- as.Date(strptime(impMain$date, format = "%Y-%m-%d"))
impMain$wkday <- sapply(impMain$date, FUN = function(x){
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday"){
      z <- "Weekend"
    }
  else{
      z <- "Weekday"
    }
  z
  })

##Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval 
##(x-axis) and the average number of steps taken, averaged across all weekday days 
##or weekend days (y-axis). 

wkmeans <- aggregate(impMain$stepsi, by = list(impMain$interval, impMain$wkday), mean)
colnames(wkmeans) <- c("interval", "wkday", "steps")
Day <- c("green", "blue")

g <- ggplot(wkmeans, aes(x=interval, y=steps)) + geom_line() + 
  facet_wrap(.~wkday,nrow = 2) + ggtitle("Average Daily steps (Weekday vs Weekend)") +
  labs(x = "Interval", y = "Average number of steps")
print(g) 


