if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
data <- read.csv('activity.csv', sep=',' , header=TRUE)

steps_per_day <- aggregate(steps~date, data=data, sum)
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)
hist(steps_per_day$steps, breaks = 15, xlab='Total number of steps per day', main='Days by total number of steps')
abline(v=mean_steps, col='green', lwd=2)
abline(v=median_steps, col='red', lwd=2, lty=3)
rug(steps_per_day$steps)


steps_per_int <- aggregate(steps~interval, data=data, mean)
max_steps_amount <- max(steps_per_int$steps)
max_steps_int <- which(steps_per_int$steps == max_steps_amount)
plot(steps_per_int$steps, type='l', xlab='5 min time intervals in a day', ylab='Average amount of steps', main='Average daily activity pattern(steps per 5 min time interval)')
abline(h=max_steps_amount, col='red', lwd=2)
abline(v=max_steps_int, col='green', lwd=2)

na_data <- sum(is.na(data$steps))
data_na_fill <- data
time_intervals <- unique(data$interval)
for (i in 1:length(time_intervals)){
	dat_int <- data_na_fill[data_na_fill$interval == time_intervals[i],]
	mean_int <- mean(dat_int$steps, na.rm=TRUE)
	data_na_fill[data_na_fill$interval == time_intervals[i],]$steps[is.na(dat_int$steps)] <- mean_int
}

steps_per_day_fill <- aggregate(steps~date, data=data_na_fill, sum)
mean_steps_fill <- mean(steps_per_day_fill$steps)
median_steps_fill <- median(steps_per_day_fill$steps)
hist(steps_per_day_fill$steps, breaks = 15, xlab='Total number of steps per day', main='Days by total number of steps')
abline(v=mean_steps_fill, col='green', lwd=2)
abline(v=median_steps_fill, col='red', lwd=2, lty=3)
rug(steps_per_day_fill$steps)

data_na_fill$weekday <- weekdays(as.Date(data_na_fill$date))
weekend <- data_na_fill$weekday == 'Saturday' | data_na_fill$weekday == 'Sunday'
steps_per_int_week <- aggregate(steps~interval, data=data_na_fill[!weekend,], mean)
steps_per_int_weekend <- aggregate(steps~interval, data=data_na_fill[weekend,], mean)
par(mfrow = c(2, 1))
plot(steps_per_int_week$steps, type='l', xlab='5 min time intervals in a day', ylab='Average amount of steps', main='Average daily activity pattern weekday')
plot(steps_per_int_weekend$steps, type='l', xlab='5 min time intervals in a day', ylab='Average amount of steps', main='Average daily activity pattern weekend day')
