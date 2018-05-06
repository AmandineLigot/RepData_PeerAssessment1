#read the data and put it in a data frame

df <- read.csv(unzip("activity.zip"), header = TRUE, sep = ",")

library("dplyr")
library("ggplot2")

#number of step taken each day, removing the NAs
a <- df %>%
  group_by(date) %>%
  summarise(sum = sum(steps, na.rm = TRUE))

#plot a histogram of total number of steps per day
ggplot(data = a, aes(date, sum)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Evolution of total number of steps taken each day") +
  theme(plot.title = element_text(hjust = 0.5), plot.margin = unit(c(0.5,1,0.5,1), "cm")) +
  scale_x_discrete(breaks = c(as.character(a$date[[1]]), as.character(a$date[[11]]), as.character(a$date[[21]]), as.character(a$date[[31]]), as.character(a$date[[41]]), as.character(a$date[[51]]), as.character(a$date[[61]])))
  
ggsave("plot1.png", path = "D:\\Git_Coursera\\RepData_PeerAssessment1\\instructions_fig", device = "png")

#Calculate and report the mean and median of the total steps per day
mean_steps <- mean(a$sum)
median_steps <- median(a$sum)

print(mean_steps)
print(median_steps)

#second question
b <- df %>%
  group_by(interval) %>%
  summarise(mean = mean(steps, na.rm = TRUE))

png(file = "D:\\Git_Coursera\\RepData_PeerAssessment1\\instructions_fig\\plot 2.png")

plot(b, type = "l", ylab = "Average number of steps taken", xlab = "Time interval")
title(main = "Average daily activity pattern")

dev.off()

#answer to the second question
time_interval <- b$interval[which(b$mean == max(b$mean))]
print(time_interval)

#third question
#Number of NAs
nb_NA <- sum(is.na(df$steps))
print(nb_NA)

#will replace the NAs by the corresponding average on the given 5min-interval and create a new dataset
df_new <- data.frame(steps = numeric(), date = factor(), interval = integer() )

for (i in 1: dim(df)[[1]]) {
  if(is.na(df$steps[i]) == TRUE) {
    
    newrow <- data.frame(steps = b$mean[which(df$interval[i] == b$interval)], date = df$date[i], interval = df$interval[i])
    df_new <- rbind(df_new,newrow)
    
  } else {
    newrow <- data.frame(steps = df$steps[i], date = df$date[i], interval = df$interval[i])
    df_new <- rbind(df_new,newrow) 
    
  }
  
}

#number of step taken each day with the 
c <- df_new %>%
  group_by(date) %>%
  summarise(sum = sum(steps))

#plot a histogram of total number of steps per day
ggplot(data = c, aes(date, sum)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Evolution of total number of steps taken each day") +
  theme(plot.title = element_text(hjust = 0.5), plot.margin = unit(c(0.5,1,0.5,1), "cm")) +
  scale_x_discrete(breaks = c(as.character(c$date[[1]]), as.character(c$date[[11]]), as.character(c$date[[21]]), as.character(c$date[[31]]), as.character(c$date[[41]]), as.character(c$date[[51]]), as.character(c$date[[61]])))

ggsave("plot3.png", path = "D:\\Git_Coursera\\RepData_PeerAssessment1\\instructions_fig", device = "png")

#Calculate and report the mean and median of the total steps per day
mean_steps_new <- mean(c$sum)
median_steps_new <- median(c$sum)

print(mean_steps_new)
print(median_steps_new)

#Fourth question
Sys.setlocale("LC_TIME", "English")

for (i in 1:dim(df_new)[[1]]) {
  if(any(weekdays(as.Date(df_new$date[i])) == c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))) {
    
    df_new$weektime[i] <- "weekday"
  } else {
    
    df_new$weektime[i] <- "weekend"
  }
    
}
df_new$weektime <- factor(df_new$weektime)

d <- df_new %>%
  group_by(interval, weektime) %>%
  summarise(mean = mean(steps))

ggplot(d, aes(interval, mean)) +
  geom_line() +
  facet_wrap(~weektime, nrow = 2, ncol = 1) +
  labs( title = "Average daily activity in weekdays and weekend", y = "Mean number of steps", x = "Time interval") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plot4.png", path = "D:\\Git_Coursera\\RepData_PeerAssessment1\\instructions_fig", device = "png")
