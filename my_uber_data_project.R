# setwd('F:\git_project\R_project_on_uber_data')

library(tidyverse)

apr_dt <- read.csv("apr14.csv")
may_dt <- read.csv("may14.csv")
jun_dt <- read.csv("jun14.csv")
jul_dt <- read.csv("jul14.csv")
aug_dt <- read.csv("aug14.csv")
sep_dt <- read.csv("sep14.csv")

data <- rbind(apr_dt,may_dt, jun_dt, jul_dt, aug_dt, sep_dt)
head(data,2)

# splitting Date.Time & add to new column
data[c('date', 'time')] <- str_split_fixed(data$Date.Time, ' ', 2)

# splitting Date  & add to new column
data[c('month', 'day','year')] <- str_split_fixed(data$date, '/', 3)

# splitting time  & add to new column
data[c('hour', 'minute','second')] <- str_split_fixed(data$time, ':', 3)

# create a week day column
data['weekDays']<-strftime(as.Date(data$date,format="%m/%d/%Y"),'%A')


data = subset(data, select = -c(date,time) )
View(data)

# convert column's data types to factor
data[5:10]<- data%>%
  select(5:10) %>%  
  mutate_all(as.integer) %>% 
  mutate_all(as.factor)


# Hourly Booking View
hour_data <- data%>%
  group_by(hour) %>%
  summarise('count'=n()) 
hour_data

ggplot(hour_data, aes(hour, count, fill=hour)) + 
  geom_bar( stat = "identity") +
  ggtitle("Hourly Trips Booking") +
  theme(legend.position = "none")


# Daily Booking View
day_data <- data%>%
  group_by(day) %>%
  summarise('count'=n()) 
day_data

ggplot(day_data, aes(day, count, fill=day)) + 
  geom_bar( stat = "identity") +
  ggtitle("Daily Trips Booking")+
  theme(legend.position = "none") 


# Monthly Booking View
month_data <- data%>%
  group_by(month) %>%
  summarise('count'=n()) 
month_data

ggplot(month_data, aes(month, count, fill=month,alpha=0.8)) + 
  geom_bar( stat = "identity") +
  ggtitle("monthly Trips Booking")


# Every Week Days Booking View
weekDays_data <- data%>%
  group_by(weekDays) %>%
  summarise('count'=n()) 
month_data

ggplot(weekDays_data, aes(weekDays, count, fill=weekDays,alpha=0.8)) + 
  geom_bar( stat = "identity") +
  ggtitle("monthly Trips Booking") 


# View on Hourly Trips Book in Every Month
month_hour <- data %>%
  group_by(month, hour) %>%
  summarize(count = n())

ggplot(month_hour, aes(hour, count, fill=month,alpha=1)) + 
  geom_bar(stat = "identity",col=c('black')) +
  ggtitle("Hourly Trips Book in Every Month")


# View on Daily Trips Book in Every Month
month_day <- data %>%
  group_by(day,month) %>%
  summarize(count = n())

ggplot(month_day, aes(day, count, fill=month,alpha=1)) + 
  geom_bar(stat = "identity",col=c('black')) +
  ggtitle("Daily Trips Book in Every Month")


# View on Every Week Day's Trips Book in Every Month
month_weekDays <- data%>%
  group_by(month, weekDays) %>%
  summarize(count = n())

ggplot(month_weekDays, aes(month, count, fill = weekDays)) +
  geom_bar( stat = "identity", position = "dodge",col=c('black')) +
  ggtitle("Trips by Day and Month")
  scale_fill_manual(values = colors)


# View on Base Bases 
ggplot(data, aes(Base,fill=Base)) + 
  geom_bar() +ggtitle("Trips by Bases") +theme(legend.position = "none")


# View on Booking by Bases in Every Month
ggplot(data, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +ggtitle("Trips by Bases in Every Month")


# View on Booking by Bases in Different week days 
ggplot(data, aes(Base, fill = weekDays)) + 
  geom_bar(position = "dodge",col=c('black')) +
  ggtitle("Trips by Bases in Different week days")
  

#Heat Map by Day and Month
ggplot(month_day, aes(day, month, fill = count)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Day and Month")


#Heat Map by Month and Day of Week
ggplot(month_weekDays, aes(weekDays, month, fill = count)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")


# Radial View on Different Week day's Booking
ggplot(weekDays_data, aes(weekDays, count, fill=count,)) + 
  geom_bar(stat="identity", width=0.6,col=c('black')) +
  coord_polar("y", start=0)+
  ggtitle("Different Week day's Trips Booking")


# Radial View on Monthly Booking
ggplot(month_data, aes(month, count, fill=month,)) + 
  geom_bar(stat="identity", width=0.6,col=c('black')) +
  coord_polar("y", start=0)+
  ggtitle("Monthly Trips Booking")