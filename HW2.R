install.packages('ggplot2')
install.packages('lubridate')



library(ggplot2)
library(lubridate)
theme_set(theme_bw())


#---------------------Loading the dataset------------------------------------------

#local data folder
setwd("Data"); 
list.files()
bike_train <- read.csv("bike_train.csv",header = T)

#--------------creating Datetime object and time fields----------------------------

bike_train$datetime = as.POSIXlt(bike_train$datetime, tz = "", format="%Y-%m-%d %H:%M:%S");
bike_train$date = date(bike_train$datetime)
bike_train$year = year(bike_train$datetime)
bike_train$month = month(bike_train$datetime)
bike_train$weekday =  wday(bike_train$datetime)
bike_train$time =  time(bike_train$datetime)
bike_train$hour =  hour(bike_train$datetime)

#--------------converting features to categorial---------------------------

bike_train$season = factor(bike_train$ season, label = c("winter", "spring", "summer", "fall"))
bike_train$holiday = factor(bike_train$ holiday, label = c("not holiday", "holiday"))
bike_train$workingday = factor(bike_train$ workingday, labels = c("not workingday", "workingday"))
bike_train$weather = factor(bike_train$ weather, labels = c("Good", "Normal", "Bad", "Very Bad"))


#-------------------------Winsorizing & cleaning the data--------------------------
#precent_value_1 <- quantile(bike_train$count,probs =0.01) - for redundency
precent_value_99 = quantile(bike_train$count,probs =0.99)

#dropping rows with extreme values
# set a new table only with the rows where count are between 1% and 99% quantile
bike_train_clean = bike_train[bike_train$count<precent_value_99,]
#proving there are no Na/Nan values
ind_rows = rowSums(is.na(bike_train_clean)) == ncol(bike_train_clean)
ind_col = colSums(is.na(bike_train_clean)) == nrow(bike_train_clean)
summary(ind_rows)
summary(ind_col)

#------------------------------------------------------------------------------------------
#----------------------------statistial conclusions-----------------------------------------------------
#------------------------------------------------------------------------------------------

#----------------------------general look the the data-----------------------------------------------
summary(bike_train_clean)
summary(bike_train_clean$count)
summary(bike_train_clean$humidity)
summary(bike_train_clean$windspeed)

#avg count by hour
hour_by_count_avg=as.data.frame(t(sapply(by(bike_train_clean$count,bike_train_clean$hour,mean), I)),row)
names(hour_by_count_avg)= c("hour", "avg")
print((hour_by_count_avg))

#avg count by weekday
weekday_by_count_avg=as.data.frame(t(sapply(by(bike_train_clean$count,bike_train_clean$weekday,mean), I)),col)
names(weekday_by_count_avg)= c("weekday", "avg")
print((weekday_by_count_avg))

#avg count by month
month_by_count_avg=as.data.frame(t(sapply(by(bike_train_clean$count,bike_train_clean$month,mean), I)),row)
names(month_by_count_avg)= c("month", "avg")
print((month_by_count_avg))

#----------------------------data histograms-----------------------------------------------
#important
ggplot(bike_train_clean)+aes(count)+
  geom_histogram(binwidth = 10);

#not depending on tmp
ggplot(bike_train_clean)+aes(temp)+
  geom_histogram(binwidth = 1);

ggplot(bike_train_clean)+aes(atemp)+
  geom_histogram(binwidth = 1);

#important - depends on velocity of the wind
ggplot(bike_train_clean)+aes(windspeed)+
  geom_histogram(binwidth = 1);

#not important
ggplot(bike_train_clean)+aes(humidity)+
  geom_histogram(binwidth = 1);

names(bike_train_clean)
#almost the same number of request during every season
ggplot(bike_train_clean, aes(season)) + stat_count(color="blue",geom = "bar")
#needs tp be avarged (taking also the holidays)
ggplot(bike_train_clean, aes(workingday)) + stat_count(color="blue",geom = "bar")
ggplot(bike_train_clean, aes(weather)) + stat_count(color="blue",geom = "bar")


#----------------------------data bar Plots-----------------------------------------------
ggplot(bike_train_clean, aes(x=season, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="season Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


ggplot(bike_train_clean, aes(x=workingday, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="blue") + 
  labs(title="Ordered Bar Chart", 
       subtitle="workingday Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(bike_train_clean, aes(x=weather, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="green") + 
  labs(title="Ordered Bar Chart", 
       subtitle="weather Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(bike_train_clean, aes(x=hour, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="orange") + 
  labs(title="Ordered Bar Chart", 
       subtitle="hour Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(bike_train_clean, aes(x=weekday, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="gray") + 
  labs(title="Ordered Bar Chart", 
       subtitle="weekday Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(bike_train_clean, aes(x=month, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="purple") + 
  labs(title="Ordered Bar Chart", 
       subtitle="month Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#----------------------------data heatmap-----------------------------------------------


sum_count_hour_by_day = aggregate(cbind(count) ~ hour + weekday , data = bike_train_clean, sum, na.rm = TRUE)
avg_count_hour_by_day = aggregate(cbind(count) ~ hour + weekday , data = bike_train_clean, mean, na.rm = TRUE)
sum_count_hour_by_month = aggregate(cbind(count) ~ hour + month , data = bike_train_clean, sum, na.rm = TRUE)
avg_count_hour_by_month = aggregate(cbind(count) ~ hour + month , data = bike_train_clean, mean, na.rm = TRUE)
sum_count_weekday_by_month = aggregate(cbind(count) ~ weekday + month , data = bike_train_clean, sum, na.rm = TRUE)
avg_count_weekday_by_month = aggregate(cbind(count) ~ weekday + month , data = bike_train_clean, mean, na.rm = TRUE)

ggplot(sum_count_hour_by_day, aes(weekday,hour, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="weekday: sunday - suterday",
       y="hour in the day: 00:00-23:00",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Bike Rental", 
       fill="Rents Count")



ggplot(avg_count_hour_by_day, aes(weekday,hour, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="weekday: sunday - suterday",
       y="hour in the day: 00:00-23:00",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Bike Rental", 
       fill="Rents Count")


ggplot(sum_count_hour_by_month, aes(month,hour, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="Month",
       y="hour in the day: 00:00-23:00",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Bike Rental", 
       fill="Rents Count")



ggplot(avg_count_hour_by_month, aes(month,hour, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="month",
       y="hour in the day: 00:00-23:00",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Bike Rental", 
       fill="Rents Count")


ggplot(sum_count_weekday_by_month, aes(month,weekday, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="Month",
       y="weekday",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Bike Rental", 
       fill="Rents Count")



ggplot(avg_count_weekday_by_month, aes(month,weekday, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="month",
       y="weekday",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Bike Rental", 
       fill="Rents Count")


#----------------------------Seasonally Statistics-----------------------------------------------

sum_count_season_hour = aggregate(cbind(count) ~ hour +season , data = bike_train_clean, sum, na.rm = TRUE)
avg_count_season_hour = aggregate(cbind(count) ~ hour +season , data = bike_train_clean, mean, na.rm = TRUE)
sum_count_season_weekday = aggregate(cbind(count) ~ weekday +season , data = bike_train_clean, sum, na.rm = TRUE)
avg_count_season_weekday = aggregate(cbind(count) ~ weekday +season , data = bike_train_clean, mean, na.rm = TRUE)

print(sum_count_season_hour)

# plot
ggplot() + geom_area(aes(y = count, x = hour, fill = season), data = sum_count_season_hour,
                     stat="identity")
ggplot() + geom_area(aes(y = count, x = hour, fill = season), data = avg_count_season_hour,
                     stat="identity")
ggplot() + geom_area(aes(y = count, x = weekday, fill = season), data = sum_count_season_weekday,
                     stat="identity")
ggplot() + geom_area(aes(y = count, x = weekday, fill = season), data = avg_count_season_weekday,
                     stat="identity")

ggplot(bike_train_clean, aes(x=windspeed, y=count)) + 
  geom_point(aes(col=season)) +   # draw points
  xlim(c(0, 100)) + 
  ylim(c(0, 800)) 


ggplot(bike_train_clean, aes(x=humidity, y=count)) + 
  geom_point(aes(col=season)) +   # draw points
  xlim(c(0, 100)) + 
  ylim(c(0, 800))



typeof(bike_train_clean$time)
#-------------------------------------Section 2------------------------------------
#-------------------------------------Linear Regression------------------------------------
#Run a linear regression that explains count variable with temp variable
reg_temp = lm(count~temp, data=bike_train_clean ) #creating a liner regression (between count and temp) object
summary(reg_temp)# shows the summary of the results received
coef(reg_temp) # shows the Coefficients estimates of the regression

#Draw a scatter plot and the regression line of this regression
ggplot(data=bike_train_clean)+aes(x=temp,y=count)+
  geom_point(color="darkblue",alpha=0.6)+
  geom_smooth(method = "lm")

#Divide the data to a train (70% of the observations) and validation (30% of the observations) datasets
n = nrow(bike_train_clean)
set.seed(1234) #making the sampeling be the same every time we run it
train = sample(1:n,size =0.7*n,replace = F ) # sampales 70% of the numbers from 1 till the number of rows in bike_train_clean
bike_train_clean_train = bike_train_clean[train,] # creats a new dataset only with the rows sampaled
bike_train_clean_validation = bike_train_clean[-train,] # creats a new dataset only with the rows that were not sampaled

print (train)

#-----------------------------------------------------------------------------------------

unclass(time)
typeof(time)
#Estimate a model with count as dep. Variable and temp and hour are the indep. variables

multi_reg = lm(count~temp+factor(time), data=bike_train_clean_train);
summary(multi_reg)

ggplot(data=bike_train_clean_train)+aes(x=temp,y=count,color = time  )  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(multi_reg)),size=1)+
  labs(title="temp against count",x="temp",y="count")
# all lines have the same slope

multi_reg = lm(count~temp+factor(time), data=bike_train_clean_train);
summary(multi_reg)

ggplot(data=bike_train_clean_train)+aes(x=temp,y=count,color = time  )  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(multi_reg)),size=1)+
  labs(title="temp against count",x="temp",y="count")


#plot the regression line you created before for sale price with GrLivArea and and LotShape


#linear regression with time as a factor variabla
multi_reg_interact <- lm(count~temp*time , data=bike_train_clean_train)
summary(multi_reg_interact)

ggplot(data=bike_train_clean_train)+aes(x=temp,y=count,color=time)  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(multi_reg_interact)),size=1)
#each line has a different slope

#a new categorical variable of period in the day
bike_train_clean_train$daytime = NA
bike_train_clean_train$time = ifelse(bike_train_clean_train$time>=06 & bike_train_clean_train$time<=11,"morning", bike_train_clean_train$time)
bike_train_clean_train$time = ifelse(bike_train_clean_train$time>=12 & bike_train_clean_train$time<=17,"noon",bike_train_clean_train$time)
bike_train_clean_train$time = ifelse(bike_train_clean_train$time>=18 & bike_train_clean_train$time<=23,"evening",bike_train_clean_train$time)
bike_train_clean_train$time = ifelse(bike_train_clean_train$time>=0 & bike_train_clean_train$time<=5,"night",bike_train_clean_train$time)

#linear regression with time as a factor variabla
multi_reg_interact = lm(count~temp*time, data=bike_train_clean_train)
summary(multi_reg_interact)

ggplot(data=bike_train_clean_train)+aes(x=temp,y=count,color=time)  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(multi_reg_interact)),size=1)


#Checking which model has the best training R^2


summary(multi_reg)$r.squared
summary(multi_reg_interact)$r.squared
# the fact that one model has a better r.squared doesn't mean anything if this model has more variables

validation_sse<-sum((predict(object =multi_reg,newdata =housing_validation ) - housing_validation$SalePrice)^2)
validation_sse_interact<-sum((predict(object =multi_reg_interact,newdata =housing_validation ) - housing_validation$SalePrice)^2)
validation_sse_interact>validation_sse
# we got that the model with interaction gives better sse, so we will choose him over the model without the interaction
