# Big Data - Econmonics
# HW2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

install.packages('ggplot2')
install.packages('lubridate')
install.packages(c("FactoMineR", "factoextra"));
install.packages("dummies");


library(ggplot2)
library(lubridate)
library("FactoMineR")
library("factoextra");
library(dummies)
theme_set(theme_bw())


#---------------------Loading the dataset------------------------------------------

#local data folder
setwd("Data"); 
list.files()
bike_train <- read.csv("bike_train.csv",header = T)
bike_test <- read.csv("bike_test.csv",header = T)
#--------------creating Datetime object and time fields----------------------------

bike_train$datetime = as.POSIXlt(bike_train$datetime, tz = "", format="%Y-%m-%d %H:%M:%S");
bike_train$date = date(bike_train$datetime)
bike_train$year = year(bike_train$datetime)
bike_train$month = month(bike_train$datetime)
bike_train$weekday =  wday(bike_train$datetime)
bike_train$hour =  hour(bike_train$datetime)
bike_test$datetime = as.POSIXlt(bike_test$datetime, tz = "", format="%Y-%m-%d %H:%M:%S");
bike_test$date = date(bike_test$datetime)
bike_test$year = year(bike_test$datetime)
bike_test$month = month(bike_test$datetime)
bike_test$weekday =  wday(bike_test$datetime)
bike_test$hour =  hour(bike_test$datetime)

#--------------converting features to categorial---------------------------

bike_train$season = factor(bike_train$ season, label = c("winter", "spring", "summer", "fall"))
bike_train$holiday = factor(bike_train$ holiday, label = c("not holiday", "holiday"))
bike_train$workingday = factor(bike_train$ workingday, labels = c("not workingday", "workingday"))
bike_train$weather = factor(bike_train$ weather, labels = c("Good", "Normal", "Bad", "Very Bad"))

bike_test$season = factor(bike_test$ season, label = c("winter", "spring", "summer", "fall"))
bike_test$holiday = factor(bike_test$ holiday, label = c("not holiday", "holiday"))
bike_test$workingday = factor(bike_test$ workingday, labels = c("not workingday", "workingday"))
bike_test$weather = factor(bike_test$ weather, labels = c("Good", "Normal", "Bad"))

#-------------------------Winsorizing & cleaning the data--------------------------
#precent_value_1 <- quantile(bike_train$count,probs =0.01) - for redundency
precent_value_99_train = quantile(bike_train$count,probs =0.99)

precent_value_99_test = quantile(bike_test$temp,probs =0.99)

#dropping rows with extreme values
# set a new table only with the rows where count are between 1% and 99% quantile
bike_train_clean = bike_train[bike_train$count<precent_value_99_train,]
bike_test_clean = bike_test[bike_test$temp<precent_value_99_test,]

#proving there are no Na/Nan values
ind_rows = rowSums(is.na(bike_train_clean)) == ncol(bike_train_clean)
ind_col = colSums(is.na(bike_train_clean)) == nrow(bike_train_clean)
summary(ind_rows)
summary(ind_col)


ind_rows = rowSums(is.na(bike_test_clean)) == ncol(bike_test_clean)
ind_col = colSums(is.na(bike_test_clean)) == nrow(bike_test_clean)
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
hour_by_count_avg =aggregate(cbind(count) ~ hour , data = bike_train_clean, mean, na.rm = TRUE)
print((hour_by_count_avg))

#avg count by weekday
weekday_by_count_avg =aggregate(cbind(count) ~ weekday , data = bike_train_clean, mean, na.rm = TRUE)
print((weekday_by_count_avg))


#avg count by month
month_by_count_avg =aggregate(cbind(count) ~ month , data = bike_train_clean, mean, na.rm = TRUE)
print(month_by_count_avg)


#avg count by workingday
workingday_by_count_avg =aggregate(cbind(count) ~ workingday , data = bike_train_clean, mean, na.rm = TRUE)
print(workingday_by_count_avg)

#avg count by weather
weather_by_count_avg =aggregate(cbind(count) ~ weather , data = bike_train_clean, mean, na.rm = TRUE)
print(weather_by_count_avg)

#avg count by season
season_by_count_avg =aggregate(cbind(count) ~ season , data = bike_train_clean, mean, na.rm = TRUE)
print(season_by_count_avg)

#----------------------------data histograms-----------------------------------------------
#important

#count histograma
ggplot(bike_train_clean)+aes(count)+
  geom_histogram(binwidth = 10);

#temp histograma - not depending on tmp
ggplot(bike_train_clean)+aes(temp)+
  geom_histogram(binwidth = 1);

#atemp histograma - not depending on שtmp
ggplot(bike_train_clean)+aes(atemp)+
  geom_histogram(binwidth = 1);

#windspeed histograma - important - depends on velocity of the wind
ggplot(bike_train_clean)+aes(windspeed)+
  geom_histogram(binwidth = 1);

#huidity histograma - not important
ggplot(bike_train_clean)+aes(humidity)+
  geom_histogram(binwidth = 1);

#season histogram - almost the same number of request during every season
ggplot(bike_train_clean, aes(season)) + stat_count(color="blue",geom = "bar")

#workingday histograma 
ggplot(bike_train_clean, aes(workingday)) + stat_count(color="blue",geom = "bar");

#weather histograma
ggplot(bike_train_clean, aes(weather)) + stat_count(color="blue",geom = "bar")


#----------------------------data bar Plots-----------------------------------------------


#season VS. count bar plot
ggplot(bike_train_clean, aes(x=season, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="season Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#weather VS. count bar plot
ggplot(bike_train_clean, aes(x=weather, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="green") + 
  labs(title="Ordered Bar Chart", 
       subtitle="weather Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#hour VS. count bar plot
ggplot(bike_train_clean, aes(x=hour, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="orange") + 
  labs(title="Ordered Bar Chart", 
       subtitle="hour Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#weekday VS. count bar plot
ggplot(bike_train_clean, aes(x=weekday, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="pink") + 
  labs(title="Ordered Bar Chart", 
       subtitle="weekday Vs count") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#weekday VS. count bar plot
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

#sum: hour VS. weekday VS. count
ggplot(sum_count_hour_by_day, aes(weekday,hour, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="weekday: sunday - suterday",
       y="hour in the day: 00:00-23:00",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Total Bike Rental", 
       fill="Rents Count")


#avg: hour VS. weekday VS. count
ggplot(avg_count_hour_by_day, aes(weekday,hour, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="weekday: sunday - suterday",
       y="hour in the day: 00:00-23:00",
       title = "Time-Series Calendar Heatmap", 
       subtitle="AVG Bike Rental", 
       fill="Rents Count")

#sum: hour VS. month VS. count
ggplot(sum_count_hour_by_month, aes(month,hour, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="Month",
       y="hour in the day: 00:00-23:00",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Total Bike Rental", 
       fill="Rents Count")


#avg: hour VS. month VS. count
ggplot(avg_count_hour_by_month, aes(month,hour, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="month",
       y="hour in the day: 00:00-23:00",
       title = "Time-Series Calendar Heatmap", 
       subtitle="AVG Bike Rental", 
       fill="Rents Count")

#sum: month VS. weekday VS. count
ggplot(sum_count_weekday_by_month, aes(month,weekday, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="Month",
       y="weekday",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Total Bike Rental", 
       fill="Rents Count")


#avg: month VS. weekday VS. count
ggplot(avg_count_weekday_by_month, aes(month,weekday, fill=count )) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="month",
       y="weekday",
       title = "Time-Series Calendar Heatmap", 
       subtitle="AVG Bike Rental", 
       fill="Rents Count")


#----------------------------Seasonally Statistics-----------------------------------------------

sum_count_season_hour = aggregate(cbind(count) ~ hour +season , data = bike_train_clean, sum, na.rm = TRUE)
avg_count_season_hour = aggregate(cbind(count) ~ hour +season , data = bike_train_clean, mean, na.rm = TRUE)
sum_count_season_weekday = aggregate(cbind(count) ~ weekday +season , data = bike_train_clean, sum, na.rm = TRUE)
avg_count_season_weekday = aggregate(cbind(count) ~ weekday +season , data = bike_train_clean, mean, na.rm = TRUE)

# sum-count area-plot: season VS. hour vS. count
ggplot() + geom_area(aes(y = count, x = hour, fill = season), data = sum_count_season_hour,
                     stat="identity")

# avg-count area-plot: season VS. hour vS. count
ggplot() + geom_area(aes(y = count, x = hour, fill = season), data = avg_count_season_hour,
                     stat="identity")

# sum-count area-plot: season VS. weekday vS. count
ggplot() + geom_area(aes(y = count, x = weekday, fill = season), data = sum_count_season_weekday,
                     stat="identity")

# avg-count area-plot: season VS. weekday vS. count
ggplot() + geom_area(aes(y = count, x = weekday, fill = season), data = avg_count_season_weekday,
                     stat="identity")

#scattered-plot: windspeed
ggplot(bike_train_clean, aes(x=windspeed, y=count)) + 
  geom_point(aes(col=season)) +   # draw points
  xlim(c(0, 100)) + 
  ylim(c(0, 800)) 

#scattered-plot: hunidity
ggplot(bike_train_clean, aes(x=humidity, y=count)) + 
  geom_point(aes(col=season)) +   # draw points
  xlim(c(0, 100)) + 
  ylim(c(0, 800))

#scattered-plot: temp
ggplot(bike_train_clean, aes(x=temp, y=count)) + 
  geom_point(aes(col=season)) +   # draw points
  xlim(c(0, 100)) + 
  ylim(c(0, 800))


#scattered-plot: atemp
ggplot(bike_train_clean, aes(x=atemp, y=count)) + 
  geom_point(aes(col=season)) +   # draw points
  xlim(c(0, 100)) + 
  ylim(c(0, 800))

#-------------------------------------Section 2-----------------------------------------------
#-------------------------------------Linear Regression---------------------------------------

#creating a liner regression (between count and temp) object
reg_temp = lm(count~temp, data=bike_train_clean ) 
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

#Estimate a model with count as dep. Variable and temp and hour are the indep. variables

multi_reg = lm(count~temp+hour, data=bike_train_clean_train);
summary(multi_reg)

ggplot(data=bike_train_clean_train)+aes(x=temp,y=count,color = hour  )  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(multi_reg)),size=1)+
  labs(title="temp against count",x="temp",y="count")

bike_train_clean_train$hour_categor = factor(bike_train_clean_train$hour)
bike_train_clean_validation$hour_categor = factor(bike_train_clean_validation$hour)


multi_reg_categorical= lm(count~temp+hour_categor, data=bike_train_clean_train);
summary(multi_reg)

ggplot(data=bike_train_clean_train)+aes(x=temp,y=count,color = hour_categor  )  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(multi_reg_categorical)),size=1)+
  labs(title="temp against count",x="temp",y="count")

#a new categorical variable of period in the day
bike_train_clean_train$daytime_categor = NA
bike_train_clean_train$daytime_categor = ifelse(bike_train_clean_train$hour>=6 & bike_train_clean_train$hour<=11,"morning", bike_train_clean_train$daytime_categor)
bike_train_clean_train$daytime_categor = ifelse(bike_train_clean_train$hour>=12 & bike_train_clean_train$hour<=17,"noon",bike_train_clean_train$daytime_categor)
bike_train_clean_train$daytime_categor = ifelse(bike_train_clean_train$hour>=18 & bike_train_clean_train$hour<=23,"evening",bike_train_clean_train$daytime_categor)
bike_train_clean_train$daytime_categor = ifelse(bike_train_clean_train$hour>=0 & bike_train_clean_train$hour<=5,"night",bike_train_clean_train$daytime_categor)

bike_train_clean_validation$daytime_categor = NA
bike_train_clean_validation$daytime_categor = ifelse(bike_train_clean_validation$hour>=6 & bike_train_clean_validation$hour<=11,"morning", bike_train_clean_validation$daytime_categor)
bike_train_clean_validation$daytime_categor = ifelse(bike_train_clean_validation$hour>=12 & bike_train_clean_validation$hour<=17,"noon",bike_train_clean_validation$daytime_categor)
bike_train_clean_validation$daytime_categor = ifelse(bike_train_clean_validation$hour>=18 & bike_train_clean_validation$hour<=23,"evening",bike_train_clean_validation$daytime_categor)
bike_train_clean_validation$daytime_categor = ifelse(bike_train_clean_validation$hour>=0 & bike_train_clean_validation$hour<=5,"night",bike_train_clean_validation$daytime_categor)

bike_test$daytime_categor = NA
bike_test$daytime_categor = ifelse(bike_test$hour>=6 & bike_test$hour<=11,"morning", bike_test$daytime_categor)
bike_test$daytime_categor = ifelse(bike_test$hour>=12 & bike_test$hour<=17,"noon",bike_test$daytime_categor)
bike_test$daytime_categor = ifelse(bike_test$hour>=18 & bike_test$hour<=23,"evening",bike_test$daytime_categor)
bike_test$daytime_categor = ifelse(bike_test$hour>=0 & bike_test$hour<=5,"night",bike_test$daytime_categor)


#linear regression with time as a factor variabla
multi_reg_daytime = lm(count~temp+daytime_categor, data=bike_train_clean_train)
summary(multi_reg_interact)

ggplot(data=bike_train_clean_train)+aes(x=temp,y=count,color=daytime_categor)  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(multi_reg_daytime)),size=1)

#linear regression with time as a factor variabla and interaction
multi_reg_daytime_interaction = lm(count~temp*daytime_categor, data=bike_train_clean_train)
summary(multi_reg_interact)

ggplot(data=bike_train_clean_train)+aes(x=temp,y=count,color=daytime_categor)  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(multi_reg_daytime_interaction)),size=1)

#Checking which model has the best training R^2
summary(multi_reg_daytime)$r.squared
summary(multi_reg_daytime_interaction)$r.squared
summary(multi_reg)$r.squared
summary(multi_reg_categorical)$r.squared
# the fact that one model has a better r.squared doesn't mean anything if this model has more variables

validation_sse=sum((predict(object = multi_reg, newdata = bike_train_clean_validation) - bike_train_clean_validation$count)^2)
validation_sse_categor=sum((predict(object = multi_reg_categorical, newdata = bike_train_clean_validation) - bike_train_clean_validation$count)^2)
validation_sse_daytime=sum((predict(object =multi_reg_daytime,newdata =bike_train_clean_validation ) - bike_train_clean_validation$count)^2)
validation_sse_daytime_interact=sum((predict(object =multi_reg_daytime_interaction,newdata =bike_train_clean_validation ) - bike_train_clean_validation$count)^2)
validation_sse_daytime_interact>validation_sse_daytime
print(validation_sse_daytime_interact)
print(validation_sse_daytime)
print(validation_sse)
print(validation_sse_categor)


#---------------------------------------c--------------------------------------------------

#-------------------------------------Section 3-----------------------------------------------
#-------------------------------------Final Model---------------------------------------
# linear regression with one variable

#creating new features - realtive humidity, australian atemp and converting categorial to dummies
bike_train_clean_train$relative_humidity = bike_train_clean_train$humidity/100;
bike_train_clean_train$australian_atemp = bike_train_clean_train$temp+0.33*bike_train_clean_train$relative_humidity*6.105*exp(((17.27*bike_train_clean_train$temp)/(237.7+bike_train_clean_train$temp)))+0.7*bike_train_clean_train$windspeed-4;
#normalizing austrlaian temp using method: (value-min(value)/max(value)-min(value))
bike_train_clean_train$normal_australian_atemp = ((bike_train_clean_train$australian_atemp-min(bike_train_clean_train$australian_atemp))/(max(bike_train_clean_train$australian_atemp)-min(bike_train_clean_train$australian_atemp)));
bike_train_clean_train$normal_atemp = ((bike_train_clean_train$atemp-min(bike_train_clean_train$atemp))/(max(bike_train_clean_train$atemp)-min(bike_train_clean_train$atemp)));
bike_train_clean_train$normal_avg_atemp = (bike_train_clean_train$normal_australian_atemp+bike_train_clean_train$normal_atemp)/2;
bike_train_clean_train = cbind(bike_train_clean_train, dummy(bike_train_clean_train$season,sep = "_"));
bike_train_clean_train = cbind(bike_train_clean_train, dummy(bike_train_clean_train$weather,sep = "_"));
#changing the names of the columns to be more functional
colnames(bike_train_clean_train)[colnames(bike_train_clean_train)=="bike_train_clean_train_spring"] = "spring";
colnames(bike_train_clean_train)[colnames(bike_train_clean_train)=="bike_train_clean_train_winter"] = "winter";
colnames(bike_train_clean_train)[colnames(bike_train_clean_train)=="bike_train_clean_train_summer"] = "summer";
colnames(bike_train_clean_train)[colnames(bike_train_clean_train)=="bike_train_clean_train_fall"] = "fall";
colnames(bike_train_clean_train)[colnames(bike_train_clean_train)=="bike_train_clean_train_Good"] = "Good";
colnames(bike_train_clean_train)[colnames(bike_train_clean_train)=="bike_train_clean_train_Normal"] = "Normal";
colnames(bike_train_clean_train)[colnames(bike_train_clean_train)=="bike_train_clean_train_Bad"] = "Bad";
colnames(bike_train_clean_train)[colnames(bike_train_clean_train)=="bike_train_clean_train_Very Bad"] = "Very_Bad";

##let's watch the correletion between the variables in order to see which weights to put
cor_data = bike_train_clean_train[,c("count", "spring","winter", "summer", "fall", "Good", "Normal","Bad","Very_Bad","normal_atemp","normal_australian_atemp","normal_avg_atemp","temp","humidity","windspeed","atemp")]
cor(cor_data)

#creating calculated weighted variable based on the correlation matrix

#-----------------agg_climate regression----------------------
bike_train_clean_train$agg_climate = (bike_train_clean_train$normal_avg_atemp)+ (bike_train_clean_train$Good+bike_train_clean_train$Normal+bike_train_clean_train$Bad+bike_train_clean_train$Very_Bad)+bike_train_clean_train$spring+bike_train_clean_train$summer +bike_train_clean_train$fall+bike_train_clean_train$winter;
summary(bike_train_clean_train$agg_climate)
agg_climate_regression = lm(count ~ agg_climate, data=bike_train_clean_train, weights=1/(agg_climate^2))
summary(agg_climate_regression)

ggplot(data=bike_train_clean_train)+aes(x=agg_climate,y=count,color=weather)  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(agg_climate_regression)),size=1)

#------------------agg_climate & hour_categor regression---------------------------------------------
agg_climate_hour_regression = lm(count ~ agg_climate+hour_categor, data=bike_train_clean_train)
summary(agg_climate_hour_regression)

ggplot(data=bike_train_clean_train)+aes(x=agg_climate,y=count,color = hour_categor)  + 
  geom_point(alpha=0.6)+ 
  geom_line(mapping = aes(y=predict(agg_climate_hour_regression)),size=1)

##--------------------------columns for validation (same as train-------------------
#again but for the validation data
#creating new features - realtive humidity, australian atemp and converting categorial to dummies
bike_train_clean_validation$relative_humidity = bike_train_clean_validation$humidity/100;
bike_train_clean_validation$australian_atemp = bike_train_clean_validation$temp+0.33*bike_train_clean_validation$relative_humidity*6.105*exp(((17.27*bike_train_clean_validation$temp)/(237.7+bike_train_clean_validation$temp)))+0.7*bike_train_clean_validation$windspeed-4;
#normalizing austrlaian temp using method: (value-mean(value)/max(value)-min(value))
bike_train_clean_validation$normal_australian_atemp = ((bike_train_clean_validation$australian_atemp-min(bike_train_clean_validation$australian_atemp))/(max(bike_train_clean_validation$australian_atemp)-min(bike_train_clean_validation$australian_atemp)));
bike_train_clean_validation$normal_atemp = ((bike_train_clean_validation$atemp-min(bike_train_clean_validation$atemp))/(max(bike_train_clean_validation$atemp)-min(bike_train_clean_validation$atemp)));
bike_train_clean_validation$normal_avg_atemp = (bike_train_clean_validation$normal_australian_atemp+bike_train_clean_validation$normal_atemp)/2;

bike_train_clean_validation = cbind(bike_train_clean_validation, dummy(bike_train_clean_validation$season,sep = "_"));
bike_train_clean_validation = cbind(bike_train_clean_validation, dummy(bike_train_clean_validation$weather,sep = "_"));
colnames(bike_train_clean_validation)[colnames(bike_train_clean_validation)=="bike_train_clean_validation_spring"] = "spring";
colnames(bike_train_clean_validation)[colnames(bike_train_clean_validation)=="bike_train_clean_validation_winter"] = "winter";
colnames(bike_train_clean_validation)[colnames(bike_train_clean_validation)=="bike_train_clean_validation_summer"] = "summer";
colnames(bike_train_clean_validation)[colnames(bike_train_clean_validation)=="bike_train_clean_validation_fall"] = "fall";
colnames(bike_train_clean_validation)[colnames(bike_train_clean_validation)=="bike_train_clean_validation_Good"] = "Good";
colnames(bike_train_clean_validation)[colnames(bike_train_clean_validation)=="bike_train_clean_validation_Normal"] = "Normal";
colnames(bike_train_clean_validation)[colnames(bike_train_clean_validation)=="bike_train_clean_validation_Bad"] = "Bad";
colnames(bike_train_clean_validation)[colnames(bike_train_clean_validation)=="bike_train_clean_validation_Very Bad"] = "Very_Bad";
bike_train_clean_validation$agg_climate = (bike_train_clean_validation$normal_avg_atemp)+ (bike_train_clean_validation$Good +bike_train_clean_validation$Normal +bike_train_clean_validation$Bad) +bike_train_clean_validation$spring+bike_train_clean_validation$summer +bike_train_clean_validation$fall+bike_train_clean_validation$winter;


validation_sse_ach=sum((predict(object =agg_climate_hour_regression,newdata =bike_train_clean_validation ) - bike_train_clean_validation$count)^2)

#------------------------------------Predict and ESS-------------------------------------------------
#----------------------------------------------------------------------------------------------------
#again but for the test data
#creating new features - realtive humidity, australian atemp and converting categorial to dummies
bike_test_clean$relative_humidity = bike_test_clean$humidity/100;
bike_test_clean$australian_atemp = bike_test_clean$temp+0.33*bike_test_clean$relative_humidity*6.105*exp(((17.27*bike_test_clean$temp)/(237.7+bike_test_clean$temp)))+0.7*bike_test_clean$windspeed-4;
#normalizing austrlaian temp using method: (value-mean(value)/max(value)-min(value))
bike_test_clean$normal_australian_atemp = ((bike_test_clean$australian_atemp-min(bike_test_clean$australian_atemp))/(max(bike_test_clean$australian_atemp)-min(bike_test_clean$australian_atemp)));
bike_test_clean$normal_atemp = ((bike_test_clean$atemp-min(bike_test_clean$atemp))/(max(bike_test_clean$atemp)-min(bike_test_clean$atemp)));
bike_test_clean$normal_avg_atemp = (bike_test_clean$normal_australian_atemp+bike_test_clean$normal_atemp)/2;

bike_test_clean = cbind(bike_test_clean, dummy(bike_test_clean$season,sep = "_"));
bike_test_clean = cbind(bike_test_clean, dummy(bike_test_clean$weather,sep = "_"));
colnames(bike_test_clean)[colnames(bike_test_clean)=="bike_test_clean_spring"] = "spring";
colnames(bike_test_clean)[colnames(bike_test_clean)=="bike_test_clean_winter"] = "winter";
colnames(bike_test_clean)[colnames(bike_test_clean)=="bike_test_clean_summer"] = "summer";
colnames(bike_test_clean)[colnames(bike_test_clean)=="bike_test_clean_fall"] = "fall";
colnames(bike_test_clean)[colnames(bike_test_clean)=="bike_test_clean_Good"] = "Good";
colnames(bike_test_clean)[colnames(bike_test_clean)=="bike_test_clean_Normal"] = "Normal";
colnames(bike_test_clean)[colnames(bike_test_clean)=="bike_test_clean_Bad"] = "Bad";
colnames(bike_test_clean)[colnames(bike_test_clean)=="bike_test_clean_Very Bad"] = "Very_Bad";
bike_test_clean$hour_categor = factor(bike_test_clean$hour);
bike_test_clean$agg_climate = (bike_test_clean$normal_avg_atemp)+ (bike_test_clean$Good +bike_test_clean$Normal +bike_test_clean$Bad +bike_test_clean$spring+bike_test_clean$summer +bike_test_clean$fall+bike_test_clean$winter);

#------------------------------------creating the columns-------------------------------------------------

bike_test_clean$results_count_predict = predict(object = agg_climate_hour_regression, newdata = bike_test_clean);
#zeroing all negetive values
bike_test_clean$results_count_predict = ifelse(bike_test_clean$results_count_predict<0,0, bike_test_clean$results_count_predict)
summary(bike_test_clean$results_count_predict)
predict_sse=sum((predict(object = agg_climate_hour_regression, newdata = bike_test_clean) - bike_test_clean$count_predict)^2)
#writing to file
write.table(bike_test_clean, file = "final_results.csv",sep=",");

#-----------------------------The End-----------------------------------------------------------
