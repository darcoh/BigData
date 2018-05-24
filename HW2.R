#local data folder
setwd("Data"); 
list.files()
bike_train <- read.csv("bike_train.csv",header = T)
print(bike_train$datetime)
bike_train$datetime = as.POSIXlt(bike_train$datetime, tz = "", format="%Y-%m-%d %H:%M:%S");
bike_train$date = as.Date(bike_train$datetime);
bike_train$time =  format(as.POSIXct(bike_train$datetime) ,format = "%H:%M:%S") 
#------------------------------------------------------------------------------------------

bike_train$season = factor(bike_train$ season, label = c("winter", "spring", "summer", "fall"))
bike_train$holiday = factor(bike_train$ holiday, label = c("not holiday", "holiday"))
bike_train$workingday = factor(bike_train$ workingday, labels = c("not workingday", "workingday"))
bike_train$weather = factor(bike_train$ weather, labels = c("Good", "Normal", "Bad", "Very Bad"))

#------------------------------------------------------------------------------------------
# Winsorizing
#precent_value_1 <- quantile(bike_train$count,probs =0.01) - for redundency
precent_value_99 = quantile(bike_train$count,probs =0.99)

#dropping rows with extreme values
# set a new table only with the rows where count are between 1% and 99% quantile
bike_train_clean = bike_train[bike_train$count<precent_value_99,]

#------------------------------------------------------------------------------------------
#proving there are no Na/Nan values
ind_rows = rowSums(is.na(bike_train_clean)) == ncol(bike_train_clean)
ind_col = colSums(is.na(bike_train_clean)) == nrow(bike_train_clean)
summary(ind_rows)
summary(ind_col)

#------------------------------------------------------------------------------------------
install.packages('ggplot2')
library(ggplot2)

# looking at the data 
summary(bike_train_clean)
summary(bike_train_clean$count)
table(bike_train_clean$count)

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


#almost the same number of request during every season
ggplot(bike_train_clean, aes(season)) + stat_count(color="blue",geom = "bar")
#needs tp be avarged (taking also the holidays)
ggplot(bike_train_clean, aes(workingday)) + stat_count(color="blue",geom = "bar")
ggplot(bike_train_clean, aes(weather)) + stat_count(color="blue",geom = "bar")


table(bike_train_clean$time,bike_train_clean$holiday);   #17 - two-way table of counts
prop.table(table(housing$SaleCondition,housing$YrSold)); #18 - two-way table of freqs



'''
quantile(bike_train_clean$count, probs = seq(0,1,0.1));

prop.table(table(bike_train_clean$count,bike_train_clean$time)); # two-way table of freqs

prop.table(table(bike_train_clean$count,bike_train_clean$time))

date_by_count_avg = by(bike_train_clean$count,bike_train_clean$time,mean)


date_by_count_avg_tmp = t(sapply(date_by_count_avg, I))
# make a data frame
date_by_count_avg_df <- as.data.frame(date_by_count_avg_tmp,row)
print((date_by_count_avg_df))
'''

date_by_count_avg <- aggregate(bike_train_clean$count, by=list(bike_train_clean$time), FUN=mean)  # aggregate
colnames(date_by_count_avg) <- c("avg", "hour")  # change column names
#cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # sort
date_by_count_avg$avg <- factor(date_by_count_avg$avg, levels = date_by_count_avg$avg)  # to retain the order in plot.

# Draw plot
ggplot(date_by_count_avg, aes(x=avg, y=hour)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="hour Vs Avg")






ggplot(date_by_count_avg_df, aes(x, y)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3")


print(data.frame(date_by_count_avg));
print(date_by_count_avg)
ggplot(bike_train_clean, aes(count,time)) + 
  geom_histogram(binwidth=30)




#--------------------------------------------------------
#------------------Exploring season----------------------
#--------------------------------------------------------

table(bike_train_clean$count,bike_train_clean$season)   #two-way table of counts
prop.table(table(bike_train_clean$count,bike_train_clean$season)) #two-way table of freqs

#--------------------------------------------------------------
#------------------Exploring "hour a day"----------------------
#--------------------------------------------------------------

#Adding a new variable to the dataset 
bike_train_clean$datetime <- bike_train_clean$split.datetime




#Run a linear regression that explains count variable with temp variable
reg_temp = lm(count~temp, data=bike_train_clean ) #creating a liner regression (between count and temp) object
summary(reg_temp)# shows the summary of the results received
coef(reg_temp) # shows the Coefficients estimates of the regression
