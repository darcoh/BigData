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
bike_train$holiday = factor(bike_train$ holiday, label = c("holiday", "not holiday"))
bike_train$workingday = factor(bike_train$ workingday, labels = c("workingday", "not workingday"))
bike_train$weather = factor(bike_train$ weather, labels = c("Good", "Normal", "Bad", "Very Bad"))

#------------------------------------------------------------------------------------------
# Winsorizing
#precent_value_1 <- quantile(bike_train$count,probs =0.01) - for redundency
precent_value_99 <- quantile(bike_train$count,probs =0.99)

#dropping rows with extreme values
# set a new table only with the rows where count are between 1% and 99% quantile
bike_train_clean <- bike_train[bike_train$count<precent_value_99,]


# looking at the data 
summary(bike_train)
summary(bike_train$count)
table(bike_train$count)
prop.table(table(bike_train$count))
quantile(bike_train$count, probs = seq(0,1,0.1))


install.packages('ggplot2')
library(ggplot2)
ggplot(aes(x = season), data = bike_train) 
geom_histogram(binwidth = 1) 
scale_x_continuous(breaks = 1:31)

ggplot(bike_train)+aes(count)+
geom_histogram(binwidth = 1)

ggplot(bike_train)+aes(season)+
geom_freqpoly(binwidth = 1)



ggplot(bike_train_clean)+aes(count)+
geom_histogram(binwidth = 10)

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