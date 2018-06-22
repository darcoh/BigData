
install.packages('ggplot2')
install.packages('lubridate')
install.packages(c("FactoMineR", "factoextra"));
install.packages("dummies");
<<<<<<< HEAD
install.packages("data.table");
install.packages('bit64')
=======
install.packages("dplyr")
install.packages("data.table")
install.packages("bit64")
>>>>>>> 6deb702f6f511d34f6b9c8f2ad4ba64eacdea50e
install.packages("sqldf")

library(ggplot2)
library(lubridate)
library("FactoMineR")
library("factoextra");
library(dummies)
library(data.table)
library(bit64)
library(dplyr)
library("data.table")
library("bit64")
library(sqldf)

setwd("finalProject")

#local data folder
list.files()
airbnbTrain_df = fread("airbnb_train.csv")
airbnbTest_df =fread("airbnb_test.csv")



##-------------------------------preprocessing--------------------------------------------

#~~~~~~~~~~~~~~~age~~~~~~~~~~~~~~~~~

#changing all people whos thier age is greater then 1900 (in years)
#total of 170760
airbnbTrain_df = airbnbTrain_df[, age := ifelse(age >= 1900, 2018-age, age)]
#count histograma
ggplot(airbnbTrain_df)+aes(age)+
  geom_histogram(binwidth = 2);

airbnbTrain_df = airbnbTrain_df[age<15|age>100, age:=NA]

sqldf("select count(*) from airbnbTrain_df where age is NULL")
sqldf("select age,count(age) from airbnbTrain_df group by age")
sqldf("select sum(count_age) from (select *,count(age) as count_age from airbnbTrain_df group by age) where (age>105 or age<14)")

ggplot(airbnbTrain_df)+aes(age)+
  geom_histogram(binwidth = 2);


summary(airbnbTrain_df[,age])



#-----------merging signup_app categories------------------------
airbnbTrain_df = airbnbTrain_df[,signup_app:= ifelse(signup_app=="iOS", "Mobile", signup_app)]
airbnbTrain_df = airbnbTrain_df[,signup_app:= ifelse(signup_app=="Android", "Mobile", signup_app)]
airbnbTrain_df = airbnbTrain_df[,signup_app:= ifelse(signup_app=="Moweb", "Mobile", signup_app)]

ggplot(airbnbTrain_df, aes(signup_app)) + stat_count(color="blue",geom = "bar")

#-----------merging first_device_type categories------------------------
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="iPad", "Tablet", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Android Tablet", "Tablet", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Android Phone", "Tablet", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="iPhone", "SmartPhone", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="SmartPhone (Other)", "SmartPhone", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Mac Desktop", "Desktop", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Desktop (Other)", "Desktop", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Windows Desktop", "Desktop", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Other/Unknown", "NA", first_device_type)]

ggplot(airbnbTrain_df, aes(first_device_type)) + stat_count(color="blue",geom = "bar")

#--------------converting features to categorial---------------------------
airbnbTrain_df$affiliate_channel = factor(airbnbTrain_df$affiliate_channel)
airbnbTrain_df$affiliate_provider = factor(airbnbTrain_df$affiliate_provider)
airbnbTrain_df$first_affiliate_tracked = factor(airbnbTrain_df$first_affiliate_tracked)
airbnbTrain_df$signup_app = factor(airbnbTrain_df$signup_app)
airbnbTrain_df$first_device_type = factor(airbnbTrain_df$first_device_type)
airbnbTrain_df$first_browser = factor(airbnbTrain_df$first_browser)

#----------------------------------------------------------------------------
#---------------------------language-----------------------------------------
#----------------------------------------------------------------------------

#---------------Counting NA----------------------------------


#---------------------------------------------------------------------------
#---------------------------affiliate_channel-------------------------------
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
#---------------------------affiliate_provider-------------------------------
#---------------------------------------------------------------------------
