
install.packages('ggplot2')
install.packages('lubridate')
install.packages(c("FactoMineR", "factoextra"));
install.packages("dummies");
install.packages("dplyr")
install.packages("data.table")
install.packages("bit64")
install.packages("sqldf")

library(ggplot2)
library(lubridate)
library("FactoMineR")
library("factoextra");
library(dummies)
library(dplyr)
library("data.table")
library("bit64")
library(sqldf)

setwd("finalProject")

#local data folder
list.files()
airbnbTrain_df <- fread("airbnb_train.csv")
airbnbTest_df <- fread("airbnb_test.csv")

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