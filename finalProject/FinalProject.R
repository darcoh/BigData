
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



#-----------merging signup_app categories on Train file------------------------
airbnbTrain_df = airbnbTrain_df[,signup_app:= ifelse(signup_app=="iOS", "Mobile", signup_app)]
airbnbTrain_df = airbnbTrain_df[,signup_app:= ifelse(signup_app=="Android", "Mobile", signup_app)]
airbnbTrain_df = airbnbTrain_df[,signup_app:= ifelse(signup_app=="Moweb", "Mobile", signup_app)]

ggplot(airbnbTrain_df, aes(signup_app)) + stat_count(color="blue",geom = "bar")

#-----------merging signup_app categorieson Test file------------------------

airbnbTest_df = airbnbTest_df[,signup_app:= ifelse(signup_app=="iOS", "Mobile", signup_app)]
airbnbTest_df = airbnbTest_df[,signup_app:= ifelse(signup_app=="Android", "Mobile", signup_app)]
airbnbTest_df = airbnbTest_df[,signup_app:= ifelse(signup_app=="Moweb", "Mobile", signup_app)]

#-----------merging first_device_type categories on Train file------------------------
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="iPad", "Tablet", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Android Tablet", "Tablet", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Android Phone", "Tablet", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="iPhone", "SmartPhone", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="SmartPhone (Other)", "SmartPhone", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Mac Desktop", "Desktop", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Desktop (Other)", "Desktop", first_device_type)]

airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Windows Desktop", "Desktop", first_device_type)]
airbnbTrain_df = airbnbTrain_df[,first_device_type:= ifelse(first_device_type=="Other/Unknown", "na", first_device_type)]

ggplot(airbnbTrain_df, aes(first_device_type)) + stat_count(color="blue",geom = "bar")

#-----------merging first_device_type categories on Test file------------------------
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="iPad", "Tablet", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Android Tablet", "Tablet", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Android Phone", "Tablet", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="iPhone", "SmartPhone", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="SmartPhone (Other)", "SmartPhone", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Mac Desktop", "Desktop", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Desktop (Other)", "Desktop", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Windows Desktop", "Desktop", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Other/Unknown", "na", first_device_type)]


#---------------------------------------------------------------------------
#---------------------------affiliate_channel-------------------------------
#---------------------------------------------------------------------------
airbnbTrain_df = airbnbTrain_df[,affiliate_channel:= ifelse(affiliate_channel=="other", "na", affiliate_channel)]

airbnbTest_df = airbnbTest_df[,affiliate_channel:= ifelse(affiliate_channel=="other", "na", affiliate_channel)]


#---------------------------------------------------------------------------
#---------------------------affiliate_provider-------------------------------
#---------------------------------------------------------------------------
airbnbTrain_df = airbnbTrain_df[,affiliate_provider:= ifelse(affiliate_provider=="other", "na", affiliate_provider)]


airbnbTest_df = airbnbTest_df[,affiliate_provider:= ifelse(affiliate_provider=="other", "na", affiliate_provider)]



#---------------------------------------------------------------------------
#---------------------------affiliate_first_tracked-------------------------------
#---------------------------------------------------------------------------

##this one doest work:
airbnbTrain_df = airbnbTrain_df[,first_affiliate_tracked:= ifelse(first_affiliate_tracked==" ", "na", first_affiliate_tracked)]

airbnbTest_df = airbnbTest_df[,first_affiliate_tracked:= ifelse(first_affiliate_tracked==" ", "na", first_affiliate_tracked)]

#---------------------------------------------------------------------------
#---------------------------signup_app--------------------------------------
#---------------------------------------------------------------------------
airbnbTrain_df = airbnbTrain_df[,first_affiliate_tracked:= ifelse(first_affiliate_tracked==" ", "na", signup_app)]


#---------------------------------------------------------------------------
#---------------------------first_device_type-------------------------------
#---------------------------------------------------------------------------
airbnbTrain_df = airbnbTrain_df[,first_affiliate_tracked:= ifelse(first_device_type=="Other/Unknown", "na", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Other/Unknown", "na", first_device_type)]


#---------------------------------------------------------------------------
#---------------------------first_browser-------------------------------
#---------------------------------------------------------------------------
airbnbTrain_df = airbnbTrain_df[,first_browser:= ifelse(first_browser=="-unknown-", "na", first_browser)]
airbnbTest_df = airbnbTest_df[,first_browser:= ifelse(first_browser=="-unknown-", "na", first_browser)]

#--------------converting features to categorial---------------------------
airbnbTrain_df$affiliate_channel = factor(airbnbTrain_df$affiliate_channel)
airbnbTrain_df$affiliate_provider = factor(airbnbTrain_df$affiliate_provider)
airbnbTrain_df$first_affiliate_tracked = factor(airbnbTrain_df$first_affiliate_tracked)
airbnbTrain_df$signup_app = factor(airbnbTrain_df$signup_app)
airbnbTrain_df$first_device_type = factor(airbnbTrain_df$first_device_type)
airbnbTrain_df$first_browser = factor(airbnbTrain_df$first_browser)

airbnbTest_df$affiliate_channel = factor(airbnbTest_df$affiliate_channel)
airbnbTest_df$affiliate_provider = factor(airbnbTest_df$affiliate_provider)
airbnbTest_df$first_affiliate_tracked = factor(airbnbTest_df$first_affiliate_tracked)
airbnbTest_df$signup_app = factor(airbnbTest_df$signup_app)
airbnbTest_df$first_device_type = factor(airbnbTest_df$first_device_type)
airbnbTest_df$first_browser = factor(airbnbTest_df$first_browser)


#----------------------------------------------------------------------------
#---------------------------language-----------------------------------------
#----------------------------------------------------------------------------
sum(is.na(airbnbTrain_df$language))

ggplot(airbnbTrain_df, aes(language)) + stat_count(color="blue",geom = "bar")

#---------------------------------------------------------------------------
#---------------------------affiliate_channel-------------------------------
#---------------------------------------------------------------------------
sum(is.na(airbnbTrain_df$affiliate_channel))

ggplot(airbnbTrain_df, aes(affiliate_channel)) + stat_count(color="blue",geom = "bar")
#---------------------------------------------------------------------------
#---------------------------affiliate_provider-------------------------------
#---------------------------------------------------------------------------
table (airbnbTrain_df$affiliate_provider)
sum(is.na(airbnbTrain_df$affiliate_provider))

ggplot(airbnbTrain_df, aes(affiliate_provider)) + stat_count(color="blue",geom = "bar")

#---------------------------------------------------------------------------
#---------------------------first_affiliate_tracked------------------------
#---------------------------------------------------------------------------
sum(is.na(airbnbTrain_df$first_affiliate_tracked))

ggplot(airbnbTrain_df, aes(first_affiliate_tracked)) + stat_count(color="blue",geom = "bar")

ggplot(airbnbTest_df, aes(first_affiliate_tracked)) + stat_count(color="blue",geom = "bar")
#---------------------------------------------------------------------------
#---------------------------signup_app--------------------------------------
#---------------------------------------------------------------------------
sum(is.na(airbnbTrain_df$signup_app))

ggplot(airbnbTrain_df, aes(signup_app)) + stat_count(color="blue",geom = "bar")
#---------------------------------------------------------------------------
#---------------------------first_device_type-------------------------------
#---------------------------------------------------------------------------
ggplot(airbnbTrain_df, aes(first_device_type)) + stat_count(color="blue",geom = "bar")

#---------------------------------------------------------------------------
#---------------------------first_browser-------------------------------
#---------------------------------------------------------------------------
ggplot(airbnbTrain_df, aes(first_browser)) + stat_count(color="blue",geom = "bar")

table(airbnbTrain_df$first_browser)
cor(data.frame(airbnbTrain_df$country_destination,airbnbTrain_df$gender,airbnbTrain_df$age,airbnbTrain_df$signup_method,airbnbTrain_df$signup_flow, airbnbTrain_df$language, airbnbTrain_df$affiliate_channel, airbnbTrain_df$affiliate_provider, airbnbTrain_df$first_affiliate_tracked, airbnbTrain_df$signup_app, airbnbTrain_df$first_device_type, airbnbTrain_df$first_browser))

    