
install.packages('ggplot2')
install.packages('lubridate')
install.packages(c("FactoMineR", "factoextra"));
install.packages("dummies");
install.packages("data.table");
install.packages('bit64')
install.packages("dplyr")
install.packages("data.table")
install.packages("bit64")
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
airbnbTest_df = fread("airbnb_test.csv")


##----------------------------------------------------------------------------------------
##-------------------------------PREPROCESSING--------------------------------------------
##----------------------------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~age~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#changing all people whos their age is greater then 1900 (in years)
#total of 170760
airbnbTrain_df = airbnbTrain_df[, age := ifelse(age >= 1900, 2018-age, age)]
#count histograma
ggplot(airbnbTrain_df)+aes(age)+
  geom_histogram(binwidth = 2);

#converting people whos age>100 and age<15 to NA
airbnbTrain_df = airbnbTrain_df[age<15|age>100, age:=NA]

sqldf("select count(*) from airbnbTrain_df where age is NULL")
sqldf("select age,count(age) from airbnbTrain_df group by age")
sqldf("select sum(count_age) from (select *,count(age) as count_age from airbnbTrain_df group by age) where (age>105 or age<15)")

ggplot(airbnbTrain_df)+aes(age)+
  geom_histogram(binwidth = 2);

summary(airbnbTrain_df$age,object = Min)


#age to bins
ageBreaks = c(15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105);
ageLabels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
               "60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-99","100-104");

airbnbTrain_df[ , ageBins := cut(age, breaks = ageBreaks, 
                                        right = FALSE, labels = ageLabels)]

ggplot(airbnbTrain_df, aes(ageBins)) + stat_count(color="blue",geom = "bar")


summary(airbnbTrain_df[,age])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Gender~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sqldf("select count(*) from airbnbTrain_df where gender=='OTHER'") #229 values
sqldf("select count(*) from airbnbTrain_df where gender=='-unknown-'") #76534
sqldf("select gender,count(gender) from airbnbTrain_df group by gender")
ggplot(airbnbTrain_df, aes(gender)) + stat_count(color="blue",geom = "bar")
#converting people of gender -unknown- to NA
airbnbTrain_df = airbnbTrain_df[gender=='-unknown-', gender:=NA];
airbnbTrain_df$gender = as.factor(airbnbTrain_df$gender);



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Date Account Created~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sqldf("select count(*) from airbnbTrain_df where date_account_created is NULL")
#converting to date format
setDT(airbnbTrain_df)[, dateAccountCreated := as.POSIXct(date_account_created, format = "%Y-%m-%d")]
setDT(airbnbTrain_df)[, yearAccountCreated := format(as.Date(dateAccountCreated), "%Y")]
setDT(airbnbTrain_df)[, monthAccountCreated := format(as.Date(dateAccountCreated), "%m")]
setDT(airbnbTrain_df)[, weekdayAccountCreated := format(as.Date(dateAccountCreated), "%u")]

ggplot(airbnbTrain_df, aes(yearAccountCreated)) + stat_count(color="blue",geom = "bar")
sqldf("select count(*) from airbnbTrain_df where yearAccountCreated==2009") #1
airbnbTrain_df = airbnbTrain_df[yearAccountCreated!=2009,]
sqldf("select count(*) from airbnbTrain_df where yearAccountCreated==2009") #---> not it's 0
sqldf("select count(*) from airbnbTrain_df where yearAccountCreated is NULL") #0
ggplot(airbnbTrain_df, aes(monthAccountCreated)) + stat_count(color="red",geom = "bar")
ggplot(airbnbTrain_df, aes(weekdayAccountCreated)) + stat_count(color="green",geom = "bar")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Date First Booking~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sqldf("select count(*) from airbnbTrain_df where date_first_booking==''") #99661

#converting date_first_booking of type "" to NA
airbnbTrain_df = airbnbTrain_df[date_first_booking=='', date_first_booking:=NA];
sqldf("select count(*) from airbnbTrain_df where date_first_booking is NULL") #99661

#converting to date format
setDT(airbnbTrain_df)[, dateFirstBooking := as.POSIXct(date_first_booking, format = "%Y-%m-%d")]
setDT(airbnbTrain_df)[, yearFirstBooking := format(as.Date(dateFirstBooking), "%Y")]
setDT(airbnbTrain_df)[, monthFirstBooking := format(as.Date(dateAccountCreated), "%m")]
airbnbTrain_df = airbnbTrain_df[is.na(yearFirstBooking), monthFirstBooking:=NA];
setDT(airbnbTrain_df)[, weekdayFirstBooking := format(as.Date(dateAccountCreated), "%u")]
airbnbTrain_df = airbnbTrain_df[is.na(yearFirstBooking), weekdayFirstBooking:=NA];

ggplot(airbnbTrain_df, aes(yearFirstBooking)) + stat_count(color="blue",geom = "bar")
sqldf("select count(*) from airbnbTrain_df where yearFirstBooking is NULL") #99661
ggplot(airbnbTrain_df, aes(monthFirstBooking)) + stat_count(color="red",geom = "bar")
sqldf("select count(*) from airbnbTrain_df where monthFirstBooking is NULL") #99661
ggplot(airbnbTrain_df, aes(weekdayFirstBooking)) + stat_count(color="green",geom = "bar")
sqldf("select count(*) from airbnbTrain_df where yearFirstBooking is NULL") #99661

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~timestamp~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sqldf("select count(*) from airbnbTrain_df where timestamp_first_active==''") #0
sqldf("select count(*) from airbnbTrain_df where timestamp_first_active is NULL") #0


setDT(airbnbTrain_df)[, timestampFirstActive := as.POSIXct(as.character(timestamp_first_active), format = "%Y%m%d%H%M%S",tz="GMT")]
summary(airbnbTrain_df$timestampFirstActive)

setDT(airbnbTrain_df)[, dateTimestampFirstActive := format(as.Date(timestampFirstActive), "%Y-%m-%d")]

sqldf("select count(*) from airbnbTrain_df where timestampFirstActive==''") #0
sqldf("select count(*) from airbnbTrain_df where timestampFirstActive is NULL") #0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~timestamp upgrades~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

airbnbTrain_df$diffDaysAccountFirstActive =as.numeric(
                as.Date(as.character(airbnbTrain_df$dateAccountCreated), format="%Y-%m-%d") -
                as.Date(as.character(airbnbTrain_df$dateTimestampFirstActive), format="%Y-%m-%d")
                ) 

airbnbTrain_df$diffDaysBookingFirstActive =as.numeric(
  as.Date(as.character(airbnbTrain_df$dateFirstBooking), format="%Y-%m-%d") -
    as.Date(as.character(airbnbTrain_df$dateTimestampFirstActive), format="%Y-%m-%d")
)         



#--diffDaysBookingFirstActive
ggplot(airbnbTrain_df, aes(diffDaysAccountFirstActive)) + stat_count(color="green",geom = "bar")
sqldf("select diffDaysAccountFirstActive, count(*) from airbnbTrain_df group by diffDaysAccountFirstActive")

#balancing tha timediff to be before, the same day, and +1&beyond
sqldf("select diffDaysAccountFirstActive, count(*) from airbnbTrain_df group by diffDaysAccountFirstActive") 

airbnbTrain_df = airbnbTrain_df[,diffDaysAccountFirstActive:= ifelse(diffDaysAccountFirstActive>0, "activeAfterAccount", diffDaysAccountFirstActive)]
airbnbTrain_df = airbnbTrain_df[,diffDaysAccountFirstActive:= ifelse(diffDaysAccountFirstActive==0, "sameActiveAccount", diffDaysAccountFirstActive)]

#converting to categorial
airbnbTrain_df$diffDaysAccountFirstActive = as.factor(airbnbTrain_df$diffDaysAccountFirstActive);

#--diffDaysBookingFirstActive

sqldf("select diffDaysBookingFirstActive, count(*) from airbnbTrain_df group by diffDaysBookingFirstActive") 
ggplot(airbnbTrain_df, aes(diffDaysBookingFirstActive)) + stat_count(color="green",geom = "bar")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~signup method~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(airbnbTrain_df$signup_method)
#asserting all normal
sqldf("select signup_method, count(*) from airbnbTrain_df group by signup_method") 
sqldf("select count(*) from airbnbTrain_df where signup_method is NULL") #0
sqldf("select count(*) from airbnbTrain_df where signup_method=='google'") #433 (few but important)
ggplot(airbnbTrain_df, aes(signup_method)) + stat_count(color="orange",geom = "bar")
airbnbTrain_df$signup_method = as.factor(airbnbTrain_df$signup_method);



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~signup flow~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(airbnbTrain_df$signup_flow)
#asserting all normal
sqldf("select signup_flow, count(*) from airbnbTrain_df group by signup_flow") 
sqldf("select count(*) from airbnbTrain_df where signup_flow is NULL"); #0
ggplot(airbnbTrain_df, aes(signup_flow)) + stat_count(color="purple",geom = "bar")


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


    
