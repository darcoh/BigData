#-------------------------------packages to install-----------------------------------------
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
install.packages("GoodmanKruskal")
install.packages("VIM")
install.packages("openxlsx")
install.packages("ggcorrplot")
install.packages("DataExplorer")
install.packages("nnet")
install.packages("class")
install.packages("gmodels")
install.packages("Amelia")
install.packages("tidyr")
install.packages("scales")
install.packages("corrplot")
install.packages("ROCR")
install.packages("randomForest")
install.packages("MASS")
install.packages("pROC")
install.packages("xgboost")

#-------------------------------Libraries in use-----------------------------------------

library(GoodmanKruskal)
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
library(openxlsx)
library(VIM)
library(ggcorrplot)
library(GoodmanKruskal)
library(DataExplorer)
library(nnet)
library(class)
library(gmodels)
library("Amelia")
library("tidyr")
library("scales")
library(corrplot)
library(ROCR)
library(randomForest)
library(MASS)
library(pROC)
library(xgboost)


#-------------------------------setting libray of FinalProject-----------------------------------------

setwd("finalProject") #local data folder
list.files();
#-------------------------------setting libray of FinalProject-----------------------------------------

set.seed(1)
airbnbTrain_extra_df = fread("airbnb_train_extra.csv")
airbnbTrain_df = fread("airbnb_train.csv")
airbnbTest_df = fread("airbnb_test.csv")
airbnbTrain_df_real = fread("airbnbTrain_df_real.csv")

colnames(airbnbTrain_df_real)
#initial columns list
colnames(airbnbTrain_extra_df);
colnames(airbnbTrain_df);

#copying relevant columns
#airbnbTrain_df$first_booking_distance_US_toHoliday = sapply(1:nrow(airbnbTrain_df), function(i) airbnbTrain_extra_df$first_booking_distance_US_toHoliday[airbnbTrain_extra_df$id==airbnbTrain_df$id[i]])
#airbnbTrain_df$account_created_distance_US_toHoliday = sapply(1:nrow(airbnbTrain_df), function(i) airbnbTrain_extra_df$account_created_distance_US_toHoliday[airbnbTrain_extra_df$id==airbnbTrain_df$id[i]])
#write.table(airbnbTrain_df, file = "airbnbTrain_df_real.csv",sep=",",row.names = FALSE);
summary(airbnbTrain_df_real)

#-------------------------------Dataset General Information-----------------------------------------
nrow(airbnbTrain_df_real)
ncol(airbnbTrain_df_real)

##----------------------------------------------------------------------------------------
##-------------------------------PREPROCESSING--------------------------------------------
##----------------------------------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ID~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sqldf("select count(*) from (select * from airbnbTrain_df group by id)");
sqldf("select id,id_count from (select *,count(*) as id_count 
        from airbnbTrain_df group by id) where id_count>1");


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~age~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#first checking the variation
data.frame(t(sqldf("select age,count(age) from airbnbTrain_df_real group by age")))
sqldf("select count(*) from airbnbTrain_df_real where age is NULL") #70398


#changing all people whos their age is greater then 1900 (in years)
airbnbTrain_df_real = airbnbTrain_df_real[, age := ifelse(age >= 1900, 2018-age, age)]
#Test
airbnbTest_df=airbnbTest_df[, age := ifelse(age >= 1900, 2018-age, age)]

#count histograma
data.frame(t(sqldf("select age,count(age) from airbnbTrain_df_real group by age")))
ggplot(airbnbTrain_df_real)+aes(age)+
  geom_histogram(binwidth = 2);


#counting the number of people under age 15 and above age 100
sqldf("select count(*) from airbnbTrain_df_real where age<15 or age>100")

#converting people whos age>100 and age<15 to -1
airbnbTrain_df_real = airbnbTrain_df_real[age<15|age>100, age:=NA]
#Test
airbnbTest_df=airbnbTest_df[age<15|age>100, age:=NA]

ggplot(airbnbTrain_df_real)+aes(age)+
  geom_histogram(binwidth = 2);

#counting total number of new null people
sqldf("select count(*) from airbnbTrain_df_real where age is NULL") #72313

#showing new data without outlies
ggplot(airbnbTrain_df_real)+aes(age)+
  geom_histogram(binwidth = 2);

#summery of the data
summary(airbnbTrain_df_real$age)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ageBins~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#age to bins
ageBreaks = c(15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105);
ageLabels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
               "60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-99","100-104");

airbnbTrain_df_real[ , ageBins := cut(age, breaks = ageBreaks, 
                                        right = FALSE, labels = ageLabels)]
#Test
airbnbTest_df[ , ageBins := cut(age, breaks = ageBreaks, 
                                right = FALSE, labels = ageLabels)]

ggplot(airbnbTrain_df_real, aes(ageBins)) + stat_count(color="blue",geom = "bar")

sqldf("select count(*) from airbnbTrain_df_real where ageBins is NULL") #72313


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Gender~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sqldf("select gender,count(gender) from airbnbTrain_df_real group by gender")
ggplot(airbnbTrain_df_real, aes(gender)) + stat_count(color="blue",geom = "bar")

sqldf("select count(*) from airbnbTrain_df_real where gender=='-unknown-'") #76534
#converting people of gender -unknown- to -1
airbnbTrain_df_real = airbnbTrain_df_real[gender=='-unknown-', gender:=NA];
#Test
airbnbTest_df = airbnbTest_df[gender=='-unknown-', gender:=NA];

#asserting they were converted to NULL
sqldf("select count(*) from airbnbTrain_df_real where gender is NULL") #76534

#ploting after factorization and changes
ggplot(airbnbTrain_df_real, aes(gender)) + stat_count(color="blue",geom = "bar")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Date Account Created~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sqldf("select count(*) from airbnbTrain_df_real where date_account_created is NULL") #0
sqldf("select date_account_created,count(*) from airbnbTrain_df_real group by date_account_created") #0

#converting to date format
setDT(airbnbTrain_df_real)[, dateAccountCreated := as.POSIXct(date_account_created, format = "%Y-%m-%d", tz="UTC")]
setDT(airbnbTrain_df_real)[, yearAccountCreated := year(as.Date(dateAccountCreated))]
setDT(airbnbTrain_df_real)[, monthAccountCreated := month(as.Date(dateAccountCreated))]
setDT(airbnbTrain_df_real)[, weekdayAccountCreated := wday(as.Date(dateAccountCreated))]

ggplot(airbnbTrain_df_real, aes(yearAccountCreated)) + stat_count(color="blue",geom = "bar")

sqldf("select yearAccountCreated,count(*) as YearCount from airbnbTrain_df_real group by yearAccountCreated");

ggplot(airbnbTrain_df_real, aes(monthAccountCreated)) + stat_count(color="blue",geom = "bar");
sqldf("select monthAccountCreated,count(*) as MonthCount from airbnbTrain_df_real group by monthAccountCreated");

ggplot(airbnbTrain_df_real, aes(weekdayAccountCreated)) + stat_count(color="blue",geom = "bar")
sqldf("select weekdayAccountCreated,count(*) as weekdayCount from airbnbTrain_df_real group by weekdayAccountCreated");


#Test converting to date format
setDT(airbnbTest_df)[, dateAccountCreated := as.POSIXct(date_account_created, format = "%Y-%m-%d", tz="UTC")]
setDT(airbnbTest_df)[, yearAccountCreated := year(as.Date(dateAccountCreated))]

setDT(airbnbTest_df)[, monthAccountCreated := month(as.Date(dateAccountCreated))]
setDT(airbnbTest_df)[, weekdayAccountCreated := wday(as.Date(dateAccountCreated))]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Date First Booking~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


sqldf("select count(*) from airbnbTrain_df_real where date_first_booking==''") #99661

#converting date_first_booking of type "" to -1
airbnbTrain_df_real = airbnbTrain_df_real[date_first_booking=='', date_first_booking:=NA];
#Test
airbnbTest_df = airbnbTest_df[date_first_booking=='', date_first_booking:=NA];

#asserting they are null
sqldf("select count(*) from airbnbTrain_df_real where date_first_booking is NULL") #99661


#converting to date format
setDT(airbnbTrain_df_real)[, dateFirstBooking := as.POSIXct(date_first_booking, format = "%Y-%m-%d", tz="UTC")]

setDT(airbnbTrain_df_real)[, yearFirstBooking := year(as.Date(dateFirstBooking))]
setDT(airbnbTrain_df_real)[, monthFirstBooking := month(as.Date(dateFirstBooking))]
setDT(airbnbTrain_df_real)[, weekdayFirstBooking :=  wday(as.Date(dateFirstBooking))]
airbnbTrain_df_real = airbnbTrain_df_real[is.na(dateFirstBooking), yearFirstBooking:=NA];
airbnbTrain_df_real = airbnbTrain_df_real[is.na(dateFirstBooking), monthFirstBooking:=NA];
airbnbTrain_df_real = airbnbTrain_df_real[is.na(dateFirstBooking), weekdayFirstBooking:=NA];


ggplot(airbnbTrain_df_real, aes(yearFirstBooking)) + stat_count(color="blue",geom = "bar")
sqldf("select count(*) from airbnbTrain_df_real where yearFirstBooking is NULL") #99661
sqldf("select yearFirstBooking,count(*) as YearCount from airbnbTrain_df_real group by yearFirstBooking");

ggplot(airbnbTrain_df_real, aes(monthFirstBooking)) + stat_count(color="red",geom = "bar")
sqldf("select count(*) from airbnbTrain_df_real where monthFirstBooking is NULL") #99661
sqldf("select monthFirstBooking,count(*) as MonthCount from airbnbTrain_df_real group by monthFirstBooking");

ggplot(airbnbTrain_df_real, aes(weekdayFirstBooking)) + stat_count(color="green",geom = "bar")
sqldf("select count(*) from airbnbTrain_df_real where weekdayFirstBooking is NULL") #99661
sqldf("select weekdayFirstBooking,count(*) as WeekdayCount from airbnbTrain_df_real group by weekdayFirstBooking");



#Test converting to date format
setDT(airbnbTest_df)[, dateFirstBooking := as.POSIXct(date_first_booking, format = "%Y-%m-%d", tz="UTC")]
setDT(airbnbTest_df)[, yearFirstBooking := year(as.Date(dateFirstBooking))]
setDT(airbnbTest_df)[, monthFirstBooking := month(as.Date(dateFirstBooking))]
setDT(airbnbTest_df)[, weekdayFirstBooking :=  wday(as.Date(dateFirstBooking))]
airbnbTest_df = airbnbTest_df[is.na(dateFirstBooking), yearFirstBooking:=NA];
airbnbTest_df = airbnbTest_df[is.na(dateFirstBooking), monthFirstBooking:=NA];
airbnbTest_df = airbnbTest_df[is.na(dateFirstBooking), weekdayFirstBooking:=NA];


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~timestamp~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#asserting no missoing values
sqldf("select count(*) from airbnbTrain_df_real where timestamp_first_active==''") #0
sqldf("select count(*) from airbnbTrain_df_real where timestamp_first_active is NULL") #0

setDT(airbnbTrain_df_real)[, timestampFirstActive := as.POSIXct(as.character(timestamp_first_active), format = "%Y%m%d%H%M%S",tz="GMT")]
summary(airbnbTrain_df_real$timestampFirstActive)
setDT(airbnbTrain_df_real)[, dateTimestampFirstActive := format(as.Date(timestampFirstActive), "%Y-%m-%d")]

#Test
setDT(airbnbTest_df)[, timestampFirstActive := as.POSIXct(as.character(timestamp_first_active), format = "%Y%m%d%H%M%S",tz="GMT")]
summary(airbnbTrain_df_real$timestampFirstActive)
setDT(airbnbTest_df)[, dateTimestampFirstActive := format(as.Date(timestampFirstActive), "%Y-%m-%d")]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~timestamp upgrades~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

airbnbTrain_df_real$diffDaysAccountFirstActive =as.numeric(
                as.Date(as.character(airbnbTrain_df_real$dateAccountCreated), format="%Y-%m-%d") -
                as.Date(as.character(airbnbTrain_df_real$dateTimestampFirstActive), format="%Y-%m-%d")
                ) 

airbnbTrain_df_real$diffDaysBookingFirstActive =as.numeric(
  as.Date(as.character(airbnbTrain_df_real$dateFirstBooking), format="%Y-%m-%d") -
    as.Date(as.character(airbnbTrain_df_real$dateTimestampFirstActive), format="%Y-%m-%d")
)         


#--diffDaysAccountFirstActive
ggplot(airbnbTrain_df_real, aes(diffDaysAccountFirstActive)) + stat_count(color="green",geom = "bar")
sqldf("select diffDaysAccountFirstActive, count(*) from airbnbTrain_df_real group by diffDaysAccountFirstActive")

#balancing tha timediff to be before, the same day, and +1&beyond
sqldf("select count(*) from airbnbTrain_df_real where diffDaysAccountFirstActive==0") 
sqldf("select count(*) from airbnbTrain_df_real where diffDaysAccountFirstActive>0") 

airbnbTrain_df_real = airbnbTrain_df_real[,diffDaysAccountFirstActive:= ifelse(diffDaysAccountFirstActive>0, "day&more", diffDaysAccountFirstActive)]
airbnbTrain_df_real = airbnbTrain_df_real[,diffDaysAccountFirstActive:= ifelse(diffDaysAccountFirstActive==0, "same", diffDaysAccountFirstActive)]


ggplot(airbnbTrain_df_real, aes(diffDaysAccountFirstActive)) + stat_count(color="green",geom = "bar")


#--diffDaysBookingFirstActive

sqldf("select diffDaysBookingFirstActive, count(*) from airbnbTrain_df_real group by diffDaysBookingFirstActive") 
ggplot(airbnbTrain_df_real, aes(diffDaysBookingFirstActive)) + stat_count(color="green",geom = "bar")
sqldf("select sum(count) 
      from (
          select diffDaysBookingFirstActive, count(*) as count 
          from airbnbTrain_df_real 
          group by diffDaysBookingFirstActive 
          having count<3
      )") 

airbnbTrain_df_real= airbnbTrain_df_real[airbnbTrain_df_real$diffDaysBookingFirstActive<= 365
                                         |is.na(airbnbTrain_df_real$diffDaysBookingFirstActive),]
ggplot(airbnbTrain_df_real, aes(diffDaysBookingFirstActive)) + stat_count(color="green",geom = "bar")


#Test

airbnbTest_df$diffDaysAccountFirstActive =as.numeric(
  as.Date(as.character(airbnbTest_df$dateAccountCreated), format="%Y-%m-%d") -
    as.Date(as.character(airbnbTest_df$dateTimestampFirstActive), format="%Y-%m-%d")
) 

airbnbTest_df$diffDaysBookingFirstActive =as.numeric(
  as.Date(as.character(airbnbTest_df$dateFirstBooking), format="%Y-%m-%d") -
    as.Date(as.character(airbnbTest_df$dateTimestampFirstActive), format="%Y-%m-%d")
)


airbnbTest_df= airbnbTest_df[airbnbTest_df$diffDaysBookingFirstActive<= 365,]


#----------
airbnbTest_df = airbnbTest_df[,diffDaysAccountFirstActive:= ifelse(diffDaysAccountFirstActive>0, "day&more", diffDaysAccountFirstActive)]
airbnbTest_df = airbnbTest_df[,diffDaysAccountFirstActive:= ifelse(diffDaysAccountFirstActive==0, "same", diffDaysAccountFirstActive)]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~signup method~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#asserting all normal
sqldf("select signup_method, count(*) from airbnbTrain_df_real group by signup_method") 
sqldf("select count(*) from airbnbTrain_df_real where signup_method is NULL") #0
sqldf("select count(*) from airbnbTrain_df_real where signup_method=='google'") #433 (few but important)
ggplot(airbnbTrain_df_real, aes(signup_method)) + stat_count(color="orange",geom = "bar")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~signup flow~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(airbnbTrain_df_real$signup_flow)
#asserting all normal
data.frame(t(sqldf("select signup_flow, count(*) from airbnbTrain_df_real group by signup_flow")))

sqldf("select count(*) from airbnbTrain_df_real where signup_flow is NULL"); #0
ggplot(airbnbTrain_df_real, aes(signup_flow)) + stat_count(color="purple",geom = "bar")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~signup app~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-----------merging signup_app categories on Train file------------------------
sqldf("select signup_app, count(*) from airbnbTrain_df_real group by signup_app") 
ggplot(airbnbTrain_df_real, aes(signup_app)) + stat_count(color="blue",geom = "bar")

airbnbTrain_df_real = airbnbTrain_df_real[,signup_app:= ifelse(signup_app=="iOS", "Mobile", signup_app)]
airbnbTrain_df_real = airbnbTrain_df_real[,signup_app:= ifelse(signup_app=="Android", "Mobile", signup_app)]
airbnbTrain_df_real = airbnbTrain_df_real[,signup_app:= ifelse(signup_app=="Moweb", "Mobile", signup_app)]

ggplot(airbnbTrain_df_real, aes(signup_app)) + stat_count(color="blue",geom = "bar")

#-----------merging signup_app categorieson Test file------------------------

airbnbTest_df = airbnbTest_df[,signup_app:= ifelse(signup_app=="iOS", "Mobile", signup_app)]
airbnbTest_df = airbnbTest_df[,signup_app:= ifelse(signup_app=="Android", "Mobile", signup_app)]
airbnbTest_df = airbnbTest_df[,signup_app:= ifelse(signup_app=="Moweb", "Mobile", signup_app)]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~First Device Type~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-----------merging first_device_type categories on Train file------------------------
sqldf("select first_device_type, count(*) from airbnbTrain_df_real group by first_device_type") 
ggplot(airbnbTrain_df_real, aes(first_device_type)) + stat_count(color="blue",geom = "bar")

#--converting to -1
sqldf("select first_device_type, count(*) from airbnbTrain_df_real where first_device_type=='Other/Unknown'")#8567
airbnbTrain_df_real = airbnbTrain_df_real[first_device_type=="Other/Unknown", first_device_type:=NA];
sqldf("select first_device_type, count(*) from airbnbTrain_df_real where first_device_type is NULL")#8567


airbnbTrain_df_real = airbnbTrain_df_real[,first_device_type:= ifelse(first_device_type=="iPad", "Tablet", first_device_type)]
airbnbTrain_df_real = airbnbTrain_df_real[,first_device_type:= ifelse(first_device_type=="Android Tablet", "Tablet", first_device_type)]
airbnbTrain_df_real = airbnbTrain_df_real[,first_device_type:= ifelse(first_device_type=="Android Phone", "Tablet", first_device_type)]
airbnbTrain_df_real = airbnbTrain_df_real[,first_device_type:= ifelse(first_device_type=="iPhone", "SmartPhone", first_device_type)]
airbnbTrain_df_real = airbnbTrain_df_real[,first_device_type:= ifelse(first_device_type=="SmartPhone (Other)", "SmartPhone", first_device_type)]
airbnbTrain_df_real = airbnbTrain_df_real[,first_device_type:= ifelse(first_device_type=="Mac Desktop", "Desktop", first_device_type)]
airbnbTrain_df_real = airbnbTrain_df_real[,first_device_type:= ifelse(first_device_type=="Desktop (Other)", "Desktop", first_device_type)]
airbnbTrain_df_real = airbnbTrain_df_real[,first_device_type:= ifelse(first_device_type=="Windows Desktop", "Desktop", first_device_type)]

ggplot(airbnbTrain_df_real, aes(first_device_type)) + stat_count(color="blue",geom = "bar")

#-----------merging first_device_type categories on Test file------------------------
airbnbTest_df = airbnbTest_df[first_device_type=="Other/Unknown", first_device_type:=NA];
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="iPad", "Tablet", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Android Tablet", "Tablet", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Android Phone", "Tablet", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="iPhone", "SmartPhone", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="SmartPhone (Other)", "SmartPhone", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Mac Desktop", "Desktop", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Desktop (Other)", "Desktop", first_device_type)]
airbnbTest_df = airbnbTest_df[,first_device_type:= ifelse(first_device_type=="Windows Desktop", "Desktop", first_device_type)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Affiliate Channel~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sqldf("select affiliate_channel, count(*) from airbnbTrain_df_real group by affiliate_channel");

sqldf("select affiliate_channel, count(*) from airbnbTrain_df_real where affiliate_channel=='other'")#7191
airbnbTrain_df_real = airbnbTrain_df_real[affiliate_channel=="other", affiliate_channel:=NA];
sqldf("select affiliate_channel, count(*) from airbnbTrain_df_real where affiliate_channel is NULL")#7191

ggplot(airbnbTrain_df_real, aes(affiliate_channel)) + stat_count(color="blue",geom = "bar")



airbnbTest_df = airbnbTest_df[affiliate_channel=="other", affiliate_channel:=NA];


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Affiliate Provider~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sqldf("select affiliate_provider, count(*) from airbnbTrain_df_real group by affiliate_provider");
sqldf("select affiliate_provider, count(*) from airbnbTrain_df_real where affiliate_provider=='other'")#10075
airbnbTrain_df_real = airbnbTrain_df_real[affiliate_provider=="other", affiliate_provider:=NA];
sqldf("select affiliate_provider, count(*) from airbnbTrain_df_real where affiliate_provider is NULL")#10075

ggplot(airbnbTrain_df_real, aes(affiliate_provider)) + stat_count(color="blue",geom = "bar")

#Test
airbnbTest_df = airbnbTest_df[affiliate_provider=="other", affiliate_provider:=NA];


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Affiliate First Tracked~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sqldf("select first_affiliate_tracked, count(*) from airbnbTrain_df_real group by first_affiliate_tracked");
sqldf("select first_affiliate_tracked, count(*) from airbnbTrain_df_real where first_affiliate_tracked==''")#4894
airbnbTrain_df_real = airbnbTrain_df_real[first_affiliate_tracked=="", first_affiliate_tracked:=NA];
sqldf("select first_affiliate_tracked, count(*) from airbnbTrain_df_real where first_affiliate_tracked is NULL")#4894
ggplot(airbnbTrain_df_real, aes(first_affiliate_tracked)) + stat_count(color="blue",geom = "bar")

#Test
airbnbTest_df = airbnbTest_df[first_affiliate_tracked=="other", first_affiliate_tracked:=NA];


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~First Browser~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sqldf("select first_browser, count(*) from airbnbTrain_df_real group by first_browser");
sqldf("select first_browser, count(*) from airbnbTrain_df_real where first_browser=='-unknown-'")#21778
airbnbTrain_df_real = airbnbTrain_df_real[first_browser=="-unknown-", first_browser:=NA];
sqldf("select first_browser, count(*) from airbnbTrain_df_real where first_browser is NULL")#21778
ggplot(airbnbTrain_df_real, aes(first_browser)) + stat_count(color="blue",geom = "bar")
sqldf("select first_browser, count(*) as count
            from airbnbTrain_df_real group by first_browser order by count desc");


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Language~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sqldf("select language, count(*) from airbnbTrain_df_real group by language");
sqldf("select language, count(*) from airbnbTrain_df_real where language is NULL")#0
ggplot(airbnbTrain_df_real, aes(language)) + stat_count(color="blue",geom = "bar")
sqldf("select language, count(*) as count
      from airbnbTrain_df_real group by language order by count desc");

#Test


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~account_created_distance_US_toHoliday~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sqldf("select account_created_distance_US_toHoliday, count(*) from airbnbTrain_df_real group by account_created_distance_US_toHoliday");
sqldf("select account_created_distance_US_toHoliday, count(*) from airbnbTrain_df_real where account_created_distance_US_toHoliday is NULL")#0
#showing data
ggplot(airbnbTrain_df_real)+aes(account_created_distance_US_toHoliday)+
  geom_histogram(binwidth = 2);

sqldf("select account_created_distance_US_toHoliday, count(*) as count
      from airbnbTrain_df_real 
      group by language 
      order by count desc");



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~first_booking_distance_US_toHoliday~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sqldf("select first_booking_distance_US_toHoliday, count(*) from airbnbTrain_df_real group by first_booking_distance_US_toHoliday");
sqldf("select first_booking_distance_US_toHoliday, count(*) from airbnbTrain_df_real where first_booking_distance_US_toHoliday is NULL")#99661
#showing data
ggplot(airbnbTrain_df_real)+aes(first_booking_distance_US_toHoliday)+
  geom_histogram(binwidth = 2);

sqldf("select first_booking_distance_US_toHoliday, count(*) as count
      from airbnbTrain_df_real 
      group by language 
      order by count desc");


#--------------converting features to categorial---------------------------
airbnbTrain_df_real$gender = as.factor(airbnbTrain_df_real$gender);
airbnbTrain_df_real$signup_method = as.factor(airbnbTrain_df_real$signup_method);
airbnbTrain_df_real$ageBins = as.factor(airbnbTrain_df_real$ageBins);
airbnbTrain_df_real$affiliate_channel = factor(airbnbTrain_df_real$affiliate_channel)
airbnbTrain_df_real$affiliate_provider = factor(airbnbTrain_df_real$affiliate_provider)
airbnbTrain_df_real$first_affiliate_tracked = factor(airbnbTrain_df_real$first_affiliate_tracked)
airbnbTrain_df_real$first_device_type = factor(airbnbTrain_df_real$first_device_type)
airbnbTrain_df_real$signup_app = factor(airbnbTrain_df_real$signup_app)
airbnbTrain_df_real$first_browser = factor(airbnbTrain_df_real$first_browser)
airbnbTrain_df_real$ageBins = as.factor(airbnbTrain_df_real$ageBins);
airbnbTrain_df_real$yearAccountCreated = as.factor(airbnbTrain_df_real$yearAccountCreated);
airbnbTrain_df_real$yearFirstBooking = as.factor(airbnbTrain_df_real$yearFirstBooking);
airbnbTrain_df_real$monthAccountCreated = as.factor(airbnbTrain_df_real$monthAccountCreated);
airbnbTrain_df_real$monthFirstBooking = as.factor(airbnbTrain_df_real$monthFirstBooking);
airbnbTrain_df_real$weekdayAccountCreated = as.factor(airbnbTrain_df_real$weekdayAccountCreated);
airbnbTrain_df_real$weekdayFirstBooking = as.factor(airbnbTrain_df_real$weekdayFirstBooking);
airbnbTrain_df_real$diffDaysAccountFirstActive = as.factor(airbnbTrain_df_real$diffDaysAccountFirstActive);
airbnbTrain_df_real$country_destination = as.factor(airbnbTrain_df_real$country_destination);
airbnbTrain_df_real$language = as.factor(airbnbTrain_df_real$language);

#---convertingthe the test to factor---
airbnbTest_df$gender = as.factor(airbnbTest_df$gender);
airbnbTest_df$signup_method = as.factor(airbnbTest_df$signup_method);
airbnbTest_df$ageBins = as.factor(airbnbTest_df$language);
airbnbTest_df$affiliate_channel = factor(airbnbTest_df$affiliate_channel)
airbnbTest_df$affiliate_provider = factor(airbnbTest_df$affiliate_provider)
airbnbTest_df$affiliate_provider = factor(airbnbTest_df$first_affiliate_tracked)
airbnbTest_df$signup_app = factor(airbnbTest_df$signup_app)
airbnbTest_df$first_device_type = factor(airbnbTest_df$first_device_type)
airbnbTest_df$first_browser = factor(airbnbTest_df$first_browser)
airbnbTest_df$ageBins = as.factor(airbnbTest_df$ageBins);
airbnbTest_df$yearAccountCreated = as.factor(airbnbTest_df$yearAccountCreated);
airbnbTest_df$yearFirstBooking = as.factor(airbnbTest_df$yearFirstBooking);
airbnbTest_df$monthAccountCreated = as.factor(airbnbTest_df$monthAccountCreated);
airbnbTest_df$monthFirstBooking = as.factor(airbnbTest_df$monthFirstBooking);
airbnbTest_df$weekdayAccountCreated = as.factor(airbnbTest_df$weekdayAccountCreated);
airbnbTest_df$weekdayFirstBooking = as.factor(airbnbTest_df$weekdayFirstBooking);


airbnbTest_df$diffDaysAccountFirstActive = as.factor(airbnbTrain_df$diffDaysAccountFirstActive);
airbnbTest_df$language = as.factor(airbnbTest_df$language);
#--------------------------------------------------------------------------------------------
#-----------------------------handling missing data------------------------------------------
#--------------------------------------------------------------------------------------------

PlotMissing(airbnbTrain_df_real);

airbnbTrain_df_real_hotdeck = hotdeck(airbnbTrain_df_real,variable = c("first_device_type","first_browser","first_affiliate_tracked",
                                                             "affiliate_channel","affiliate_provider","age","ageBins","gender"))

PlotMissing(airbnbTrain_df_real_hotdeck);

#--------------------------------------------------------------------------------------------
#-----------------------------barplots of data------------------------------------------
#--------------------------------------------------------------------------------------------

#------------date first booking---------------
ggplot(airbnbTrain_df_real, aes(date_first_booking, fill = country_destination) ) +
  geom_bar(position = "fill")

#changing dataset to fir without NA in date first booking
summary(airbnbTrain_df_real_hotdeck$country_destination)
summary(airbnbTrain_df_real$country_destination)

airbnbTrain_df_real_hotdeck_booking = airbnbTrain_df_real_hotdeck[!is.na(airbnbTrain_df_real_hotdeck$date_first_booking),]
airbnbTrain_df_real_booking= airbnbTrain_df_real[!is.na(airbnbTrain_df_real$date_first_booking),]

summary(airbnbTrain_df_real_hotdeck_booking$country_destination)
summary(airbnbTrain_df_real_booking$country_destination)

#-----------age bins---------------

ggplot(airbnbTrain_df_real_booking, aes(ageBins, fill = country_destination) )+
        geom_bar(position = "fill") +ggtitle("regular")
        

ggplot(airbnbTrain_df_real_hotdeck_booking, aes(ageBins, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------gender---------------

ggplot(airbnbTrain_df_real_booking, aes(gender, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(gender, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------yearAccountCreated---------------

ggplot(airbnbTrain_df_real_booking, aes(yearAccountCreated, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(yearAccountCreated, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------monthAccountCreated---------------

ggplot(airbnbTrain_df_real_booking, aes(monthAccountCreated, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(monthAccountCreated, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------weekdayAccountCreated ---------------

ggplot(airbnbTrain_df_real_booking, aes(weekdayAccountCreated, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(weekdayAccountCreated, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")



#-----------yearFirstBooking---------------

ggplot(airbnbTrain_df_real_booking, aes(yearFirstBooking, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(yearFirstBooking, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------monthFirstBooking---------------

ggplot(airbnbTrain_df_real_booking, aes(monthFirstBooking, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(monthFirstBooking, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------weekdayFirstBooking---------------

ggplot(airbnbTrain_df_real_booking, aes(weekdayFirstBooking, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(weekdayFirstBooking, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------diffDaysAccountFirstActive ---------------

ggplot(airbnbTrain_df_real_booking, aes(diffDaysAccountFirstActive , fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(diffDaysAccountFirstActive , fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")



#-----------diffDaysBookingFirstActive ---------------

ggplot(airbnbTrain_df_real_booking, aes(diffDaysBookingFirstActive, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(diffDaysBookingFirstActive, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------signup_method---------------

ggplot(airbnbTrain_df_real_booking, aes(signup_method, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(signup_method, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------signup_flow---------------

ggplot(airbnbTrain_df_real_booking, aes(signup_flow, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(signup_flow, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------signup_app---------------

ggplot(airbnbTrain_df_real_booking, aes(signup_app, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(signup_app, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------first_device_type---------------

ggplot(airbnbTrain_df_real_booking, aes(first_device_type, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(first_device_type, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------affiliate_channel---------------

ggplot(airbnbTrain_df_real_booking, aes(affiliate_channel, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(affiliate_channel, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------affiliate_provider---------------

ggplot(airbnbTrain_df_real_booking, aes(affiliate_provider, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(affiliate_provider, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------first_affiliate_tracked---------------

ggplot(airbnbTrain_df_real_booking, aes(first_affiliate_tracked, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(first_affiliate_tracked, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------first_browser---------------

ggplot(airbnbTrain_df_real_booking, aes(first_browser, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(first_browser, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------language---------------

ggplot(airbnbTrain_df_real_booking, aes(language, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(language, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")




#-----------account_created_distance_US_toHoliday---------------

ggplot(airbnbTrain_df_real_booking, aes(account_created_distance_US_toHoliday, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(account_created_distance_US_toHoliday, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#-----------first_booking_distance_US_toHoliday---------------

ggplot(airbnbTrain_df_real_booking, aes(first_booking_distance_US_toHoliday, fill = country_destination) )+
  geom_bar(position = "fill") +ggtitle("regular")


ggplot(airbnbTrain_df_real_hotdeck_booking, aes(first_booking_distance_US_toHoliday, fill = country_destination) ) +
  geom_bar(position = "fill") +ggtitle("hotdeck")


#----------------------------------------------------------------------------------------------
#------------------------------------correlations----------------------------------------------
#----------------------------------------------------------------------------------------------

PlotMissing(airbnbTrain_df_real_booking);
PlotMissing(airbnbTrain_df_real_hotdeck_booking);

#changing NA to "NA"
airbnbTrain_df_real_booking = airbnbTrain_df_real_booking[is.na(affiliate_provider), affiliate_provider:="NA"];
airbnbTrain_df_real_booking = airbnbTrain_df_real_booking[is.na(first_device_type), first_device_type:="NA"];
airbnbTrain_df_real_booking = airbnbTrain_df_real_booking[is.na(first_affiliate_tracked), first_affiliate_tracked:="NA"];
airbnbTrain_df_real_booking = airbnbTrain_df_real_booking[is.na(affiliate_channel), affiliate_channel:="NA"];
airbnbTrain_df_real_booking = airbnbTrain_df_real_booking[is.na(first_browser), first_browser:="NA"];
airbnbTrain_df_real_booking = airbnbTrain_df_real_booking[is.na(ageBins), ageBins:="NA"];
airbnbTrain_df_real_booking = airbnbTrain_df_real_booking[is.na(gender), gender:="NA"];

#refactoring
airbnbTrain_df_real_booking$country_destination= factor(airbnbTrain_df_real_booking$country_destination)
airbnbTrain_df_real_booking$affiliate_provider= factor(airbnbTrain_df_real_booking$affiliate_provider)
airbnbTrain_df_real_booking$first_browser= factor(airbnbTrain_df_real_booking$first_browser)

airbnbTrain_df_real_hotdeck_booking$country_destination= factor(airbnbTrain_df_real_hotdeck_booking$country_destination)
airbnbTrain_df_real_hotdeck_booking$affiliate_provider= factor(airbnbTrain_df_real_hotdeck_booking$affiliate_provider)
airbnbTrain_df_real_hotdeck_booking$first_browser= factor(airbnbTrain_df_real_hotdeck_booking$first_browser)


varset1 = c("country_destination",
            "gender","ageBins","signup_method","signup_flow",
            "language","affiliate_channel", "affiliate_provider",
            "first_affiliate_tracked","signup_app","first_device_type",
            "first_browser", "first_booking_distance_US_toHoliday",
            "account_created_distance_US_toHoliday","yearAccountCreated",                   
            "monthAccountCreated","weekdayAccountCreated","yearFirstBooking",                    
            "monthFirstBooking","weekdayFirstBooking",
            "diffDaysAccountFirstActive","diffDaysBookingFirstActive");

#regular dataset
airbnbTrain_corrdf_booking = subset(airbnbTrain_df_real_booking, select = varset1)
GKmatrix1 = GKtauDataframe(airbnbTrain_corrdf_booking)
plot(GKmatrix1, corrColors = "blue")
colnames(airbnbTrain_df_real_booking)

#hotdeck dataset
airbnbTrain_corrdf_hotdeck = subset(airbnbTrain_df_real_hotdeck_booking, select = varset1)
GKmatrix2 = GKtauDataframe(airbnbTrain_corrdf_hotdeck)
plot(GKmatrix2, corrColors = "blue")



#----------------------------------------------------------------------------------------------
#------------------------------------modeling----------------------------------------------
#----------------------------------------------------------------------------------------------

#converting prediction column to 0 for date_first_booking==NA
airbnbTrain_df_real_booking_NA = airbnbTrain_df_real[is.na(date_first_booking),];
airbnbTrain_df_real_booking_NA$"prediction_destination"=0;

#-----------Divide the data to a train (70%) and validation (30%)---------------------------
n = nrow(airbnbTrain_df_real_booking)
set.seed(1) 
train = sample(1:n,size =0.7*n,replace = F);
#regular dataset
airbnbTrain_df_real_booking_train = airbnbTrain_df_real_booking[train,] 
airbnbTrain_df_real_booking_validation = airbnbTrain_df_real_booking[-train,] 
#hotdeck dataset
airbnbTrain_df_real_hotdeck_booking_train = airbnbTrain_df_real_hotdeck_booking[train,] 
airbnbTrain_df_real_hotdeck_booking_validation = airbnbTrain_df_real_hotdeck_booking[-train,] 



#--------------------------------------random forest Regular dataset-------------------------------------------


maxRandomForestBookingMTrys = 0;
maxRandomForestBookingNTree = 0;
maxRandomForestBookingPredict =0;


for (i in 3:10) 
  {
  for (j in 10:50) 
    {
      print(i)
      print(j)
      modelRandom = randomForest(country_destination~ gender+ ageBins+signup_method+signup_flow+
                                   language + affiliate_channel + affiliate_provider+
                                   first_affiliate_tracked + signup_app +first_device_type +
                                   first_browser + first_booking_distance_US_toHoliday +
                                   account_created_distance_US_toHoliday +yearAccountCreated +                  
                                   monthAccountCreated + weekdayAccountCreated + yearFirstBooking +                    
                                   monthFirstBooking + weekdayFirstBooking +
                                   diffDaysAccountFirstActive +diffDaysBookingFirstActive,
                                 data=airbnbTrain_df_real_booking_train,mtry=i,ntree=j);
      
      prediction = predict(modelRandom, airbnbTrain_df_real_booking_validation, type="class");
      predTable = table(predictions=prediction, actual=airbnbTrain_df_real_booking_validation$country_destination)
      #--accuracy matrix
      
      if((sum(diag(predTable))/sum(predTable))>maxPredict)
      {
        maxRandomForestBookingPredict = sum(diag(predTable))/sum(predTable);
        maxRandomForestBookingMTrys = i;
        maxRandomForestBookingNTree = j;
        maxRandomForestBookingModel = modelRandom;
      }
  }
}


modelRandom = randomForest(country_destination~ gender+ ageBins+signup_method+signup_flow+
                             language + affiliate_channel + affiliate_provider+
                             first_affiliate_tracked + signup_app +first_device_type +
                             first_browser + first_booking_distance_US_toHoliday +
                             account_created_distance_US_toHoliday +yearAccountCreated +                  
                             monthAccountCreated + weekdayAccountCreated + yearFirstBooking +                    
                             monthFirstBooking + weekdayFirstBooking +
                             diffDaysAccountFirstActive +diffDaysBookingFirstActive,
                           data=airbnbTrain_df_real_booking_train,mtry=3,ntree=50);

prediction = predict(modelRandom, airbnbTrain_df_real_booking_validation, type="class");
predTable = table(predictions=prediction, actual=airbnbTrain_df_real_booking_validation$country_destination)

#--accuracy matrix
sum(diag(predTable))/sum(predTable)

importance(modelRandom)
varImpPlot(modelRandom)

predictionWithProb = predict(modelRandom, airbnbTrain_df_real_booking_validation, type="prob");
auc = auc(airbnbTrain_df_real_booking_validation$country_destination,predictionWithProb[,2])
plot(roc(airbnbTrain_df_real_booking_validation$country_destination,predictionWithProb[,2]))




#--------------------------------------random forest hotdeck-------------------------------------------


maxRandomForestHotdeckMTrys = 0;
maxRandomForestHotdeckNTree = 0;
maxRandomForestHotdeckPredict =0;

for (i in 3:10) 
{
  for (j in 10:50) 
  {
    print(i)
    print(j)
    modelRandomHotdeck = randomForest(country_destination~ gender+ ageBins+signup_method+signup_flow+
                                 language + affiliate_channel + affiliate_provider+
                                 first_affiliate_tracked + signup_app +first_device_type +
                                 first_browser + first_booking_distance_US_toHoliday +
                                 account_created_distance_US_toHoliday +yearAccountCreated +                  
                                 monthAccountCreated + weekdayAccountCreated + yearFirstBooking +                    
                                 monthFirstBooking + weekdayFirstBooking +
                                 diffDaysAccountFirstActive +diffDaysBookingFirstActive,
                               data=airbnbTrain_df_real_hotdeck_booking_train,mtry=i,ntree=j);
    
    predictionHotdeck = predict(modelRandomHotdeck, airbnbTrain_df_real_hotdeck_booking_validation, type="class");
    predTableHotdeck = table(predictions=predictionHotdeck, actual=airbnbTrain_df_real_hotdeck_booking_validation$country_destination)
    #--accuracy matrix
    
    if((sum(diag(predTableHotdeck))/sum(predTableHotdeck))>maxRandomForestHotdeckPredict)
    {
      maxRandomForestHotdeckPredict = sum(diag(predTableHotdeck))/sum(predTableHotdeck);
      maxRandomForestHotdeckMTrys = i;
      maxRandomForestHotdeckNTree = j;
      maxRandomForestHotdeckModel = mododelelRandomHotdeck;
    }
  }
}


modelRandomHotdeck = randomForest(country_destination~ gender+ ageBins+signup_method+signup_flow+
                                    language + affiliate_channel + affiliate_provider+
                                    first_affiliate_tracked + signup_app +first_device_type +
                                    first_browser + first_booking_distance_US_toHoliday +
                                    account_created_distance_US_toHoliday +yearAccountCreated +                  
                                    monthAccountCreated + weekdayAccountCreated + yearFirstBooking +                    
                                    monthFirstBooking + weekdayFirstBooking +
                                    diffDaysAccountFirstActive +diffDaysBookingFirstActive,
                                  data=airbnbTrain_df_real_hotdeck_booking_train,mtry=3,ntree=45);

predictionHotdeck = predict(modelRandomHotdeck, airbnbTrain_df_real_hotdeck_booking_validation, type="class");
predTableHotdeck = table(predictions=predictionHotdeck, actual=airbnbTrain_df_real_hotdeck_booking_validation$country_destination)

maxRandomForestHotdeckModel = modelRandomHotdeck

#--accuracy matrix
sum(diag(predTableHotdeck))/sum(predTableHotdeck);
importance(maxRandomForestHotdeckModel)
varImpPlot(maxRandomForestHotdeckModel)

predictionWithProbHodeck = predict(maxRandomForestHotdeckModel, airbnbTrain_df_real_hotdeck_booking_validation, type="prob");
auc = auc(airbnbTrain_df_real_hotdeck_booking_validation$country_destination,predictionWithProbHodeck[,2])
plot(roc(airbnbTrain_df_real_hotdeck_booking_validation$country_destination,predictionWithProbHodeck[,2]))

#-----------------------------------------XGBoost regular data-------------------------------------


trainLabelRegularMatrix = recode(airbnbTrain_df_real_booking_train$country_destination,"1"=0,"2"=1)
validationLabelRegularMatrix = recode(airbnbTrain_df_real_booking_validation$country_destination,"1"=0,"2"=1)

trainRegularDataframeToMatrix = airbnbTrain_df_real_booking_train[,
       c("gender","ageBins","signup_method","signup_flow","language",
      "affiliate_channel","affiliate_provider","first_affiliate_tracked",
      "signup_app","first_device_type","first_browser","yearAccountCreated",
      "first_booking_distance_US_toHoliday","account_created_distance_US_toHoliday",
      "monthAccountCreated","weekdayAccountCreated","yearFirstBooking",
      "monthFirstBooking","weekdayFirstBooking","diffDaysAccountFirstActive",
      "diffDaysBookingFirstActive")]

validationRegularDataframeToMatrix = airbnbTrain_df_real_booking_validation[,
      c("gender","ageBins","signup_method","signup_flow",
        "affiliate_channel","affiliate_provider","first_affiliate_tracked",
        "signup_app","first_device_type","first_browser","yearAccountCreated",
        "first_booking_distance_US_toHoliday","account_created_distance_US_toHoliday",
        "monthAccountCreated","weekdayAccountCreated","yearFirstBooking",
        "monthFirstBooking","weekdayFirstBooking","diffDaysAccountFirstActive",
        "diffDaysBookingFirstActive")]
  
trainRegularMatrix = data.matrix(trainRegularDataframeToMatrix[,])
validationRegularMatrix = data.matrix(validationRegularDataframeToMatrix[,])

xgboostTrainRegularModel = xgboost(data = trainRegularMatrix, label = trainLabelRegularMatrix  , max.depth = 9, eta = 1, nthread = 2, nround = 45, objective = "binary:logistic")

predictonXgbRegular = predict(xgboostTrainRegularModel, validationRegularMatrix)
predictonXgbRegular = as.numeric(predictonXgbRegular > 0.5)

errXgbRegular = mean(predictonXgbRegular != validationLabelRegularMatrix)
print(errXgbRegular)


importance_matrix = xgb.importance(model = xgboostTrainRegularModel)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


regularPredictionTable = table(predictions=predictonXgbRegular, actual=validationLabelRegularMatrix)

#--accuracy matrix
sum(diag(regularPredictionTable))/sum(regularPredictionTable);

auc = auc(validationLabelRegularMatrix,predictonXgbRegular)
print(auc)
plot(roc(validationLabelRegularMatrix,predictonXgbRegular))




#-----------------------------------------XGBoost Hotdeck data-------------------------------------
trainLabelHotdeckMatrix = recode(airbnbTrain_df_real_hotdeck_booking_train$country_destination,"1"=0,"2"=1)
validationLabelHotdeckMatrix = recode(airbnbTrain_df_real_hotdeck_booking_validation$country_destination,"1"=0,"2"=1)

trainHotdeckDataframeToMatrix = airbnbTrain_df_real_hotdeck_booking_train[,
                    c("gender","ageBins","signup_method","signup_flow","language",
                      "affiliate_channel","affiliate_provider","first_affiliate_tracked",
                      "signup_app","first_device_type","first_browser","yearAccountCreated",
                      "first_booking_distance_US_toHoliday","account_created_distance_US_toHoliday",
                      "monthAccountCreated","weekdayAccountCreated","yearFirstBooking",
                      "monthFirstBooking","weekdayFirstBooking","diffDaysAccountFirstActive",
                      "diffDaysBookingFirstActive")]

validationHotdeckDataframeToMatrix = airbnbTrain_df_real_hotdeck_booking_validation[,
                    c("gender","ageBins","signup_method","signup_flow",
                      "affiliate_channel","affiliate_provider","first_affiliate_tracked",
                      "signup_app","first_device_type","first_browser","yearAccountCreated",
                      "first_booking_distance_US_toHoliday","account_created_distance_US_toHoliday",
                      "monthAccountCreated","weekdayAccountCreated","yearFirstBooking",
                      "monthFirstBooking","weekdayFirstBooking","diffDaysAccountFirstActive",
                      "diffDaysBookingFirstActive")]

trainHotdeckMatrix = data.matrix(trainHotdeckDataframeToMatrix[,])
validationHotdeckMatrix = data.matrix(validationHotdeckDataframeToMatrix[,])

xgboostTrainHotdeckModel = xgboost(data = trainHotdeckMatrix, label = trainLabelHotdeckMatrix  , max.depth = 9, eta = 1, nthread = 2, nround = 45, objective = "binary:logistic")

predictonXgbHotdeck = predict(xgboostTrainHotdeckModel, validationHotdeckMatrix)
predictonXgbHotdeck = as.numeric(predictonXgbHotdeck > 0.5)

errXgbHotdeck = mean(predictonXgbHotdeck != validationLabelHotdeckMatrix)
print(errXgbHotdeck)


importance_matrix_Hotdeck = xgb.importance(model = xgboostTrainHotdeckModel)
print(importance_matrix_Hotdeck)
xgb.plot.importance(importance_matrix = importance_matrix_Hotdeck)


hotdeckPredictionTable = table(predictions=predictonXgbHotdeck, actual=validationLabelHotdeckMatrix)

#--accuracy matrix
sum(diag(hotdeckPredictionTable))/sum(hotdeckPredictionTable);

aucHotdeck = auc(validationLabelRegularMatrix,predictonXgbHotdeck)
print(aucHotdeck)
plot(roc(validationLabelHotdeckMatrix,predictonXgbHotdeck))






model = glm(country_destination ~ ageBins, data = airbnbTrain_df_real_booking_train, family = binomial)
model
predict = predict(model, type = 'response')
table(airbnbTrain_df_real_booking_train$country_destination, predict > 0.5)

ROCRpred = prediction(predict, airbnbTrain_df_real_booking_train$country_destination)
ROCRperf = performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))








#---------replace na with -1 for the multinom -----------------------------------

airbnbTrain_df_real[is.na(age), age := -1]
airbnbTrain_df_real[is.na(diffDaysBookingFirstActive), diffDaysBookingFirstActive := -1]
airbnbTrain_df_real[is.na(dateFirstBooking), dateFirstBooking := -1]
airbnbTrain_df_real[is.na(first_booking_distance_US_toHoliday), first_booking_distance_US_toHoliday := -1]
data = airbnbTrain_df_real[complete.cases(airbnbTrain_df_real), ]

#--------------------------logistic-regression-multinom----------------

multi_model <- multinom(country_destination~gender+first_device_type+signup_flow+first_booking_distance_US_toHoliday+yearFirstBooking+monthFirstBooking+diffDaysBookingFirstActive 
                    ,data=data)
summary(multi_model)

#-------------------------extras-------------------------

plot_correlation(airbnbTrain_df_real_booking , use = "pairwise.complete.obs")

colnames(airbnbTrain_df_real)





#------------------Model 2 KNN--------------------------

prc_test_pred = knn(train = airbnbTrain_df_real , test = airbnbTest_df,cl = prc_train_labels, k=100000)


CrossTable(x = prc_train_labels, y = prc_test_pred, prop.chisq = FALSE)
