
install.packages('ggplot2')
install.packages('lubridate')
install.packages(c("FactoMineR", "factoextra"));
install.packages("dummies");
install.packages("data.table");
install.packages('bit64')
install.packages("sqldf")

library(ggplot2)
library(lubridate)
library("FactoMineR")
library("factoextra");
library(dummies)
library(data.table)
library(bit64)
library(sqldf)


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




