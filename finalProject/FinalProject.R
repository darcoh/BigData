
install.packages('ggplot2')
install.packages('lubridate')
install.packages(c("FactoMineR", "factoextra"));
install.packages("dummies");


library(ggplot2)
library(lubridate)
library("FactoMineR")
library("factoextra");
library(dummies)


#local data folder
list.files()
airbnbTrain_df <- read.csv("airbnb_train.csv",header = T)
airbnbTest_df <- read.csv("airbnb_test.csv.csv",header = T)
