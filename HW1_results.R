#--------------------------------------------------------
#----------------General Look----------------------------
#--------------------------------------------------------

setwd("Data"); #1 - local data folder-github oriented (https://github.com/darcoh/BigData)
list.files(); #2
housing <- read.csv("housing.csv",header = T) ; #3
housing_rows = nrow(housing);
housing_cols = ncol(housing);
sprintf("There are %d rows and %d columns",housing_rows,housing_cols); #4
names(housing); #5
summary(housing); #6

#--------------------------------------------------------
#------------------Exploring SalePrice-------------------
#--------------------------------------------------------

mean(housing$SalePrice); #7
summary(housing$SalePrice); #8
quantile(housing$SalePrice, probs=c(0.95)); #9
quant_5_per_gap <- seq(0,1,by=0.05); #10
quantile(housing$SalePrice, probs=quant_5_per_gap); #11
by(housing$SalePrice,housing$YrSold,mean); #12

#--------------------------------------------------------
#------------------Exploring LotArea---------------------
#--------------------------------------------------------

by(housing$LotArea,housing$YrSold,mean); #13
cor(housing$LotArea, housing$SalePrice); #14

#--------------------------------------------------------
#------------------Exploring SaleCondition---------------
#--------------------------------------------------------

SalesCondition_Count_Table <- table(housing$SaleCondition); 
print(SalesCondition_Count_Table); #15+16
table(housing$SaleCondition,housing$YrSold);   #17 - two-way table of counts
prop.table(table(housing$SaleCondition,housing$YrSold)); #18 - two-way table of freqs
HousingAfter1980= subset(housing,housing$YearBuilt>1980);  #19 - creating table of houses after 1980
cor(HousingAfter1980$LotArea, HousingAfter1980$SalePrice); #20

#--------------------------------------------------------
#------------------Exploring LotFrontage-----------------
#--------------------------------------------------------

Housing_NoNA_LotFrontage=(subset(housing, housing$LotFrontage!= "NA"));
mean(Housing_NoNA_LotFrontage$LotFrontage); #21
#another shorter option for 21 :  mean(housing$LotFrontage,na.rm = T);
HousingAfter1980$LotFrontage_Has_NA = is.na(HousingAfter1980$LotFrontage); #22
HousingAfter1980_LotFrontage_True = subset(HousingAfter1980, HousingAfter1980$LotFrontage_Has_NA==F);
cor(HousingAfter1980_LotFrontage_True$SalePrice,HousingAfter1980_LotFrontage_True$LotFrontage)#23

#--------------------The End------------------------------