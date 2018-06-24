import datetime
import holidays
import pandas as pd
import airbnb
import numpy as np

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~adding extra data~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#--loading data as df
df_train = pd.read_csv(r"C:\Users\Itamar\Desktop\itamar\tau\year_3\airbnb_train.csv");

#--creating 2 new columns
df_train["first_booking_distance_US_toHoliday"] = np.NaN;
df_train["account_created_distance_US_toHoliday"] = np.NaN;

#--the script
us_holidays = holidays.UnitedStates();
count_nans = 0;
for i in range(df_train.shape[0]):
    firstBookingBoolean = True;
    accountCreatedBoolean = True;
    continueFirstBookingBoolean=True;
    continueAccountCreatedBoolean=True;
    if(pd.isna(df_train['date_first_booking'][i])):
        count_nans+=1;
        firstBookingBoolean = False;
    if(pd.isna(df_train['date_account_created'][i])):
        count_nans+=1;
        accountCreatedBoolean = False;
    if(firstBookingBoolean==True):
        date_time_first_booking = datetime.datetime.strptime(df_train['date_first_booking'][i], '%Y-%m-%d');
    if(accountCreatedBoolean==True):
        date_time_account_created = datetime.datetime.strptime(df_train['date_account_created'][i], '%Y-%m-%d');
    for j in range(0,365):
        if(firstBookingBoolean==True):
            DateFirstBooking = datetime.date(date_time_first_booking.year,date_time_first_booking.month,date_time_first_booking.day);
            if(continueFirstBookingBoolean==True):
                if(DateFirstBooking in us_holidays):
                    df_train.loc[i,"first_booking_distance_US_toHoliday"] = j;
                    continueFirstBookingBoolean = False;
                else:
                    date_time_first_booking = DateFirstBooking + datetime.timedelta(days=1);
        if(accountCreatedBoolean==True):
            DateAccountCreated = datetime.date(date_time_account_created.year,date_time_account_created.month,date_time_account_created.day);
            if(continueAccountCreatedBoolean==True):
                if(DateAccountCreated in us_holidays):
                    continueAccountCreatedBoolean = False;
                    df_train.loc[i,"account_created_distance_US_toHoliday"] = j;
                else:
                    date_time_account_created = DateAccountCreated + datetime.timedelta(days=1);

#--writing to new csv file
df_train.to_csv(r"C:\Users\Itamar\Desktop\itamar\tau\year_3\train_with_holiday_distance.csv")
    
