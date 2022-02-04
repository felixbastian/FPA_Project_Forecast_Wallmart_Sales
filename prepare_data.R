# Run packages ---------------------------------------------
install.packages('forecast', dependencies = TRUE) #For acf, pacf, etc.
# install.packages('aTSA') # For augmented dickey fuller
# install.packages("ggplot2")
# #install.packages("gridExtra") #plotting arrangement
# install.packages("urca") #ur.df and augmented dickey fuller
# #install.packages('Metrics') #RMSSEE (accuracy for point forecast)
# install.packages('zoo')#time series format
# install.packages('xts')#time series format
# install.packages("tsibble")#alt time series format
# install.packages('fpp2')#outliers
# install.packages('tidyverse')
# install.packages('gsubfn') #unpack list from function to return multiple values
# install.packages('TSstudio') #for train-test split of ts()

library(ggplot2)
#library(gridExtra)
library(aTSA)
library(urca)
library(dplyr)
#library(Metrics)
library(forecast)
library(zoo)
library(xts)
library(tsibble)
library(fpp2)
library(tidyverse)
library(gsubfn)
library(TSstudio)

# Create several sample splits --------------------------------------------
#Split data using a training/testing separation
split <- function(data_set, test_size){
  
  #we take last year as testing set
  train_test_split <- ts_split(data_set, sample.out = test_size)
  train <- train_test_split$train
  test <- train_test_split$test
  
  list(train_set = train, test_set = test)
}
#The data goes from 2011-01-29 - 2016-06-19 for all categories

# Define time series interval --------------------------------------------

#Convert to daily frequency with 365 intervals
convertToDailyTS365 <- function(data_set){
  
  #Get rid of leap year entry
  data_set <- data_set[format(time(data_set), "%m %d") != "02 29"]
  
  ## Create a daily Date object - helps my work on dates
  inds <- seq(as.Date("2011-01-29"), as.Date("2016-06-19"), by = "day")
  
  ## Create a time series object
  #as.numeric... is working out what day of the year the starting date is:
  data_set <- ts(data_set,
                 start = c(2011, as.numeric(format(inds[1], "%j"))),
                 frequency = 365)
  return(data_set)
  
}

#Convert to daily frequency with 7 day intervals
convertToDailyTS7 <- function(data_set){
  
  ## Create a daily Date object - helps my work on dates
  inds <- seq(as.Date("2011-01-29"), as.Date("2016-06-19"), by = "day")
  
  ## Create a time series object
  #as.numeric... is working out what day of the year the starting date is:
  data_set <- ts(data_set,
                 start = c(2011, as.numeric(format(inds[1], "%j"))),
                 frequency = 7)
  return(data_set)
}

#Convert to weekly frequency with 52 weeks intervals
convertToWeeklyTS <- function(data_set){
  
  # data_set <- data_set[data_set != '2016-02-29']
  # print(data_set)
  weekly <- apply.weekly(data_set,sum)
  
  data_set <- ts(weekly,
                 start = c(2011, 29),
                 frequency = 52)
  return(data_set)
}

# Exogenous --------------------------------------------

create_exog_vars_daily <- function(data_set){
  
  #Get rid of leap year entry
  #data_set <- data_set[format(time(data_set), "%m %d") != "02 29"]
  
  # Get weekdays from date
  exog_variables = data.frame(weekdays(time(Hobbies)))
  
  # Modify exogenous variables
  colnames(exog_variables) = c('day_of_week')
  
  #insert specific event dates such as christmas or thanksgiving
  #special_dates = read.csv("/content/special_dates.csv")
  
  # One hot encode
  exog_variables$is_mon <- ifelse(exog_variables$day_of_week == 'Monday', 1, 0)
  exog_variables$is_tue <- ifelse(exog_variables$day_of_week == 'Tuesday', 1, 0)
  exog_variables$is_wed <- ifelse(exog_variables$day_of_week == 'Wednesday', 1, 0)
  exog_variables$is_thu <- ifelse(exog_variables$day_of_week == 'Thursday', 1, 0)
  exog_variables$is_fri <- ifelse(exog_variables$day_of_week == 'Friday', 1, 0)
  exog_variables$is_sat <- ifelse(exog_variables$day_of_week == 'Saturday', 1, 0)
  exog_variables$is_sun <- ifelse(exog_variables$day_of_week == 'Sunday', 1, 0)
  
  #exog_variables <- rbind(exog_variables, special_dates)
  
  # Add variable is_weekend <- actually I can't because it is perfectly correlated with [Sat, Sun]
  # exog_variables$is_weekend <- ifelse(exog_variables$is_sat == 1, 1, ifelse(exog_variables$is_sun == 1, 1, 0))
  
  # Drop original column with 'Saturday', 'Sunday', etc.
  exog_variables <- as.matrix(exog_variables[3:8])
  
  return(exog_variables)
}
