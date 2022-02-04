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


# Load data and preprocessing ---------------------------------------------


#The data goes from 2011-01-29 - 2016-06-19 for all categories
data_read = read.csv("/content/Projectdata.csv")

#rename column "date"
names(data_read)[names(data_read) == "ï..date"] <- "date"

# data <- read.zoo(data, format = "%Y-%m-%d")
head(data,2)


# change to timeseries
data <- read.zoo(data_read, format = "%Y-%m-%d")

#convert to xts (more compatible than zoo)
#data <- as.xts(data_zoo)
data <- data[1:800]
head(data,1)


#Define variables

Hobbies     = data$Hobbies_CA_1 + data$Hobbies_CA_2 + data$Hobbies_CA_3
Household_1 = data$Household_1_CA_1 + data$Household_1_CA_2 + data$Household_1_CA_3
Household_2 = data$Household_2_CA_1 + data$Household_2_CA_2 + data$Household_2_CA_3
Foods_1     = data$Foods_1_CA_1 + data$Foods_1_CA_2 + data$Foods_1_CA_3
Foods_2     = data$Foods_2_CA_1 + data$Foods_2_CA_2 + data$Foods_2_CA_3
Foods_3     = data$Foods_3_CA_1 + data$Foods_3_CA_2 + data$Foods_3_CA_3

Store_1 = data$Hobbies_CA_1 + data$Household_1_CA_1 + data$Household_2_CA_1 + data$Foods_1_CA_1 + data$Foods_2_CA_1 + data$Foods_3_CA_1
Store_2 = data$Hobbies_CA_2 + data$Household_1_CA_2 + data$Household_2_CA_2 + data$Foods_1_CA_2 + data$Foods_2_CA_2 + data$Foods_3_CA_2
Store_3 = data$Hobbies_CA_3 + data$Household_1_CA_3 + data$Household_2_CA_3 + data$Foods_1_CA_3 + data$Foods_2_CA_3 + data$Foods_3_CA_3

#The data shows that there is a recurring pattern of 0 (or close to 0) for all categories at the end of each year (YYYY-12-25)
#Since this happens for all categories in every year, we take out the outliers and check whether the 0 have been removed

#INDICES IDENTIFIED AS OUTLIER NEED TO BE TAKEN INTO ACCOUNT WHEN ANALYZING RESULTS
# tsclean uses linear interpolation

Hobbies     <- tsclean(Hobbies)
Household_1 <- tsclean(Household_1)
Household_2 <- tsclean(Household_2)
Foods_1     <- tsclean(Foods_1)
Foods_2     <- tsclean(Foods_2)
Foods_3     <- tsclean(Foods_3)

#..for adf test
TimeSeries = data.frame(Hobbies, Household_1, Household_2,Foods_1,Foods_2,Foods_3)


# check stationarity for specific time series ---------------------------------------------

#Visual stationarity check
source('check_stationarity.R')
checkStat(Hobbies)

#implement Augmented Dickey-fuller
adf(timeSeries)

#Result
#Negative value in the test means rejection; all relevant parameters must be negative to reject null

#Type: none, diff = 0
#Even at a p-value of 0.05; all 6 variables show a unit root. Therefore, the null-hypothesis is not rejected.
#The variables are all non-stationary

#Type: none, diff = 1
#At a p-value of 0.01; the first difference of all six variables show no unit root.
#The null-hypothesis is rejected´; all six variables become stationary by taking the first difference



# Main pipeline ---------------------------------------------

Benchmark <- function(data_set, scope, horizon){
  source('prepare_data.R')
  ##------Define scope
  if (scope=='dailyTS365'){
    exog_variables <- create_exog_vars_daily(data_set)
    data_set <- convertToDailyTS365(data_set)
  } 
  
  if (scope=='dailyTS7'){
    exog_variables <- create_exog_vars_daily(data_set)
    data_set <- convertToDailyTS7(data_set)
  } 
  else if(scope =='weekly'){
    #weekly data moves by a day because of leap year and uneven division of 365/7
    data_set <- convertToWeeklyTS(data_set)
  }
  
  
  ##------split train-test
  list[train_set, test_set] <- split(data_set, ratio)
  if(scope!='weekly'){
    exog_vars_train <- exog_variables[1:length(train_set) , ]
    exog_vars_test <- exog_variables[(length(train_set)+1):(length(train_set) + length(test_set)) , ]
  }
  else {
    exog_vars_train = 0
    exog_vars_test = 0
  }
  
  source('tuning.R')
  ##------Tune Benchmark parameters with validation set
  tunedParameters <- tune(train_set, horizon, exog_vars_train, scope)
  
  source('evaluation.R')
  
  ##------Benchmark on test set iteratively (including train_set)
  TEST_evaluationDF <- evaluate(test_set, train_set, horizon, tunedParameters, scope, exog_vars_train, exog_vars_test)
  #TEST_evaluationDF <- evaluate(tunedParameters[[8]], tunedParameters[[7]], horizon, tunedParameters, scope, exog_vars_train, exog_vars_test)
  
  ##------Plot benchmark results
  #plotResult(data_set, train_set, test_set, TEST_evaluationDF, horizon)
  
  #return results
  #list(data_set=data_set, train_set=train_set, test_set=test_set, TEST_evaluationDF=TEST_evaluationDF, horizon=horizon)
  return(TEST_evaluationDF)
  #print(data.frame(TEST_evaluationDF))
}

# Call pipeline with parameters ---------------------------------------------

#define data extraction: Benchmark(dataset, frequency, horizon)
Benchmark(Hobbies,'DailyTS7',1)
