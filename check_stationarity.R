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

# stationarity testing ---------------------------------------------

checkStat <- function(timeSeries){
  
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  print(autoplot(timeSeries))
  acf(timeSeries)
  pacf(timeSeries)
  
  #Plot time series
  
  time_series_plot(timeSeries)
  
  #Plot first difference of time series
  
  time_series_plot(diff(timeSeries))
  
    
    
    
}

adf <- function(timeSeries){
  # First difference of data
  TimeSeries_diff <- as.data.frame(diff(as.matrix(TimeSeries), lag = 1))
  colnames(TimeSeries_diff) = c('Hobbies', 'Household_1', 'Household_2', 'Foods_1', 'Foods_2', 'Foods_3')
  
  #We conduct three types of adf tests to account for:
  # Drift, Drift and Trend or None
  #We look at the plots to identify which series needs to be tested in which way
  #Drift -> Intercept (Necessary if data does not move around 0)
  #Trend -> Series shows a positive or negative direction over time
  
  time_series_plot(Hobbies)
  time_series_plot(diff(Hobbies))
  
  #Hobbies: Drift (maybe Trend)
  #Household_1: Drift and Trend
  #Household_2: Drift and Trend
  #Foods_1: Drift
  #Foods_2: Drift
  #Foods_3: Drift
  
  #Hobbies_diff: none
  #Household_1_diff:none
  #Household_2_diff: none
  #Foods_1_diff: none
  #Foods_2_diff: none
  #Foods_3_diff: none
  
  
  # Summary of ADF test of level variables ---------------------------------
  
  #We select the number of lags based on the BIC criteria
  
  lt.adf.TimeSeries.none  = list(
    Hobbies = ur.df(TimeSeries$Hobbies, type="none", selectlags = c('BIC')),
    Household_1 = ur.df(TimeSeries$Household_1, type="none", selectlags = c('BIC')),
    Household_2 = ur.df(TimeSeries$Household_2, type='none', selectlags = c('BIC')),
    Foods_1 = ur.df(TimeSeries$Foods_1, type='none', selectlags = c('BIC')),
    Foods_2 = ur.df(TimeSeries$Foods_2, type='none', selectlags = c('BIC')),
    Foods_3 = ur.df(TimeSeries$Foods_3, type='none', selectlags = c('BIC'))
  )
  
  lt.adf.TimeSeries.drift  = list(
    Hobbies = ur.df(TimeSeries$Hobbies, type="drift", selectlags = c('BIC')),
    Household_1 = ur.df(TimeSeries$Household_1, type="drift", selectlags = c('BIC')),
    Household_2 = ur.df(TimeSeries$Household_2, type='drift', selectlags = c('BIC')),
    Foods_1 = ur.df(TimeSeries$Foods_1, type='drift', selectlags = c('BIC')),
    Foods_2 = ur.df(TimeSeries$Foods_2, type='drift', selectlags = c('BIC')),
    Foods_3 = ur.df(TimeSeries$Foods_3, type='drift', selectlags = c('BIC'))
  )
  
  lt.adf.TimeSeries.trend  = list(
    Hobbies = ur.df(TimeSeries$Hobbies, type="trend", selectlags = c('BIC')),
    Household_1 = ur.df(TimeSeries$Household_1, type="trend", selectlags = c('BIC')),
    Household_2 = ur.df(TimeSeries$Household_2, type='trend', selectlags = c('BIC')),
    Foods_1 = ur.df(TimeSeries$Foods_1, type='trend', selectlags = c('BIC')),
    Foods_2 = ur.df(TimeSeries$Foods_2, type='trend', selectlags = c('BIC')),
    Foods_3 = ur.df(TimeSeries$Foods_3, type='trend', selectlags = c('BIC'))
  )
  
  # Summary of ADF test of 1st differenced variables ---------------------------------
  
  lt.adf.TimeSeries_diff.none  = list(
    Hobbies = ur.df(TimeSeries_diff$Hobbies, type='none', selectlags = c('BIC')),
    Household_1 = ur.df(TimeSeries_diff$Household_1, type='none', selectlags = c('BIC')),
    Household_2 = ur.df(TimeSeries_diff$Household_2, type='none', selectlags = c('BIC')),
    Foods_1 = ur.df(TimeSeries_diff$Foods_1, type='none', selectlags = c('BIC')),
    Foods_2 = ur.df(TimeSeries_diff$Foods_2, type='none', selectlags = c('BIC')),
    Foods_3 = ur.df(TimeSeries_diff$Foods_3, type='none', selectlags = c('BIC'))
  )
  
  # ADF - Result interpretation ---------------------------------------------
  
  summary(lt.adf.TimeSeries.drift$Foods_1)
  #summary(lt.adf.TimeSeries_diff.none$Hobbies)
  print("-----------------------------------------------------------")
  
  #For the result interpretation, we follow a general to specific approach
  
  print('Level Variable with Drift and Trend')
  cbind(t(lt.adf.TimeSeries.trend$Hobbies@teststat), lt.adf.TimeSeries.trend$Hobbies@cval)
  
  print('Level Variable with Drift')
  cbind(t(lt.adf.TimeSeries.drift$Hobbies@teststat), lt.adf.TimeSeries.drift$Hobbies@cval)
  
  print('Level Variable with None') 
  cbind(t(lt.adf.TimeSeries.none$Hobbies@teststat),  lt.adf.TimeSeries.none$Hobbies@cval)
  print("-----------------------------------------------------------")
  
  
  print('1st Diff. Variable with None')
  cbind(t(lt.adf.TimeSeries_diff.none$Hobbies@teststat),  lt.adf.TimeSeries_diff.none$Hobbies@cval)
  
}
