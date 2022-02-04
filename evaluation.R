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

# evaluation ---------------------------------------------

evaluate <- function(data_set, preliminary, horizon, tunedParameters, scope, exog_variables_train, exog_variables_test){
  #preliminary is the timeseries that will be out in from of the data_set 
  #(if whole train_set should be inlcuded as preliminary information in the evaluation of test_set)
  #for in-sample accuracy it would only contain the train set up to the validation part
  
  forecast_df.naive <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.naive_prob <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.sNaive <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.ses <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.MA <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.SARIMA <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.HW <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.state_space <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.HW_FT_SARIMA <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.SARIMAX <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  forecast_df.SS_FT_SARIMAX <- setNames(as.data.frame(matrix(ncol=(length(colNames)+1), nrow=0)), colNames)
  
  #SARIMAX fit
  if(scope != 'weekly'){
    SARIMAX <- auto.arima(preliminary, seasonal=TRUE, xreg = exog_variables_train)
    SARIMAX_test <- Arima(data_set, model=SARIMAX, xreg = exog_variables_test)
    SARIMAX_forecast_all <- fitted(SARIMAX_test, h=horizon) # obtian 1-step ahead forecast for entire test set
  }
  
  for (x in 1:length(data_set)){
    
    #only use the preliminary data for the prediction of the first element
    if(x==1) timeSeries <- preliminary
    
    #if not first element, gradually add elements to the timeseries to be evaluated
    else{
      #Define timeSeries that fills iteratively with acquired information and includes the preliminary
      timeSeries <- ts(c(preliminary,data_set[1:x-1]), start= start(preliminary), frequency = frequency(data_set))
    }
    
    #Probabilistic forecast parameter
    probQuantiles = c(50,67,95,99)
    
    #naive
    #cannot calculate error beyond time series -> for two step ahead forecast for last element MAE=(prediction-last element/1) instead of 2
    #accuracy = OK
    naive = naive(timeSeries, h=horizon,level = probQuantiles)
    naive.RMSSE <- RMSSE(data.frame(naive)['Point.Forecast'], timeSeries, data_set[x:length(data_set)], horizon)
    #return the accuracy of test set (access specific metric via [2,x])
    naive.accuracy <- data.frame(accuracy(naive, data_set))[2,1:5]
    naive.prob <- data.frame(naive)
    
    
    #print(data.frame(naive)['Lo.50'])
    #Probabilistic
    # naive
    # data.frame(naive)
    
    # #sNaive
    #longer forecast horizon does not change forecast as long as horizon is not above 365 days
    #since it should always takes dates from a year ago -> but it doesn't (guess not in scope of sNaive)
    sNaive = snaive(timeSeries, h=horizon,level = probQuantiles)
    sNaive.RMSSE <- RMSSE(data.frame(sNaive)['Point.Forecast'], timeSeries, data_set[x:length(data_set)], horizon)
    sNaive.accuracy <- data.frame(accuracy(sNaive, data_set))[2,1:5]
    
    #Probabilistic
    
    #Simple exponential Smoothing
    a = as.numeric(tunedParameters[[1]][1])
    ses <- ses(timeSeries, alpha=a, h = horizon,level = probQuantiles)
    ses.RMSSE <- RMSSE(data.frame(ses)['Point.Forecast'], timeSeries, data_set[x:length(data_set)], horizon)
    ses.accuracy <- data.frame(accuracy(ses, data_set))[2,1:5]
    
    #Probabilistic
    
    # 4. MA
    optimal_order <- as.numeric(tunedParameters[[2]])
    MA_forecast <- MA(timeSeries, optimal_order, horizon)
    MA.RMSSE <- RMSSE(data.frame(MA_forecast), timeSeries, data_set[x:length(data_set)], horizon)
    MA.accuracy <- data.frame(accuracy(ts(MA_forecast, start = start(data_set)), data_set[x:(x+(horizon-1))]), row.names=NULL)
    
    # # 5. ESX
    # #NEEDS TO BE DONE
    
    # # 6. SARIMA
    SARIMA_parameters = tunedParameters[[3]]
    if (scope == 'dailyTS7'){
      SARIMA_implementation <- Arima(timeSeries, order = c(SARIMA_parameters[1:3]), seasonal = c(SARIMA_parameters[4:6]))
    }
    else if (scope=='weekly'){
      SARIMA_implementation <- Arima(timeSeries, order = c(SARIMA_parameters[1:3]))
    }
    
    SARIMA <- forecast(SARIMA_implementation, h=horizon,level = probQuantiles)
    SARIMA.RMSSE <- RMSSE(data.frame(SARIMA)['Point.Forecast'], timeSeries, data_set[x:length(data_set)], horizon)
    SARIMA.accuracy <- data.frame(accuracy(SARIMA, data_set))[2,1:5]
    
    #Probabilistic
    
    # 7. SARIMAX
    if(scope!='weekly'){
      SARIMAX_forecast = SARIMAX_forecast_all[x:(x+horizon-1)]
      SARIMAX.RMSSE <- RMSSE(data.frame(SARIMAX_forecast), timeSeries, data_set[x:length(data_set)], horizon)
      SARIMAX.accuracy <- data.frame(accuracy(ts(SARIMAX_forecast, start = start(data_set)), data_set[x:(x+(horizon-1))]), row.names=NULL)
    }
    
    # 8. Holt-Winters
    HW <- ets(timeSeries, model=tunedParameters[[4]], use.initial.values=TRUE,h=horizon)
    HW_forecast <- forecast(HW, h=horizon, xreg=fourier(data_set, K=4),level = probQuantiles)
    HW.RMSSE <- RMSSE(data.frame(HW_forecast)['Point.Forecast'], timeSeries, data_set[x:length(data_set)], horizon)
    HW.accuracy <- data.frame(accuracy(ts(data.frame(HW_forecast)['Point.Forecast'], start = start(data_set)), data_set[x:(x+(horizon-1))]), row.names=NULL)
    
    #Probabilistic
    
    
    # 9. State-space
    # More info: https://robjhyndman.com/papers/ComplexSeasonality.pdf
    state_space_model <- tbats(timeSeries, model=tunedParameters[[5]], h=horizon)
    state_space_forecast <- forecast(state_space_model, h=horizon)
    state_space.RMSSE <- RMSSE(data.frame(state_space_forecast)['Point.Forecast'], timeSeries, data_set[x:length(data_set)], horizon)
    #state_space.accuracy <- data.frame(accuracy(state_space_forecast, data_set))[2,1:5]
    state_space.accuracy <- data.frame(accuracy(ts(data.frame(state_space_forecast)['Point.Forecast'], start = start(data_set)), data_set[x:(x+(horizon-1))]), row.names=NULL)
    
    
    # A state space approach to the problem (of applying seasonality to daily data)
    # is possible using TBATS models,
    # as described in this paper on complex seasonality. 
    # The tbats() function which implements TBATS models will automatically
    # select the Fourier order as well as the other aspects of the model. 
    # One advantage of the TBATS model is the seasonality is allowed to change slowly over time.
    
    #10. Arithmetric mean Holt-Winters & (S)ARIMA
    SS_FT_SARIMAX <- (data.frame(SARIMAX_forecast)+data.frame(state_space_forecast)['Point.Forecast'])/2
    SS_FT_SARIMAX.RMSSE <- RMSSE(SS_FT_SARIMAX, timeSeries, data_set[x:length(data_set)], horizon)
    SS_FT_SARIMAX.accuracy <- data.frame(accuracy(ts(SS_FT_SARIMAX, start = start(data_set)), data_set[x:(x+(horizon-1))]), row.names=NULL)
    
    #11. Arithmetric mean Holt-WintersX & (S)ARIMAX
    HW_FT_SARIMA <- (data.frame(HW_forecast)['Point.Forecast']+data.frame(SARIMA)['Point.Forecast'])/2
    HW_FT_SARIMA.RMSSE <- RMSSE(HW_FT_SARIMA, timeSeries, data_set[x:length(data_set)], horizon)
    HW_FT_SARIMA.accuracy <- data.frame(accuracy(ts(HW_FT_SARIMA, start = start(data_set)), data_set[x:(x+(horizon-1))]), row.names=NULL)
    
    
    #create a new rowforecast_df.naive_prob
    
    new_row.naive <- setNames(data.frame(data.frame(naive)['Point.Forecast'],naive.RMSSE,naive.accuracy, naive.prob),c('naive',colNames))
    new_row.sNaive <- setNames(data.frame(data.frame(sNaive)['Point.Forecast'],sNaive.RMSSE,sNaive.accuracy),c('sNaive',colNames))
    new_row.MA <- setNames(data.frame(data.frame(MA_forecast),MA.RMSSE,MA.accuracy),c('MA',colNames))
    new_row.ses <- setNames(data.frame(data.frame(ses)['Point.Forecast'],ses.RMSSE,ses.accuracy),c('ses',colNames))
    new_row.SARIMA <- setNames(data.frame(data.frame(SARIMA)['Point.Forecast'],SARIMA.RMSSE,SARIMA.accuracy),c('SARIMA',colNames))
    new_row.HW <- setNames(data.frame(data.frame(HW_forecast)['Point.Forecast'],HW.RMSSE,HW.accuracy),c('HW',colNames))
    new_row.state_space <- setNames(data.frame(data.frame(state_space_forecast)['Point.Forecast'],state_space.RMSSE,state_space.accuracy),c('state_space',colNames))
    new_row.HW_FT_SARIMA <- setNames(data.frame(HW_FT_SARIMA,HW_FT_SARIMA.RMSSE,HW_FT_SARIMA.accuracy),c('HW_FT_SARIMA',colNames))
    if(scope!='weekly')new_row.SARIMAX <- setNames(data.frame(data.frame(SARIMAX_forecast),SARIMAX.RMSSE,SARIMAX.accuracy),c('SARIMAX',colNames))
    #else new_row.SARIMAX <- setNames(data.frame(data.frame(SARIMAX_forecast),0,c(0,0,0,0,0,0)),c('SARIMAX',colNames))
    new_row.SS_FT_SARIMAX <- setNames(data.frame(SS_FT_SARIMAX,SS_FT_SARIMAX.RMSSE,SS_FT_SARIMAX.accuracy),c('SS_FT_SARIMAX',colNames))
    
    
    #add new row to dataframe
    forecast_df.naive_prob <- rbind(forecast_df.naive_prob,naive.prob)
    forecast_df.naive <<- rbind(forecast_df.naive, new_row.naive)
    forecast_df.sNaive <- rbind(forecast_df.sNaive, new_row.sNaive)
    forecast_df.MA <- rbind(forecast_df.MA, new_row.MA, row.names = NULL)
    forecast_df.ses <- rbind(forecast_df.ses, new_row.ses)
    forecast_df.SARIMA <- rbind(forecast_df.SARIMA, new_row.SARIMA)
    forecast_df.HW <- rbind(forecast_df.HW, new_row.HW)
    forecast_df.state_space <- rbind(forecast_df.state_space, new_row.state_space)
    forecast_df.HW_FT_SARIMA <- rbind(forecast_df.HW_FT_SARIMA, new_row.HW_FT_SARIMA)  
    forecast_df.SS_FT_SARIMAX <- rbind(forecast_df.SS_FT_SARIMAX, new_row.SS_FT_SARIMAX)    
    if(scope!='weekly')forecast_df.SARIMAX <- rbind(forecast_df.SARIMAX, new_row.SARIMAX)
    
    forecast_benchmarks <- list(forecast_df.naive, forecast_df.sNaive,forecast_df.MA, forecast_df.ses, forecast_df.SARIMA, forecast_df.HW, forecast_df.state_space, forecast_df.HW_FT_SARIMA, forecast_df.SARIMAX, forecast_df.naive_prob)
  }
  return(forecast_benchmarks)
}