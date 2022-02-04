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

# tuning ---------------------------------------------

tune <- function(data_set, horizon, exog_vars_train, scope){
  
  #split(data_set, [number of items in validation set (based on percentage = 25 %)])
  list[train_set, val_set] <- split(data_set, round(length(data_set)*0.25))
  
  #1. naive needs no tuning
  #2. snaive needs no tuning
  
  # 3. ES
  
  # Find the best alpha based on RMSE
  alpha <- seq(.1, .3, by = .05)
  RMSE <- NA
  for(i in seq_along(alpha)) {
    preds <- c()
    for (j in 2:length(val_set)){
      fit <- data.frame(ses(c(train_set, val_set[1:j-1]), alpha = alpha[i], h = 1))$Point.Forecast
      preds <- c(preds, fit)
    }
    preds <- ts(preds, start=start(val_set[2:length(val_set)]), frequency=frequency(val_set))
    RMSE[i] <- data.frame(accuracy(preds, val_set[2:length(val_set)]))$RMSE
  }
  # convert to a data frame and 
  # idenitify min alpha value
  alpha.fit <- data_frame(alpha, RMSE)
  alpha.min <- filter(alpha.fit, 
                      RMSE == min(RMSE))
  
  ses.parameters <- alpha.min[1]
  
  # 4. MA
  orders <- seq(2,5, by=1)
  RMSE <- NA
  for (i in seq_along(orders)){
    forecasts <- MA(train_set, i, length(val_set))
    RMSE[i] <- data.frame(accuracy(forecasts, val_set))$RMSE
  }
  orders.fit <- data_frame(orders, RMSE)
  orders.min <- filter(orders.fit, RMSE == min(RMSE))
  MA.order <- orders.min[1]
  
  # 5. ESX
  # ESX = es(data_set, model="ZZZ", h=horizon, holdout=TRUE, xreg=exog_vars_train)
  
  # 6. SARIMA
  
  #R will not fit an ARIMA model with seasonality greater than 350
  #One approach to overcome this problem is to apply a 
  #Fourier series approach where the seasonal pattern is modelled
  # using Fourier terms with short-term time series dynamics allowed in the error.
  
  # However, in this case seasonality is assumed to be fixed - 
  #the pattern is not allowed to change over time. 
  # The tbats state space model however, will not automatically select the Fourier order
  #but also allows slow seasonality changes over time
  # 
  # the seasonal pattern is smooth for small values of K
  #(but more wiggly seasonality can be handled by increasing K
  
  #with fourier but not working:
  #SARIMA <- auto.arima(train_set, seasonal = FALSE, xreg=fourier(Hobbies, K=1))
  
  
  SARIMA <- auto.arima(train_set, seasonal = TRUE)
  SARIMA.parameters <- arimaorder(SARIMA)
  
  
  
  # 7. SARIMAX
  if(scope!='weekly'){
    SARIMAX <- auto.arima(data_set, seasonal=TRUE, xreg = exog_vars_train)
    SARIMAX.parameters <- arimaorder(SARIMAX)
  }
  else SARIMAX.parameters = 0
  
  
  
  # 8. Holt-Winters
  # HW_model <- holt(train_set, h = horizon, xreg=fourier(train_set, K=4))
  HW_model <- ets(train_set)
  
  # 9. State-space
  # Parameters such as use.box.cox, use.trend, use.damped.trend were left NULL on purpose,
  # model decides based on AIC which combination is the best
  tbats_model = tbats(train_set)
  
  #How to access specific elements in list of vectors
  # tunedParameters <- list(1, c(2,3), c(4,5))
  # print(tunedParameters[[2]][2])
  
  
  tunedParameters <- list(ses.parameters,MA.order, SARIMA.parameters, HW_model, tbats_model,SARIMAX.parameters)
  #VAL_evaluationDF <- evaluate(val_set, train_set, horizon, tunedParameters)
  
  return(tunedParameters)
  
}