# FPA_Project_Forecast_Wallmart_Sales

Kaggle challenge\
FPA Project to create deterministic and probabilistic forecasting benchmarks. Analysis of different subsidiaries and different product categories along varying time horizons.


## Implementation:
main.R

## Stationarity testing:
check_stationarity.R

- Visual via ACF & PACF
- p-testing via Augmented Dickey-Fuller

## evaluation:
evaluation.R
- naive
- snaive
- Moving Average
- SARIMA
- SARIMAX
- ESX
- state-space with tbats

## Parameter tuning:
tuning.R & prepare_data.R
- TimeSeries conversion to different intervals (Daily-7,Daily-365,Weekly-52)
- Analysis on different train-test splits
- Inclusion of exogenous variables
