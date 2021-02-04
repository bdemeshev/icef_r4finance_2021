# Class 4. Times series foresting

library(lubridate) # working with dates
library(fable) # basic time series models (ETS, ARIMA, ...)
library(feasts) # feature extraction (times series test), plots
library(tsibbledata) # time series examples
library(tidyverse) # data manipulations
library(tsibble) # modern time series class

glimpse(aus_production)

gg_tsdisplay(aus_production, Beer)

tail(aus_production)

# train-test split approach
aus_train = filter(aus_production,
                   Quarter <= ymd("2008-06-01"))

aus_test = filter(aus_production,
                   Quarter > ymd("2008-06-01"))
aus_test
aus_train

# estimate a pool of models
models = model(aus_train,
               snaive = SNAIVE(Beer),
               ets_aaa = ETS(Beer ~ error("A") + trend("A") + season("A")),
               sarima_111_100 = ARIMA(Beer ~ pdq(1, 1, 1) + PDQ(1, 0, 0)),
               sarima_aut = ARIMA(Beer),
               log_ets_aaa = ETS(log(Beer) ~ error("A") + trend("A") + season("A")),
               log_sarima_111_100 = ARIMA(log(Beer) ~ pdq(1, 1, 1) + PDQ(1, 0, 0)))

models

models[['sarima_aut']][[1]]

fcst = forecast(models, h = "2 years")
fcst

fcst %>% filter(.model == "sarima_111_100")

accuracy(fcst, aus_production)

# 2020
# old tradition — one model to estimate (forecast)
# new tradition — many models to estimate: fable, modeltime, forecastML

# https://otexts.com/fpp3/
# https://cran.r-project.org/web/packages/forecastML/
# https://cran.r-project.org/web/packages/modeltime/

