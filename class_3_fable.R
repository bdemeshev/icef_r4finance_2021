# Class 3. Times series foresting

library(lubridate) # working with dates
library(fable) # basic time series models (ETS, ARIMA, ...)
library(feasts) # feature extraction (times series test), plots
library(tsibbledata) # time series examples
library(tidyverse) # data manipulations
library(tsibble) # modern time series class


# tsibble data class ----

# many ways to store time series data in R
# the first one: ts class — regular time series
# ... many many time series data formats: zoo, xts, ...
# finally: tsibble (modern)

glimpse(aus_production)
head(aus_production)

# index variable (time)
# data columns (Beer, Tobacco, Bricks)
# key (grouping variable)

glimpse(aus_retail)

# Rob Hyndman Teaching page
# https://robjhyndman.com/teaching/
# Forecasting Principles and Practice,
# https://otexts.com/fpp3/

# you start from ordinary data frame
df = tibble(year = c(2001, 2002, 2003, 2004, 2001, 2002, 2003, 2004),
            x = c(4, 6, 2, 4, 4, 5, 4, 8),
            city = c("Moscow", "Moscow", "Moscow", "Moscow",
                     "SPB", "SPB", "SPB", "SPB"))
df

# we specify roles of columns: index (time), key (regions etc), data columns
df2 = as_tsibble(df, index = year, key = city)
df2
autoplot(df2, x)


# from character to Date class ----

class("2021-01-28")

# yearly data — just write the year as number!
# monthly data

# workhorses: ymd, dmy, mdy, ...
class(ymd("2021-01-28"))
ymd("2021-01-28") + days(4)
dmy("28-01-2021") + days(30)

# 2021-12-01 567
# 2021-11-01 467
# 2021-10-01 343
# 2021-09-01 123

# special classes for monthly and quarterly dates

df = tibble(month = c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01"),
            x = c(5, 6, 3, 2))
df
# month: from character class to Date class
df2 = mutate(df, month = ymd(month))

glimpse(df)
glimpse(df2)

# month: from Date to yearmonth class
df3 = mutate(df2, month = yearmonth(month))
glimpse(df3)

df3bis = mutate(df, month = yearmonth(ymd(month)))
df3bis

dmy("13.09.1999")

dmy("13 09 1999")
dmy("13091999")

?ymd


# high freq data
x = c("2011-12-31 12:59:59", "2010-01-01 12:11", "2010-01-01 12", "2010-01-01")
parse_date_time(x, "Ymd HMS", truncated = 3)

# make some plots ----
glimpse(aus_production)
autoplot(aus_production, Beer)
gg_season(aus_production, Beer)
gg_subseries(aus_production, Beer)
gg_tsdisplay(aus_production, Beer)

# Auto Correlation Function: bottom left
# acf(2) = 0.5
# first interpretation:
# sample correlation (y_t, y_{t-2}) = 0.5
# second interpretation:
# OLS
# hat_y_t = intercept + 0.5 y_{t-2}

ACF(aus_production, Beer)

gg_lag(aus_production, Beer)
# y_t on one axis
# y_{t-k} on the other axis


autoplot(aus_production, Beer) +
ggtitle("Beer production in Australia") +
  ylab("Beer production in megalitres") + xlab("Time")

?aus_production

# what we may extract from time series? ----

ACF(aus_production, Beer) # Auto Correlation Function

PACF(aus_production, Beer) # Partial Auto Correlation Function

PACF(aus_production, Beer) %>% autoplot()
# pacf(3) = 0.573
# correlation interpretation
# correlation between y_t and y_{t-3} cleansed from y_{t-1} and y_{t-2}
# "direct" effect of y_{t-3} on y_t
# regression interpretation
# OLS:
# hat y_t = intercept + hat beta_1 y_{t-1} + hat beta_2 y_{t-2} + 0.573 y_{t-3}

# use KPSS and not DF by default
?unitroot_kpss

unitroot_kpss(aus_production[["Beer"]])
# KPSS test:
# H0: ts is stationary
# Ha: ts is not stationary
# pvalue = 0.01 < 0.05, conclusion is that H0 is rejected


help(package = "feasts")

# forecasting with simple models ----

# Idea 1: Always use simple models for comparison!
# Four trivial models:
# 1. Naive model:
# hat y_{t+1} = y_t
# 2. Seasonal naive model (monthly data):
# hat y_{t+1} = y_{t-11}
# 3. Mean model
# hat y_{t+1} = average of all past observations
# 4. Random walk with drift
# hat y_{t+1} = y_t + hat drift

# Model averaging.
# Sometimes 0.5 * Seasonal Naive + 0.5 * Complex model is better than
# Complex model

glimpse(aus_production)
models = aus_production %>% model(naive_model = NAIVE(Beer),
                                  seas_naive_model = SNAIVE(Beer),
                                  mean_model = MEAN(Beer),
                                  drift_model = RW(Beer ~ drift()))
models
fcsts = forecast(models, h = "2 years")

# point forecasts and distributional forecasts:
fcsts

# predictive intervals
hilo(fcsts, level = 90)

autoplot(fcsts, aus_production)

# ETS models ----

models = aus_production %>% model(ets_aaa = ETS(Beer ~ error("A") + trend("A") + season("A")),
                                  trivial = SNAIVE(Beer))
fcsts = forecast(models, h = "1 year")
fcsts
fcsts %>% hilo(level = 95)

# automatic ETS ----
models = aus_production %>% model(ets_aaa = ETS(Beer),
                                  trivial = SNAIVE(Beer))
models[["ets_aaa"]][[1]]
# Forecasting Principles and Practice:
# https://otexts.com/fpp3/

# model comparison on training sample ----
accuracy(models)

# model comparison using train-test split ----
tail(aus_production, 8)
aus_train = filter(aus_production, Quarter <= yq("2008 Q2"))
aus_train
aus_test = filter(aus_production, Quarter >= yq("2008 Q3"))
aus_test

models = aus_train %>% model(seas_naive = SNAIVE(Beer),
                             ets_aaa = ETS(Beer ~ error("A") + trend("A") + season("A")),
                             ets_auto = ETS(Beer))
models
fcsts = forecast(models, h = "2 years")
fcsts

accuracy(fcsts, aus_test)

# automatic variable transformations ----

# from y_t go to log(y_t)
# model log(y_t) as ETS()
# make predictions
# go back from forecast of log(y_t) to forecast of y_t

models = aus_train %>% model(seas_naive = SNAIVE(Beer),
                             ets_aaa = ETS(Beer ~ error("A") + trend("A") + season("A")),
                             ets_auto = ETS(Beer),
                             ets_aaa_ln = ETS(log(Beer) ~ error("A") + trend("A") + season("A")))
models
fcsts = forecast(models, h = "2 years")
fcsts

accuracy(fcsts, aus_test)

?accuracy
?MPE
# Look in Forecasting Principles and Practice for acronyms

# model averaging ----

models = aus_train %>% model(seas_naive = SNAIVE(Beer),
                             ets_aaa = ETS(Beer ~ error("A") + trend("A") + season("A")),
                             ets_auto = ETS(Beer),
                             ets_aaa_ln = ETS(log(Beer) ~ error("A") + trend("A") + season("A")))
models_more = mutate(models, hybrid = (ets_aaa_ln + ets_aaa + ets_auto) / 3)
fcsts = forecast(models_more, h = "2 years")
fcsts

accuracy(fcsts, aus_test)

# sometimes the average of two models is better than each model (!)

# model comparison using cross-validation ----
# you are interested in one step ahead forecast
# Q. why using train-test split comparison is not a good idea?
# A. the problem is that the variance of MAE (or other forecasting metrics) will be big
# slide or stretch many train + one observation test split

# create all the training samples:
aus_production

aus_prod_stretched = stretch_tsibble(aus_production, .init = 10, .step = 1)
head(aus_prod_stretched, 20)
# 209 training splits

models = aus_prod_stretched %>% model(seas_naive = SNAIVE(Beer),
                             ets_aaa = ETS(Beer ~ error("A") + trend("A") + season("A")),
                             ets_aaa_ln = ETS(log(Beer) ~ error("A") + trend("A") + season("A")))
# 209 * 3 = 627 models estimated
models

fcsts = forecast(models, h = 1)

accuracy(fcsts, aus_production)

