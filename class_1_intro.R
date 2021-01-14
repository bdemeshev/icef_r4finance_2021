# attach package ----
library(tidyverse) # data manipulation, plotting...
library(rio) # input/output
library(fable) # time series

# check the name of the package
# install missing package: tools - install packages - install

# to execute line or selection of lines you hit ctrl + enter

# code style ----
# a space after every comma and not before (!)
# space after and before arithmetics operations
x=5+y  # bad style
x = 5 + y # HMQ tyle

x = 5 # my current style
x <- 5 # my old style, google style guide
5 -> x
# <- once upon a time it was one special key on S computer

# input-output operations ----
d = cars
d
glimpse(d)
head(d)
tail(d)
d[3:7, ]
d[, c("speed", "dist")]

export(d, "cars_data.dta")
export(d, "cars_data.xlsx")
export(d, "cars_data.csv") # the most simple (!) use csv

help("export")

getwd() # check current folder
setwd("/home/boris/Documents/icef_r4finance_2021/") # set working folder
export(d, "cars_data.csv") # the most simple (!) use csv

new_data = import("cars_data.csv")
new_data

# data transformations ----

d = diamonds
glimpse(d)

# for bigger data sets you should look at data.table package
# sql

help("diamonds")

# create a new variables ----
d2 = mutate(d, log_weight = log(carat), carat2 = carat ^ 2)
glimpse(d2)

# filter observations ----

d3 = filter(d, cut == "Ideal", carat > median(carat))
glimpse(d3)

# selecting columns ----
d4 = select(d, carat, price, x, y, z) # my style
glimpse(d4)


# warning (!)
# the word "select" is too popular, there are many "select" functions :)
d4 = dplyr::select(d, carat, price, x, y, z) # my style
help("select")

d4bis = d[, c("carat", "price", "x", "y", "z")] # another way
glimpse(d4bis)

# go from 2d to 1d and back ----
d4[7, "carat"] # d4 is a 2d data table

weight_vector = pull(d4, carat) # weight_vector has 1 dimension
weight_vector[7]

weight_vector_bis = d4[["carat"]] # another approach to go from 2d to 1d

a = c(5, 6, 7, -8, NA) # vector
b = c(-2, 7, 4.5, NA, 9) # vector
my_small_dataset = tibble(price = a, quantity = b)
my_small_dataset # 2d dataset
glimpse(my_small_dataset)

# other types of data ----
# tables, vectors are the most popular
# lists
my_list = list(7, c(7, 8, NA), cars, cos)
my_list[[1]] * 9
mean(my_list[[2]])
mean(my_list[[2]], na.rm = TRUE)
glimpse(my_list[[3]])
my_list[[4]](pi) # cos(pi)
# model estimation results are usually lists
# data tables are lists of standard type
str(cars)
cars[[1]]

write_rds(my_list, "my_list.Rds") # to save a list
getwd()
# setwd("~/Documents/azbuka/R/") # use tab to save time :)


new_list = read_rds("my_list.Rds") # to read a list
new_list[[4]](0) # forth element is just a cosine function

# natural order of operations ----
# invention by Leonard Euler and Alexis Clairot:
# f(x)
# we are used to this:
# cos(sin(0)) # we start operations from RIGHT to LEFT

# that would be more intuitive:
# 0.sin.cos

# channels, methods, pipes...
# pipe = %>%
0 %>% sin() %>% cos() # natural
cos(sin(0))

# combo of actions:
# take diamonds data set
# 1. create log of price
# 2. sort according to log of price
# 3. filter x > mean(x)
# 4. filter top 5 observations wrt to price

# Euler-style way
res = top_n(filter(arrange(mutate(diamonds,
                            log_price = log(price)), log_price),
                   x > mean(x)), n = 5, price)
# Natural order of operations
res = diamonds %>% mutate(log_price = log(price)) %>%
  arrange(log_price) %>%
  filter(x > mean(x)) %>%
  top_n(n = 5, price)
res

# operation by line
d2 = mutate(diamonds, log_price = log(price))
d3 = arrange(d2, log_price)
d4 = filter(d3, x > mean(x))
res = top_n(d4, n = 5, price)


# multiple regression ----

library(estimatr)

# heteroscedasticity robust regression
model_a = lm_robust(data = diamonds, formula = price ~ carat + x + y + z)
# model:
# price_i = beta_1 + beta_c carat_i + beta_x x_i + beta_y y_i + beta_z z_i + u_i
summary_a = summary(model_a)
# H0: beta_z = 0
# Ha: beta_z <> 0
# at 1% significance level
beta_hat_z = coef(model_a)["z"]
z_p_value = 0.2282
z_p_value > 0.01
# H0 is not rejected, beta_hat_z is NOT significant
z_p_value = model_a[["p.value"]][5]
z_p_value = model_a[["p.value"]][["z"]]
z_p_value
str(z_p_value)

model_b = lm_robust(data = diamonds, formula = price ~ carat)
# model_b is Restricted model
# model_a is Unrestricted model
library(lmtest) # many tests for cross-sectional econometrics models
waldtest(model_b, model_a) # Wald test
# H0: both models are TRUE (Restricted is TRUE, Unrestricted is TRUE)
# Ha: only Unrestricted model is TRUE, Restricted is FALSE
# p_value < 2.2 * 10^(-16)
# conclusion: H0 is rejected
# we prefer Unrestricted model

# predictions

new_data = tibble(carat = c(5, 0.10), x = c(2, 1), y = c(3, 2), z = c(4, 2))
new_data

predictions = predict(model_a, newdata = new_data) # point forecast
predictions = predict(model_a, newdata = new_data, interval = "prediction") # interval forecast

help("predict.lm")

residuals = diamonds[["price"]] - model_a[["fitted.values"]]
# for some models "resid()" will work

predictions

# summary statistics ----
summary(diamonds) # built-in, it's ugly :(

library(skimr)
skim(diamonds) # that's much better (!)

res = group_by(diamonds, color) %>% skim()
glimpse(res)

export(res, "summary.csv")


# plots ----

# two principles
# 1. the plot is self-explanatory
# 2. it has no junk (remove 3d, unused color, etc)
# A good plot is like 1 or 2 pages of text, it takes time (!)

base_plot = qplot(data = cars, x = speed, y = dist) # fast

base_plot + labs(x = "Speed of a car in miles per hour",
  y = "Stopping distance in feet", title = "Speed of a car vs stopping distnance in 1920s")



