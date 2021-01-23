# R for Finance. Class 2.

# More data wrangling
# ETS models for TS

library(tidyverse) # data manipulations, plots...
library(rio) # input-output

d = diamonds
glimpse(d)

# grouping data by a factor variable ----
# functions: vector of numbers -> a scalar
v = c(5, 6, -2)
sum(v)
mean(v)
sd(v)
max(v)

# functions: vector -> vector
cos(v)

# summarize
# pipe style syntax
short_summ = d %>% summarize(mean_price = mean(price), std_price = sd(price), max_carat = max(carat))
# Euler style syntax
short_summ = summarize(d, mean_price = mean(price), std_price = sd(price), max_carat = max(carat))

short_summ


# special command group_by()
# calculates nothing
short_summ = d %>% group_by(cut) %>% summarize(mean_price = mean(price), std_price = sd(price), max_carat = max(carat))
short_summ

head(d, 7)

short_summ = d %>% group_by(cut, color) %>% summarize(mean_price = mean(price), std_price = sd(price), max_carat = max(carat))
short_summ

d
d %>% group_by(cut, color)


# usually we put group_by before summarize or before mutate
# let's calculate deviations from group means
d2 = d %>% group_by(cut) %>% mutate(dev_price = price - mean(price), mean_price = mean(price))
head(d2, 10)

glimpse(d2)

# dummy variables ----
# removed order information from cut variable
d3 = d %>% mutate(cut2 = factor(cut, ordered = FALSE))
glimpse(d3)

# lm command will introduce dummy variables automatically
model = lm(data = d3, price ~ carat + cut2)
summary(model)

# how to change base level ----
d3bis = d %>% mutate(cut2 = relevel(factor(cut, ordered = FALSE), "Good"))
model_bis = lm(data = d3bis, price ~ carat + cut2)
summary(model_bis)


# we may introduce dummy variables manually
d4 = diamonds %>% mutate(good = case_when(cut == "Good" ~ 1,
                                          TRUE ~ 0))
head(d4)
model = lm(data = d4, price ~ carat + good)
summary(model)

# case_when one more example
d5 = diamonds %>% mutate(cut_uni =
              case_when(cut %in% c("Ideal", "Premium") ~ "ide_prem",
                        TRUE ~ as.character(cut)))

head(d5)


# pivot operations ----

short_summ = d %>% group_by(cut, color) %>% summarize(mean_price = mean(price), std_price = sd(price), max_carat = max(carat))
short_summ

# I would like to have
# cut as a column names
# color as a row names
# mean_price in table
?pivot_wider()

short_summ_selection = select(short_summ, color, cut, mean_price)
short_summ_selection
wide_repr = pivot_wider(short_summ_selection,
            names_from = cut, values_from = mean_price)
wide_repr

pivot_longer(wide_repr, cols = Fair:Ideal,
             values_to = "mean_price",
             names_to = "cut")
?pivot_longer

# let's play in a toy example from ts ----
toy = tibble(year = c(2020, 2021), jan = c(45, 67), feb = c(23, 67), march = c(21, 34))
toy

pivot_longer(toy, cols = jan:march, values_to = "gdp", names_to = "month")

# factor variable -> dummies

# group_by operations
# long table <--> wide table
# join operations

# join operations ----

d = diamonds
second_df = tibble(cut = unique(d$cut))
second_df = mutate(second_df, var = c("I like it", "I don't like it", "soso", "I don't like it", "I don't like it"))
second_df

res = left_join(d, second_df, by = "cut")
# inner_join()
# full_join()
# anti_join()

res
