# Loading Library 
library(readxl)
library(readr)
library(tidyverse)
library(zoo)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)

-------------------------------------------------------------------------------

# Section 1: Data Collection and Preprocessing

# Importing and data cleaning for Gold Demand dataset
gold_demand <- read_xlsx("Gold_demand.xlsx", 
                         sheet = "Consumer", skip = 4)
gold_demand <- gold_demand[,-c(2:20)]
gold_demand <- gold_demand %>% 
  rename(Country = ...1)

golddemand_us <- gold_demand %>%
  filter(Country == "United States") %>%
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "Quarter",
    values_to = "Gold_Demand_Tonnes"
  ) %>%
  mutate(
    Quarter = gsub("'", " 20", Quarter),
    Quarter = as.yearqtr(Quarter, format = "Q%q %Y")
  ) %>%
  select(Quarter, Gold_Demand_Tonnes)

view(golddemand_us)

# Importing and data cleaning for Gold Price Dataset
gold_price <- read_xlsx("Gold_prices.xlsx",
                        sheet = "Quarterly_Avg", skip = 3)
gold_price <- gold_price %>%
  rename(Quarter = ...1) %>% 
  slice(-(1:125)) %>%
  mutate(Quarter = as.yearqtr(as.Date(Quarter))) %>% 
  select(Quarter, USD)

# Importing and Data Cleaning for US Inflation Dataset
us_inflation <- read_xls("us_inflation.xls", skip = 2)
us_inflation <- us_inflation %>% 
  rename(Country = `Country Name`) %>%
  filter(Country == "United States") %>%
  pivot_longer(
    cols = `1960`:`2024`,
    names_to = "Year",
    values_to = "Inflation_Rate"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2010) %>% 
  select(-c(2,3,4))

us_inflation <- us_inflation %>% 
  mutate(Quarter = as.yearqtr(Year)) %>%
  complete(Quarter = seq(min(Quarter), max(Quarter), by = 0.25)) %>% 
  fill(Inflation_Rate, .direction = "down") %>% 
  select(-c(2,3))

# Importing and Data Cleaning for Interest Rates
interest_rates <- read.csv("interest_rates.csv")
interest_rates <- interest_rates %>%
  rename(Date = observation_date) %>%
  slice(-(1:666)) %>%
  mutate(Date = mdy(Date)) %>%
  filter(month(Date) %in% c(3,6,9,12)) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  select(-c(1)) %>%
  rename(interest_rate = FEDFUNDS)

# Importing and Data Cleaning of USD index Dataset
usd_index <- read_csv("usd_index.csv", show_col_types = FALSE)
usd_index <- usd_index %>%
  mutate(Date = as.Date(observation_date, format = "%m/%d/%y")) %>%
  filter(Date >= as.Date("2010-01-01")) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  group_by(Quarter) %>%
  summarise(USD_Index = mean(DEXUSEU, na.rm = TRUE)) %>% 
  arrange(Quarter)


# Checking for NA values 
sum(is.na(golddemand_us))
sum(is.na(golddemand_us))
sum(is.na(gold_price))
sum(is.na(us_inflation))
sum(is.na(usd_index))
sum(is.na(interest_rates))

# Checking for outliers 
tsoutliers(golddemand_us$Gold_Demand_Tonnes)
tsoutliers(gold_price$USD)
tsoutliers(us_inflation$Inflation_Rate)
tsoutliers(usd_index$USD_Index)
tsoutliers(interest_rates$interest_rates)

# Data Merging 
data <- list(
  golddemand_us,
  gold_price,
  us_inflation, 
  usd_index, 
  interest_rates
) %>%
  reduce(full_join, by = "Quarter") %>%
  arrange(Quarter)

view(data)

data_clean <- data %>% 
  arrange(Quarter) %>%
  mutate(across(
    where(is.numeric), 
    ~na.interp(ts(.x, frequency = 4))
  ))

golddemand_us <- ts(golddemand_us$Gold_Demand_Tonnes, start = c(2010,1), frequency = 4)
gold_pricets <- ts(gold_price$USD, start = c(2010,1), frequency = 4)
us_inflationts <- ts(us_inflation$Inflation_Rate, start = c(2010,1), frequency = 4)
interes_ratests <- ts(interest_rates$interest_rate, start = c(2010,1), frequency = 4)
data_ts <- ts(data_clean, start = c(2010,1), frequency = 4)

# Checking Stationarity - Gold Demand 
autoplot(golddemand_us)

lambda_gdus <- BoxCox.lambda(golddemand_us)
ndiffs(golddemand_us)
nsdiffs(golddemand_us)
adf.test(golddemand_us)
golddemand_us %>% diff() %>% diff(lag = 4) %>% ggtsdisplay()

# Checking Stationarity - Gold Prices 
autoplot(gold_pricets)

lambda_gp <- BoxCox.lambda(gold_pricets)
ndiffs(gold_pricets)
nsdiffs(gold_pricets)
adf.test(gold_pricets)

gold_pricets %>% diff() %>% diff() %>% checkresiduals()

adf.test(diff(diff(gold_pricets)))

# Checking Stationarity - Inflation 
autoplot(us_inflationts)

lambda_if <- BoxCox.lambda(us_inflationts)
ndiffs(us_inflationts)
nsdiffs(us_inflationts)
adf.test(us_inflationts)

us_inflationts %>% diff() %>% diff() %>% ggtsdisplay()
adf.test(diff(diff(us_inflationts)))

# Checking Stationarity - Interest Rate
autoplot(interes_ratests)

lambda_ir <- BoxCox.lambda(interes_ratests)
ndiffs(interes_ratests)
nsdiffs(interes_ratests)
adf.test(interes_ratests)       

interes_ratests %>% diff() %>% diff() %>% checkresiduals()
adf.test(diff(diff(interes_ratests)))

# Observation for any seasonal patters or structural breaks 
decomp <- stl(golddemand_us, s.window = "periodic")
autoplot(decomp)

# ------------------------------------------------------------------------------

# Section 2: Regression with ARIMA Errors
fitmr <- tslm(
  Gold_Demand_Tonnes ~ USD + Inflation_Rate + USD_Index + interest_rate,
  data = data_ts
)

summary(fitmr)

checkresiduals(fitmr)

coefficients(fitmr)  

# ARIMA error model
fit <- auto.arima(data_clean$Gold_Demand_Tonnes,
                  xreg = cbind(
                    data_clean$USD,
                    data_clean$Inflation_Rate,
                    data_clean$USD_Index,
                    data_clean$interest_rate
                  ))  

summary(fit)
ggtsdisplay(residuals(fit, type = 'response'), 
            main = "ARIMA errors")
checkresiduals(fit, test = FALSE)
checkresiduals(fit, plot = FALSE)

 # Visualised forecast with the ARIMA error model 
fcast <- forecast(fit, xreg = cbind(
  USD = rep(mean(data_clean$USD), 12),
  Inflation_Rate = rep(mean(data_clean$Inflation_Rate), 12),
  USD_Index = rep(mean(data_clean$USD_Index), 12),
  interest_rate = rep(mean(data_clean$interest_rate), 12)
))

autoplot(fcast) + 
  xlab("Year") + 
  ylab("Change in Gold Demand (in Tonnes)") + 
  ggtitle("Forecast with ARIMA(2,0,0)(2,1,1)")

#------------------------------------------------------------------------

# Section 3: Advanced Forecasting Method

# Part A: Forecast Combination (Ensemble)
y <- data_ts[, "Gold_Demand_Tonnes"]
train_set <- window(y, end = c(2019,4))
test_set <- window(y, start = c(2020,1))
h <- length(test_set)

ETS <- forecast(ets(train_set), h = h)
ARIMA <- forecast(auto.arima(train_set, lambda = 0), h = h)
STLF <- stlf(train_set, lambda = 0, h = h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + STLF[["mean"]])/3

# 
accuracy(ETS)
accuracy(ARIMA)
accuracy(STLF)
accuracy(Combination, test_set)

# visualisation 
autoplot(y) + 
  autolayer(ETS, series = "ETS", PI = FALSE) + 
  autolayer(ARIMA, series)


