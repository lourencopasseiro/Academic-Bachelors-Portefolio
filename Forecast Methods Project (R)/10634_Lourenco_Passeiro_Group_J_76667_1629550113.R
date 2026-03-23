library(fpp3)
library(fma)
library(wooldridge)
library(tidyverse)
library(readr)
library(urca)

vehicle_sales <- read_csv("2o ano 2oSEM/vehicle_sales.csv")


# Let's get our index of the timeseries
vehicle_sales$Date <- as.Date(paste(vehicle_sales$Year, vehicle_sales$Month, "01", sep = "-"), format = "%Y-%b-%d")

# Passing it to a tsibble
vehicle_sales <- 
  vehicle_sales %>% 
  mutate(date = yearmonth(Date)) %>% 
  select('date', 'New', 'Used', 'Total Sales New', 'Total Sales Used') %>%
  tsibble(index=date)


# Checking our dependent variable
vehicle_sales %>%
  autoplot(`Total Sales New`)


# The variances are not constant then we need to do a transformation in the variables
# Squared-root

vehicle_sales %>%
  autoplot(sqrt(`Total Sales New`))

# Is it stationary?
# Still no

# While checking our dependent variables we were able to see that they were not stationary, even though the variance,
# with the squared transformations, remained fairly constant, we could see a trend in the total sales of new vehicles,
# then let's find an order of differentiation in order to obtain stationary data.

vehicle_sales %>%
  autoplot(sqrt(`Total Sales New`) %>% difference(lag = 12) %>% difference()) # difference(lag = 12) to remove the seasonality of the data since we are working with monthly data

# Now we can see stationary


# Checking ACF and PACF
vehicle_sales %>% 
  gg_tsdisplay(difference(difference(sqrt(`Total Sales New`), 12), 1), plot_type='partial', lag_max=36) 


# Seasonal MA(1) model, even though there are various lags significantly different from zero 
# we can see that the lag=12, is where there are a higher differentiation, and that 
# its neighbors are the result of that differentiation. By analyzing the rest of the lags 
# we could say that lag=2 or lag=7/lag=9 could also be took into account in the model selection. 

# Selecting the candidate model 

summary(ur.df(na.omit(difference(difference(sqrt(vehicle_sales$`Total Sales New`), 12),1)), 
              type='none', selectlags='AIC'))

# (H0: B1 = 0; H1: B1 != 0) We can reject H0, since our t value is smaller than all of the critical values of each confidence interval percentage 
# This means that we do not need to make more differences


# Let's fit all of the possible models
fit1 <- vehicle_sales %>%
  model(
    arima011 = ARIMA(sqrt(`Total Sales New`) ~ pdq(0, 1, 1) + PDQ(0,1,2)), 
    arima012 = ARIMA(sqrt(`Total Sales New`) ~ pdq(0, 1, 2) + PDQ(0,1,2)),
    arima212 = ARIMA(sqrt(`Total Sales New`) ~ pdq(2, 1, 2) + PDQ(0,1,2)),
    arima112 = ARIMA(sqrt(`Total Sales New`) ~ pdq(1, 1, 2) + PDQ(0,1,2)),
    arima111 = ARIMA(sqrt(`Total Sales New`) ~ pdq(1, 1, 1) + PDQ(0,1,2)),
    arima211 = ARIMA(sqrt(`Total Sales New`) ~ pdq(2, 1, 1) + PDQ(0,1,2)),
    auto = ARIMA(sqrt(`Total Sales New`))
  ) 

# Check the best models:
fit1 %>%
  glance() %>%
  arrange(AICc) %>%
  select(.model:BIC)


# By looking to the results we can say that the best models are:
# arima212, arima112 and arima012, being arima212 the best one

fit1 %>% 
  select(arima212) %>% 
  report()


# Checking the residuals
fit1 %>% 
  select(arima212) %>% 
  gg_tsresiduals(lag_max=36)
fit1 %>% 
  select(arima112) %>% 
  gg_tsresiduals(lag_max=36)
fit1 %>% 
  select(arima012) %>% 
  gg_tsresiduals(lag_max=36)


# Ljung-box statistic test
fit1 %>%
  select(arima212) %>% 
  augment() %>%
  features(.innov, ljung_box, lag=12, dof = 6) 
fit1 %>%
  select(arima112) %>% 
  augment() %>%
  features(.innov, ljung_box, lag=12, dof = 5)
fit1 %>%
  select(arima012) %>% 
  augment() %>%
  features(.innov, ljung_box, lag=12, dof = 4)


# Since the p-value of all of the models is less than the commonly used significance level of 0.05,
# we reject the null hypothesis. This suggests that there is significant autocorrelation 
# in the residuals of the models at a lag of 12.

# After several different testings on the transformations of the data, and after looking
# to all of the possibilities of a SARIMA model, we can say that the selected model 
# still exhibits certain limitations. While we were looking to the ACF of the residuals, 
# it was noted that certain lags retained significant different from zero. 
# This suggests that our model may not fully capture all the underlying patterns present in the data,
# despite exhaustive efforts to optimize the model parameters. 


# Forecasting with the three models

fit1 %>% 
  select(arima212) %>% 
  forecast(h = 12) %>% 
  autoplot(vehicle_sales)
fit1 %>% 
  select(arima112) %>% 
  forecast(h = 12) %>% 
  autoplot(vehicle_sales)
fit1 %>% 
  select(arima012) %>% 
  forecast(h = 12) %>% 
  autoplot(vehicle_sales)

# Accuracy

# Divide the dataset between train(2002-2021) and test(2022-2023)
vehicle_sales_train <- vehicle_sales[1:(nrow(vehicle_sales) - 24), ]
vehicle_sales_test <- vehicle_sales[(nrow(vehicle_sales) - 23):nrow(vehicle_sales), ]


# Visualizing the accuracy of the models
vehicle_sales_fit <- vehicle_sales_train %>%
  model(arima212 = ARIMA(sqrt(`Total Sales New`) ~ pdq(2, 1, 2) + PDQ(0,1,2)),
        arima112 = ARIMA(sqrt(`Total Sales New`) ~ pdq(1, 1, 2) + PDQ(0,1,2)),
        arima012 = ARIMA(sqrt(`Total Sales New`) ~ pdq(0, 1, 2) + PDQ(0,1,2))
        )

vehicle_sales_fc <- vehicle_sales_fit %>% 
  forecast(vehicle_sales_test)

vehicle_sales_fc %>%
  autoplot(bind_rows(vehicle_sales_train, vehicle_sales_test), level = NULL) +
  labs(y = '', title = 'New Vehicle Sales') +
  guides(colour = guide_legend(title = "Forecast"))

# Comparing the accuracy measures
accuracy(vehicle_sales_fc, vehicle_sales)

# We reach the conclusion that the best model is ARIMA(2,1,2)(0,1,2)[12]
