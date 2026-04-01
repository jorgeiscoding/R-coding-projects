library(fpp3)
library(readxl)

emp_ts <- us_employment %>%
  filter(year(Month) >= 2000, Title == "Retail Trade") |>
  select(-Series_ID)

emp_ts



#------ plotting the data -----------------------------------------
emp_ts %>%
  autoplot(Employed) +
  labs(title = "US retail employment time series")







#------ exploring seasonality -------------------------------------
emp_ts %>%
  gg_season(Employed) +
  labs(title = "Seasonal plot")

emp_ts %>%
  gg_subseries(Employed) +
  labs(title = "Subseries plot")








#------ autocorrelation in the original data ----------------------
emp_ts %>%
  ACF(Employed) %>%
  autoplot() +
  labs(title = "ACF of original series")


emp_ts %>%
  PACF(Employed) %>%
  autoplot() +
  labs(title = "PACF of original series")





#------ differencing ----------------------------------------------
emp_ts %>%
  mutate(diff_emp = difference(Employed)) %>%
  autoplot(diff_emp) +
  labs(title = "First differenced series",
       y = "First difference")

emp_ts %>%
  mutate(season_diff = difference(Employed, lag = 12)) %>%
  autoplot(season_diff) +
  labs(title = "Seasonally differenced series",
       y = "Seasonal difference")

emp_ts %>%
  mutate(double_diff = difference(difference(Employed), lag = 12)) %>%
  autoplot(double_diff) +
  labs(title = "First + seasonal differenced series",
       y = "Double difference")









#------ ACF and PACF after differencing ---------------------------
emp_ts %>%
  mutate(double_diff = difference(difference(Employed), lag = 12)) %>%
  ACF(double_diff) %>%
  autoplot() +
  labs(title = "ACF of differenced series")

emp_ts %>%
  mutate(double_diff = difference(difference(Employed), lag = 12)) %>%
  PACF(double_diff) %>%
  autoplot() +
  labs(title = "PACF of differenced series")







#------ setting train and test sets -------------------------------
emp_train <- emp_ts %>%
  filter(year(Month) <= 2016)

emp_test <- emp_ts %>%
  filter(year(Month) >= 2017)







#------ fitting automatic ARIMA model -----------------------------
auto_arima <- emp_train %>%
  model(ARIMA(Employed))

report(auto_arima)








#------ forecasting -----------------------------------------------
fc <- auto_arima %>%
  forecast(new_data = emp_test)

fc %>%
  autoplot(emp_ts) +
  labs(title = "ARIMA forecasts",
       y = "Retail employment")








#------ forecast accuracy -----------------------------------------
accuracy(fc, emp_test)






#------ residual diagnostics --------------------------------------
aug <- auto_arima %>%
  augment()

autoplot(aug, .innov) +
  labs(title = "Residuals from ARIMA model",
       y = "Residuals")

aug %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "ACF of residuals")

aug %>%
  ggplot(aes(x = .innov)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of residuals",
       x = "Residuals",
       y = "Frequency")

aug %>%
  features(.innov, ljung_box, lag = 24, dof = 0)











#------ comparison with smoothing models ---------------------------
fit <- emp_train %>%
  model(
    Naive = NAIVE(Employed),
    SNaive = SNAIVE(Employed),
    ETS = ETS(Employed),
  )

fc <- fit %>%
  forecast(h = "3 years")

accuracy(fc, emp_ts)

