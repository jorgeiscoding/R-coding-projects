library(fpp3)
library(readxl)

#------ loading the data --------------------------------------
us_employment
print(unique(us_employment["Title"]), n = 100)

emp_ts <- us_employment %>%
  filter(year(Month) >= 2000, Title == "Manufacturing") %>%
  select(-Series_ID)

emp_ts

#------ plotting the data -------------------------------------
emp_ts %>%
  autoplot(Employed) +
  labs(title = "US manufacturing employment",
       y = "Employment")


#------ split into train and test -----------------------------
emp_train <- emp_ts %>%
  filter(year(Month) <= 2017)

emp_test <- emp_ts %>%
  filter(year(Month) >= 2018)


#------ differencing-------------------------------------------

# Original series
emp_train %>%
  autoplot(Employed) +
  labs(title = "Training data: original series",
       y = "Employment")

# First difference
emp_train %>%
  mutate(diff1 = difference(Employed)) %>%
  autoplot(diff1) +
  labs(title = "First differenced series",
       y = "First difference")

# Seasonal difference
emp_train %>%
  mutate(seas_diff = difference(Employed, lag = 12)) %>%
  autoplot(seas_diff) +
  labs(title = "Seasonally differenced series",
       y = "Seasonal difference")

# First + seasonal difference
emp_train %>%
  mutate(double_diff = difference(difference(Employed), lag = 12)) %>%
  autoplot(double_diff) +
  labs(title = "First + seasonal differenced series",
       y = "Double difference")





#- ACF and PACF of the differenced series ---------------------

emp_train %>%
  mutate(double_diff = difference(difference(Employed), lag = 12)) %>%
  ACF(double_diff, lag_max = 24) %>%
  autoplot() +
  labs(title = "ACF of first + seasonal differenced series")

emp_train %>%
  mutate(double_diff = difference(difference(Employed), lag = 12)) %>%
  PACF(double_diff, lag_max = 24) %>%
  autoplot() +
  labs(title = "PACF of first + seasonal differenced series")






#- fitting ARIMA candidates (p,d,q)(P,D,Q) --------------------
# we already know d = 1 and D = 1

manual_fit <- emp_train %>%
  model(
    ARIMA_110_011 = ARIMA(Employed ~ pdq(1,1,0) + PDQ(0,1,1)),
    ARIMA_110_110 = ARIMA(Employed ~ pdq(1,1,0) + PDQ(1,1,0)),
    ARIMA_110_111 = ARIMA(Employed ~ pdq(1,1,0) + PDQ(1,1,1)),
    ARIMA_111_011 = ARIMA(Employed ~ pdq(1,1,1) + PDQ(0,1,1)),
    ARIMA_210_011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    ARIMA_210_111 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(1,1,1)),
    ARIMA_211_011 = ARIMA(Employed ~ pdq(2,1,1) + PDQ(0,1,1)),
    ARIMA_211_111 = ARIMA(Employed ~ pdq(2,1,1) + PDQ(1,1,1)),
  )


# Comparing AIC
glance(manual_fit) %>%
  arrange(AIC)

# residual diagnostics for manual candidates

manual_aug <- manual_fit %>%
  augment()

# Residual plots
manual_aug %>%
  ggplot(aes(x = Month, y = .innov, colour = .model)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(vars(.model), scales = "free_y") +
  labs(title = "Residuals of manual ARIMA candidates",
       y = "Residuals")

# Residual ACF
manual_aug %>%
  ACF(.innov) %>%
  autoplot() +
  facet_wrap(vars(.model), scales = "free_y") +
  labs(title = "Residual ACF for manual ARIMA candidates")

# Ljung-Box test (dof is number of parameters: sum of AR and MA coefficients)

manual_aug %>%
  filter(.model == "ARIMA_110_011") %>%
  features(.innov, ljung_box, lag = 24, dof = 2)

manual_aug %>%
  filter(.model == "ARIMA_110_110") %>%
  features(.innov, ljung_box, lag = 24, dof = 2)

manual_aug %>%
  filter(.model == "ARIMA_110_111") %>%
  features(.innov, ljung_box, lag = 24, dof = 3)

manual_aug %>%
  filter(.model == "ARIMA_111_011") %>%
  features(.innov, ljung_box, lag = 24, dof = 3)

manual_aug %>%
  filter(.model == "ARIMA_210_011") %>%
  features(.innov, ljung_box, lag = 24, dof = 3)

manual_aug %>%
  filter(.model == "ARIMA_210_111") %>%
  features(.innov, ljung_box, lag = 24, dof = 3)

manual_aug %>%
  filter(.model == "ARIMA_211_011") %>%
  features(.innov, ljung_box, lag = 24, dof = 4)

manual_aug %>%
  filter(.model == "ARIMA_211_111") %>%
  features(.innov, ljung_box, lag = 24, dof = 5)







#- fitting automatic ARIMA --------------------------------

auto_fit <- emp_train %>%
  model(
    Auto = ARIMA(Employed)
  )

report(auto_fit)

# forecasting both manual and automatic models

compare_fit <- emp_train %>%
  model(
    Manual = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    Auto   = ARIMA(Employed)
  )

compare_fc <- compare_fit %>%
  forecast(new_data = emp_test)

compare_fc %>%
  autoplot(emp_ts, level = NULL) +
  labs(title = "Manual ARIMA vs Automatic ARIMA forecasts",
       y = "Retail employment")

# comparing forecast accuracy on the test set

accuracy(compare_fc, emp_test)


# compare with benchmark models again

benchmark_fit <- emp_train %>%
  model(
    Manual = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    Auto   = ARIMA(Employed),
    Naive  = NAIVE(Employed),
    SNaive = SNAIVE(Employed),
    ETS    = ETS(Employed)
  )

benchmark_fc <- benchmark_fit %>%
  forecast(new_data = emp_test)

accuracy(benchmark_fc, emp_test)

benchmark_fc %>%
  autoplot(emp_ts, level = NULL) +
  labs(title = "forecasts",
       y = "Manufacturing employment")

