library(fpp3)
library(readxl)

#------ reading data from source ----------------------------------
file_path <- "Week 5/Tutorial_5.xlsx"
sheet_name <- "Data"

#------ storing data in a tibble ----------------------------------
raw_df <- read_excel(file_path, sheet = sheet_name)
raw_df

#------ storing data as a tsibble ---------------------------------
nsw_ts <- raw_df %>%
  mutate(Month = yearmonth(`Month-year`)) %>%
  select(Month, NSW) %>%
  as_tsibble(index = Month)
nsw_ts

#------ setting train and test sets -------------------------------

nsw_train <- nsw_ts %>%
  filter(year(Month) <= 2019)

nsw_test <- nsw_ts %>%
  filter(year(Month) >= 2020)

nsw_train
nsw_test


#------ fitting models to the data --------------------------------

fit <- nsw_train %>%
  model(
    Naive = NAIVE(NSW),
    SNaive = SNAIVE(NSW),
    ETS_ANN = ETS(NSW ~ error("A") + trend("N") + season("N")),
    ETS_AAN = ETS(NSW ~ error("A") + trend("A") + season("N")),
    ETS_AAA = ETS(NSW ~ error("A") + trend("A") + season("A")),
    ETS_AAM = ETS(NSW ~ error("A") + trend("A") + season("M")),
    ETS_AMM = ETS(NSW ~ error("A") + trend("M") + season("M")),
    ETS_MMM = ETS(NSW ~ error("M") + trend("M") + season("M")),
  )

fit

#------ forecasting the test interval using the fitted models ----

fc <- fit %>%
  forecast(h = "6 years")
fc


#------ plotting models against actual the data -------------------

fc %>%
  filter(.model == "Naive") %>%
    autoplot(nsw_ts) +
    ggtitle("Naïve")

fc %>%
  filter(.model == "SNaive") %>%
    autoplot(nsw_ts) +
    ggtitle("Seasonal Naïve")

fc %>%
  filter(.model == "ETS_ANN") %>%
    autoplot(nsw_ts) +
    ggtitle("ETS_ANN")

fc %>%
  filter(.model == "ETS_AAN") %>%
    autoplot(nsw_ts) +
    ggtitle("ETS_AAN")

fc %>%
  filter(.model == "ETS_AAA") %>%
    autoplot(nsw_ts) +
    ggtitle("ETS_AAA")

fc %>%
  filter(.model == "ETS_AAM") %>%
    autoplot(nsw_ts) +
    ggtitle("ETS_AAM")

fc %>%
  filter(.model == "ETS_AMM") %>%
    autoplot(nsw_ts) +
    ggtitle("AMM")

fc %>%
  filter(.model == "ETS_MMM") %>%
    autoplot(nsw_ts) +
    ggtitle("MMM")

#------ calculating the accuracy of the models -------------------
accuracy(fc, nsw_ts)

#------ time series cross validation -----------------------------
#Init means which ones are gonna be included in the first part.
nsw_cv <- nsw_ts %>%
  stretch_tsibble(.init = 120, .step = 12)

cv_fit <- nsw_cv %>%
  model(
    Naive = NAIVE(NSW),
    SNaive = SNAIVE(NSW),
    ETS_ANN = ETS(NSW ~ error("A") + trend("N") + season("N")),
    ETS_AAN = ETS(NSW ~ error("A") + trend("A") + season("N")),
    ETS_AAA = ETS(NSW ~ error("A") + trend("A") + season("A")),
  )

#we should also MMM

cv_fc <- cv_fit %>%
  forecast(h = 12)

accuracy(cv_fc, nsw_ts)


