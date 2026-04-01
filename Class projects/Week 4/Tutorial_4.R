library(fpp3)
library(readxl)

#------ reading data from source ----------------------------------
file_path <- "Week 2/Tutorial_4.xlsx"
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


#------ plotting components of the data ---------------------------
nsw_dc <- nsw_ts %>%
  model(STL(NSW)) %>%
  components() %>%
  autoplot()
nsw_dc


#------ plotting components of the data after log transformation --
nsw_dc <- nsw_ts %>%
  model(STL(log(NSW))) %>%
  components() %>%
  autoplot()
nsw_dc


#------ filtering the data ----------------------------------------
nsw_ts <- nsw_ts %>%
  filter(Month >= yearmonth("2015 Jan"))


#------ fit a naive forecasting method to the data ----------------
fc_naive <- nsw_ts %>%
  model(NAIVE(NSW)) %>%
  forecast(h = 24)            # we can also write h = "2 years"

autoplot(nsw_ts) +
  autolayer(fc_naive, level = NULL)


#------ fit a seasonal naive forecasting method to the data -------
fc_snaive <- nsw_ts %>%
  model(SNAIVE(NSW)) %>%
  forecast(h = 24)

autoplot(nsw_ts) +
  autolayer(fc_snaive, level = NULL)


#------ fit a simple exponential smoothing method to the data -----
fc_ses <- nsw_ts %>%
  model(ETS(NSW ~ error("A") + trend("N") + season("N"))) %>%
  forecast(h = 24)

autoplot(nsw_ts) +
  autolayer(fc_ses, level = NULL)


#------ fit a trend-adjusted exponential smoothing method (Holt's) -
fc_holt <- nsw_ts %>%
  model(ETS(NSW ~ error("A") + trend("A") + season("N"))) %>%
  forecast(h = 24)

autoplot(nsw_ts) +
  autolayer(fc_holt, level = NULL)


#------ fit a seasonal exponential smoothing method (Holt Winters')-
fc_hw_add <- nsw_ts %>%
  model(ETS(NSW ~ error("A") + trend("A") + season("A"))) %>%
  forecast(h = 24)

autoplot(nsw_ts) +
  autolayer(fc_hw_add, level = NULL)


#------ fit a multiplicative Holt-Winters' method ------------------
fc_hw_mul <- nsw_ts %>%
  model(ETS(NSW ~ error("A") + trend("A") + season("M"))) %>%
  forecast(h = 24)

autoplot(nsw_ts) +
  autolayer(fc_hw_mul, level = NULL)
