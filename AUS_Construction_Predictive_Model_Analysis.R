library(fpp3)
library(readxl)

#------ reading data from source ----------------------------------
file_path <- "MBAS921_2026T1_Assessment_2_data.xlsx"
sheet_name <- "Data"

#------ storing data in a tibble ----------------------------------
raw_df <- read_excel(file_path, sheet = sheet_name)
raw_df

#------ storing data as a tsibble ---------------------------------
nsw_ts <- raw_df %>%
  mutate(Month = yearmonth(`Date`)) %>%
  select(Month, NSW) %>%
  as_tsibble(index = Month)
nsw_ts

qld_ts <- raw_df %>%
  mutate(Month = yearmonth(`Date`)) %>%
  select(Month, QLD) %>%
  as_tsibble(index = Month)
qld_ts

#------ plotting components of the data ---------------------------
nsw_comp <- nsw_ts %>%
  model(STL(NSW)) %>%
  components() %>%
  mutate(State = "NSW")

qld_comp <- qld_ts %>%
  model(STL(QLD)) %>%
  components() %>%
  mutate(State = "QLD")

combined_dc <- bind_rows(nsw_comp, qld_comp)

### Comparison chart

# Combine

combined_dc %>%
  pivot_longer(
    cols = c(trend, season_adjust, season_year, remainder),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    component = recode(
      component,
      season_adjust = "Original Series",
      season_year = "Seasonality",
      trend = "Trend",
      remainder = "Remainder"
    )
  ) %>%
  ggplot(aes(x = Month, y = value, colour = component)) +
  geom_line() +
  facet_grid(component ~ State, scales = "free_y") +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "STL Decomposition Comparison: NSW vs QLD",
    x = "Year",
    y = "Value",
    colour = "Component"
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )


###Prediction

#------ point and interval forecast validation -----------------------------

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
#NSW
fc %>%
  filter(.model == "Naive") %>%
  autoplot(nsw_ts) +
  ggtitle("NSW Naïve")

fc %>%
  filter(.model == "SNaive") %>%
  autoplot(nsw_ts) +
  ggtitle("NSW Seasonal Naïve")

fc %>%
  filter(.model == "ETS_ANN") %>%
  autoplot(nsw_ts) +
  ggtitle("NSW ETS_ANN")

fc %>%
  filter(.model == "ETS_AAN") %>%
  autoplot(nsw_ts) +
  ggtitle("NSW ETS_AAN")

fc %>%
  filter(.model == "ETS_AAA") %>%
  autoplot(nsw_ts) +
  ggtitle("NSW ETS_AAA")

fc %>%
  filter(.model == "ETS_AAM") %>%
  autoplot(nsw_ts) +
  ggtitle("NSW ETS_AAM")

fc %>%
  filter(.model == "ETS_AMM") %>%
  autoplot(nsw_ts) +
  ggtitle("NSW AMM")

fc %>%
  filter(.model == "ETS_MMM") %>%
  autoplot(nsw_ts) +
  ggtitle("NSW MMM")

fc %>%
  autoplot(nsw_ts) +
  facet_wrap(~ .model, ncol = 2, nrow = 4, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "NSW Forecasts from All Models",
    x = "Year",
    y = "Value"
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )



#------ calculating the accuracy of the models -------------------
accuracy(fc, nsw_ts)

#QLD 

#------ setting train and test sets -------------------------------

qld_train <- qld_ts %>%
  filter(year(Month) <= 2019)

qld_test <- qld_ts %>%
  filter(year(Month) >= 2020)

qld_train
qld_test


#------ fitting models to the data --------------------------------

fit <- qld_train %>%
  model(
    Naive = NAIVE(QLD),
    SNaive = SNAIVE(QLD),
    ETS_ANN = ETS(QLD ~ error("A") + trend("N") + season("N")),
    ETS_AAN = ETS(QLD ~ error("A") + trend("A") + season("N")),
    ETS_AAA = ETS(QLD ~ error("A") + trend("A") + season("A")),
    ETS_AAM = ETS(QLD ~ error("A") + trend("A") + season("M")),
    ETS_AMM = ETS(QLD ~ error("A") + trend("M") + season("M")),
    ETS_MMM = ETS(QLD ~ error("M") + trend("M") + season("M")),
  )

fit

#------ forecasting the test interval using the fitted models ----

fc <- fit %>%
  forecast(h = "6 years")
fc


#QLD
fc %>%
  filter(.model == "Naive") %>%
  autoplot(qld_ts) +
  ggtitle("QLD Naïve")

fc %>%
  filter(.model == "SNaive") %>%
  autoplot(qld_ts) +
  ggtitle("QLD Seasonal Naïve")

fc %>%
  filter(.model == "ETS_ANN") %>%
  autoplot(qld_ts) +
  ggtitle("QLD ETS_ANN")

fc %>%
  filter(.model == "ETS_AAN") %>%
  autoplot(qld_ts) +
  ggtitle("QLD ETS_AAN")

fc %>%
  filter(.model == "ETS_AAA") %>%
  autoplot(qld_ts) +
  ggtitle("QLD ETS_AAA")

fc %>%
  filter(.model == "ETS_AAM") %>%
  autoplot(qld_ts) +
  ggtitle("QLD ETS_AAM")

fc %>%
  filter(.model == "ETS_AMM") %>%
  autoplot(qld_ts) +
  ggtitle("QLD AMM")

fc %>%
  filter(.model == "ETS_MMM") %>%
  autoplot(qld_ts) +
  ggtitle("QLD MMM")

fc %>%
  autoplot(qld_ts) +
  facet_wrap(~ .model, ncol = 2, nrow = 4, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "QLD Forecasts from All Models",
    x = "Year",
    y = "Value"
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )

#------ calculating the accuracy of the models -------------------
accuracy(fc, qld_ts)

#------ time series cross validation -----------------------------
#Init means which ones are gonna be included in the first part.
nsw_cv <- nsw_ts %>%
  stretch_tsibble(.init = 120, .step = 12)

nswcv_fit <- nsw_cv %>%
  model(
    Naive = NAIVE(NSW),
    SNaive = SNAIVE(NSW),
    ETS_ANN = ETS(NSW ~ error("A") + trend("N") + season("N")),
    ETS_AAN = ETS(NSW ~ error("A") + trend("A") + season("N")),
    ETS_AAA = ETS(NSW ~ error("A") + trend("A") + season("A")),
    #ETS_AAM = ETS(NSW ~ error("A") + trend("A") + season("M")),
    #ETS_AMM = ETS(NSW ~ error("A") + trend("M") + season("M")),
    #ETS_MMM = ETS(NSW ~ error("A") + trend("M") + season("M"))
  )

nswcv_fc <- nswcv_fit %>%
  forecast(h = 12)

accuracy(nswcv_fc, nsw_ts)


#QLD Validation

qld_cv <- qld_ts %>%
  stretch_tsibble(.init = 120, .step = 12)

qldcv_fit <- qld_cv %>%
  model(
    Naive = NAIVE(QLD),
    SNaive = SNAIVE(QLD),
    ETS_ANN = ETS(QLD ~ error("A") + trend("N") + season("N")),
    ETS_AAN = ETS(QLD ~ error("A") + trend("A") + season("N")),
    ETS_AAA = ETS(QLD ~ error("A") + trend("A") + season("A")),
    #ETS_AAM = ETS(QLD ~ error("A") + trend("A") + season("M")),
    #ETS_AMM = ETS(QLD ~ error("A") + trend("M") + season("M")),
    #ETS_MMM = ETS(QLD ~ error("A") + trend("M") + season("M"))
  )

qldcv_fc <- qldcv_fit %>%
  forecast(h = 12)

accuracy(qldcv_fc, qld_ts)
