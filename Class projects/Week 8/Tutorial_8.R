library(fpp3)
library(readxl)
library(GGally)

#------ reading data from source ----------------------------------
us_change

us_change %>%
  pivot_longer(c(Consumption, Income), names_to="Series") %>%
  autoplot(value) + 
  labs(y = "% change")

#------ plotting the data -----------------------------------------
us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point()

#------ adding a linear regression model --------------------------
us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#------ modelling the linear relationship -------------------------
simple_model <- us_change %>%
  model(TSLM(Consumption ~ Income))

report(simple_model)

#------ plotting all variables ------------------------------------
us_change %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")

#------ scatter plots for all combinations -------------------------
us_change %>%
  GGally::ggpairs(columns = 2:6)

#------ modelling a multivariate linear relationship ---------------
fitted_model <- us_change %>%
  model(TSLM(Consumption ~ Income + Production +
               Savings + Unemployment))
  
report(fitted_model)

#------ plotting actual and fitted values --------------------------
augment(fitted_model) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure"
  ) 

#------ plotting actual vs fitted values ---------------------------
augment(fitted_model) %>%
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  )

#------ verifying the performance -------------------------------
augment(fitted_model) %>%
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)

#------ the impact of outliers: original data -------------------
base_model <- lm(Consumption ~ Income, data = as.data.frame(us_change))
summary(base_model)


ggplot(us_change, aes(x = Income, y = Consumption)) +
  geom_point() +
  coord_cartesian(ylim = range(-4.2,2.5), xlim = range(-4,6.5)) +
  geom_abline(intercept = coef(base_model)[1], 
              slope = coef(base_model)[2],
              color = "black", linewidth = 0.2)




#------ the impact of outliers: adding an outlier and plotting it --
data_out1 <- us_change %>%
  add_row(Quarter = yearquarter("2019 Q3"), 
          Income = 1, Consumption = -4
  )

ggplot(data_out1, aes(x = Income, y = Consumption)) +
  geom_point() +
  coord_cartesian(ylim = range(-4.2,2.5), xlim = range(-4,6.5)) +
  geom_point(data = data.frame(Income = 1, Consumption = -4),
             aes(x = Income, y = Consumption),
             shape = 1, size = 4, stroke = 1.2, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.2) +
  geom_abline(intercept = coef(base_model)[1], 
              slope = coef(base_model)[2],
              color = "black", linewidth = 0.2)

                   


#------ the impact of outliers: adding an outlier and plotting it --
data_out2 <- us_change %>%
  add_row(Quarter = yearquarter("2019 Q3"), 
          Income = 6, Consumption = -4
  )

ggplot(data_out2, aes(x = Income, y = Consumption)) +
  geom_point() +
  coord_cartesian(ylim = range(-4.2,2.5), xlim = range(-4,6.5)) +
  geom_point(data = data.frame(Income = 6, Consumption = -4),
             aes(x = Income, y = Consumption),
             shape = 1, size = 4, stroke = 1.2, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.2) +
  geom_abline(intercept = coef(base_model)[1], 
              slope = coef(base_model)[2],
              color = "black", linewidth = 0.2)

#------ categorical variables --------------------------------------
new_model <- us_change %>%
  model(TSLM(Consumption ~ trend() + season()))

report(new_model)




#------ scenario-based forecasting --------------------------------
scen_model <- us_change %>%
  model(lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

fc <- scen_model %>% forecast(new_data = future_scenarios)

us_change %>%
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")
