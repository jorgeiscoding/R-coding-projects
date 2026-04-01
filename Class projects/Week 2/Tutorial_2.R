install.packages("fpp3")
install.packages("readxl")

library(fpp3)
library(readxl)
library(ggplot2)

# paths
file_path <- "Week 2/Tutorial_2.xlsx"
sheet_name = "Data"
# Read the data and remove NAs
raw_df <- read_excel(file_path, sheet = sheet_name)
df <- na.omit(raw_df)

# Calculate column means
state_means <- colMeans(df[3:10])
state_means
# Create a Dataframe
means_df <- data.frame(State = names(state_means),Avg = state_means)
means_df

#Plot
ggplot(means_df, aes(x = reorder(State, -Avg), y = Avg, fill = Avg)) +
  geom_col(width = 0.7, color = "black") +      # Bars with black borders
  geom_text(aes(label = round(Avg, 1)),        # Add labels on top of bars
            vjust = -0.5, size = 3.5) +
  scale_fill_gradient(low = "skyblue", high = "navy") +  # Gradient based on value
  labs(title = "Average Turnover by State",
       x = "State",
       y = "Average Turnover") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
