# Import tidyverse library.
library(tidyverse)

# Import the data set (turtle_sales.csv).
turtle_sales <- read.csv(file.choose(), header=TRUE) 

# Inspect the data 
view(turtle_sales)

str(turtle_sales)

glimpse(turtle_sales)

dim(turtle_sales) 

# Remove Redundant Columns 

Turtle2<- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

head(Turtle2)

# Inspect the cleaned dataframe Turtle 2 before creating the visualizations 

view(Turtle2)

str(Turtle2)

glimpse(Turtle2)
dim(Turtle2)

class(Turtle2$Product)

summary(Turtle2)


# Convert Product category to a factor from a numeric value 

Turtle2$Product <- as.factor(Turtle2$Product)

str(Turtle2)

# Create plots to review the data 
# Load ggplot2 library for creating plots 

library(ggplot2)

# Create a bar chart to determine the top ranking platform by global sales 

ggplot(data = Turtle2, aes(x = reorder(Platform, -Global_Sales, FUN = sum), y = Global_Sales)) +
  geom_bar(stat = "summary", fun = "sum", fill = "blue") +
  labs(x = "Platform", y = "Global Sales", title = "Global Sales by Platform")

# Create scatter plots to compare sales 

# Scatter plot of EU_Sales vs Global_Sales

ggplot(Turtle2, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  labs(x = "EU Sales", y = "Global Sales", title = "Scatterplot of EU Sales vs Global Sales")

# Scatterplot of NA_Sales vs Global_Sales

ggplot(Turtle2, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  labs(x = "NA Sales", y = "Global Sales", title = "Scatterplot of NA Sales vs Global Sales")

# Scatterplot of NA_Sales vs EU_Sales

ggplot(Turtle2, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  labs(x = "NA Sales", y = "EU Sales", title = "Scatterplot of NA Sales vs EU Sales")


# Create histograms to analyse the distribution of sales

# NA_Sales distribution Histogram  

ggplot(Turtle2, aes(x = NA_Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "NA Sales", y = "Frequency", title = "Histogram of NA Sales")


# EU_Sales distribution Histogram

ggplot(Turtle2, aes(x = EU_Sales)) +
  geom_histogram(binwidth =1, fill = "blue", color = "black") +
  labs(x = "EU Sales", y = "Frequency", title = "Histogram of EU Sales")

# Global_Sales distribution Histogram

ggplot(Turtle2, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 4, fill = "blue", color = "black") +
  labs(x = "Global Sales", y = "Frequency", title = "Histogram of Global Sales")


# Create box plots to analyse the distribution of sales

# NA_Sales box plot

ggplot(Turtle2, aes(y = NA_Sales)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(y = "NA Sales", title = "Box plot of NA Sales")

# EU_Sales box plot
ggplot(Turtle2, aes(y = EU_Sales)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(y = "EU Sales", title = "Box plot of EU Sales")

# Global_Sales box plot 
ggplot(Turtle2, aes(y = Global_Sales)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(y = "Global Sales", title = "Box plot of Global Sales")


# Look for the products driving sales on the platforms 

# Filter the data by platform
filtered_data <- Turtle2 %>% 
  filter(Platform == "Wii")

# Sort the data by global sales in descending order
sorted_data <- filtered_data %>% 
  arrange(desc(Global_Sales))

# Show the top 20 products
head(sorted_data, 20)
top_products_wii <- head(sorted_data, 20)

ggplot(data = top_products_wii, aes(x = Product, y = Global_Sales)) +
  geom_col(fill = "Green") +
  labs(x = "Product", y = "Global Sales", title = "Top 10 Global Wii Products")

# Function to plot Top 10 Global Products for a specified platform
plot_top_products <- function(data, platform_name) {
  filtered_data <- data %>% 
    filter(Platform == platform_name)
  
  sorted_data <- filtered_data %>% 
    arrange(desc(Global_Sales))
  
  top_products <- head(sorted_data, 20)
  
  plot <- ggplot(data = top_products, aes(x = reorder(Product, -Global_Sales), y = Global_Sales)) +
    geom_col(fill = "Green") +
    labs(x = "Product",
         y = "Global Sales",
         title = paste("Top 10 Global", platform_name, "Products")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(plot)
}

# Create plots for the top platforms
top_ps3 <- plot_top_products(Turtle2, "PS3")
top_ds <- plot_top_products(Turtle2, "DS")
top_gb <- plot_top_products(Turtle2, "GB")
top_x360 <- plot_top_products(Turtle2, "X360")

# Display plots
top_ps3
top_ds
top_gb
top_x360





## From the data we can see that the platforms with the most global sales are the Wii, Xbox 360, PS3, DS and then the GameBoy. 
## Driving sales for the Wii was the product 107 with greater than 60M sales. This was the top ranking product of them all


# Assignment 5 

## Explore the data 

# Summary statistics for NA_Sales, EU_Sales, and Global_Sales
summary_stats <- Turtle2 %>% 
  summarise(
    min_NA_Sales = min(NA_Sales),
    max_NA_Sales = max(NA_Sales),
    mean_NA_Sales = mean(NA_Sales),
    min_EU_Sales = min(EU_Sales),
    max_EU_Sales = max(EU_Sales),
    mean_EU_Sales = mean(EU_Sales),
    min_Global_Sales = min(Global_Sales),
    max_Global_Sales = max(Global_Sales),
    mean_Global_Sales = mean(Global_Sales)
  )

# Print summary statistics
print(summary_stats)

# Create a summary of the data frame
summary(Turtle2)

# Determine the impact on sales per Product_ID 
# Group by product_id and calculate the sum of sales for each product
sales_per_product <- Turtle2 %>% 
  group_by(Product) %>% 
  summarise(
    total_NA_Sales = sum(NA_Sales),
    total_EU_Sales = sum(EU_Sales),
    total_Global_Sales = sum(Global_Sales)
  )

# Print the sales per product
print(sales_per_product)

# Create a summary of the sales_per_product data frame
summary(sales_per_product)

# Create some Graphs to determine insights into the data set 

# Histograms for distribution insights

# NA Sales histogram
ggplot(data = Turtle2, aes(x = NA_Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "NA Sales", y = "Frequency", title = "Histogram of NA Sales")

# EU Sales histogram
ggplot(data = Turtle2, aes(x = EU_Sales)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(x = "EU Sales", y = "Frequency", title = "Histogram of EU Sales")

# Global Sales histogram
ggplot(data = Turtle2, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(x = "Global Sales", y = "Frequency", title = "Histogram of Global Sales")

# Create plots to determine the normality of the sales data 

# Q-Q plot for NA Sales
ggplot(Turtle2) +
  geom_qq(aes(sample = NA_Sales), line = "s") +
  labs(title = "Q-Q plot for NA Sales")

# Q-Q plot for EU Sales
ggplot(Turtle2) +
  geom_qq(aes(sample = EU_Sales), line = "s") +
  labs(title = "Q-Q plot for EU Sales")

# Q-Q plot for Global Sales
ggplot(Turtle2) +
  geom_qq(aes(sample = Global_Sales), line = "s") +
  labs(title = "Q-Q plot for Global Sales")

# Perform Shapiro-Wilk test 

shapiro.test(Turtle2$NA_Sales)
shapiro.test(Turtle2$EU_Sales)
shapiro.test(Turtle2$Global_Sales)

install.packages("moments")

# Load the moments package
library(moments)

# Skewness
skewness(Turtle2$NA_Sales)
skewness(Turtle2$EU_Sales)
skewness(Turtle2$Global_Sales)

# Kurtosis
kurtosis(Turtle2$NA_Sales)
kurtosis(Turtle2$EU_Sales)
kurtosis(Turtle2$Global_Sales)

cor(Turtle2[, c("NA_Sales", "EU_Sales", "Global_Sales")])






# Create a simple regression model 

linear_model <- lm(Global_Sales ~ NA_Sales, data = Turtle2)
summary(linear_model)

# Determine the correlation between the sales columns 
cor.test(Turtle2$NA_Sales, Turtle2$Global_Sales)
cor.test(Turtle2$EU_Sales, Turtle2$Global_Sales)

# Create scatter plots with linear regression lines 

# NA_Sales vs. Global_Sales
ggplot(data = Turtle2, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "NA Sales", y = "Global Sales", title = "Linear Regression: NA Sales vs. Global Sales")

# EU_Sales vs. Global_Sales
ggplot(data = Turtle2, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "EU Sales", y = "Global Sales", title = "Linear Regression: EU Sales vs. Global Sales")

# Create a multiple linear regression model only using the numerical values 
multiple_linear_model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = Turtle2)
summary(multiple_linear_model)

# Determine the correlation between the numeric columns 
cor_matrix <- cor(Turtle2[, c("NA_Sales", "EU_Sales", "Global_Sales")])
cor_matrix

# Predict global sales based on the provided values

## NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
## NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
## NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
## NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
## NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

sales_data <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                         EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

predicted_global_sales <- predict(multiple_linear_model, sales_data)
predicted_global_sales

observed_data <- Turtle2[Turtle2$NA_Sales %in% sales_data$NA_Sales & Turtle2$EU_Sales %in% sales_data$EU_Sales,]
observed_global_sales <- observed_data$Global_Sales
observed_global_sales

comparison <- data.frame(Observed = observed_global_sales,
                         Predicted = predicted_global_sales)
comparison

# Create a chart to visualize the comparison 
observed_vs_predicted <- data.frame(
  Index = rep(c(1, 2, 3, 4, 5), 2),
  Sales = c(67.85, 23.21, 6.04, 4.32, 3.53, 71.468572, 6.856083, 4.248367, 4.134744, 26.431567),
  Type = rep(c("Observed", "Predicted"), each = 5)
)

library(ggplot2)

ggplot(observed_vs_predicted, aes(x = Index, y = Sales, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Data Points",
    y = "Global Sales",
    title = "Comparison of Observed and Predicted Global Sales"
  ) +
  scale_fill_manual("Legend", values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal()


# Remove the outlier
Turtle2_clean <- Turtle2 %>% filter(Product != "107")

# Create a simple regression model with the cleaned dataset
linear_model_clean <- lm(Global_Sales ~ NA_Sales, data = Turtle2_clean)
summary(linear_model_clean)

# Create scatter plots with linear regression lines for the cleaned dataset

# NA_Sales vs. Global_Sales
ggplot(data = Turtle2_clean, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "NA Sales", y = "Global Sales", title = "Linear Regression: NA Sales vs. Global Sales (Cleaned Data)")

# EU_Sales vs. Global_Sales
ggplot(data = Turtle2_clean, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "EU Sales", y = "Global Sales", title = "Linear Regression: EU Sales vs. Global Sales (Cleaned Data)")

# Create a multiple linear regression model only using the numerical values for the cleaned dataset
multiple_linear_model_clean <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = Turtle2_clean)
summary(multiple_linear_model_clean)

# Determine the correlation between the numeric columns for the cleaned dataset
cor_matrix_clean <- cor(Turtle2_clean[, c("NA_Sales", "EU_Sales", "Global_Sales")])
cor_matrix_clean

library(ggplot2)
library(moments)

# Residuals from the multiple linear regression model (without outlier)
residuals <- multiple_linear_model$residuals

# Create a histogram to visualise the distribution of residuals
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(x = "Residuals", y = "Density", title = "Distribution of Residuals")

# Normality test using Shapiro-Wilk test
shapiro_test <- shapiro.test(residuals)
shapiro_test

# Calculate skewness and kurtosis
skewness <- skewness(residuals)
kurtosis <- kurtosis(residuals)

cat("Skewness:", skewness, "\n")
cat("Kurtosis:", kurtosis, "\n")


# Load the required libraries
library(dplyr)
library(ggplot2)
library(moments)

# Log-transform the sales values

Turtle2_clean$log_NA_Sales <- log1p(Turtle2_clean$NA_Sales)
Turtle2_clean$log_EU_Sales <- log1p(Turtle2_clean$EU_Sales)
Turtle2_clean$log_Global_Sales <- log1p(Turtle2_clean$Global_Sales)

# Create a log-transformed multiple linear regression model

log_multiple_linear_model_clean <- lm(log_Global_Sales ~ log_NA_Sales + log_EU_Sales, data = Turtle2_clean)
summary(log_multiple_linear_model_clean)

# Calculate the correlation matrix for log-transformed sales values

log_cor_matrix_clean <- cor(Turtle2_clean[, c("log_NA_Sales", "log_EU_Sales", "log_Global_Sales")])
log_cor_matrix_clean

# Obtain residuals from the log-transformed multiple linear regression model

log_residuals <- log_multiple_linear_model_clean$residuals

# Plot the distribution of the log residuals

ggplot(data.frame(log_residuals), aes(x = log_residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(x = "Log Residuals", y = "Density", title = "Distribution of Log Residuals")

# Perform a Shapiro-Wilk test to assess normality of log residuals

log_shapiro_test <- shapiro.test(log_residuals)
log_shapiro_test

log_skewness <- skewness(log_residuals)
log_kurtosis <- kurtosis(log_residuals)

# Calculate skewness and kurtosis of the log residuals

cat("Log Skewness:", log_skewness, "\n")
cat("Log Kurtosis:", log_kurtosis, "\n")



# Remove products with 0 sales values and the outlier
filtered_data <- Turtle2 %>%
  filter(Product != "107" & 
           NA_Sales > 0 & 
           EU_Sales > 0 & 
           Global_Sales > 0)

# Display the filtered data
filtered_data

# Create a multiple linear regression model using the filtered data
filtered_multiple_linear_model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = filtered_data)
summary(filtered_multiple_linear_model)

# Determine the correlation between the numeric columns for the filtered dataset
filtered_cor_matrix <- cor(filtered_data[, c("NA_Sales", "EU_Sales", "Global_Sales")])
filtered_cor_matrix

# Log-transform the sales values
filtered_data$log_NA_Sales <- log1p(filtered_data$NA_Sales)
filtered_data$log_EU_Sales <- log1p(filtered_data$EU_Sales)
filtered_data$log_Global_Sales <- log1p(filtered_data$Global_Sales)

# Create a log-transformed multiple linear regression model using the filtered data
log_filtered_multiple_linear_model <- lm(log_Global_Sales ~ log_NA_Sales + log_EU_Sales, data = filtered_data)

# Create a multiple linear regression model using the filtered data
filtered_multiple_linear_model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = filtered_data)
summary(filtered_multiple_linear_model)

# Determine the correlation between the numeric columns for the filtered dataset
filtered_cor_matrix <- cor(filtered_data[, c("NA_Sales", "EU_Sales", "Global_Sales")])
filtered_cor_matrix

# Log-transform the sales values
filtered_data$log_NA_Sales <- log1p(filtered_data$NA_Sales)

filtered_data$log_EU_Sales <- log1p(filtered_data$EU_Sales)
filtered_data$log_Global_Sales <- log1p(filtered_data$Global_Sales)

# Create a log-transformed multiple linear regression model using the filtered data
log_filtered_multiple_linear_model <- lm(log_Global_Sales ~ log_NA_Sales + log_EU_Sales, data = filtered_data)

# Use the filtered data to calculate the residuals
log_filtered_residuals <- log_filtered_multiple_linear_model$residuals

# Create a histogram to visualize the distribution of log residuals
library(ggplot2)
library(moments)

ggplot(data.frame(log_filtered_residuals), aes(x = log_filtered_residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(x = "Log Filtered Residuals", y = "Density", title = "Distribution of Log Filtered Residuals")

# Perform a normality test using the Shapiro-Wilk test
log_filtered_shapiro_test <- shapiro.test(log_filtered_residuals)
log_filtered_shapiro_test

# Calculate skewness and kurtosis for the log residuals
log_filtered_skewness <- skewness(log_filtered_residuals)
log_filtered_kurtosis <- kurtosis(log_filtered_residuals)

cat("Log Filtered Skewness:", log_filtered_skewness, "\n")
cat("Log Filtered Kurtosis:", log_filtered_kurtosis, "\n")

Turtle2_clean$log_residuals <- log_residuals

Turtle2_clean_no_outliers <- Turtle2_clean %>% filter(abs(log_residuals) <= 0.5)

log_multiple_linear_model_clean_no_outliers <- lm(log_Global_Sales ~ log_NA_Sales + log_EU_Sales, data = Turtle2_clean_no_outliers)
summary(log_multiple_linear_model_clean_no_outliers)

log_clean_no_outliers_residuals <- log_multiple_linear_model_clean_no_outliers$residuals

ggplot(data.frame(log_clean_no_outliers_residuals), aes(x = log_clean_no_outliers_residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(x = "Log Residuals", y = "Density", title = "Distribution of Log Residuals (No Outliers)")

log_clean_no_outliers_shapiro_test <- shapiro.test(log_clean_no_outliers_residuals)
log_clean_no_outliers_shapiro_test

log_clean_no_outliers_skewness <- skewness(log_clean_no_outliers_residuals)
log_clean_no_outliers_kurtosis <- kurtosis(log_clean_no_outliers_residuals)

cat("Log Clean (No Outliers) Skewness:", log_clean_no_outliers_skewness, "\n")
cat("Log Clean (No Outliers) Kurtosis:", log_clean_no_outliers_kurtosis, "\n")

# Create a new data frame with the provided sales data
sales_data_filtered <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                                  EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Add log-transformed sales columns to the sales_data_filtered data frame
sales_data_filtered$log_NA_Sales <- log1p(sales_data_filtered$NA_Sales)
sales_data_filtered$log_EU_Sales <- log1p(sales_data_filtered$EU_Sales)

# Predict global sales using the log-transformed model
predicted_log_global_sales_filtered <- predict(log_multiple_linear_model_clean_no_outliers, sales_data_filtered)

# Convert the predicted log-transformed global sales back to the original scale
predicted_global_sales_filtered <- expm1(predicted_log_global_sales_filtered)

# Find the index of the provided data points in the original dataset
index <- match(sales_data$NA_Sales, Turtle2$NA_Sales)

# Extract the observed global sales values from the original dataset
observed_global_sales <- Turtle2$Global_Sales[index]

# Compare the predicted values to the observed values
comparison <- data.frame(Observed = observed_global_sales,
                         Predicted = predicted_global_sales_filtered)
comparison

# Create a data frame for the chart
observed_vs_predicted_filtered <- data.frame(
  Index = rep(c(1, 2, 3, 4, 5), 2),
  Sales = c(observed_global_sales, predicted_global_sales_filtered),
  Type = rep(c("Observed", "Predicted"), each = 5)
)

# Create a bar chart to visualize the comparison
library(ggplot2)

ggplot(observed_vs_predicted_filtered, aes(x = Index, y = Sales, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Data Points",
    y = "Global Sales",
    title = "Comparison of Observed and Predicted Global Sales (Filtered Data)"
  ) +
  scale_fill_manual("Legend", values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal()

## There is still significant difference beween the observed and predicted values 

