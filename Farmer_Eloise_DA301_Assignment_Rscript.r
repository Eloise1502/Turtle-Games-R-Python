# LSE Data Analytics Online Career Accelerator - Eloise Farmer
# DA301: Advanced Analytics for Organisational Impact

# EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note the data file was already cleaned
## in the python segment of this project and is imported here.
## The outputs are displayed and discussed in the technical report and in
## the presentation to stakeholders.

###############################################################################

# Load and Explore the Data 
# Load tidyverse for data import and handling
library(tidyverse)

# Import the cleaned data
clean_reviews <- read_csv("turtle_reviews_clean.csv")

# View the first few rows
head(clean_reviews)

# Check structure
summary(clean_reviews)
glimpse(clean_reviews)

# Data provided had spelling error (renumeration > remuneration)
clean_reviews <- clean_reviews %>%
  rename(remuneration = renumeration)
glimpse(clean_reviews)

###############################################################################

# Basic graphs to get started with EDA

# Loyalty points distribution
ggplot(clean_reviews, aes(x = loyalty_points)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "white", alpha=0.7) +
  labs(title = "Distribution of Loyalty Points", x = "Loyalty Points", y = "Count")
  

# Improving the graph for business audience
# Calculate the IQR as interested to know what % customer are outliers
# Extract loyalty points
lp <- clean_reviews$loyalty_points

# Calculate quartiles and IQR
Q1  <- quantile(lp, 0.25, na.rm = TRUE)
Q3  <- quantile(lp, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define outlier cut-offs
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Calculate % outliers
is_outlier  <- lp < lower_limit | lp > upper_limit
n_outliers  <- sum(is_outlier, na.rm = TRUE)
n_total     <- sum(!is.na(lp))
outlier_pct <- (n_outliers / n_total) * 100

cat("Lower limit:", lower_limit, "\n")
cat("Upper limit:", upper_limit, "\n")
cat("Outliers:", n_outliers, "of", n_total, 
    "(", round(outlier_pct, 1), "% )\n")

# Improve graph appearance for presentation
ggplot(clean_reviews, aes(x = loyalty_points)) +
  geom_histogram(binwidth = 250, fill = "#2C7FB8", color = "white",
                 boundary = 0, closed = "left") +
  geom_vline(xintercept = as.numeric(upper_limit),
             linetype = "dashed", color = "#D7301F", size = 1) +
  labs(
    title    = "Distribution of Loyalty Points",
    subtitle = paste0("Outliers above ~", round(upper_limit),
                      " points (", round(outlier_pct, 1), "% of customers)"),
    x = "Loyalty Points", y = "Number of Customers"
  ) +
  theme_minimal(base_size = 13)

###############################################################################

# Boxplot of loyalty points
ggplot(clean_reviews, aes(y = loyalty_points)) +
  geom_boxplot(fill = "tomato", alpha=0.6) +
  labs(title = "Boxplot of Loyalty Points", y = "Loyalty Points")

# Scatter: remuneration vs loyalty points
ggplot(clean_reviews, aes(x = remuneration, y = loyalty_points)) +
  geom_point(alpha=0.6, color="darkgreen") +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title = "Remuneration vs Loyalty Points")

# Scatter: spending score vs loyalty points
ggplot(clean_reviews, aes(x = spending_score, y = loyalty_points)) +
  geom_point(alpha=0.6, color="navy") +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title = "Spending Score vs Loyalty Points")

# Boxplot: loyalty points across age groups 
clean_reviews <- clean_reviews %>%
  mutate(age_band = case_when(
    age < 25 ~ "<25",
    age < 40 ~ "25-39",
    age < 55 ~ "40-54",
    TRUE ~ "55+"
  ))

ggplot(clean_reviews, aes(x = age_band, y = loyalty_points, fill = age_band)) +
  geom_boxplot(alpha=0.6) +
  labs(title = "Loyalty Points across Age Groups", x = "Age Group", y = "Loyalty Points") +
  theme(legend.position = "none")

###############################################################################

# Create a heatmap for the variable to gain a broader understanding 
# install.packages("corrplot")
library(tidyverse)
library(ggcorrplot)

# Select/rename the variables for the matrix - make sure 'product' is numeric
corr_df <- clean_reviews %>%
  transmute(
    Age              = age,
    Pay           = remuneration,
    `Spending Score` = spending_score,
    `Loyalty Points` = loyalty_points,
    Product          = as.numeric(product)
  )

# Correlation matrix (pairwise in case of any NAs)
C <- cor(corr_df, use = "pairwise.complete.obs", method = "pearson")

# Full heatmap
ggcorrplot(
  C,
  type       = "full",                 # show both triangles
  lab        = TRUE,                   # print coefficients
  lab_size   = 3.8,
  show.diag  = TRUE,                   # keep the 1.00 diagonal
  outline.col= "grey90",
  colors     = c("#3b4cc0", "white", "#b40426"),  # neg → pos
  tl.srt     = 45,                     # rotate x labels
  tl.cex     = 12/12,                  # label size
  legend.title = "Corr"
) +
  ggtitle("Correlation Matrix of Key Variables") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "right"
  )

###############################################################################

# Need to check relationship between education and gender on loyalty points
# Although the feature importance analysis in Python has already indicated
# that it has minimal relationship 

# Gender vs Loyalty Points
ggplot(clean_reviews, aes(x = gender, y = loyalty_points, fill = gender)) +
  geom_boxplot() +
  labs(title = "Loyalty Points by Gender", x = "Gender", y = "Loyalty Points") +
  theme_minimal()

# Education vs Loyalty Points
ggplot(clean_reviews, aes(x = education, y = loyalty_points, fill = education)) +
  geom_boxplot() +
  labs(title = "Loyalty Points by Education", x = "Education", y = "Loyalty Points") +
  theme_minimal()

###############################################################################

# Create a Multiple linear regression model using selected (numeric) features.
## Evaluate the goodness of fit and interpret the model summary statistics.
## Create a visual demonstration of the model

# Build a regression-ready dataframe
mlr_df <- clean_reviews %>%
  select(loyalty_points, remuneration, spending_score)

# Fit the multiple linear regression model
mlr_model <- lm(loyalty_points ~ remuneration + spending_score, data = mlr_df)

# Summarise the model
summary(mlr_model)

# Diagnostic plots for regression assumptions
par(mfrow=c(2,2))
plot(mlr_model)
par(mfrow=c(1,1))

# Add predictions directly to the dataset used for training
mlr_df <- mlr_df %>%
  mutate(predicted = predict(mlr_model, newdata = mlr_df))

# Plot actual vs predicted loyalty points
ggplot(mlr_df, aes(x = predicted, y = loyalty_points)) +
  geom_point(alpha=0.6, color="blue") +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  labs(title = "Multiple Linear Regression: Actual vs Predicted Loyalty Points",
       x = "Predicted Loyalty Points", y = "Actual Loyalty Points") +
  theme_minimal()

# Test model on an example business scenario (corrected for k£)
new_data <- data.frame(
  remuneration   = c(25, 60, 90),  # = £25k, £60k, £90k
  spending_score = c(30, 70, 95)
)

predict(mlr_model, newdata = new_data)
## This able to predict the loyalty points based on inputed remun & spending

# To validate the predictive performance of this model, 
## a 70/30 train-test split was performed

# Train/Test Split Validation 
## set.seed(123) ensures the random train/test split is reproducible, 
## so the results can be replicated exactly.

set.seed(123)  

# Create a 70/30 split
n <- nrow(mlr_df)
train_index <- sample(seq_len(n), size = 0.7*n)

train_data <- mlr_df[train_index, ]
test_data  <- mlr_df[-train_index, ]

# Fit model on training data
mlr_train <- lm(loyalty_points ~ remuneration + spending_score, data = train_data)

# Summarise training model
summary(mlr_train)

# Predict on test data
test_data$predicted <- predict(mlr_train, newdata = test_data)

# Calculate RMSE (Root Mean Squared Error) for test predictions
rmse <- sqrt(mean((test_data$loyalty_points - test_data$predicted)^2))
cat("Test RMSE:", rmse, "\n")

# Compare actual vs predicted on test set
ggplot(test_data, aes(x = predicted, y = loyalty_points)) +
  geom_point(alpha=0.6, color="darkorange") +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  labs(title = "Test Data: Actual vs Predicted Loyalty Points",
       x = "Predicted Loyalty Points", y = "Actual Loyalty Points") +
  theme_minimal()

# Mean Absolute Error (MAE) on test data
mae <- mean(abs(test_data$loyalty_points - test_data$predicted))
cat("Test MAE:", mae, "\n")

## Assess Multicollinearity of MLR
library(car)
vif(mlr_model)

# Test MLR on a new customer example
# Define the new customer (remun 45, spending 55)
new_customer <- data.frame(
  remuneration   = 45,  # income in k£
  spending_score = 55   # spending score out of 100
)

# Predict loyalty points
predicted_points <- predict(mlr_model, newdata = new_customer)
predicted_points
## Results used in business presentation to provide example of MLR use case for marketing team

# End of Script