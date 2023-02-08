library("tidymodels")
library("tidyverse")
library("stringr")
library("rlang")
library("caret")


dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"

bike_sharing_df <- read_csv(dataset_url)

spec(bike_sharing_df)



bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)



# TASK 1: Split training and testing data

# Use the `initial_split()`, `training()`, and `testing()` functions to split the dataset
# With seed 1234
set.seed(1234)
# prop = 3/4
bike_sharing_split <- initial_split(bike_sharing_df ,prop = 3/4)
# train_data 
train_data <- training(bike_sharing_split )
# test_data
test_data <- testing(bike_sharing_split )


# TASK2 : Build a linear regression model using weather variables only

# Define the model specification

# Use `linear_reg()` with engine `lm` and mode `regression`
lm_model_weather_spec <- linear_reg(engine = "lm", mode = "regression" )


# Fit the model to the training data
lm_model_weather <- lm_model_weather_spec  %>%
  fit(RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE 
      + SOLAR_RADIATION + RAINFALL + SNOWFALL, data = train_data)

print(lm_model_weather$fit)


#TASK 3 : Build a linear regression model using all variables

lm_model_all <- lm_model_weather_spec %>%
  fit(RENTED_BIKE_COUNT ~ ., data = train_data)

summary(lm_model_all$fit)


#TASK 4 :Model evaluation and identification of important variables

#1

# test_results_weather for lm_model_weather model
test_results_weather <- predict(lm_model_weather, new_data = train_data)

# test_results_all for lm_model_all
test_results_all <- predict(lm_model_all, new_data = train_data)


#2

# Create a truth column
train_data$truth <- train_data$RENTED_BIKE_COUNT

# Bind predictions and truth column to train_data
train_result_weather <- cbind(train_data, test_results_weather )
train_result_all <- cbind(train_data, test_results_all)

#RMSE RSQUERED MAE i use this code beceause the rsq and rmse function dont wana work
postResample(pred = test_results_weather, obs = train_data$truth)
postResample(pred = test_results_all, obs = train_data$truth)



# 3

lm_model_all$fit$coefficients



# 4
stack(abs(lm_model_all$fit$coefficients)) 

sorted_coef <- sort(abs(lm_model_all$fit$coefficients), decreasing = TRUE)

coef_df <- data.frame(coefficient = names(sorted_coef), value = sorted_coef)



ggplot(coef_df, aes(x =value , y = reorder(coefficient, value))) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  xlab("Coef") + 
  ylab("reorder(Variable. Coef") 































































































