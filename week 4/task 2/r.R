library("tidymodels")
library("tidyverse")
library("stringr")



# Dataset URL
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
spec(bike_sharing_df)



bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)



lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")



set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)



ggplot(data = train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() 




# Plot the higher order polynomial fits
ggplot(data=train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")




##task 1 

# Fit a linear model with higher order polynomial on some important variables 

# #HINT: Use ploy function to build polynomial terms, lm_poly <- RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(HUMIDITY, 4) .....
lm_poly <- lm_spec %>% fit(RENTED_BIKE_COUNT ~ poly(RAINFALL, 8) + poly(HUMIDITY, 4) +  poly(TEMPERATURE, 6) + poly(DEW_POINT_TEMPERATURE, 5) + poly(SOLAR_RADIATION, 5) + poly(SNOWFALL,5) + SPRING + 
                             SUMMER + AUTUMN + HOLIDAY + WIND_SPEED + VISIBILITY + `18` + `19` + `8` + `21` + `20` + `4` + `5` + `22` + `3` + `17` + `11` + `10` + `2` + `12` + `6` + `14` + `13` + `1` + `15` +
                             `7` + `23` + `0` + `16`, data = train_data)




#task 2

# Use predict() function to generate test results for `lm_poly`
test_predictions <- predict(lm_poly, new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)


#task 3
test_predictions[test_predictions < 0] <- 0

#task 4

test_results_rsq <- rsq(test_predictions, truth = truth, estimate = .pred)
test_results_rmse <- rmse(test_predictions, truth = truth, estimate = .pred)





###TASK: Add interaction terms



#task 1

lm_intera <- lm_spec %>% fit(RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY*TEMPERATURE + 
                               SPRING*SUMMER*AUTUMN*HOLIDAY*`12`*`5`*`2`*`0`*`23`*`19` + 
                               poly(RAINFALL, 8) + poly(HUMIDITY, 4) +  poly(TEMPERATURE, 6) +
                               poly(DEW_POINT_TEMPERATURE, 5) + poly(SOLAR_RADIATION, 5) +
                               poly(SNOWFALL,5) + SPRING + 
                               SUMMER + AUTUMN + HOLIDAY + WIND_SPEED + VISIBILITY + `1` +
                               `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` +
                               `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` +
                               `18` + `19` + `20` + `21` + `22` + `23`+`0`, data = train_data)






#task 2

intera_test_results <- lm_intera %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

intera_test_results[intera_test_results<0] <- 0

new_test_results_rsq <- rsq(intera_test_results, truth = truth, estimate = .pred)
new_test_results_rmse <- rmse(intera_test_results, truth = truth, estimate = .pred)


new_test_results_rsq 
new_test_results_rmse



#task 3

glmnet_spec <- linear_reg(penalty = 0.2, mixture = 0.5) %>%
  set_engine("glmnet")


# task 4 

fit_model <- fit(glmnet_spec, formula = RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY*TEMPERATURE + 
                   +                    SPRING*SUMMER*AUTUMN*HOLIDAY*`12`*`5`*`2`*`0`*`23`*`19` + 
                   +                    poly(RAINFALL, 8) + poly(HUMIDITY, 4) +  poly(TEMPERATURE, 6) +
                   +                    poly(DEW_POINT_TEMPERATURE, 5) + poly(SOLAR_RADIATION, 5) +
                   +                    poly(SNOWFALL,5) + SPRING + 
                   +                    SUMMER + AUTUMN + HOLIDAY + WIND_SPEED + VISIBILITY + `1` +
                   +                    `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` +
                   +                    `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` +
                   +                    `18` + `19` + `20` + `21` + `22` + `23`+`0`, data = train_data)






#task 5
glmnet_test_results <- fit_model %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

glmnet_test_results [glmnet_test_results <0] <- 0

glmnet_results_rsq <- rsq(glmnet_test_results, truth = truth, estimate = .pred)
glmnet_results_rmse <- rmse(glmnet_test_results, truth = truth, estimate = .pred)

glmnet_results_rmse
glmnet_results_rsq





###Experiment to search for improved models

#task 1

#first set model_prediction function and model_evaluation function.

## model_prediction
model_prediction <- function(lm_model, test_data) {
  reg_test_results <- lm_model %>%
    predict(new_data = test_data) %>%
    mutate(truth=test_data$RENTED_BIKE_COUNT)
  reg_test_results[reg_test_results<0] <- 0
  return(reg_test_results)
}

##model_evaluation
model_evaluation <- function(reg_test_results) {
  rmse <- rmse(reg_test_results, truth = truth, estimate = .pred)
  r2 <- rsq(reg_test_results, truth = truth, estimate = .pred)
  print(r2)
  print(rmse)
}


# Model 1
model_1 <- glmnet_test_results

model_1_rsq <- rsq(glmnet_test_results, truth = truth, estimate = .pred)
model_1_rmse <- rmse(glmnet_test_results, truth = truth, estimate = .pred)


#model 2
model_2_recipe <- glmnet_spec %>% fit(RENTED_BIKE_COUNT ~ RAINFALL+HUMIDITY+TEMPERATURE+DEW_POINT_TEMPERATURE+
                                     `19`+ `8` + `21` + `20` + `4` + `5` + AUTUMN + `22` + `3` + `17` +
                                     SOLAR_RADIATION + SNOWFALL + `11` + `10` + `2` + `12` + `6` + SUMMER +
                                     `14` + SPRING + `13` + HOLIDAY + `1` + `15` + `7` + `23` + WIND_SPEED + 
                                     `0` + `16` + VISIBILITY, data = train_data)


model_2 <- model_prediction(model_2_recipe, test_data)
model_evaluation(model_2)
model_2_rmse <- rmse(model_2, truth = truth, estimate = .pred)
model_2_rsq <- rsq(model_2, truth = truth, estimate = .pred)


#model 3
model_3_recipe <- glmnet_spec %>% fit(RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY*TEMPERATURE +
                                        poly(RAINFALL, 8) + poly(HUMIDITY, 5) + 
                                          `18`*`19`*`8`*`21`*`20`*`4` + poly(TEMPERATURE, 5) +
                                        poly(DEW_POINT_TEMPERATURE, 5) + `19` + `8` + 
                                          `21` + `20` + `4`, data = train_data)

model_3 <- model_prediction(model_3_recipe, test_data)
model_evaluation(model_3)
model_3_rmse <- rmse(model_3, truth = truth, estimate = .pred)
model_3_rsq <- rsq(model_3, truth = truth, estimate = .pred)




#model 4

model_4_recipe <- glmnet_spec %>% fit(RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY*TEMPERATURE  + 
                                        `18`*`19`*`8`*`21`*`20`*`4`*`5`*SPRING*SUMMER*AUTUMN*HOLIDAY + 
                                           poly(RAINFALL, 8) + poly(HUMIDITY, 5) + poly(TEMPERATURE, 5) +
                                           poly(DEW_POINT_TEMPERATURE, 5) + poly(SOLAR_RADIATION, 5) + 
                                           poly(SNOWFALL, 5) + SPRING  + AUTUMN + SUMMER + HOLIDAY +
                                           `18` + `19` + `8` + `21` + `20` + `4` + `5` + WIND_SPEED +
                                           VISIBILITY, data = train_data)

model_4 <- model_prediction(model_4_recipe, test_data)
model_evaluation(model_4)
model_4_rmse <- rmse(model_4, truth = truth, estimate = .pred)
model_4_rsq <- rsq(model_4, truth = truth, estimate = .pred)




#model 5

model_5_recipe <- glmnet_spec %>% fit(RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY*TEMPERATURE + 
                                        poly(RAINFALL, 8) + poly(HUMIDITY, 5) + 
                                        `18`*`19`*`8`*`21`*`20`*`4`*`5`*`22`*`3`*`17` +
                                        poly(TEMPERATURE, 5) + poly(DEW_POINT_TEMPERATURE, 5) + 
                                        `19` + `8` + `21` + `20` + `4` + `5` + `22` + `3` + `17` +
                                        AUTUMN, data = train_data)

model_5 <- model_prediction(model_5_recipe, test_data)
model_evaluation(model_5)
model_5_rmse <- rmse(model_5, truth = truth, estimate = .pred)
model_5_rsq <- rsq(model_5, truth = truth, estimate = .pred)





#task 2
# Create a data frame to store the RMSE and R squared values of all the models
model_results_df <- data.frame(Model = c("Model_1", "Model_2", "Model_3", "Model_4", "Model_5"),
                               RSQ = c(model_1_rsq$.estimate, model_2_rsq$.estimate, model_3_rsq$.estimate, model_4_rsq$.estimate, model_5_rsq$.estimate),
                               RMSE = c(model_1_rmse$.estimate, model_2_rmse$.estimate, model_3_rmse$.estimate, model_4_rmse$.estimate,model_5_rmse$.estimate))



# Write the data frame to a .csv file
write.csv(model_results_df, "model_results_df.csv", row.names = FALSE)



#task 3
ggplot(model_results_df, aes(x = Model, y = RSQ, fill = RSQ)) +
  geom_col() +
  labs(title = "R Squared Values of Different Models",
       x = "Model",
       y = "R Squared") +
  scale_fill_gradient(low = "red", high = "green")




#task 3 test

ggplot(model_results_df, aes(x=Model, y=RSQ, fill=RSQ)) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_bar(aes(y=RMSE, fill=RMSE), stat="identity", position="dodge", width=0.5) +
  scale_fill_gradient(low = "blue", high = "red") + 
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) + 
  labs(x="Model", y="RSQ & RMSE", fill="Values") + 
  theme_classic()































































































