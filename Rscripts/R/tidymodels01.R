# Loading the libraries ----
library(tidyverse)
library(tidymodels)
library(caret)


# Loading the data ----
ames <- AmesHousing::make_ames()


# Split into train and test set ----
set.seed(123)
split <- initial_split(ames, prop = 0.7, strata = "Sale_Price")
ames_train <- split %>% training()
ames_test <- split %>% testing()


# Training without feature engineering ----
train_control <- trainControl(method = "cv", number = 3)

hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

knn_fit <- train(Sale_Price ~ .,
                 data = ames_train,
                 method = "knn",
                 trControl = train_control,
                 tuneGrid = hyper_grid,
                 metric = "RMSE")

# print the results
knn_fit

ggplot(knn_fit)

# predict on new data
predictions <- predict(knn_fit, newdata = ames_test)

# evaluate the predictions
yardstick::rmse_vec(ames_test$Sale_Price, predictions)

# save the model
# saveRDS(knn_fit, "knn_fit.rds")


# Feature engineering ----
pipeline <- recipe(Sale_Price ~ ., data = ames_train) %>% 
    step_nzv(all_nominal()) %>% 
    step_integer(matches("Qual|Cond|QC|Qu")) %>% 
    step_center(all_numeric(), -all_outcomes()) %>% 
    step_scale(all_numeric(), -all_outcomes()) %>% 
    step_dummy(all_nominal(), -all_outcomes(), one_hot = FALSE)


train_control <- trainControl(method = "cv", number = 3)

hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

knn_fit2 <- train(
    pipeline,
    data = ames_train,
    method = "knn",
    trControl = train_control,
    tuneGrid = hyper_grid,
    metric = "RMSE"
)

# print the results
knn_fit2

ggplot(knn_fit2)

# predict on new data
predictions <- predict(knn_fit2, newdata = ames_test)

# evaluate the predictions
yardstick::rmse_vec(ames_test$Sale_Price, predictions)

# save the model
# saveRDS(knn_fit2, "knn_fit2.rds")
