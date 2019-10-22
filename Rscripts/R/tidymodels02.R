# 1.0 LOADING PACKAGES ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(recipes)
library(parsnip)

# 2.0 EXPLORATORY DATA ANALYSIS ----
telco <- readr::read_csv("data/Telco-Customer-Churn.csv")

telco %>% skimr::skim()

telco <- telco %>% 
    select(-customerID) %>% 
    drop_na()

# 3.0 MODELLING WITH TIDYMODELS ----
# 3.1 Train and test split ----
set.seed(seed = 1991)

train_test_split <- rsample::initial_split(telco, prop = 0.8)
train_test_split

train_tbl <- train_test_split %>% rsample::training()
test_tbl <- train_test_split %>% rsample::testing()

# 3.2 A simple recipe ----
recipe_simple <- recipe(Churn ~ ., data = train_tbl) %>% 
    step_string2factor(all_nominal(), -all_outcomes()) %>% 
    prep(data = train_tbl)

train_baked <- recipe_simple %>% bake(train_tbl)
test_baked <- recipe_simple %>% bake(test_tbl)

# 3.3 Fit the model ----
logistic_glm <- logistic_reg(mode = "classification") %>% 
    set_engine("glm") %>% 
    fit(Churn ~ ., data = train_baked)

# 3.4 Model evaluation ----
predictions_glm <- logistic_glm %>% 
    predict(new_data = test_baked) %>% 
    bind_cols(test_baked %>% select(Churn))

predictions_glm %>% head()

predictions_glm %>% 
    yardstick::conf_mat(Churn, .pred_class) %>% 
    purrr::pluck(1) %>% 
    as_tibble() %>% 
    ggplot(aes(Prediction, Truth, alpha = n)) +
    geom_tile(show.legend = FALSE) +
    geom_text(aes(label = n), color = "blue", alpha = 1, size = 8)


predictions_glm %>% 
    yardstick::metrics(Churn, .pred_class) %>% 
    select(-.estimator) %>% 
    filter(.metric == "accuracy")

tibble(
    accuracy = predictions_glm %>% yardstick::accuracy(Churn, .pred_class) %>% select(.estimate) %>% pull(),
    precision = predictions_glm %>% yardstick::precision(Churn, .pred_class) %>% select(.estimate) %>% pull(),
    recall = predictions_glm %>% yardstick::recall(Churn, .pred_class) %>% select(.estimate) %>% pull(),
    f1_score = predictions_glm %>% yardstick::f_meas(Churn, .pred_class) %>% select(.estimate) %>% pull()
)



# 4.0 REFERENCE -----
# https://towardsdatascience.com/modelling-with-tidymodels-and-parsnip-bae2c01c131c