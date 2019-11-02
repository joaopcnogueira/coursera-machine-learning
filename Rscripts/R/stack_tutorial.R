library(tidyverse)
library(tidymodels)
library(caret)
library(caretEnsemble)
library(skimr)

titanic <- read_csv("../notebooks/data/titanic_train.csv") %>% 
    select(-c(Ticket, PassengerId))

titanic %>% skim()

titanic <- titanic %>% 
    mutate(Survived = ifelse(Survived == 1, "yes", "no"),
           Survived = factor(Survived, levels = c("yes", "no")))


# FEATURE ENGINEERING ----
pipeline <- recipe(Survived ~ ., data = titanic) %>% 
    
    # Feature Engineering the Cabin column
    step_mutate(Cabin = substr(Cabin, 1, 1)) %>% 
    step_unknown(Cabin, new_level = "Missing") %>% 
    step_integer(Cabin) %>%
    
    # Feature Engineering the Name column
    step_mutate(title = str_extract(Name, "(Mrs|Mr|Miss)")) %>% 
    step_unknown(title, new_level = "other") %>% 
    step_integer(title) %>% 
    step_rm(Name) %>% 
    
    # Feature Engineering the Embarked column
    step_modeimpute(Embarked) %>% 
    step_integer(Embarked) %>% 
    
    # Feature Engineering the Sex column
    step_modeimpute(Sex) %>% 
    step_integer(Sex) %>% 
    
    # Feature Engineering the Age column
    step_mutate(Age = ifelse(is.na(Age), -99, Age)) %>% 
    
    prep(titanic)
    

train <- pipeline %>% 
    bake(titanic)


# RANDOM FOREST ----
train_control <- trainControl(method = "cv", number = 3)

rf <- train(Survived ~ ., 
            data = train, 
            method = "rf", 
            metric = "Accuracy", 
            trControl = train_control)

# accuracy = 0.8204
rf$results %>% 
    filter(mtry == rf$bestTune %>% pull()) %>% 
    select(mtry, Accuracy)


# GRADIENT BOOSTING MACHINES ----
gbm <- train(Survived ~ .,
             data = train,
             method = "xgbTree",
             metric = "Accuracy",
             trControl = train_control)

# accuracy = 0.8204265
gbm$bestTune


# STACKING ----
train_control <- trainControl(method = "cv", number = 3, savePredictions = "final", classProbs = TRUE)
algo_list <- c('rf', 'xgbTree', 'svmRadial')
set.seed(1234)
models <- caretList(Survived ~ ., data = train, trControl = train_control, methodList = algo_list)
set.seed(1234)
stacker <- caretStack(models, method = "glm", metric = "Accuracy", trControl = train_control)
stacker # Accuracy = 0.8339

train %>% 
    select(Survived) %>% 
    cbind(stacker %>% predict(train)) %>% 
    yardstick::accuracy(Survived, predictions)


# PREDICTIONS AT THE TEST SET ----
titanic_test <- read_csv("../notebooks/data/titanic_test.csv")

titanic_test %>% skim()

pipeline <- pipeline %>% 
    step_mutate(Fare = ifelse(is.na(Fare), -99, Fare)) %>% 
    prep(titanic)

test <- pipeline %>% bake(titanic_test)

test_predictions <- predict(stacker, test)
    
titanic_test %>% 
    select(PassengerId) %>% 
    cbind(Survived = test_predictions) %>% 
    mutate(Survived = ifelse(Survived == "yes", 1, 0)) %>% 
    write_csv("data/stack_submission.csv")
