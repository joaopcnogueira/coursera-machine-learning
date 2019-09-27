# 01. LOAD PACKAGES ----
library(randomForest)
library(mlbench)
library(caret)


# 02. LOAD DATA ----
data(Sonar)
x <- Sonar[,1:60]
y <- Sonar[,61]


# 03. MODEL TRAINING ----
train_control <- trainControl(method="cv", number=10)

seed <- 7
metric <- "Accuracy"

set.seed(seed)
mtry <- sqrt(ncol(x))
tune_grid <- expand.grid(.mtry = mtry)

rf_default <- train(Class ~ .,
                    data = Sonar,
                    method = "rf",
                    metric = metric,
                    tuneGrid = tune_grid,
                    trControl = train_control)
print(rf_default)


# 04. HYPERPARAMETER TUNNING ----

# 04.1 Random Search ----
train_control <- trainControl(method = "cv", number = 10, search = "random")
set.seed(seed)

mtry <- sqrt(ncol(x))

rf_random <- train(Class ~ .,
                   data = Sonar,
                   method = "rf",
                   metric = metric,
                   tuneLength = 15,
                   trControl = train_control)
print(rf_random)
plot(rf_random)


# 04.2 Grid Search ----
train_control <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(seed)
tune_grid <- expand.grid(.mtry=c(1:15))

rf_grid <- train(Class ~ .,
                 data = Sonar,
                 method = "rf",
                 metric = metric,
                 tuneGrid = tune_grid,
                 trControl = train_control)
print(rf_grid)
plot(rf_grid)


# 04.3 Tune Using Algorithm Tools (tuneRF) ----
set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor = 1.5, improve = 1e-5, ntreeTry = 500)

print(bestmtry)
plot(bestmtry)


# 05. CRAFT YOUR OWN PARAMETER SEARCH ----

# 05.1 Tune Manually ----
train_control <- trainControl(method = "cv", number = 10, search = "grid")
tune_grid <- expand.grid(.mtry=c(sqrt(ncol(x))))

modellist <- list()
for (ntree in c(1000, 1500, 2000, 25000)) {
    set.seed(seed)
    fit <- train(Class ~ ., 
                 data = Sonar, 
                 method = "rf", 
                 metric = metric, 
                 tuneGrid = tune_grid,
                 trControl = train_control,
                 ntree = ntree)
    key <- toString(ntree)
    modellist[[key]] <- fit
}

# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


# 05.2 Extend Caret ----
customRF <- list(type="Classification", library="randomForest", loop=NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid"){}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    randomForest(x, y, mtry = param$mtry, ntree = param$ntree, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata)
}

customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata, type = "prob")
}

customRF$sort <- function(x) x[order(x[,1]),]

customRF$levels <- function(x) x$classes

# train model
train_control <- trainControl(method="cv", number = 10)
tune_grid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 25000))

set.seed(seed)

custom_rf <- train(Class ~ .,
                   data = Sonar,
                   method = customRF,
                   metric = metric,
                   tuneGrid = tune_grid,
                   trControl = train_control)
print(custom_rf)
plot(custom_rf)
