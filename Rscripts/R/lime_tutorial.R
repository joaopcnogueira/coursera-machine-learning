# 1.0 REQUIRED PACKAGES ----
# install vip from github repo: devtools::install_github("koalaverse/vip")
library(lime)       # ML local interpretation
library(vip)        # ML global interpretation
library(pdp)        # ML global interpretation
library(ggplot2)    # visualization pkg leveraged by above packages
library(caret)      # ML model building
library(h2o)        # ML model building

# other useful packages
library(tidyverse)  # Use tibble, dplyr
library(rsample)    # Get HR Data via rsample::attrition
library(gridExtra)  # Plot multiple lime plots on one graph


# initialize h2o
h2o.init()

h2o.no_progress()


# 2.0 CREATE DATA SETS ----
df <- rsample::attrition %>% 
    mutate_if(is.ordered, factor, ordered = FALSE) %>% 
    mutate(Attrition = factor(Attrition, levels = c("Yes","No")))

index <- 1:5
train_obs <- df[-index,]
local_obs <- df[index,]


# 3.0 TRAINING MODELS ----
# create h2o objects for modeling
y <- "Attrition"
x <- setdiff(names(train_obs), y)
train_obs.h2o <- as.h2o(train_obs)
local_obs.h2o <- as.h2o(local_obs)


# Create Random Forest model with ranger via caret
fit_caret <- train(
    Attrition ~ .,
    data = train_obs,
    method = 'ranger',
    trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
    tuneLength = 1,
    importance = 'impurity'
)

# Create h2o models
h2o_rf <- h2o.randomForest(x, y, training_frame = train_obs.h2o)
h2o_glm <- h2o.glm(x, y, training_frame = train_obs.h2o, family = "binomial")
h2o_gbm <- h2o.gbm(x, y, training_frame = train_obs.h2o)

# Ranger model --> model type not built in to LIME
fit_ranger <- ranger::ranger(
    Attrition ~ .,
    data = train_obs,
    importance = 'impurity',
    probability = TRUE
)

# 4.0 GLOBAL INTERPRETATION ----
vip(fit_ranger) + ggtitle("ranger: RF")


# 4.1 built-in PDP (partial dependence plots) support in H2O ----
h2o.partialPlot(h2o_rf, data = train_obs.h2o, cols = "MonthlyIncome")

# 4.2 ICE (individual conditional expectation) curves ----
fit_ranger %>% 
    pdp::partial(pred.var = "MonthlyIncome", grid.resolution = 25, ice = TRUE) %>% 
    autoplot(rug = TRUE, train = train_obs, alpha = 0.1, center = TRUE)


# 5.0 LOCAL INTERPRETATION (LIME) ----
# 5.1 lime::lime() ----
explainer_caret <- lime::lime(train_obs, fit_caret, n_bins = 5)

class(explainer_caret)

summary(explainer_caret)

# 5.2 lime::explain() ----
explanation_caret <- lime::explain(
    x = local_obs,
    explainer = explainer_caret,
    n_permutations = 5000,
    dist_fun = "gower",
    kernel_width = .75,
    n_features = 5,
    feature_select = "highest_weights",
    labels = "Yes"
)

tibble::glimpse(explanation_caret)

plot_features(explanation_caret)


