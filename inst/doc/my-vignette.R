## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(EZtune)

## ---- eval = FALSE------------------------------------------------------------
#  eztune(x, y, method = "svm", optimizer = "hjn", fast = TRUE, cross = NULL, loss = "default")
#  

## ---- eval = FALSE------------------------------------------------------------
#  eztune_cv(x, y, model, cross = 10)
#  

## ---- eval = FALSE------------------------------------------------------------
#  data(lichen)
#  data(lichenTest)
#  data(mullein)
#  data(mulleinTest)
#  

## -----------------------------------------------------------------------------
library(mlbench)
data(Ionosphere)

y <- Ionosphere[, 35]
x <- Ionosphere[, -c(2, 35)]
dim(x)


## -----------------------------------------------------------------------------
ion_default <- eztune(x, y)
ion_default$n
ion_default$loss
eztune_cv(x, y, ion_default)

## -----------------------------------------------------------------------------
ion_svm <- eztune(x, y, fast = FALSE, cross = 3, loss = "auc")
ion_svm$nfold
ion_svm$loss
eztune_cv(x, y, ion_svm)


## -----------------------------------------------------------------------------
ion_gbm <- eztune(x, y, method = "gbm", optimizer = "ga", fast = 50)
ion_gbm$n
ion_gbm$loss
eztune_cv(x, y, ion_gbm)


## -----------------------------------------------------------------------------
data(BostonHousing2)

x <- BostonHousing2[, c(1:4, 7:19)]
y <- BostonHousing2[, 6]
dim(x)



## -----------------------------------------------------------------------------
bh_default <- eztune(x, y)
bh_default$n
bh_default$loss
eztune_cv(x, y, bh_default)


## -----------------------------------------------------------------------------
bh_ga <- eztune(x, y, optimizer = "ga")
bh_ga$n
bh_ga$loss
eztune_cv(x, y, bh_ga)


## -----------------------------------------------------------------------------
bh_gbm <- eztune(x, y, method = "gbm", fast = 0.75, loss = "mae")
bh_gbm$n
bh_gbm$loss
eztune_cv(x, y, bh_gbm)


## -----------------------------------------------------------------------------
library(mlbench)
library(rsample)
data(Sonar)
sonar_split <- initial_split(Sonar, strata = Class)
sonar_train <- training(sonar_split)
sonar_test <- testing(sonar_split)
sonar_folds <- vfold_cv(sonar_train)


## -----------------------------------------------------------------------------
model <- eztune(x = subset(sonar_train, select = -Class), 
                y = sonar_train$Class, method = "svm", optimizer = "hjn", 
                fast = 0.5)

model$loss


## -----------------------------------------------------------------------------
library(yardstick)
predictions <- predict(model, sonar_test)
acc <- accuracy_vec(truth = sonar_test$Class, estimate = predictions[, 1])
auc <- roc_auc_vec(truth = sonar_test$Class, estimate = predictions[, 2])
acc
auc


## -----------------------------------------------------------------------------
library(dplyr)
library(mlbench)
library(rsample)
data(BostonHousing2)
bh <- mutate(BostonHousing2, lcrim = log(crim)) %>%
  dplyr::select(-town, -medv, -crim)
bh_split <- initial_split(bh)
bh_train <- training(bh_split)
bh_test <- testing(bh_split)
bh_folds <- vfold_cv(bh_train)


## -----------------------------------------------------------------------------
model <- eztune(x = subset(bh_train, select = -cmedv), y = bh_train$cmedv, 
                method = "svm", optimizer = "hjn", fast = 0.5)

sqrt(model$loss)


## -----------------------------------------------------------------------------
predictions <- predict(model, bh_test)
rmse <- rmse_vec(truth = bh_test$cmedv, estimate = predictions)
mae <- mae_vec(truth = bh_test$cmedv, estimate = predictions)
rmse
mae


