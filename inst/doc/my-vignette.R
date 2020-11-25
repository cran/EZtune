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


