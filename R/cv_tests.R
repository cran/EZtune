
#------------------------------------------------------------------------------
#                           GBM regression cv error
#------------------------------------------------------------------------------

gbm.reg.cv <- function(x, y, cross, tr, id, nmin, shr) {
  dat <- cbind(y, x)
  yval <- cbind(rep(0, nrow(x)), y)
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, cross)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    gbm.t <- gbm::gbm(y ~ ., distribution = "gaussian",
                      n.trees = tr, interaction.depth = id,
                      n.minobsinnode = nmin, shrinkage = shr, data = train)
    yval[xvs == i, 1] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                          n.trees = round(tr))
  }
  mse <- mean((yval[, 1] - yval[, 2])^2)
  mse
}

gbm.reg.mse.cv <- function(x, y, model, cross = 10) {
  gbm.reg.cv(x, y, cross = cross, tr = model$n.trees,
             id = model$interaction.depth,
             nmin = model$n.minobsinnode,
             shr = model$shrinkage)
}


#------------------------------------------------------------------------------
#                           GBM binary cv accuracy
#------------------------------------------------------------------------------

gbm.bin.cv <- function(x, y, cross, tr, id, nmin, shr) {
  dat <- cbind(y, x)
  yval <- cbind(rep(0, nrow(x)), y)
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, cross)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                      interaction.depth = id, n.trees = tr,
                      shrinkage = shr, data = train, n.minobsinnode = nmin)
    yval[xvs == i, 1] <- round(gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                                n.trees = tr))
  }
  err <- mean(round(yval[, 1]) != yval[, 2])
  err
}

gbm.bin.acc.cv <- function(x, y, model, cross = 10) {

  y <- as.numeric(as.factor(y)) - 1
  1 - gbm.bin.cv(x, y, cross = cross, tr = model$n.trees,
                 id = model$interaction.depth,
                 nmin = model$n.minobsinnode,
                 shr = model$shrinkage)

}

#------------------------------------------------------------------------------
#                           SVM regression cv mse
#------------------------------------------------------------------------------

svm.reg.opt.cv <- function(params, cross) {
  pr <- NULL
  try(pr <- e1071::svm(y ~ ., data = dat, cost = params[1],
                       gamma = 2^params[2], epsilon = params[3],
                       cross = cross))
  if(!is.null(pr)){
    mse <- pr$tot.MSE
  } else {
    mse <- 1e+150
  }
  mse
}

svm.reg.mse.cv <- function(x, y, model, cross = 10) {
  dat <- cbind(y, x)
  pr <- e1071::svm(y ~ ., data = dat, epsilon = model$epsilon,
                   cost = model$cost, gamma = model$gamma,
                   cross = cross)
  pr$tot.MSE
}


#------------------------------------------------------------------------------
#                           SVM binary cv accuracy
#------------------------------------------------------------------------------

svm.bin.opt.cv <- function(params, cross) {
  pr <- NULL
  try(pr <- e1071::svm(as.factor(y) ~ ., data = dat, cost = params[1],
                       gamma = 2^params[2], cross = cross))
  if(!is.null(pr)){
    acc <- pr$tot.accuracy
  } else {
    acc <- 0
  }
  1 - 0.01 * acc
}

svm.bin.acc.cv <- function(x, y, model, cross = 10) {
  dat <- cbind(y, x)

  pr <- e1071::svm(as.factor(y) ~ ., data = dat, cost = model$cost,
                   gamma = model$gamma, cross = cross)
  0.01 * pr$tot.accuracy
}


#------------------------------------------------------------------------------
#                           ADA binary cv accuracy
#------------------------------------------------------------------------------

ada.bin.cv <- function(x, y, cross, nu, iter, maxd) {
  dat <- cbind(y, x)
  yval <- cbind(rep(0, nrow(x)), y)
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, cross)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                      nu = nu, iter = iter, data = train,
                      control = rpart.control(maxdepth = maxd))
    yval[xvs == i, 1] <- stats::predict(ada.t, newdata = test, type = "prob")[,2]
  }
  err <- mean(round(yval[, 1]) != yval[, 2])
  err
}

ada.bin.acc.cv <- function(x, y, model, cross = 10) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- cbind(y, x)

  1 - ada.bin.cv(dat[, -1], dat[, 1], cross = cross,
                 nu = model$nu, iter = model$iter,
                 maxd = model$maxdept)
}



