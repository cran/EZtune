
#------------------------------------------------------------------------------
#                           GBM regression ga function
#------------------------------------------------------------------------------

gbm.reg.ga <- function(x = x, y = y, cross = NULL, fast = FALSE) {

  dat <- cbind(y, x)

  # initialize list
  results <- list()

  #------------------------------------------------------------------------------
  #                           GBM regression resub function
  #------------------------------------------------------------------------------

  # Function for regular speed
  gbm.reg.opt.resub <- function(params){
    pr <- NULL
    try(pr <- gbm::gbm(y ~ ., distribution = "gaussian",
                       n.trees = round(params[1]),
                       interaction.depth = round(params[2]),
                       n.minobsinnode = round(params[3]),
                       shrinkage = params[4],
                       data = dat))
    if(!is.null(pr)){
      pred <- gbm::predict.gbm(pr, newdata = dat, type = "response",
                               n.trees = round(params[1]))
      mse <- mean((dat[, 1] - pred)^2)
    } else {
      mse <- 1e+150
    }
    mse
  }


  #------------------------------------------------------------------------------
  #                           GBM regression CV function
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

  gbm.reg.opt.cv <- function(params, cross) {
    pr <- NULL
    try(pr <- gbm.reg.cv(dat[, -1], dat[, 1],
                         cross = cross, tr = round(params[1]),
                         id = round(params[2]), nmin = round(params[3]),
                         shr = params[4]))
    if(!is.null(pr)){
      mse <- pr
    } else {
      mse <- 1e+150
    }
    mse
  }



  #------------------------------------------------------------------------------
  #                           GBM regression fast functions
  #------------------------------------------------------------------------------

  gbm.reg.pred.fast <- function(x, y, n, tr, id, nmin, shr) {
    dat <- cbind(y, x)
    dat <- dat[sample(nrow(dat)), ]
    train <- dat[c(1:n), ]
    test <- dat[-c(1:n), ]
    gbm.t <- gbm::gbm(y ~ ., distribution = "gaussian",
                      n.trees = tr, interaction.depth = id,
                      n.minobsinnode = nmin, shrinkage = shr, data = train)
    pred <- stats::predict(gbm.t, newdata = test, type="response",
                           n.trees = round(tr))
    mean((test$y - pred)^2)
  }

  gbm.reg.opt.fast <- function(params, n){
    pr <- NULL
    try(pr <- gbm.reg.pred.fast(dat[, -1], dat[, 1], n = n, tr = round(params[1]),
                                id = round(params[2]), nmin = round(params[3]),
                                shr = params[4]))
    if(!is.null(pr)){
      mse <- pr
    } else {
      mse <- 1e+150
    }
    mse
  }

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(x) {-1 * gbm.reg.opt.resub(x)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(x) {-1 * gbm.reg.opt.fast(x, n)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(x) {-1 * gbm.reg.opt.cv(x, cross)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }

    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(x) {-1 * gbm.reg.opt.fast(x, n)}
    results$n <- n
  }

  ga.obj <- GA::ga(type = "real-valued", fitness = fit, parallel = 2,
                   maxiter = 10, run = 5, lower = c(50, 1, 5, 0.001),
                   upper = c(5000, 15, 10, 0.1))

  results$n.trees <- as.integer(round(ga.obj@solution[1, 1]))
  results$interaction.depth <- as.integer(round(ga.obj@solution[1, 2]))
  results$n.minobsinnode <- as.integer(round(ga.obj@solution[1, 3]))
  results$shrinkage <- as.numeric(ga.obj@solution[1, 4])
  results$mse <- as.numeric(-1 * ga.obj@fitnessValue)
  results$model <- gbm::gbm(y ~ ., distribution = "gaussian", data = dat,
                            n.trees = results$n.trees,
                            interaction.depth = results$interaction.depth,
                            n.minobsinnode = results$n.minobsinnode,
                            shrinkage = results$shrinkage)

  results
}
