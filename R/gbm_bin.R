
#------------------------------------------------------------------------------
#                           GBM binary hjn function
#------------------------------------------------------------------------------

gbm.bin.hjn <- function(x = x, y = y, cross = cross, fast = fast) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- cbind(y, x)

  # initialize list
  results <- list()

  #------------------------------------------------------------------------------
  #                           GBM binary resub function
  #------------------------------------------------------------------------------

  gbm.bin.opt.resub <- function(params, dat){
    pr <- NULL
    try(pr <- gbm::gbm(y ~ ., distribution = "bernoulli",
                       n.trees = round(params[1]),
                       interaction.depth = round(params[2]),
                       n.minobsinnode = round(params[3]),
                       shrinkage = params[4],
                       data = dat))
    if(!is.null(pr)){
      pred <- gbm::predict.gbm(pr, newdata = dat, type="response",
                               n.trees = round(params[1]))
      err <- mean(dat[, 1] != round(pred))
    } else {
      err <- 1
    }
    err
  }


  #------------------------------------------------------------------------------
  #                           GBM binary CV functions
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

  gbm.bin.opt.cv <- function(params, cross, dat) {
    pr <- NULL
    try(pr <- gbm.bin.cv(dat[, -1], as.numeric(as.factor(dat[, 1])) - 1,
                         cross = cross, tr = round(params[1]), id = round(params[2]),
                         nmin = round(params[3]), shr = params[4]))
    if(!is.null(pr)){
      err <- pr
    } else {
      err <- 1
    }
    err
  }



  #------------------------------------------------------------------------------
  #                           GBM binary fast functions
  #------------------------------------------------------------------------------

  gbm.bin.pred.fast <- function(x, y, n, tr, id, nmin, shr) {
    dat <- cbind(y, x)
    dat <- dat[sample(nrow(dat)), ]
    train <- dat[c(1:n), ]
    test <- dat[-c(1:n), ]
    gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                      interaction.depth = id, n.trees = tr,
                      shrinkage = shr, data = train, n.minobsinnode = nmin)
    pred <- round(gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                   n.trees = tr))
    mean(pred != test$y)
  }

  gbm.bin.opt.fast <- function(params, n, dat){
    pr <- NULL
    try(pr <- gbm.bin.pred.fast(dat[, -1], dat[, 1], n = n, tr = round(params[1]),
                                id = round(params[2]),
                                nmin = round(params[3]), shr = params[4]))
    if(!is.null(pr)){
      err <- pr
    } else {
      err <- 1
    }
    err
  }

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(x) {gbm.bin.opt.resub(x, dat)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(x) {gbm.bin.opt.fast(x, n, dat)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(x) {gbm.bin.opt.cv(x, cross, dat)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }

    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(x) {gbm.bin.opt.fast(x, n, dat)}
    results$n <- n
  }

  hjn.obj <- optimx::hjn(par = c(500, 5, 8, 0.09), fn = fit,
                         lower = c(50, 1, 5, 0.001),
                         upper = c(3000, 15, 12, 0.1))

  results$n.trees <- as.integer(round(hjn.obj$par[1]))
  results$interaction.depth <- as.integer(round(hjn.obj$par[2]))
  results$n.minobsinnode <- as.integer(round(hjn.obj$par[3]))
  results$shrinkage <- as.numeric(hjn.obj$par[4])
  results$accuracy <- as.numeric(1 - hjn.obj$value)
  results$model <- gbm::gbm(y ~ ., distribution = "bernoulli", data = dat,
                            n.trees = results$n.trees,
                            interaction.depth = results$interaction.depth,
                            n.minobsinnode = results$n.minobsinnode,
                            shrinkage = results$shrinkage)

  results
}


