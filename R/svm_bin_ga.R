#------------------------------------------------------------------------------
#                           SVM binary ga function
#------------------------------------------------------------------------------

svm.bin.ga <- function(x = x, y = y, cross = NULL, fast = FALSE) {

  dat <- cbind(y, x)

  #------------------------------------------------------------------------------
  #                           SVM binary CV function
  #------------------------------------------------------------------------------

  svm.bin.opt.cv <- function(params, cross) {
    dat <- dat
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


  #------------------------------------------------------------------------------
  #                           SVM binary fast functions
  #------------------------------------------------------------------------------

  svm.bin.pred.fast <- function(x, y, n, cost, gamma) {
    dat <- cbind(y, x)
    dat <- dat[sample(nrow(dat)), ]
    train <- dat[c(1:n), ]
    test <- dat[-c(1:n), ]
    svm.t <- e1071::svm(as.factor(y) ~ ., data = train, cost = cost, gamma = gamma)
    pred <- stats::predict(svm.t, newdata = test[, -1])
    mean(pred != test$y)
  }

  svm.bin.opt.fast <- function(params, n){
    pr <- NULL
    try(pr <- svm.bin.pred.fast(dat[, -1], dat[, 1], n = n, cost = params[1],
                                gamma = 2^params[2]))
    if(!is.null(pr)){
      err <- pr
    } else {
      err <- 1
    }
    err
  }


  #------------------------------------------------------------------------------
  #                           SVM binary resub function
  #------------------------------------------------------------------------------

  # Function for regular speed
  svm.bin.opt.resub <- function(params){
    pr <- NULL
    try(pr <- e1071::svm(as.factor(y) ~ ., data = dat, cost = params[1], gamma = 2^params[2]))
    if(!is.null(pr)){
      err <- mean(dat[, 1] != pr$fitted)
    } else {
      err <- 1
    }
    err
  }
  # initialize list
  results <- list()

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(x) {1 - svm.bin.opt.resub(x)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(x) {1 - svm.bin.opt.fast(x, n)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(x) {1 - svm.bin.opt.cv(x, cross)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }
    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(x) {1 - svm.bin.opt.fast(x, n)}
    results$n <- n
  }

  ga.obj <- GA::ga(type = "real-valued", fitness = fit, parallel = 2,
                   maxiter = 10, run = 5, lower = c(1, -10), upper = c(1042, 5))

  results$cost <- as.numeric(ga.obj@solution[1, 1])
  results$gamma <- as.numeric(2^ga.obj@solution[1, 2])
  results$accuracy <- as.numeric(ga.obj@fitnessValue)
  results$model <- e1071::svm(as.factor(y) ~ ., data = dat,
                              cost = results$cost, gamma = results$gamma)

  results
}

