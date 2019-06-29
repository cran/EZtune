#' @import utils
utils::globalVariables(c("dat"))

#------------------------------------------------------------------------------
#                           SVM regression ga function
#------------------------------------------------------------------------------

svm.reg.ga <- function(x = x, y = y, cross = NULL, fast = FALSE) {

  dat <- cbind(y, x)

  # initialize list
  results <- list()

  #------------------------------------------------------------------------------
  #                           SVM regression resub function
  #------------------------------------------------------------------------------

  # Function for regular speed
  svm.reg.opt.resub <- function(params){
    pr <- NULL
    try(pr <- e1071::svm(y ~ ., data = dat, cost = params[1],
                         gamma = 2^params[2], epsilon = params[3]))
    if(!is.null(pr)){
      mse <- mean((dat[, 1] - pr$fitted)^2)
    } else {
      mse <- 1e+150
    }
    mse
  }


  #------------------------------------------------------------------------------
  #                           SVM regression CV function
  #------------------------------------------------------------------------------

  svm.reg.opt.cv <- function(params, cross) {
    dat <- dat
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


  #------------------------------------------------------------------------------
  #                           SVM regression fast functions
  #------------------------------------------------------------------------------

  svm.reg.pred.fast <- function(x, y, n, cost, gamma, epsilon) {
    dat <- cbind(y, x)
    dat <- dat[sample(nrow(dat)), ]
    train <- dat[c(1:n), ]
    test <- dat[-c(1:n), ]
    svm.t <- e1071::svm(y ~ ., data = train, cost = cost, gamma = gamma,
                        epsilon = epsilon)
    pred <- stats::predict(svm.t, newdata = test[, -1])
    mean((test$y - pred)^2)
  }

  svm.reg.opt.fast <- function(params, n){
    pr <- NULL
    try(pr <- svm.reg.pred.fast(dat[, -1], dat[, 1], n = n, cost = params[1],
                                gamma = 2^params[2], epsilon = params[3]))
    if(!is.null(pr)){
      mse <- pr
    } else {
      mse <- 1e+150
    }
    mse
  }

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(x) {-1 * svm.reg.opt.resub(x)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(x) {-1 * svm.reg.opt.fast(x, n)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(x) {-1 * svm.reg.opt.cv(x, cross)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }

    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(x) {-1 * svm.reg.opt.fast(x, n)}
    results$n <- n
  }

  ga.obj <- GA::ga(type = "real-valued", fitness = fit, parallel = 2,
                   maxiter = 10, run = 5, lower = c(1, -10, 0),
                   upper = c(1042, 5, 0.5))

  results$cost <- as.numeric(ga.obj@solution[1, 1])
  results$gamma <- as.numeric(2^ga.obj@solution[1, 2])
  results$epsilon <- as.numeric(ga.obj@solution[1, 3])
  results$mse <- as.numeric(-1 * ga.obj@fitnessValue)
  results$model <- e1071::svm(y ~ ., data = dat, cost = results$cost,
                              gamma = results$gamma, epsilon = results$epsilon)

  results
}
