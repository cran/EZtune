#' @importFrom rpart rpart.control
#'
#------------------------------------------------------------------------------
#                           ADA binary hjn function
#------------------------------------------------------------------------------

ada.bin.hjn <- function(x = x, y = y, cross = cross, fast = fast) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- cbind(y, x)

  # initialize list
  results <- list()

  #------------------------------------------------------------------------------
  #                           ADA binary resub function
  #------------------------------------------------------------------------------

  # Function for regular speed
  ada.bin.opt.resub <- function(params){
    pr <- NULL
    # dat$y <- as.numeric(as.factor(dat[, 1])) - 1
    try(pr <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                       nu = params[1], iter = round(params[2]),
                       data = dat,
                       control = rpart::rpart.control(maxdepth = round(params[3]))))
    if(!is.null(pr)){
      pred <- stats::predict(pr, newdata = dat, type = "prob")[,2]
      err <- mean(dat$y != round(pred))
    } else {
      err <- 1
    }
    err
  }


  #------------------------------------------------------------------------------
  #                           ADA binary CV functions
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
                        control = rpart::rpart.control(maxdepth = maxd))
      yval[xvs == i, 1] <- stats::predict(ada.t, newdata = test, type = "prob")[,2]
    }
    err <- mean(round(yval[, 1]) != yval[, 2])
    err
  }

  ada.bin.opt.cv <- function(params, cross) {
    pr <- NULL
    try(pr <- ada.bin.cv(dat[, -1], dat[, 1], cross = cross,
                         nu = round(params[1]), iter = round(params[2]),
                         maxd = round(params[3])))
    if(!is.null(pr)){
      err <- pr
    } else {
      err <- 1
    }
    err
  }



  #------------------------------------------------------------------------------
  #                           ADA binary fast functions
  #------------------------------------------------------------------------------

  ada.bin.pred.fast <- function(x, y, n, nu, iter, maxd) {
    dat <- cbind(y, x)
    dat <- dat[sample(nrow(dat)), ]
    train <- dat[c(1:n), ]
    test <- dat[-c(1:n), ]
    ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                      nu = nu, iter = iter, data = train,
                      control = rpart::rpart.control(maxdepth = maxd))
    pred <- round(stats::predict(ada.t, newdata = test, type = "prob")[,2])
    mean(pred != test$y)
  }

  ada.bin.opt.fast <- function(params, n){
    pr <- NULL
    try(pr <- ada.bin.pred.fast(dat[, -1], dat[, 1], n = n,
                                nu = round(params[1]), iter = round(params[2]),
                                maxd = round(params[3])))
    if(!is.null(pr)){
      err <- pr
    } else {
      err <- 1
    }
    err
  }

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(x) {ada.bin.opt.resub(x)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(x) {ada.bin.opt.fast(x, n)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(x) {ada.bin.opt.cv(x, cross)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }

    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(x) {ada.bin.opt.fast(x, n)}
    results$n <- n
  }

  hjn.obj <- optimx::hjn(par = c(0.7, 100, 2), fn = fit,
                         lower = c(0.01, 50, 1),
                         upper = c(1, 300, 20))

  results$nu <- as.numeric(round(hjn.obj$par[1]))
  results$iter <- as.integer(round(hjn.obj$par[2]))
  results$maxdepth <- as.integer(round(hjn.obj$par[3]))
  results$accuracy <- as.numeric(1 - hjn.obj$value)
  results$model <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                            nu = results$nu, iter = results$iter,
                            data = dat,
                            control = rpart::rpart.control(maxdepth = results$maxdepth))

  results
}



