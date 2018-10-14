#----------------------------------------------------------------------
#
#             Optimize adaboost using genetic algorithm
#
#----------------------------------------------------------------------

ga.ada.b <- function(x, y, cv = FALSE, fold = 10) {
  # Creates a dataset using x and y
  dat <- cbind(y, x)

  # Function computes the accuracy of adaboost using resubstitution. It
  # is the default fitness function because it is so much faster than
  # cross-validation.
  ada.gars <- function(iter, nu) {
    ada.r <- ada::ada(as.factor(y) ~ ., loss = "exponential", iter = iter,
                      nu = nu, data = dat)
    mean(round(stats::predict(ada.r, newdata = x, type = "prob")[,2]) == y)
  }

  # function that computes the accuracy for adaboost using cross-validation.
  # The accuracy is used as the fitness function for the genetic algorithm
  # when cv = TRUE.
  ada.gacv <- function(iter, nu) {
    xval <- rep(0, nrow(x))
    xvs <- rep(1:fold, length = nrow(x))
    xvs <- sample(xvs)
    for(i in 1:fold) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential", iter = iter,
                        nu = nu, data = train)
      xval[xvs == i] <- stats::predict(ada.t, newdata = test, type = "prob")[,2]
    }
    mean(round(xval) == y)
  }

  # identifies if resubstitution or cross-validation will be used to
  # assess fitness.
  if(!cv) {fun <- ada.gars} else {fun <- ada.gacv}

  # Runs the genetic algorithm to determine optimal iter and nu.
  genA <- GA::ga(type = "real-valued", fitness = function(z) fun(z[1], z[2]),
                 lower = c(10, 0.01), upper = c(100, 3), popSize = 50,
                 maxiter = 5, run = 5, parallel = 2)

  # produces the summary of the genetic algorithm
  mod <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                  iter = genA@solution[1, 1],
                  nu = genA@solution[1, 2], data = dat)

  smry <- matrix(c(genA@solution[1, ], genA@fitnessValue), ncol = 1)
  rownames(smry) <- c("Iter", "Nu", "Accuracy")

  list(iter = genA@solution[1, 1],
       nu = genA@solution[1, 2],
       accuracy = genA@fitnessValue,
       loss = "exponential",
       summary = smry,
       best.model = mod)
}


#----------------------------------------------------------------------
#
#                   Optimize adaboost using optim
#
#----------------------------------------------------------------------

opt.ada.b <- function(x, y, cv = FALSE, fold = 10) {
  # make a dataframe from the x matrix and y vector.
  dat <- cbind(y, x)

  # Function for estimating error rate using resubstitution
  ada.rs <- function(param) {
    ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential", data = dat,
                      iter = param[1], nu = param[2])
    p <- stats::predict(ada.t, newdata = dat, type = "prob")[,2]
    mean(round(p) != y)
  }

  # Function for estimating error rate using cross-validation
  ada.cv <- function(param) {
    xval <- rep(0, nrow(dat))
    xvs <- rep(1:fold, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:fold) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential", data = train,
                        iter = param[1], nu = param[2])
      xval[xvs == i] <- round(stats::predict(ada.t, newdata = test,
                                           type = "prob")[,2])
    }
    mean(xval != y)
  }

  # Select the correct function for optimization
  if(!cv) {fun <- ada.rs} else {fun <- ada.cv}

  # Optimization function
  opt <- stats::optim(c(100, 0.5), fn = fun, method = "L-BFGS-B",
               lower = c(10, 0.01),
               upper = c(1000, 5))

  mod <- ada::ada(as.factor(y) ~ ., loss = "exponential", data = dat,
                 iter = opt$par[1], nu = opt$par[2])

  smry <- matrix(c(opt$par, 1 - opt$value), ncol = 1)
  rownames(smry) <- c("Iter", "Nu", "Accuracy")

  list(iter = opt$par[1],
       nu = opt$par[2],
       accuracy = 1 - opt$value,
       loss = "exponential",
       summary = smry,
       best.model = mod)

}


#----------------------------------------------------------------------
#
#                   Cross-validation for adaboost
#
#----------------------------------------------------------------------

ada.b.cv <- function(x, y, param, fold) {
  dat <- cbind(y, x)
  xval <- rep(0, nrow(dat))
  xvs <- rep(1:fold, length = nrow(dat))
  xvs <- sample(xvs)
  for(i in 1:fold) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential", data = train,
                      iter = param[1], nu = param[2])
    xval[xvs == i] <- round(stats::predict(ada.t, newdata = test,
                                         type = "prob")[, 2])
  }
  mean(xval == y)
}
