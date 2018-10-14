#----------------------------------------------------------------------
#
#               Optimize SVM using genetic algorithm
#
#----------------------------------------------------------------------

ga.svm.b <- function(x, y, cv = FALSE, fold = 10) {

  dat <- cbind(y, x)

  svm.rs <- function(epsilon, cost) {
    s <- e1071::svm(as.factor(y) ~ ., epsilon = epsilon, cost = cost,
                    kernel = "radial", data = dat)
    mean(stats::predict(s) == y)
  }

  svm.cv = function(epsilon, cost) {
    s <- e1071::svm(as.factor(y) ~ ., epsilon = epsilon, cost = cost,
                    kernel = "radial", data = dat, cross = fold)
    s$tot.accuracy / 100
  }

  if(!cv) {fun <- svm.rs} else {fun <- svm.cv}

  genA <- GA::ga(type = "real-valued", fitness = function(z) fun(z[1], z[2]),
                 lower = c(0.01, 0.1), upper = c(3, 100), popSize = 50,
                 maxiter = , run = 5, parallel = 2)

  mod <- e1071::svm(as.factor(y) ~ ., epsilon = genA@solution[1, 1],
                    cost = genA@solution[1, 2],
                    kernel="radial", data = dat)

  smry <- matrix(c(genA@solution[1, ], genA@fitnessValue), ncol = 1)
  rownames(smry) <- c("Epsilon", "Cost", "Accuracy")

  list(epsilon = genA@solution[1, 1],
       cost = genA@solution[1, 2],
       accuracy = genA@fitnessValue,
       kernel = "radial",
       summary = smry,
       best.model = mod)

}


#----------------------------------------------------------------------
#
#                       Optimize SVM using optim
#
#----------------------------------------------------------------------


opt.svm.b <- function(x, y, cv = FALSE, fold = 10) {
  # make a dataframe from the x matrix and y vector.
  dat <- cbind(y, x)

  # Function for error rate using resubstitution
  svm.rs <- function(param) {
    s <- e1071::svm(as.factor(y) ~ ., epsilon = param[1], cost = param[2],
                    kernel = "radial", data = dat)
    1 - mean(stats::predict(s) == y)
  }

  # Function for error rate using cross validation
  svm.cv <- function(param) {
    s <- e1071::svm(as.factor(y) ~ ., epsilon = param[1], cost = param[2],
                    kernel = "radial", data = dat, cross = fold)
    1 - s$tot.accuracy / 100
  }

  # Selects the correct function based on cv choice
  if(!cv) {fun <- svm.rs} else {fun <- svm.cv}

  # optimize on error rate
  opt <- stats::optim(c(0.1, 1), fun, method = "L-BFGS-B",
               lower = c(0.01, 0.1), upper = c(3, 100))

  mod <- e1071::svm(as.factor(y) ~ ., epsilon = opt$par[1], cost = opt$par[2],
                    kernel = "radial", data = dat)

  smry <- matrix(c(opt$par, 1 - opt$value), ncol = 1)
  rownames(smry) <- c("Epsilon", "Cost", "Accuracy")

  list(summary = smry,
       epsilon = opt$par[1],
       cost = opt$par[2],
       accuracy = 1 - opt$value,
       kernel = "radial",
       best.model = mod)
}


#----------------------------------------------------------------------
#
#                     Cross-validation for SVM
#
#----------------------------------------------------------------------

svm.b.cv <- function(x, y, param, fold) {
  dat <- cbind(y, x)
  s <- e1071::svm(as.factor(y) ~ ., epsilon = param[1], cost = param[2],
                  kernel = "radial", data = dat, cross = fold)
  s$tot.accuracy / 100

}
