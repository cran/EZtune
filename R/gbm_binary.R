#----------------------------------------------------------------------
#
#               Optimize GBM using genetic algorithm
#
#----------------------------------------------------------------------


ga.gbm.b <- function(x, y, cv = FALSE, fold = 10) {
  # Make the dataset
  dat = cbind(y, x)

  # Function for computing error rate using resubstitution
  gbm.gars <- function(id, tr, shr) {
    gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli", interaction.depth = id,
                      n.trees = tr, shrinkage = shr, data = dat)
    p <- gbm::predict.gbm(gbm.t, newdata = dat, type="response", n.trees = tr)
    mean(round(p) == y)
  }

  # Function for computing error rate using cross-validation
  gbm.gacv <- function(id, tr, shr) {
    xval <- rep(0, nrow(dat))
    xvs <- rep(1:fold, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:fold) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                        interaction.depth = id, n.trees = tr,
                        shrinkage = shr, data = train)
      xval[xvs == i] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                n.trees = tr)
    }
    mean(round(xval) == y)
  }

  # assess fitness.
  if(!cv) {fun <- gbm.gars} else {fun <- gbm.gacv}

  # Runs the genetic algorithm to determine optimal iter and nu.
  genA <- GA::ga(type = "real-valued", fitness = function(z) fun(z[1], z[2], z[3]),
                 lower = c(1, 50, 0.001), upper = c(10, 5000, 3), popSize = 50,
                 maxiter = 5, run = 5, parallel = 2)

  mod <- gbm::gbm(as.factor(y) ~ ., distribution = "bernoulli",
                  interaction.depth = genA@solution[1, 1],
                  n.trees = round(genA@solution[1, 2]),
                  shrinkage = genA@solution[1, 3],
                  data = dat)

  smry <- matrix(c(genA@solution[1, ], genA@fitnessValue), ncol = 1)
  rownames(smry) <- c("Interaction Depth", "Number of Trees", "Shrinkage", "Accuracy")

  list(summary = smry,
       interaction.depth = genA@solution[1, 1],
       n.trees = round(genA@solution[1, 2]),
       shrinkage = genA@solution[1, 3],
       accuracy = genA@fitnessValue,
       best.model = mod)
}


#----------------------------------------------------------------------
#
#                       Optimize GBM using optim
#
#----------------------------------------------------------------------


opt.gbm.b <- function(x, y, cv = FALSE, fold = 10) {
  # Make the dataset
  dat = cbind(y, x)

  # Function for computing error rate using resubstitution
  gbm.rs <- function(param) {
    gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                      interaction.depth = round(param[1]),
                      n.trees = round(param[2]), shrinkage = param[3],
                      data = dat)
    p <- gbm::predict.gbm(gbm.t, newdata = dat, type="response",
                 n.trees = round(param[2]))
    mean(round(p) != y)
  }

  # Function for computing error rate using cross-validation
  gbm.cv <- function(param) {
    xval <- rep(0, nrow(dat))
    xvs <- rep(1:fold, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:fold) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                        interaction.depth = round(param[1]),
                        n.trees = round(param[2]), shrinkage = param[3],
                        data = train)
      xval[xvs == i] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                n.trees = round(param[2]))
    }
    mean(round(xval) != y)
  }

  # select function to assess fitness
  if(!cv) {fun <- gbm.rs} else {fun <- gbm.cv}

  # optimize on error rate
  opt <- stats::optim(c(2, 500, 0.1), fun, method = "L-BFGS-B",
               lower = c(1, 50, 0.001), upper = c(10, 5000, 3))

  mod <- gbm::gbm(y ~ ., distribution = "bernoulli",
                  interaction.depth = round(opt$par[1]),
                  n.trees = round(opt$par[2]),
                  shrinkage = opt$par[3],
                  data = dat)

  smry <- matrix(c(opt$par, 1 - opt$value), ncol = 1)
  rownames(smry) <- c("Interaction Depth", "Number of Trees",
                      "Shrinkage", "Accuracy")

  list(summary = smry,
       interaction.depth = round(opt$par[1]),
       n.trees = round(opt$par[2]),
       shrinkage = opt$par[3],
       accuracy = 1 - opt$value,
       best.model = mod)
}


#----------------------------------------------------------------------
#
#                     Cross-validation for GBM
#
#----------------------------------------------------------------------


gbm.b.cv <- function(x, y, param, fold) {
  dat <- cbind(y, x)
  xval <- rep(0, nrow(dat))
  xvs <- rep(1:fold, length = nrow(dat))
  xvs <- sample(xvs)
  for(i in 1:fold) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                      interaction.depth = round(param[1]),
                      n.trees = round(param[2]), shrinkage = param[3],
                      data = train)
    xval[xvs == i] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                              n.trees = param[2])
  }
  mean(round(xval) == y)
}
