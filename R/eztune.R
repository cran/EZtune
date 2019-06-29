#' Supervised Learning Function
#'
#' \code{eztune} is a function that automatically tunes adaboost, support vector
#' machines, and gradient boosting machines. An optimization algorithm is
#' used to find a good set of tuning parameters for the selected model. The
#' function optimizes on a validation dataset, the resubstitution accuracy,
#' or the cross validated accuracy.
#' @param x Matrix or data frame containing the dependent variables.
#' @param y Vector responses.Can either be a factor or a numeric vector.
#' @param method Model to be fit. Choices are "ada" for adaboost, "gbm" for
#'  gradient boosting machines, and "svm" for support vector machines.
#' @param optimizer Optimization method. Options are "ga" to use a genetic
#'  algorithm and "hjn" to use a Hooke-Jeeves optimizer.
#' @param fast Indicates if the function should use a subset of the
#'  observations when optimizing to speed up calculation time. A value
#'  of TRUE will use the smaller of 50\% of the data or 200 observations
#'  for model fitting, a number between 0 and 1 specifies the proportion
#'  of data that will be used to fit the model, and a postive integer
#'  specifies the number of observations that will be used to fit the
#'  model. A model is computed using a random selection of data and
#'  the remaining data are used to validate model performance.
#'  Validation accuracy or MSE is used as the optimization measure.
#' @param cross If an integer k > 1 is specified, k-fold cross-validation
#'  is used to fit the model. This method is very slow for large datasets.
#'  This parameter is ignored unless \code{fast = FALSE}.
#' @return Function returns a summary of the fitted tuning parameters,
#'  the accuracy or MSE, and the best model.
#'
#' \item{accuracy}{Best accuracy obtained by optimizing algorithm
#' for classification models.}
#' \item{mse}{Best mean squared error obtained by optimizing algorithm
#' for regression models.}
#' \item{model}{Model using optimized parameters. Adaboost model
#'  comes from package ada (ada object), gbm model comes from package gbm
#'  (gbm.object object), svm (svm object) model comes from package e1071.}
#' \item{n}{Number of observations used in model training when
#' fast option is used}
#' \item{nfold}{Number of folds used if cross validation is used
#' for optimization.}
#' \item{cost}{Tuning parameter for svm.}
#' \item{gamma}{Tuning parameter for svm.}
#' \item{epsilon}{Tuning parameter for svm regression.}
#' \item{iter}{Tuning parameter for adaboost.}
#' \item{nu}{Tuning parameter for adaboost.}
#' \item{shrinkage}{Tuning parameter for adaboost and gbm.}
#' \item{n.trees}{Tuning parameter for gbm.}
#' \item{interaction.depth}{Tuning parameter for gbm.}
#' \item{n.minobsinnode}{Tuning parameter for gbm.}
#'
#' @examples
#' library(mlbench)
#' data(Sonar)
#' sonar <- Sonar[sample(1:nrow(Sonar), 100), ]
#'
#' y <- sonar[, 61]
#' x <- sonar[, 1:10]
#'
#' # Optimize an SVM using the default fast setting and Hooke-Jeeves
#' eztune(x, y)
#'
#' # Optimize an SVM with 3-fold cross validation and Hooke-Jeeves
#' eztune(x, y, fast = FALSE, cross = 3)
#'
#' # Optimize GBM using training set of 50 observations and Hooke-Jeeves
#' eztune(x, y, method = "gbm", fast = 50)
#'
#' # Optimize SVM with 25% of the observations as a training dataset
#' # using a genetic algorithm
#' eztune(x, y, method = "svm", optimizer = "ga", fast = 0.25)
#'
#' @export
#'
eztune <- function(x, y, method = "svm", optimizer = "hjn", fast = TRUE,
                   cross = NULL) {

  if(length(unique(y)) == 2) {
    y <- as.numeric(as.factor(y)) - 1
    type <- "bin"
  } else {
    y <- as.numeric(as.character(y))
    type <- "reg"
  }

  if(fast > 1) {
    fast <- round(fast)
  }

  if(!is.null(cross)) {
    cross <- round(cross)
  }


  command <- paste(type, method, optimizer, sep = ".")

  ezt <- switch(command,
                bin.ada.ga = ada.bin.ga(x, y, cross = cross, fast = fast),
                bin.ada.hjn = ada.bin.hjn(x, y, cross = cross, fast = fast),
                bin.gbm.ga = gbm.bin.ga(x, y, cross = cross, fast = fast),
                bin.gbm.hjn = gbm.bin.hjn(x, y, cross = cross, fast = fast),
                bin.svm.ga = svm.bin.ga(x, y, cross = cross, fast = fast),
                bin.svm.hjn = svm.bin.hjn(x, y, cross = cross, fast = fast),
                reg.gbm.ga = gbm.reg.ga(x, y, cross = cross, fast = fast),
                reg.gbm.hjn = gbm.reg.hjn(x, y, cross = cross, fast = fast),
                reg.svm.ga = svm.reg.ga(x, y, cross = cross, fast = fast),
                reg.svm.hjn = svm.reg.hjn(x, y, cross = cross, fast = fast)
  )


  ezt
}
