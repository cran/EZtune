#' Tuning supervised learning models.
#'
#' The EZtune package contains two functions and four datasets. The functions
#' are eztune and eztune.cv. The datasets are lichen, lichenTest, mullein, and
#' mulleinTest
#'
#' @section Eztune functions:
#' The EZtune functions are eztune and eztune.cv. The function eztune will
#' autotune an adaboost, gradient boosting machine, and support vector
#' machine using a genetic algorithm or a quasi-Newton-Raphson optimizer.
#' The eztune.cv function returns the cross-validated error for an object
#' produced by eztune.
#'
#' @section EZtune datasets:
#' The lichen dataset has 840 observations and 40 variables. Seven of the
#' variables are binary response variables. The lichenTest dataset is a test
#' dataset for the lichen dataset that consists of 300 observations and
#' the same 40 variables.
#'
#' The mullein dataset consists of 12094 observations and 32 variables. One
#' variable is a binary response. The mulleinTest dataset is a test
#' dataset for the mullein data that consists of 1512 observations
#' and the same 32 variables as mullein.
#'
"_PACKAGE"

#' Supervised Learning Function
#'
#' eztune is a single function that will tune adaboost, support vector
#' machines, and gradient boosting machines. It currently only
#' tunes models with a binary response. An optimizer is used to find
#' a good set of tuning parameters for the selected model. The
#' function optimizes on either the resubstitution accuracy or the
#' cross-validated accuracy.
#' @param x Matrix or data frame containing the dependent variables.
#' @param y Numeric vector of 0s and 1s for the response.
#' @param type Type of response (binary is the only option at this time).
#' @param method Model to be fit. Choices are "ada" for adaboost, "gbm" for
#'  gradient boosting machines, and "svm" for support vector machines.
#' @param optimizer Optimization method. Options are "ga" to use a genetic
#'  algorithm to optimize and "optim" to use a quasi-Newton-Raphson optimizer.
#' @param cv Indicates if the cross-validation accuracy should be used to fit.
#'  FALSE to use resubstitution accuracy to optimize (faster but less accurate)
#'  and TRUE to use cross-validation accuracy (slower and more accurate).
#' @param fold The number of folds to use for n-fold cross validation. This is
#'  ignored if cv = FALSE.
#' @keywords adaboost, svm, gbm, tuning
#' @return Function returns a summary of the fitted tuning parameters,
#'  the accuracy, and the best model.
#'
#' \item{summary}{Matrix that contains the values of the tuning parameters
#'  and the final accuracy.}
#' \item{accuracy}{Best accuracy obtained by optimizing algorithm.}
#' \item{best.model}{Model using optimized parameters. Adaboost model
#'  comes from package ada, gbm model comes from package gbm, svm
#'  model comes from package e1071.}
#' \item{iter}{Tuning parameter for adaboost.}
#' \item{nu}{Tuning parameter for adaboost.}
#' \item{loss}{Loss used in adaboost fitting.}
#' \item{interaction.depth}{Tuning parameter for gbm.}
#' \item{n.trees}{Tuning parameter for gbm.}
#' \item{shrinkage}{Tuning parameter for gbm.}
#' \item{cost}{Tuning parameter for svm.}
#' \item{epsilon}{Tuning parameter for svm.}
#' \item{kernel}{Kernel used in svm fitting.}
#'
#' @examples
#' library(mlbench)
#' data(Glass)
#'
#' glass <- Glass[as.numeric(as.character(Glass$Type)) < 3, ]
#' glass <- glass[sample(1:nrow(glass), 80), ]
#' y <- ifelse(glass$Type == 1, 0, 1)
#' x <- glass[, 1:9]
#'
#' eztune(x, y, type = "binary", method = "gbm", optimizer = "optim")
#'
#' @export
#'
eztune <- function(x, y, type = "binary", method = "ada",
                   optimizer = "optim", cv = FALSE, fold = 10) {

  command <- paste(type, method, optimizer, sep = ".")

  ezt <- switch(command,
                binary.ada.ga = ga.ada.b(x, y, cv = cv, fold = fold),
                binary.ada.optim = opt.ada.b(x, y, cv = cv, fold = fold),
                binary.gbm.ga = ga.gbm.b(x, y, cv = cv, fold = fold),
                binary.gbm.optim = opt.gbm.b(x, y, cv = cv, fold = fold),
                binary.svm.ga = ga.svm.b(x, y, cv = cv, fold = fold),
                binary.svm.optim = opt.svm.b(x, y, cv = cv, fold = fold))

  ezt
}
