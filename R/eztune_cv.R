#' Cross Validated Accuracy for Supervised Learning Model
#'
#' \code{eztune_cv} returns the cross-validated
#' accuracy for a model returned by \code{eztune}.
#' The function \code{eztune} can tune a model using validation data,
#' resubstitution or cross validation. If resubstitution or a fast method
#' is used to tune the model, the accuracy obtained from the function
#' may not be accurate. The function
#' \code{eztune_cv} will return a cross-validated accuracy for such a model.
#' @param x Matrix or data frame containing the dependent variables used
#' to create the model.
#' @param y Vector of the response used to create the model. Can be either
#' numeric or a factor.
#' @param model Object generated with the function \code{eztune}.
#' @param cross Number of folds to use for n-fold cross-validation.
#' @return Function returns a numeric value that represents the
#' cross-validated accuracy of the model.
#'
#' @examples
#' library(mlbench)
#' data(Sonar)
#' sonar <- Sonar[sample(1:nrow(Sonar), 100), ]
#'
#' y <- sonar[, 61]
#' x <- sonar[, 1:10]
#'
#' sonar_default <- eztune(x, y)
#' eztune_cv(x, y, sonar_default)
#'
#' sonar_svm <- eztune(x, y, fast = FALSE, cross = 3)
#' eztune_cv(x, y, sonar_svm)
#'
#' sonar_gbm <- eztune(x, y, method = "gbm", fast = 50)
#' eztune_cv(x, y, sonar_gbm)
#'
#'
#' @export
#'
eztune_cv <- function(x, y, model, cross = 10) {

  if(length(unique(y)) == 2) {
    y <- as.numeric(as.factor(y)) - 1
    type <- "bin.acc"
  } else {
    y <- as.numeric(as.character(y))
    type <- "reg.mse"
  }

  mod <- substr(class(model$model)[1], 1, 3)
  type <- paste(mod, type, "cv", sep = ".")

  res <- switch(type,
                ada.bin.acc.cv = ada.bin.acc.cv(x, y, model, cross = cross),
                gbm.bin.acc.cv = gbm.bin.acc.cv(x, y, model, cross = cross),
                svm.bin.acc.cv = svm.bin.acc.cv(x, y, model, cross = cross),
                gbm.reg.mse.cv = gbm.reg.mse.cv(x, y, model, cross = cross),
                svm.reg.mse.cv = svm.reg.mse.cv(x, y, model, cross = cross))

  res
}
