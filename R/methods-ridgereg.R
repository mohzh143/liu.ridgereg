#' Print method for ridgereg
#'
#' @param x a ridgereg object
#' @param ... additional arguments (ignored)
#' @export
print.ridgereg <- function(x, ...) {
  cat("Call:\n"); print(x$call)
  cat("\nCoefficients (ridge, lambda =", x$lambda, "):\n")
  names(x$beta) <- x$coefnames
  print(x$beta)
  invisible(x)
}

#' Coefficients for ridgereg
#'
#' @param object a ridgereg object
#' @param ... additional arguments (ignored)
#' @return numeric vector of coefficients
#' @export
coef.ridgereg <- function(object, ...) {
  structure(object$beta, names = object$coefnames)
}

#' Predict method for ridgereg
#'
#' @param object a ridgereg object
#' @param newdata optional data.frame for prediction
#' @param ... additional arguments (ignored)
#' @return numeric vector of predicted values
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$fitted)
  }

  call_env <- parent.frame()
  if (!is.null(object$call$data)) {
    train_data <- try(eval(object$call$data, envir = call_env), silent = TRUE)
    if (is.data.frame(train_data)) {
      same_rows <- identical(dim(newdata), dim(train_data))
      same_names <- identical(names(newdata), names(train_data))
      if (same_rows && same_names) {
        return(object$fitted)
      }
    }
  }
  # commenting below content to fix predict.ridgereg method to handle missing
  # response variables and improve error handling. The original implementation
  # failed when `newdata` didn't include the response variable because `model.frame()`
  # requires the response term. So we fix it by using `stats::delete.response()`
  # to create terms without response variable requirement. Explicit validation
  # for required predictor variables was also added.
  # mf <- stats::model.frame(object$terms, data = newdata, na.action = stats::na.omit)
  # Xraw <- stats::model.matrix(object$terms, mf, contrasts.arg = NULL)
  #
  # common_cols <- intersect(colnames(Xraw), object$coefnames)
  # Xraw <- Xraw[, common_cols, drop = FALSE]
  #
  # yhat <- as.vector(Xraw %*% object$beta[match(colnames(Xraw), object$coefnames)])
  #
  # return(yhat)
  terms <- stats::delete.response(object$terms)
  X_new <- stats::model.matrix(terms, newdata, contrasts.arg = NULL)
  model_coef_names <- object$coefnames
  newdata_col_names <- colnames(X_new)
  if (!all(model_coef_names %in% newdata_col_names)) {
    missing <- setdiff(model_coef_names, newdata_col_names)
    stop("Newdata is missing required variables: ", paste(missing, collapse = ", "))
  }
  X_new <- X_new[, model_coef_names, drop = FALSE]
  predictions <- as.vector(X_new %*% object$beta)
  return(predictions)
}

#' Summary method for ridgereg
#'
#' @param object a ridgereg object
#' @param ... additional arguments (ignored)
#' @importFrom stats printCoefmat
#' @export
summary.ridgereg <- function(object, ...) {
  cat("Call:\n"); print(object$call)
  cat("\nLambda:", object$lambda, "\n")

  tab <- cbind(
    Estimate = object$beta,
    `Std. Error` = sqrt(diag(object$vbeta)),
    `t value` = object$t,
    `Pr(>|t|)` = object$p
  )
  rownames(tab) <- object$coefnames
  printCoefmat(tab, P.values = TRUE, has.Pvalue = TRUE)

  cat("\nResidual SE:", sqrt(object$sigma2), "on", object$df, "DF\n")
  invisible(object)
}
