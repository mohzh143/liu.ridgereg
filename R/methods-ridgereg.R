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
  # Return named coefficients
  structure(object$beta, names = object$coefnames)
}

#' Predicted/fitted values for ridgereg
#'
#' @param object a ridgereg object
#' @param newdata optional data.frame for prediction
#' @param ... additional arguments (ignored)
#' @return numeric vector of fitted or predicted values
#' @export
pred.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$fitted)
  }

  Xnew <- model.matrix(object$terms, newdata)
  idx <- seq_len(ncol(Xnew))
  if (object$has_intercept) {
    idx <- setdiff(idx, match("(Intercept)", colnames(Xnew)))
  }

  if (length(idx) > 0) {
    Xnew[, idx] <- scale(Xnew[, idx, drop = FALSE],
                         center = object$x_means[idx],
                         scale = object$x_sds[idx])
  }

  as.numeric(object$y_mean + Xnew %*% object$beta)
}

#' Predict method for ridgereg (alias)
#'
#' @param object a ridgereg object
#' @param newdata optional data.frame for prediction
#' @param ... additional arguments (ignored)
#' @return numeric vector of predicted values
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted)

  mf   <- stats::model.frame(object$terms, data = newdata, na.action = stats::na.omit)
  Xraw <- stats::model.matrix(object$terms, mf)

  X <- Xraw
  idx <- seq_len(ncol(Xraw))
  if (object$has_intercept) idx <- setdiff(idx, match("(Intercept)", colnames(Xraw)))

  if (length(idx) > 0) {
    # use training means/sds
    xm <- object$x_means[colnames(Xraw)]; xs <- object$x_sds[colnames(Xraw)]
    xs[idx][xs[idx] == 0] <- 1
    X[, idx] <- scale(Xraw[, idx, drop=FALSE], center = xm[idx], scale = xs[idx])
  }
  as.vector(X %*% object$beta)
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
  tab <- cbind(Estimate = object$beta,
               `Std. Error` = sqrt(diag(object$vbeta)),
               `t value`    = object$t,
               `Pr(>|t|)`   = object$p)
  rownames(tab) <- object$coefnames
  printCoefmat(tab, P.values = TRUE, has.Pvalue = TRUE)
  cat("\nResidual SE:", sqrt(object$sigma2), "on", object$df, "DF\n")
  invisible(object)
}
