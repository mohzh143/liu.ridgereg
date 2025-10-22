#' Ridge regression (LiU Lab Bonus version)
#'
#' Fits a ridge regression model using centered and standardized predictors.
#' The intercept term is not penalized. The method returns coefficients,
#' fitted values, residuals, and inference statistics.
#'
#' @param formula model formula
#' @param data data.frame
#' @param lambda nonnegative tuning parameter (scalar)
#' @param method character, one of c("normal","qr"), default "normal"
#' @return an object of class 'ridgereg'
#' @examples
#' fit <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)
#' summary(fit)
#' @export
ridgereg <- function(formula, data, lambda, method = c("normal", "qr")) {
  stopifnot(is.numeric(lambda), length(lambda) == 1, lambda >= 0)
  method <- match.arg(method)

  # setup
  mf   <- stats::model.frame(formula = formula, data = data, na.action = stats::na.omit)
  trm  <- stats::terms(mf)
  Xraw <- stats::model.matrix(trm, mf)  # includes intercept
  y    <- stats::model.response(mf)
  stopifnot(is.numeric(y))

  has_int <- "(Intercept)" %in% colnames(Xraw)

  # center/scale predictors
  idx <- seq_len(ncol(Xraw))
  if (has_int) idx <- setdiff(idx, match("(Intercept)", colnames(Xraw)))

  x_means <- rep(0, ncol(Xraw)); names(x_means) <- colnames(Xraw)
  x_sds   <- rep(1, ncol(Xraw)); names(x_sds)   <- colnames(Xraw)

  if (length(idx) > 0) {
    x_means[idx] <- colMeans(Xraw[, idx, drop = FALSE])
    x_sds[idx]   <- apply(Xraw[, idx, drop = FALSE], 2, stats::sd)
    x_sds[idx][x_sds[idx] == 0] <- 1
    Xraw[, idx] <- scale(Xraw[, idx, drop = FALSE],
                         center = x_means[idx], scale = x_sds[idx])
  }
  X <- Xraw
  y_mean <- mean(y)
  y_centered <- y - y_mean

  n <- nrow(X)
  p <- ncol(X)

  # ridge fit
  if (method == "normal") {
    pen <- diag(p)
    if (has_int) pen[match("(Intercept)", colnames(X)), match("(Intercept)", colnames(X))] <- 0
    XtX <- crossprod(X)
    Xty <- crossprod(X, y_centered)
    beta <- solve(XtX + lambda * pen, Xty)
  } else {
    rlam <- sqrt(lambda)
    pen_cols <- seq_len(p)
    if (has_int) pen_cols <- setdiff(pen_cols, match("(Intercept)", colnames(X)))
    A <- matrix(0, nrow = length(pen_cols), ncol = p)
    for (j in seq_along(pen_cols)) A[j, pen_cols[j]] <- rlam
    Z <- rbind(X, A)
    z <- c(y_centered, rep(0, nrow(A)))
    qrZ <- qr(Z)
    beta <- qr.coef(qrZ, z)
  }

  # fitted values
  yhat_centered <- as.vector(X %*% beta)
  yhat <- y_mean + yhat_centered
  resid <- y - yhat

  # residual variance
  df <- n - qr(X)$rank
  sigma2 <- sum(resid^2) / max(1, df)

  # approximate inference
  pen <- diag(p)
  if (has_int) pen[match("(Intercept)", colnames(X)), match("(Intercept)", colnames(X))] <- 0
  XtX <- crossprod(X)
  M   <- solve(XtX + lambda * pen)
  vbeta <- M %*% XtX %*% M * sigma2
  se    <- sqrt(diag(vbeta))
  tval  <- beta / se
  pval  <- 2 * (1 - stats::pt(abs(tval), df = df))

  # output object
  names(beta) <- colnames(X)
  out <- list(
    call = match.call(),
    terms = trm,
    lambda = lambda,
    beta = as.numeric(beta),
    coefnames = colnames(X),
    fitted = yhat,
    resid = resid,
    df = df,
    sigma2 = sigma2,
    vbeta = vbeta,
    t = as.numeric(tval),
    p = as.numeric(pval),
    has_intercept = has_int,
    x_means = x_means,
    x_sds = x_sds,
    y_mean = y_mean
  )
  class(out) <- "ridgereg"
  out
}
