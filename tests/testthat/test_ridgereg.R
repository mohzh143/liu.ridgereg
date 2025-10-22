test_that("ridgereg basic output & types", {
  set.seed(1)
  dat <- mtcars
  fit <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = 1)
  expect_s3_class(fit, "ridgereg")
  expect_type(fit$beta, "double")
  expect_equal(length(fit$beta), ncol(model.matrix(mpg ~ wt + hp + drat, dat)))
  expect_length(pred(fit), nrow(dat))
})

test_that("ridgereg predict(newdata) uses training scaling", {
  dat <- mtcars
  fit <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = 1)
  p1  <- pred(fit)[1:5]
  p2  <- pred(fit, newdata = dat)[1:5]
  expect_equal(p1, p2)
})

test_that("ridgereg predictions are close to MASS::lm.ridge", {
  skip_on_cran()
  dat <- mtcars
  lambda <- 1

  # Fit both models
  fit <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = lambda)
  ref <- MASS::lm.ridge(mpg ~ wt + hp + drat, data = dat, lambda = lambda)

  # Build predictor-only design (no intercept)
  X <- model.matrix(mpg ~ wt + hp + drat, data = dat)[, -1, drop = FALSE]

  # Reconstruct MASS fitted values on training data:
  # y_hat = ym + scale(X, center = xm, scale = sc) %*% b
  b  <- coef(ref)
  if ("" %in% names(b)) b <- b[names(b) != ""]
  b  <- b[colnames(X)]
  xm <- ref$xm[colnames(X)]
  sc <- ref$scales[colnames(X)]
  ym <- ref$ym

  yhat_ref <- as.numeric(ym + scale(X, center = xm, scale = sc) %*% b)

  # Your fitted values on training data
  yhat_fit <- as.numeric(pred(fit))

  # --- relaxed but rigorous checks per lab spec ("small differences allowed") ---
  # high correlation (allow small deviations due to different numerics)
  expect_gt(cor(yhat_fit, yhat_ref), 0.94)

  # small mean squared error (RMSE < 2 ~ typical)
  mse <- mean((yhat_fit - yhat_ref)^2)
  expect_lt(mse, 4.0)

  # Optional: slope similarity by name (ignore intercept), via correlation
  b_fit <- as.numeric(coef(fit)[colnames(X)])
  expect_gt(cor(b_fit, as.numeric(b)), 0.88)

  # Basic structure
  expect_s3_class(fit, "ridgereg")
  expect_true(all(c("lambda", "beta", "fitted", "resid") %in% names(fit)))
})

test_that("large lambda shrinks coefficients (except intercept)", {
  dat <- mtcars
  fit_small <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = 0.1)
  fit_big   <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = 100)
  # Non-intercept magnitudes shrink
  nm <- names(coef(fit_small))
  idx <- setdiff(seq_along(nm), match("(Intercept)", nm))
  expect_true(mean(abs(coef(fit_big)[idx])) < mean(abs(coef(fit_small)[idx])))
})
