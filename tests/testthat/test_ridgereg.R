test_that("ridgereg basic output & types", {
  set.seed(1)
  dat <- mtcars
  fit <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = 1)
  expect_s3_class(fit, "ridgereg")
  expect_type(fit$beta, "double")
  expect_equal(length(fit$beta), ncol(model.matrix(mpg ~ wt + hp + drat, dat)))
  expect_length(predict(fit), nrow(dat))
})

test_that("ridgereg predict(newdata) uses training scaling", {
  dat <- mtcars
  fit <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = 1)
  p1  <- predict(fit)[1:5]
  p2  <- predict(fit, newdata = dat)[1:5]
  expect_equal(p1, p2)
})

test_that("ridgereg predictions are close to MASS::lm.ridge", {
  skip_on_cran()
  dat <- mtcars
  lambda <- 1

  fit <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = lambda)
  ref <- MASS::lm.ridge(mpg ~ wt + hp + drat, data = dat, lambda = lambda)

  X <- model.matrix(mpg ~ wt + hp + drat, data = dat)[, -1, drop = FALSE]

  b  <- coef(ref)
  if ("" %in% names(b)) b <- b[names(b) != ""]
  b  <- b[colnames(X)]
  xm <- ref$xm[colnames(X)]
  sc <- ref$scales[colnames(X)]
  ym <- ref$ym
  yhat_ref <- as.numeric(ym + scale(X, center = xm, scale = sc) %*% b)

  yhat_fit <- as.numeric(predict(fit))

  # "small differences allowed"
  expect_gt(cor(yhat_fit, yhat_ref), 0.94)

  mse <- mean((yhat_fit - yhat_ref)^2)
  expect_lt(mse, 4.0)

  b_fit <- as.numeric(coef(fit)[colnames(X)])
  expect_gt(cor(b_fit, as.numeric(b)), 0.88)

  expect_s3_class(fit, "ridgereg")
  expect_true(all(c("lambda", "beta", "fitted", "resid") %in% names(fit)))
})

test_that("large lambda shrinks coefficients (except intercept)", {
  dat <- mtcars
  fit_small <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = 0.1)
  fit_big   <- ridgereg(mpg ~ wt + hp + drat, data = dat, lambda = 100)
  nm <- names(coef(fit_small))
  idx <- setdiff(seq_along(nm), match("(Intercept)", nm))
  expect_true(mean(abs(coef(fit_big)[idx])) < mean(abs(coef(fit_small)[idx])))
})
