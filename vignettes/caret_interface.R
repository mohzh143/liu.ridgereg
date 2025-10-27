myRidgeModel <- list(
  type = "Regression",
  library = "liu.ridgereg",
  parameters = data.frame(
    parameter = "lambda",
    class = "numeric",
    label = "Ridge Penalty"
  ),

  grid = function(x, y, len = NULL, search = "grid") {
    data.frame(lambda = 10^seq(-2, 2, length.out = len))
  },

  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    dat <- as.data.frame(x)
    dat$.outcome <- y
    model <- ridgereg(
      formula = as.formula(".outcome ~ ."),
      data = dat,
      lambda = param$lambda,
      ...
    )
    return(model)
  },

  predict = function(modelFit, newdata, submodels = NULL) {
    newdata_df <- as.data.frame(newdata)
    predict(modelFit, newdata = newdata_df)
  },

  prob = NULL,
  sort = function(x) {
    x[order(x$lambda), ]
  }
)
