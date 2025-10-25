# liu.ridgereg

[![R-CMD-check](https://github.com/mohzh143/liu.ridgereg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mohzh143/liu.ridgereg/actions/workflows/R-CMD-check.yaml)  

**An R package implementing multiple linear regression and ridge regression using S3 methods**

---

## Overview

The **`liu.ridgereg`** package provides an implementation of multiple linear regression and ridge regression models in R using the **S3 object-oriented system**.  
It includes generic methods such as `print()`, `summary()`, `coef()`, `predict()`, and `plot()` for model interpretation and visualization.

The package also contains **one vignette** demonstrating real-world predictive modeling:
**Boston housing price prediction** using `BostonHousing` (ridge regression)

---

## Installation

You can install the package from your local development directory:

```r
# From the package root directory
devtools::install()
```

Then load the package:

```r
library(liu.ridgereg)
```

---

## Main Functions

| Function | Description |
|-----------|-------------|
| `ridgereg()` | Fit a ridge regression model with a specified lambda |
| `linreg()` | Fit an ordinary least squares linear regression |
| `coef()` | Extract estimated coefficients |
| `predict()` | Compute fitted or predicted values |
| `resid()` | Extract residuals |
| `summary()` | Display model statistics |
| `plot()` | Plot diagnostic or performance visualizations |

---

## Example

```r
library(liu.ridgereg)

# Fit a ridge regression model
model <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 0.1)

# Display results
summary(model)

# Predict
pred <- pred(model, newdata = mtcars)
head(pred)
```

---

## Vignettes

You can view the example reports after building vignettes:

```r
vignette(package = "liu.ridgereg")
```

- [Predictive modeling with ridgereg (BostonHousing)](./doc/ridgereg.html)

---

## Learning Highlights

This package was developed as part of the **732A94 Advanced R Programming** course at **Linköping University**.  
It demonstrates:
- Implementation of **S3 classes and methods**  
- **Model validation and visualization**  
- Integration of **real datasets** (`BostonHousing`, `nycflights13`)  
- Creation of **vignettes**, **tests**, and **documentation** following CRAN standards  

---

## Authors

- **Siyuan Xu** – <ningschue@gmail.com>  
- **Mohan Zhang** – <mohzh143@student.liu.se>  
- **Chenzhi Ni** – <cheni622@student.liu.se>  

---

## License

MIT License (see [LICENSE](./LICENSE))

---

## Citation

If you use this package in your work, please cite it as:

> Xu, S., Zhang, M., & Ni, C. (2025). *liu.ridgereg: Ridge and Linear Regression Implemented with S3 Methods.*  
> Linköping University, Course 732A94 Advanced R Programming.

---

## See Also

- `?ridgereg` for model documentation  
- `vignette("ridgereg", package = "liu.ridgereg")` for ridge model example  

---


