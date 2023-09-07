
# MOR

<!-- badges: start -->
<!-- badges: end -->

The goal of `MOR` is to provides post-estimation function to calculate the Median Odds Ratio
(MOR) from a multilevel binary logistic regression model fit. Currently this package
provides the median odds ratio in case of two-level random intercept model and three-level random intercept model only where the model is fitted using the R packages [`GLMMadaptive`](https://drizopoulos.github.io/GLMMadaptive/index.html) or [`glmmTMB`](https://glmmtmb.github.io/glmmTMB/).

## Installation

You can install the development version of MOR from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("shafayetShafee/MOR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MOR)
## basic example code
```

