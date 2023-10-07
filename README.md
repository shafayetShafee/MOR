
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MOR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://lifecycle.r-lib.org/articles/stages.html#stable)
![CRAN status](https://img.shields.io/badge/CRAN-not%20published-red)
[![test-coverage](https://github.com/shafayetShafee/MOR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/shafayetShafee/MOR/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/shafayetShafee/MOR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shafayetShafee/MOR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/shafayetShafee/MOR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shafayetShafee/MOR?branch=main)
<!-- badges: end -->

## Overview

The goal of `MOR` is to provides post-estimation function to calculate
the Median Odds Ratio (MOR) from a multilevel binary logistic regression
model fit. Currently this package provides the median odds ratio in case
of two-level random intercept model and three-level random intercept
model only where the model is fitted using the R packages
[`lme4`](https://github.com/lme4/lme4/) or
[`GLMMadaptive`](https://drizopoulos.github.io/GLMMadaptive/index.html)
or [`glmmTMB`](https://glmmtmb.github.io/glmmTMB/).

## Installation

`{MOR}` is not on CRAN yet. You can install the development version of
MOR from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("shafayetShafee/MOR")
```

## Example

``` r
library(MOR)

data("mlm_data1", package = "MOR")
data("mlm_data2", package = "MOR")

# fitting two level random intercept model using lme4 package
model <- lme4::glmer(Yij ~ X1c + X2b + (1 | cluster),
  family = "binomial", data = mlm_data1
)

mor(model)
#> # A tibble: 1 × 4
#>   term        estimate ci_lower ci_upper
#>   <chr>          <dbl>    <dbl>    <dbl>
#> 1 mor_cluster     4.42     3.51     5.58
## to get 90% CI
mor(model, conf.level = 0.90)
#> # A tibble: 1 × 4
#>   term        estimate ci_lower ci_upper
#>   <chr>          <dbl>    <dbl>    <dbl>
#> 1 mor_cluster     4.42     3.64     5.38

# fitting two level random intercept model using GLMMadaptive package
model1 <- GLMMadaptive::mixed_model(
  fixed = Yij ~ X1c + X2b,
  random = ~ 1 | cluster,
  family = binomial("logit"), data = mlm_data1
)

mor(model1)
#> # A tibble: 1 × 4
#>   term        estimate ci_lower ci_upper
#>   <chr>          <dbl>    <dbl>    <dbl>
#> 1 mor_cluster     4.43     3.51     5.60


# fitting two level random intercept model using glmmTMB package
model2 <- glmmTMB::glmmTMB(Yij ~ X1c + X2b + (1 | cluster),
  family = binomial("logit"), data = mlm_data1
)

mor(model2)
#> # A tibble: 1 × 4
#>   term        estimate ci_lower ci_upper
#>   <chr>          <dbl>    <dbl>    <dbl>
#> 1 mor_cluster     4.42     3.50     5.59


# fitting three level random intercept model using glmmTMB package
model3 <- glmmTMB::glmmTMB(Yijk ~ X1c + X2b + (1 | ea) + (1 | ea:hh),
  family = "binomial", data = mlm_data2
)

mor(model3)
#> # A tibble: 2 × 4
#>   term      estimate ci_lower ci_upper
#>   <chr>        <dbl>    <dbl>    <dbl>
#> 1 mor_ea:hh     4.21     3.78     4.68
#> 2 mor_ea        6.29     5.38     7.36


# fitting three level random intercept model using lme4 package
model4 <- lme4::glmer(Yijk ~ X1c + X2b + (1 | ea) + (1 | ea:hh),
  family = "binomial", data = mlm_data2
)

mor(model4)
#> # A tibble: 2 × 4
#>   term      estimate ci_lower ci_upper
#>   <chr>        <dbl>    <dbl>    <dbl>
#> 1 mor_ea:hh     4.21     3.78     4.68
#> 2 mor_ea        6.29     5.38     7.36
## to get 90% CI
mor(model4, conf.level = 0.90)
#> # A tibble: 2 × 4
#>   term      estimate ci_lower ci_upper
#>   <chr>        <dbl>    <dbl>    <dbl>
#> 1 mor_ea:hh     4.21     3.85     4.60
#> 2 mor_ea        6.29     5.52     7.18
```

## Code of Conduct

Please note that the MOR project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
