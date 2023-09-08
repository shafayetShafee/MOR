
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MOR

<!-- badges: start -->
<!-- badges: end -->

The goal of `MOR` is to provides post-estimation function to calculate
the Median Odds Ratio (MOR) from a multilevel binary logistic regression
model fit. Currently this package provides the median odds ratio in case
of two-level random intercept model and three-level random intercept
model only where the model is fitted using the R packages
[`GLMMadaptive`](https://drizopoulos.github.io/GLMMadaptive/index.html)
or [`glmmTMB`](https://glmmtmb.github.io/glmmTMB/).

## Installation

You can install the development version of MOR from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("shafayetShafee/MOR")
```

## Example

``` r
library(MOR)

data("mlm_data1", package = "MOR")
data("mlm_data2", package = "MOR")

# fitting two level random intercept model using GLMMadaptive package
model1 <- GLMMadaptive::mixed_model(fixed = Yij ~ X1c + X2b,
                              random =  ~ 1 | cluster,
                              family = binomial("logit"), data = mlm_data1)
mor(model1)
#> # A tibble: 1 × 5
#>   term        estimate std.error ci_lower ci_upper
#>   <chr>          <dbl>     <dbl>    <dbl>    <dbl>
#> 1 mor_cluster     3.09      1.15     2.34     4.08

# fitting two level random intercept model using glmmTMB package
model2 <- glmmTMB::glmmTMB(Yij ~ X1c + X2b + (1 | cluster),
                      family = binomial("logit"), data = mlm_data1)

mor(model2)
#> # A tibble: 1 × 5
#>   term        estimate std.error ci_lower ci_upper
#>   <chr>          <dbl>     <dbl>    <dbl>    <dbl>
#> 1 mor_cluster     3.08      1.15     2.34     4.07

# fitting three level random intercept model using glmmTMB package
model3 = glmmTMB::glmmTMB(Yijk ~ X1c + X2b + (1 | ea) + (1 | ea:hh),
                     family = "binomial", data = mlm_data2)

mor(model3)
#> # A tibble: 2 × 5
#>   term      estimate std.error ci_lower ci_upper
#>   <chr>        <dbl>     <dbl>    <dbl>    <dbl>
#> 1 mor_ea:hh     4.29      1.18     3.11     5.91
#> 2 mor_ea        5.91      1.20     4.12     8.48

# or
model4 = glmmTMB::glmmTMB(Yijk ~ X1c + X2b + (1 | ea/hh),
                          family = "binomial", data = mlm_data2)


mor(model4)
#> # A tibble: 2 × 5
#>   term      estimate std.error ci_lower ci_upper
#>   <chr>        <dbl>     <dbl>    <dbl>    <dbl>
#> 1 mor_hh:ea     4.29      1.18     3.11     5.91
#> 2 mor_ea        5.91      1.20     4.12     8.48
```
