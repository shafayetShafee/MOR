# Calculate Median Odds Ratio from a fitted multilevel binary logistic model object.

Calculate Median Odds Ratio from a fitted multilevel binary logistic
model object.

## Usage

``` r
mor(object, conf.int = TRUE, conf.level = 0.95, ...)
```

## Arguments

- object:

  An `glmerMod` object created by
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) or an
  `MixMod` object created by
  [`GLMMadaptive::mixed_model()`](https://drizopoulos.github.io/GLMMadaptive/reference/mixed_model.html)
  or an `glmmTMB` object created by
  [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html).
  See Details.

- conf.int:

  Logical indicating whether or not to include a confidence interval
  using the Delta method in the tidied output. Defaults to `TRUE`.

- conf.level:

  The confidence level to use for the confidence interval if
  `conf.int = TRUE`. Must be strictly greater than 0 and less than 1.
  Defaults to 0.95, which corresponds to a 95 percent confidence
  interval.

- ...:

  Currently not used.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with columns,

- `term`: Name of the estimate

- `estimate`: Estimate of Median Odds Ratio (MOR)

- `ci_lower`: Lower bound of the confidence interval of MOR

- `ci_upper`: Upper bound of the confidence interval of MOR

## Details

Median Odds Ratio (MOR) is suggested as a measure of heterogeneity that
quantifies cluster heterogeneity and facilitates a direct comparison
between covariate effects and the magnitude of heterogeneity in terms of
well-known odds ratios (Larsen et al. 2000; Larsen and Merlo 2005)

## References

Larsen K, Merlo J (2005). “Appropriate assessment of neighborhood
effects on individual health: integrating random and fixed effects in
multilevel logistic regression.” *American journal of epidemiology*,
**161**(1), 81–88.  
  
Larsen K, Petersen JH, Budtz-Jørgensen E, Endahl L (2000). “Interpreting
parameters in the logistic regression model with random effects.”
*Biometrics*, **56**(3), 909–914.

## Examples

``` r
data("mlm_data1")
data("mlm_data2")

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
  family = binomial("logit"),
  data = mlm_data1
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
```
