# A Simulated Multilevel Data

This data contains data of 70 Enumeration Areas (EA) where each EA
contains 30 Households (HH) and each HH has 5 individuals.

## Usage

``` r
mlm_data2
```

## Format

A tibble with 10,500 rows and 6 variables:

- `ea`:

  Enumeration Area ID

- `hh`:

  Household ID

- `id`:

  Individual ID

- `X1c`:

  A continuous covariate

- `X2b`:

  A binary covariate

- `Yijk`:

  Response Variable
