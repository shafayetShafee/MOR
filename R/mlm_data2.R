#' A Simulated Multilevel Data
#'
#' This data contains data of 70 Enumeration Areas (EA) where each EA contains 30 Households (HH) and
#' each HH has 5 individuals.
#'
#' @format A tibble with 10,500 rows and 6 variables:
#' \describe{
#'   \item{`ea`}{Enumeration Area ID}
#'   \item{`hh`}{Household ID}
#'   \item{`id`}{Individual ID}
#'   \item{`X1c`}{A continuous covariate}
#'   \item{`X2b`}{A binary covariate}
#'   \item{`Yijk`}{Response Variable}
#' }
"mlm_data2"
