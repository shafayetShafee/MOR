# Changelog

## MOR 0.2.3

- Removed the argument to get `SE` of `MOR`, since the there was
  mistakes in implementation to get `SE` of `MOR`, but afraid not, `CI`
  was correctly calculated using the Delta method.

## MOR 0.2.2

- Done some internal changes

## MOR 0.2.1

- Fixed a bug in making `NULL` when `conf.int = FALSE`

## MOR 0.2.0

- Added method for `glmerMod` object from
  [`lme4::glmer`](https://rdrr.io/pkg/lme4/man/glmer.html)
- Added a bit info on MOR and references in function doc

## MOR 0.1.0

- Created the package.
- Created the `mor` generic function.
- Added `mor` method for `MixMod` (model object from
  [`GLMMadaptive`](https://drizopoulos.github.io/GLMMadaptive/index.html)
  package)
- Added `mor` method for `glmmTMB` (model object from
  [`glmmTMB`](https://glmmtmb.github.io/glmmTMB/) package)
