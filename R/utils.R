#' @title Check Whether an Object Is a Strict Boolean Value
#'
#' @description
#' This function tests whether an object is exactly TRUE or exactly FALSE. It
#' uses [`base::identical()`] to ensure strict checking of type, length, and value,
#' returning TRUE only for a single, non-missing logical scalar.
#'
#' @param x An R object to check.
#'
#' @return A logical scalar: TRUE if `x` is exactly `TRUE` or `FALSE` otherwise.
#'
#' @examples
#' is_bool(TRUE)                 # TRUE
#' is_bool(FALSE)                # TRUE
#' is_bool(NA)                   # FALSE
#' is_bool(c(TRUE, FALSE))       # FALSE
#' is_bool("TRUE")               # FALSE
#' is_bool(1L)                   # FALSE
#'
#' @noRd
is_bool <- function(x) {
  identical(x, TRUE) || identical(x, FALSE)
}


#' @title Check whether an object is considered invalid
#'
#' @description
#' This function tests whether a R object is considered invalid. An object is
#' invalid if it is [base::missing], `NULL`, length zero, a `try-error`, a
#' `simpleError`, a list whose elements are all invalid, or an atomic vector
#' whose elements are all NA.
#'
#' @details
#' Lists are checked recursively: a list is invalid only when all of its
#' elements are invalid.
#'
#' @param x An R object to check.
#'
#' @return A logical scalar: TRUE if the object is invalid, FALSE otherwise.
#'
#' @examples
#' is_invalid(NULL)                 # TRUE
#' is_invalid(character(0))         # TRUE
#' is_invalid(c(NA, NA))            # TRUE
#' is_invalid(c(1, NA))             # FALSE
#'
#' is_invalid(list(NA, NULL))       # TRUE
#' is_invalid(list(NA, 5))          # FALSE
#'
#' # try-error example
#' err1 <- try(
#'   stop("err"), silent = TRUE
#' )
#' is_invalid(err1)                 # TRUE
#'
#' # simpleError example
#' err2 <- simpleError("oops")
#' is_invalid(err2)                 # TRUE
#'
#' is_invalid(5)                    # FALSE
#' is_invalid("txt")                # FALSE
#'
#' @noRd
is_invalid <- function(x) {
  if (
    missing(x) ||
      is.null(x) ||
      length(x) == 0 ||
      inherits(x, "try-error") ||
      inherits(x, "simpleError")
  ) {
    return(TRUE)
  }

  if (is.list(x)) {
    return(all(vapply(x, is_invalid, logical(1))))
  }

  if (is.atomic(x)) {
    return(all(is.na(x)))
  }

  FALSE
}


#' @title Check that an R object is a scalar boolean
#'
#' @description
#' This function checks whether the input `x` is a single, non-missing logical
#' value (`TRUE` or `FALSE`). If `x` is [is_invalid] or not a `boolean`, it
#' raises a error using [`cli::cli_abort()`].
#'
#' @param x An R object to validate.
#'
#' @return Invisibly returns TRUE. Throws a error of class
#'    [`rlang::rlang_error`]  if `x` is not a scalar boolean or an [is_invaild()]
#'    object
#'
#' @examples
#' check_logical(TRUE)          # passes
#' check_logical(FALSE)         # passes
#' \dontrun{
#' check_logical(NA)            # throws error
#' check_logical(c(TRUE,FALSE)) # throws error
#' check_logical("TRUE")        # throws error
#' }
#'
#' @noRd
check_logical <- function(x) {
  if (is_invalid(x) || !is_bool(x)) {
    err_msg <- paste0(
      "{.var {substitute(x)}} must be a scaler ",
      "boolean (`TRUE` or `FALSE`)"
    )
    cli::cli_abort(c("x" = err_msg), call = NULL)
  }
  invisible(TRUE)
}


check_args <- function(conf_int, conf_level) {
  check_logical(conf_int)
  if (is_invalid(conf_level) || !(conf_level > 0 && conf_level < 1)) {
    err_msg <- "`conf_level` must be a numeric less than 1 and greated than 0"
    cli::cli_abort(c("x" = err_msg), call = NULL)
  }
  invisible(TRUE)
}


#' @title Check whether a variable is within a specified numerical bounds.
#'
#' @description
#' This function validates that a variable `x` is:
#' - numeric
#' - length 1
#' - not invalid, i.e. not `NULL`, `NA`, empty, `try-error`, or `simpleError`
#' - within the specified bounds
#'
#' @param x variable to validate.
#' @param lower Numeric lower bound. Default 0.
#' @param upper Numeric upper bound. Default 1.
#' @param lower_include Logical, whether to include the lower bound.
#'    Default TRUE.
#' @param upper_include Logical, whether to include the upper bound.
#'    Default TRUE.
#'
#' @return Invisibly returns TRUE if all checks pass; Otherwise, throws
#'    error of class [rlang::rlang_error].
#'
#' @noRd
check_numeric_bound <- function(
  x,
  lower = 0,
  upper = 1,
  lower_include = TRUE,
  upper_include = TRUE
) {
  if (is_invalid(x) || !is.numeric(x) || length(x) != 1L) {
    type_err_msg <- paste0(
      "{.var {substitute(x)}} must be a single, non-missing numeric value."
    )
    cli::cli_abort(c("x" = type_err_msg), call = NULL)
  }

  if (lower_include) {
    lower_ok <- x >= lower
    lower_symbol <- cli::symbol$geq
  } else {
    lower_ok <- x > lower
    lower_symbol <- ">"
  }

  if (!lower_ok) {
    lower_symbol <- if (lower) {
      lower_err_msg <- paste0(
        "{.var {substitute(x)}} must be {lower_symbol} {lower}."
      )
    }
    cli::cli_abort(c("x" = lower_err_msg), call = NULL)
  }

  if (lower_include) {
    upper_ok <- x <= upper
    upper_symbol <- cli::symbol$leq
  } else {
    upper_ok <- x < upper
    upper_symbol <- "<"
  }

  if (!upper_ok) {
    upper_err_msg <- paste0(
      "{.var {substitute(x)}} must be {upper_symbol} {upper}."
    )
    cli::cli_abort(c("x" = upper_err_msg), call = NULL)
  }

  invisible(TRUE)
}
