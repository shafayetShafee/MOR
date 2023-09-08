#' Calculate Median Odds Ratio from a fitted multilevel binary logistic model object
#'
#' @param object An `MixMod` object created by `GLMMadaptive::mixed_model()` or
#'   an `glmmTMB` object created by `glmmTMB::glmmTMB()`.
#' @param se Logical indicating whether or not to include the standard error of
#'   MOR estimate. Defaults to `TRUE`.
#' @param conf.int Logical indicating whether or not to include a confidence
#'   interval in the tidied output. Defaults to `TRUE`.
#' @param conf.level The confidence level to use for the confidence interval
#'  if `conf.int = TRUE`. Must be strictly greater than 0 and less than 1.
#'  Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Currently not used.
#'
#' @return A named vector with elements:
#'  * `mor`:      Estimate of Median Odds Ratio
#'  * `se_mor`:   Standard Error of MOR
#'  * `ci_lower`: Lower bound on the confidence interval of MOR
#'  * `ci_upper`: Upper bound on the confidence interval of MOR
#'
#' @examples
#' library(GLMMadaptive)
#' data("guImmun", package = "mlmRev")
#' model <- mixed_model(fixed = immun ~ kid2p + mom25p + ord,
#'                      random =  ~ 1 | mom,
#'                      family = binomial("logit"),
#'                      data = guImmun)
#'
#' mor(model)
#'
#' @export
mor <- function(object, se = TRUE, conf.int = TRUE, conf.level = 0.95, ...) {
  UseMethod("mor")
}

#' @export
mor.default <- function(object, ...) {
  message(sprintf("`mor` does not work for models of class '%s'.", class(object)[1]))
  message("It currently supports `MixMod` object (fitted model object) from `GLMMadaptive` package and `glmmTMB` object from `glmmTMB` package")
}

#' @export
mor.MixMod <- function(object, se = TRUE, conf.int = TRUE, conf.level = 0.95, ...) {

  obj_family <- object$family
  if(obj_family$family != "binomial" && obj_family$link != "logit") {
    stop("MOR can only be calculated for Multilevel Logistic Regression Model i.e. for `MixMod` with `binomial` family with `logit` link",
         call. = FALSE)
  }

  if(!all(dim(object$D) == 1)) {
    stop("MOR can only be calculated for Two level random intercept model.", call. = FALSE)
  }

  sigma_u_sq_hat <- object$D[[1]]
  var_sigma_u_sq_hat <- vcov_orig_scale(object)

  mor_hat <- exp(sqrt(2 * sigma_u_sq_hat) * stats::qnorm(0.75))
  log_mor_hat <- log(mor_hat)

  log_mor_int_expr <- function(x) sqrt(2 * x) * stats::qnorm(0.75)
  J <- numDeriv::jacobian(log_mor_int_expr, x = sigma_u_sq_hat)
  log_se_mor_hat <- as.numeric(sqrt(t(J) %*% var_sigma_u_sq_hat %*% J))
  se_mor_hat <- exp(log_se_mor_hat)

  crit_val <- stats::qnorm((1 - conf.level)/2, lower.tail = F)
  ci <- log_mor_hat + c(-1, 1) * crit_val * log_se_mor_hat
  ci_exp <- exp(ci)
  ci_lower <- ci_exp[1]
  ci_upper <- ci_exp[2]

  if(!se) {
    se = NULL
  }

  if(!conf.int) {
    ci_lower = NULL
    ci_upper = NULL
  }

  return(c(
    mor = mor_hat, se_mor = se_mor_hat, ci_lower = ci_lower, ci_upper = ci_upper
  ))
}


# helper funs -------------------------------------------------------------

# taken from GLMMadaptive pkg
chol_transf <- function (x) {
  if (any(is.na(x) | !is.finite(x)))
    stop("NA or infinite values in 'x'.\n")
  if (is.matrix(x)) {
    k <- nrow(x)
    U <- chol(x)
    U[cbind(1:k, 1:k)] <- log(U[cbind(1:k, 1:k)])
    U[upper.tri(U, TRUE)]
  } else {
    nx <- length(x)
    k <- round((-1 + sqrt(1 + 8 * nx))/2)
    mat <- matrix(0, k, k)
    mat[upper.tri(mat, TRUE)] <- x
    mat[cbind(1:k, 1:k)] <- exp(mat[cbind(1:k, 1:k)])
    res <- crossprod(mat)
    attr(res, "L") <- t(mat)[lower.tri(mat, TRUE)]
    res
  }
}

# taken from GLMMadaptive pkg
vcov.MixMod <- function (object, parm = "var-cov", ...) {
  parm <- match.arg(parm)
  V <- solve(object$Hessian)
  if (parm == "var-cov") {
    D <- object$D
    diag_D <- ncol(D) > 1 && all(abs(D[lower.tri(D)]) < sqrt(.Machine$double.eps))
    include <- if (diag_D) {
      unconstr_D <- log(diag(D))
      n_betas <- length(object$coefficients)
      seq(n_betas + 1, n_betas + length(unconstr_D))
    } else {
      unconstr_D <- chol_transf(D)
      n_betas <- length(object$coefficients)
      seq(n_betas + 1, n_betas + length(unconstr_D))
    }
    return(V[include, include, drop = FALSE])
  }
}

# copied from https://stackoverflow.com/a/76925070/10858321 by @statmerkur
D_chol_to_D <- function(x) {
  # transform the log-cholesky parameterized value back to original scale
  D <- chol_transf(x)
  D[upper.tri(D, diag = TRUE)]
}

vcov_orig_scale <- function(model) {
  D <- model$D
  D_chol_entries <- chol_transf(D)
  V_chol <- stats::vcov(model, parm = "var-cov")
  J <- numDeriv::jacobian(D_chol_to_D, D_chol_entries)
  V <- J %*% V_chol %*% t(J)
  colnames(V) <- colnames(V_chol)
  rownames(V) <- rownames(V_chol)
  return(V)
}
