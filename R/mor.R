#' Calculate Median Odds Ratio from a fitted multilevel binary logistic model object.
#'
#' @param object An `glmerMod` object created by [`lme4::glmer()`] or an `MixMod` object
#'   created by [`GLMMadaptive::mixed_model()`] or
#'   an `glmmTMB` object created by [`glmmTMB::glmmTMB()`]. See Details.
#' @param se Logical indicating whether or not to include the standard error of
#'   MOR estimate. Defaults to `TRUE`.
#' @param conf.int Logical indicating whether or not to include a confidence
#'   interval in the tidied output. Defaults to `TRUE`.
#' @param conf.level The confidence level to use for the confidence interval
#'  if `conf.int = TRUE`. Must be strictly greater than 0 and less than 1.
#'  Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param ... Currently not used.
#'
#' @details
#' Median Odds Ratio (MOR) is suggested as a measure of heterogeneity that quantifies
#' cluster heterogeneity and facilitates a direct comparison between covariate effects
#' and the magnitude of heterogeneity in terms of well-known odds ratios
#' \insertCite{larsen2000interpreting,larsen2005appropriate}{MOR}
#'
#' @return a [tibble][tibble::tibble-package] with columns,
#'  * `term`:      Name of the estimate
#'  * `estimate`:  Estimate of Median Odds Ratio (MOR)
#'  * `std.error`: Standard Error of MOR
#'  * `ci_lower`:  Lower bound of the confidence interval of MOR
#'  * `ci_upper`:  Upper bound of the confidence interval of MOR
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' data("mlm_data1")
#' data("mlm_data2")
#'
#' # fitting two level random intercept model using lme4 package
#' model <- lme4::glmer(Yij ~ X1c + X2b + (1 | cluster),
#'   family = "binomial", data = mlm_data1
#' )
#'
#' mor(model)
#'
#' # fitting two level random intercept model using GLMMadaptive package
#' model1 <- GLMMadaptive::mixed_model(
#'   fixed = Yij ~ X1c + X2b,
#'   random = ~ 1 | cluster,
#'   family = binomial("logit"),
#'   data = mlm_data1
#' )
#'
#' mor(model1)
#'
#' # fitting two level random intercept model using glmmTMB package
#' model2 <- glmmTMB::glmmTMB(Yij ~ X1c + X2b + (1 | cluster),
#'   family = binomial("logit"), data = mlm_data1
#' )
#'
#' mor(model2)
#'
#' # fitting three level random intercept model using glmmTMB package
#' model3 <- glmmTMB::glmmTMB(Yijk ~ X1c + X2b + (1 | ea) + (1 | ea:hh),
#'   family = "binomial", data = mlm_data2
#' )
#'
#' mor(model3)
#'
#' # fitting three level random intercept model using lme4 package
#' model4 <- lme4::glmer(Yijk ~ X1c + X2b + (1 | ea) + (1 | ea:hh),
#'   family = "binomial", data = mlm_data2
#' )
#'
#'
#' mor(model4)
#'
#' @importFrom Rdpack reprompt
#' @export
mor <- function(object, se = TRUE, conf.int = TRUE, conf.level = 0.95, ...) {
  check_args(se, conf.int, conf.level)
  UseMethod("mor")
}


#' @export
mor.default <- function(object, ...) {
  err_msg <- paste0(
    sprintf("`mor` does not work for models of class '%s'.", class(object)[1]),
    "It currently supports `MixMod` object (fitted model object) from `GLMMadaptive` package and `glmmTMB` object from `glmmTMB` package"
  )
  stop(err_msg, call. = FALSE)
}


#' @importFrom stats qnorm
#' @export
mor.MixMod <- function(object, se = TRUE, conf.int = TRUE, conf.level = 0.95, ...) {
  obj_family <- object$family
  if (obj_family$family != "binomial" && obj_family$link != "logit") {
    stop("MOR can only be calculated for Multilevel Logistic Regression Model i.e. for `MixMod` with `binomial` family with `logit` link",
      call. = FALSE
    )
  }

  if (!all(dim(object$D) == 1)) {
    stop("MOR can only be calculated for Two level random intercept model.", call. = FALSE)
  }

  grp_var_name <- object$id_name

  sigma_u_sq_hat <- object$D[[1]]
  var_sigma_u_sq_hat <- vcov_orig_scale(object)

  mor_hat <- exp(sqrt(2 * sigma_u_sq_hat) * qnorm(0.75))
  log_mor_hat <- log(mor_hat)

  log_mor_int_expr <- function(x) sqrt(2 * x) * qnorm(0.75)
  J <- numDeriv::jacobian(log_mor_int_expr, x = sigma_u_sq_hat)
  log_se_mor_hat <- as.numeric(sqrt(t(J) %*% var_sigma_u_sq_hat %*% J))
  se_mor_hat <- exp(log_se_mor_hat)

  crit_val <- qnorm((1 - conf.level) / 2, lower.tail = FALSE)
  ci <- log_mor_hat + c(-1, 1) * crit_val * log_se_mor_hat
  ci_exp <- exp(ci)
  ci_lower <- ci_exp[1]
  ci_upper <- ci_exp[2]

  if (!se) {
    se_mor_hat <- NULL
  }

  if (!conf.int) {
    ci_lower <- NULL
    ci_upper <- NULL
  }

  return(tibble::tibble(
    term = paste0("mor_", grp_var_name),
    estimate = mor_hat,
    std.error = se_mor_hat,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}


#' @importFrom stats qnorm vcov
#' @export
mor.glmmTMB <- function(object, se = TRUE, conf.int = TRUE, conf.level = 0.95, ...) {
  obj_family <- object$modelInfo$family
  if (obj_family$family != "binomial" && obj_family$link != "logit") {
    stop("MOR can only be calculated for Multilevel Binary Logistic Regression Model i.e. for `MixMod` with `binomial` family with `logit` link",
      call. = FALSE
    )
  }

  par_vcov <- diag(vcov(object, full = TRUE))
  par_names <- names(par_vcov)
  grp_var_names <- object$modelInfo$grpVar
  grp_var_no <- length(grp_var_names)
  ran_par_no <- sum(grepl("theta", par_names))

  pattern <- paste0("theta.*[", paste0(grp_var_names, collapse = "|"), "]")
  ran_terms_idx <- which(grepl(pattern, par_names))

  se_logsd <- sqrt(par_vcov)[ran_terms_idx]
  logsd <- object$sdr$par.fixed[ran_terms_idx]
  sigma_sq_hat <- unname(exp(2 * logsd))
  # by delta method
  se_sigma_sq_hat <- se_logsd * 2 * exp(2 * logsd)
  var_sigma_sq_hat <- se_sigma_sq_hat^2

  if (grp_var_no == 1) {
    if (ran_par_no > 1) {
      stop("MOR can only be calculated for Two level random intercept model.", call. = FALSE)
    }

    mor_hat <- exp(sqrt(2 * sigma_sq_hat) * qnorm(0.75))
    log_mor_hat <- log(mor_hat)

    log_mor_expr <- function(x) {
      # x is ran-effect parameterized as sd
      sqrt(2 * x) * qnorm(0.75)
    }

    J <- numDeriv::jacobian(log_mor_expr, x = sigma_sq_hat)
    log_se_mor_hat <- as.numeric(sqrt(t(J) %*% var_sigma_sq_hat %*% J))
    se_mor_hat <- exp(log_se_mor_hat)

    crit_val <- stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE)
    ci <- log_mor_hat + c(-1, 1) * crit_val * log_se_mor_hat
    ci_exp <- exp(ci)
    ci_lower <- ci_exp[1]
    ci_upper <- ci_exp[2]

    if (!se) {
      se_mor_hat <- NULL
    }

    if (!conf.int) {
      ci_lower <- NULL
      ci_upper <- NULL
    }

    return(tibble::tibble(
      term = paste0("mor_", grp_var_names),
      estimate = mor_hat,
      std.error = se_mor_hat,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    ))
  } else if (grp_var_no == 2) {
    if (!ran_par_no == 2) {
      stop("MOR can only be calculated for Three level random intercept model.", call. = FALSE)
    }

    fix_terms_names <- names(attr(object$modelInfo$terms$cond$fixed, "dataClasses"))
    model_terms_names <- names(object$frame)
    diff <- setdiff(model_terms_names, fix_terms_names)
    possible_nested_term <- c(
      paste0(diff, collapse = ":"),
      paste0(rev(diff), collapse = ":")
    )
    nested_term_names <- intersect(grp_var_names, possible_nested_term)
    third_lvl_var_names <- setdiff(grp_var_names, nested_term_names)

    sigma_ujk_idx <- grepl(nested_term_names, names(se_sigma_sq_hat), fixed = TRUE)
    sigma_ujk <- sigma_sq_hat[sigma_ujk_idx]
    mor1_hat <- exp(sqrt(2 * sigma_ujk) * qnorm(0.75))
    log_mor1_hat <- log(mor1_hat)

    log_mor1_expr <- function(x) {
      # x is ran-effect parameterized as sd
      sqrt(2 * x) * qnorm(0.75)
    }

    J1 <- numDeriv::jacobian(log_mor1_expr, x = sigma_ujk)
    log_se_mor1_hat <- as.numeric(sqrt(t(J1) %*% var_sigma_sq_hat[sigma_ujk_idx] %*% J1))
    se_mor1_hat <- exp(log_se_mor1_hat)

    # mor2 calc. ----------------------
    mor2_hat <- exp(sqrt(2 * sum(sigma_sq_hat)) * qnorm(0.75))
    log_mor2_hat <- log(mor2_hat)

    log_mor2_expr <- function(x) {
      # x is ran-effect parameterized as variance
      sqrt(2 * sum(x)) * qnorm(0.75)
    }

    J2 <- numDeriv::jacobian(log_mor2_expr, x = sigma_sq_hat)
    log_se_mor2_hat <- as.numeric(sqrt(J2 %*% diag(var_sigma_sq_hat) %*% t(J2)))
    se_mor2_hat <- exp(log_se_mor2_hat)

    crit_val <- stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE)

    ci_1 <- log_mor1_hat + c(-1, 1) * crit_val * log_se_mor1_hat
    ci_1_exp <- exp(ci_1)
    ci_lower_1 <- ci_1_exp[1]
    ci_upper_1 <- ci_1_exp[2]

    ci_2 <- log_mor2_hat + c(-1, 1) * crit_val * log_se_mor2_hat
    ci_2_exp <- exp(ci_2)
    ci_lower_2 <- ci_2_exp[1]
    ci_upper_2 <- ci_2_exp[2]

    if (!se) {
      se_mor1_hat <- NULL
      se_mor2_hat <- NULL
    }

    if (!conf.int) {
      ci_lower_1 <- NULL
      ci_upper_1 <- NULL
      ci_lower_2 <- NULL
      ci_upper_2 <- NULL
    }

    return(tibble::tibble(
      term = c(paste0("mor_", nested_term_names), paste0("mor_", third_lvl_var_names)),
      estimate = c(mor1_hat, mor2_hat),
      std.error = c(se_mor1_hat, se_mor2_hat),
      ci_lower = c(ci_lower_1, ci_lower_2),
      ci_upper = c(ci_upper_1, ci_upper_2)
    ))
  }
}


#' @importFrom stats qnorm vcov
#' @export
mor.glmerMod <- function(object, se = TRUE, conf.int = TRUE, conf.level = 0.95, ...) {
  s <- summary(object)
  if (s$family != "binomial" && s$link != "logit") {
    stop("MOR can only be calculated for Multilevel Logistic Regression Model i.e. for `glmerMod` with `binomial` family with `logit` link",
      call. = FALSE
    )
  }

  ran_eff_df <- as.data.frame(s$varcor)
  grp_var_no <- length(s$ngrps)
  grp_var_name <- unique(ran_eff_df$grp)

  if (grp_var_no == 1) {
    if (nrow(ran_eff_df) > 1) {
      stop("MOR can only be calculated for Two level random intercept model.", call. = FALSE)
    }

    grp_var_name_pat <- paste0("cov_", grp_var_name, ".\\(Intercept\\)")
    sigma_u_sq <- ran_eff_df$vcov
    vcov_mat <- merDeriv::vcov.glmerMod(object, full = TRUE, ranpar = "sd")
    idx <- which(grepl(grp_var_name_pat, colnames(vcov_mat)))
    se_sigma_u <- sqrt(diag(vcov_mat)[idx])

    mor_hat <- exp(sqrt(2 * sigma_u_sq) * qnorm(0.75))
    log_mor_hat <- log(mor_hat)
    log_se_mor_hat <- sqrt(2) * qnorm(0.75) * se_sigma_u
    se_mor_hat <- exp(log_se_mor_hat)

    crit_val <- qnorm((1 - conf.level) / 2, lower.tail = FALSE)
    ci <- log_mor_hat + c(-1, 1) * crit_val * log_se_mor_hat
    ci_exp <- exp(ci)
    ci_lower <- ci_exp[1]
    ci_upper <- ci_exp[2]

    if (!se) {
      se_mor_hat <- NULL
    }

    if (!conf.int) {
      ci_lower <- NULL
      ci_upper <- NULL
    }

    return(tibble::tibble(
      term = paste0("mor_", grp_var_name),
      estimate = mor_hat,
      std.error = se_mor_hat,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    ))
  } else if (grp_var_no == 2) {
    if (nrow(ran_eff_df) > 2) {
      stop("MOR can only be calculated for Three level random intercept model.", call. = FALSE)
    }

    sigma_hat <- ran_eff_df$sdcor
    var_sigma_hat <- diag(diag(2 * solve(object@optinfo$derivs$Hessian))[1:2])

    mor1_hat <- exp(sqrt(2 * sigma_hat[1]^2) * qnorm(0.75))
    log_mor1_hat <- log(mor1_hat)
    log_mor1_expr <- function(x) sqrt(2 * x^2) * qnorm(0.75)
    J <- numDeriv::jacobian(log_mor1_expr, x = sigma_hat[1])
    log_se_mor1_hat <- as.numeric(sqrt(t(J) %*% var_sigma_hat[1, 1] %*% J))
    se_mor1_hat <- exp(log_se_mor1_hat)

    mor2_hat <- exp(sqrt(2 * sum(sigma_hat^2)) * qnorm(0.75))
    log_mor2_hat <- log(mor2_hat)
    log_mor2_expr <- function(x) sqrt(2 * sum(x^2)) * qnorm(0.75)
    J2 <- numDeriv::jacobian(log_mor2_expr, x = sigma_hat)
    log_se_mor2_hat <- as.numeric(sqrt(J2 %*% var_sigma_hat %*% t(J2)))
    se_mor2_hat <- exp(log_se_mor2_hat)

    crit_val <- stats::qnorm((1 - conf.level) / 2, lower.tail = FALSE)

    ci_1 <- log_mor1_hat + c(-1, 1) * crit_val * log_se_mor1_hat
    ci_1_exp <- exp(ci_1)
    ci_lower_1 <- ci_1_exp[1]
    ci_upper_1 <- ci_1_exp[2]

    ci_2 <- log_mor2_hat + c(-1, 1) * crit_val * log_se_mor2_hat
    ci_2_exp <- exp(ci_2)
    ci_lower_2 <- ci_2_exp[1]
    ci_upper_2 <- ci_2_exp[2]

    if (!se) {
      se_mor1_hat <- NULL
      se_mor2_hat <- NULL
    }

    if (!conf.int) {
      ci_lower_1 <- NULL
      ci_upper_1 <- NULL
      ci_lower_2 <- NULL
      ci_upper_2 <- NULL
    }

    return(tibble::tibble(
      term = c(paste0("mor_", grp_var_name[1]), paste0("mor_", grp_var_name[2])),
      estimate = c(mor1_hat, mor2_hat),
      std.error = c(se_mor1_hat, se_mor2_hat),
      ci_lower = c(ci_lower_1, ci_lower_2),
      ci_upper = c(ci_upper_1, ci_upper_2)
    ))
  }
}





# helper funs -------------------------------------------------------------

# taken from GLMMadaptive pkg
chol_transf <- function(x) {
  if (any(is.na(x) | !is.finite(x))) {
    stop("NA or infinite values in 'x'.\n")
  }
  if (is.matrix(x)) {
    k <- nrow(x)
    U <- chol(x)
    U[cbind(1:k, 1:k)] <- log(U[cbind(1:k, 1:k)])
    U[upper.tri(U, TRUE)]
  } else {
    nx <- length(x)
    k <- round((-1 + sqrt(1 + 8 * nx)) / 2)
    mat <- matrix(0, k, k)
    mat[upper.tri(mat, TRUE)] <- x
    mat[cbind(1:k, 1:k)] <- exp(mat[cbind(1:k, 1:k)])
    res <- crossprod(mat)
    attr(res, "L") <- t(mat)[lower.tri(mat, TRUE)]
    res
  }
}


# copied from https://stackoverflow.com/a/76925070/10858321 by @statmerkur
D_chol_to_D <- function(x) {
  # transform the log-cholesky parameterized value back to original scale
  D <- chol_transf(x)
  D[upper.tri(D, diag = TRUE)]
}


#' @importFrom stats vcov
vcov_orig_scale <- function(model) {
  D <- model$D
  D_chol_entries <- chol_transf(D)
  V_chol <- vcov(model, parm = "var-cov")
  J <- numDeriv::jacobian(D_chol_to_D, D_chol_entries)
  V <- J %*% V_chol %*% t(J)
  colnames(V) <- colnames(V_chol)
  rownames(V) <- rownames(V_chol)
  return(V)
}


is_bool <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}


check_logical <- function(x) {
  if (invalid(x) || !is_bool(x)) {
    stop(
      paste0("`", substitute(x), "` must be a boolean i.e. `TRUE` or `FALSE`"),
      call. = FALSE
    )
  }
}


check_args <- function(se, conf.int, conf.level) {
  check_logical(se)
  check_logical(conf.int)
  if (invalid(conf.level) || !(conf.level > 0 && conf.level < 1)) {
    stop("`conf.level` must be a numeric less than 1 and greated than 0", call. = FALSE)
  }
}


# copied from https://github.com/r-gregmisc/gtools/blob/master/R/invalid.R
invalid <- function(x) {
  if (missing(x) || is.null(x) || length(x) == 0 || is(x, "try-error")) {
    return(TRUE)
  }
  if (is.list(x)) {
    return(all(sapply(x, invalid)))
  } else if (is.vector(x)) {
    return(all(is.na(x)))
  } else {
    return(FALSE)
  }
}
