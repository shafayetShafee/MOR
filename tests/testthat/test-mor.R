test_that("Expecting Errors for fitting models with family other than binomial", {
  # taken from
  # https://drizopoulos.github.io/GLMMadaptive/articles/GLMMadaptive.html
  set.seed(1234)
  n <- 100
  K <- 8
  t_max <- 15

  DF <- data.frame(
    id = rep(seq_len(n), each = K),
    time = c(replicate(n, c(0, sort(runif(K - 1, 0, t_max))))),
    sex = rep(gl(2, n / 2, labels = c("male", "female")), each = K)
  )

  X <- model.matrix(~ sex * time, data = DF)

  betas <- c(2.13, -0.25, 0.24, -0.05)
  D11 <- 0.48

  b <- rnorm(n, sd = sqrt(D11))
  eta_y <- drop(X %*% betas + b[DF$id])
  DF$y <- rpois(n * K, exp(eta_y))

  gm1 <- GLMMadaptive::mixed_model(
    fixed = y ~ sex * time,
    random = ~ 1 | id,
    data = DF,
    family = poisson()
  )
  expect_error(
    mor(gm1),
    "MOR can only be calculated for Multilevel Logistic Regression Model i.e. for `MixMod` with `binomial` family with `logit` link"
  )
})


test_that("Expecting Errors for fitting two level random slope model using GLMMadaptive", {
  data("mlm_data1")

  # fitting two level random slope model using GLMMadaptive package
  model1 <- GLMMadaptive::mixed_model(
    fixed = Yij ~ X1c + X2b,
    random = ~ X1c | cluster,
    family = binomial("logit"),
    data = mlm_data1
  )

  expect_error(
    mor(model1),
    "MOR can only be calculated for Two level random intercept model."
  )
})


test_that("Expecting Errors for fitting two level random slope model using glmmTMB", {
  data("mlm_data1")

  # fitting two level random slope model using glmmTMB package
  model2 <- glmmTMB::glmmTMB(
    Yij ~ X1c + X2b + (X1c | cluster),
    family = binomial("logit"),
    data = mlm_data1
  )

  expect_error(
    mor(model2),
    "MOR can only be calculated for Two level random intercept model."
  )
})


test_that("Expecting Errors for fitting three level random slope model using glmmTMB", {
  data("mlm_data2")

  # fitting two level random slope model using glmmTMB package
  model3 <- glmmTMB::glmmTMB(
    Yijk ~ X1c + X2b + (X1c | ea:hh) + (1 | ea),
    family = "binomial",
    data = mlm_data2
  )

  expect_error(
    mor(model3),
    "MOR can only be calculated for Three level random intercept model."
  )
})


test_that("Expecting Equal results for fitting three level random slope model using glmmTMB using different style", {
  data("mlm_data2")

  # fitting two level random slope model using glmmTMB package
  model4 <- glmmTMB::glmmTMB(
    Yijk ~ X1c + X2b + (1 | ea:hh) + (1 | ea),
    family = "binomial",
    data = mlm_data2
  )

  model5 <- glmmTMB::glmmTMB(
    Yijk ~ X1c + X2b + (1 | ea) + (1 | ea:hh),
    family = "binomial",
    data = mlm_data2
  )

  expect_equal(mor(model4), mor(model5))
})


test_that("Expecting Errors for fitting two level random slope model using lme4", {
  data("mlm_data1")

  # fitting two level random slope model using glmmTMB package
  model6 <- lme4::glmer(
    Yij ~ X1c + X2b + (X1c | cluster),
    family = binomial("logit"),
    data = mlm_data1
  )

  expect_error(
    mor(model6),
    "MOR can only be calculated for Two level random intercept model."
  )
})


test_that("Expecting Errors for fitting three level random slope model using lme4", {
  data("mlm_data2")

  # fitting two level random slope model using glmmTMB package
  model7 <- lme4::glmer(
    Yijk ~ X1c + X2b + (X1c | ea:hh) + (1 | ea),
    family = "binomial",
    data = mlm_data2
  )

  expect_error(
    mor(model7),
    "MOR can only be calculated for Three level random intercept model."
  )
})


test_that("Expecting Equal results for fitting three level random slope model using lme4 using different style", {
  data("mlm_data2")

  # fitting two level random slope model using lme4 package
  model8 <- lme4::glmer(
    Yijk ~ X1c + X2b + (1 | ea:hh) + (1 | ea),
    family = "binomial",
    data = mlm_data2
  )

  model9 <- lme4::glmer(
    Yijk ~ X1c + X2b + (1 | ea) + (1 | ea:hh),
    family = "binomial",
    data = mlm_data2
  )

  expect_equal(mor(model8), mor(model9))
})


test_that("Expecting Errors for mor.default", {
  data("mlm_data1")

  # fitting two level random slope model using glmmTMB package
  model5 <- stats::glm(Yij ~ X1c + X2b, family = "binomial", data = mlm_data1)

  expect_error(
    mor(model5),
    "`mor` does not work for models of class *."
  )

  expect_error(
    mor(1:3),
    "`mor` does not work for models of class *."
  )
})


test_that("Check for equivalent answer of mor for different package fit (two level int)", {
  data("mlm_data1")

  model10 <- lme4::glmer(
    Yij ~ X1c + X2b + (1 | cluster),
    family = "binomial",
    data = mlm_data1
  )

  model11 <- GLMMadaptive::mixed_model(
    fixed = Yij ~ X1c + X2b,
    random = ~ 1 | cluster,
    family = "binomial",
    data = mlm_data1
  )

  model12 <- glmmTMB::glmmTMB(
    Yij ~ X1c + X2b + (1 | cluster),
    family = "binomial",
    data = mlm_data1
  )

  expect_equal(mor(model10), mor(model11), tolerance = 0.1)
  expect_equal(mor(model10), mor(model11), tolerance = 0.1)
  expect_equal(mor(model11), mor(model12), tolerance = 0.1)
})


test_that("Check for equivalent answer of mor for different package fit (three level int)", {
  data("mlm_data2")

  model13 <- lme4::glmer(
    Yijk ~ X1c + X2b + (1 | ea:hh) + (1 | ea),
    family = "binomial",
    data = mlm_data2
  )

  model14 <- glmmTMB::glmmTMB(
    Yijk ~ X1c + X2b + (1 | ea:hh) + (1 | ea),
    family = "binomial",
    data = mlm_data2
  )

  expect_equal(mor(model13), mor(model14), tolerance = 0.1)
})


test_that("Testing for valid arguments", {
  data("mlm_data1")

  model15 <- lme4::glmer(
    Yij ~ X1c + X2b + (1 | cluster),
    family = "binomial",
    data = mlm_data1
  )

  expect_error(
    mor(model15, conf.int = NULL),
    "`conf.int` must be a boolean i.e. `TRUE` or `FALSE`"
  )

  expect_error(
    mor(model15, conf.int = NA),
    "`conf.int` must be a boolean i.e. `TRUE` or `FALSE`"
  )

  expect_error(
    mor(model15, conf.int = NaN),
    "`conf.int` must be a boolean i.e. `TRUE` or `FALSE`"
  )

  expect_error(
    mor(model15, conf.int = 1),
    "`conf.int` must be a boolean i.e. `TRUE` or `FALSE`"
  )

  expect_error(
    mor(model15, conf.int = "1"),
    "`conf.int` must be a boolean i.e. `TRUE` or `FALSE`"
  )

  expect_error(
    mor(model15, conf.level = 1),
    "`conf.level` must be a numeric less than 1 and greated than 0"
  )

  expect_error(
    mor(model15, conf.level = 0),
    "`conf.level` must be a numeric less than 1 and greated than 0"
  )

  expect_error(
    mor(model15, conf.level = 95),
    "`conf.level` must be a numeric less than 1 and greated than 0"
  )

  expect_error(
    mor(model15, conf.level = NULL),
    "`conf.level` must be a numeric less than 1 and greated than 0"
  )

  expect_error(
    mor(model15, conf.level = NA),
    "`conf.level` must be a numeric less than 1 and greated than 0"
  )

  expect_error(
    mor(model15, conf.level = NaN),
    "`conf.level` must be a numeric less than 1 and greated than 0"
  )
})
