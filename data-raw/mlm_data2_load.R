l <- 70 # number of EA
m <- 30 # number of HH
n <- 5 # size of each HH
N <- l * m * n

set.seed(1083)

x1c <- rnorm(N)
x2 <- rnorm(N)
x2b <- ifelse(x2 <= 0.5, 0, 1)

sigma_u_sq <- 2.5
sigma_v_sq <- 1.5

beta0 <- -1
beta1 <- 1.75
beta2 <- 0.67

ujk <- rep(rnorm(l * m, 0, sqrt(sigma_u_sq)), each = n)
vk <- rep(rnorm(l, 0, sqrt(sigma_v_sq)), each = m * n)

eta_ijk <- beta0 + beta1 * x1c + beta2 * x2b + ujk + vk
pi_ijk <- exp(eta_ijk) / (1 + exp(eta_ijk))
yijk <- rbinom(N, size = 1, prob = pi_ijk)

mlm_data2 <- tibble::tibble(
  ea = rep(seq(l), each = m * n),
  hh = rep(rep(seq(m), each = n), times = l),
  id = rep(seq(n), times = l * m),
  X1c = x1c,
  X2b = x2b,
  Yijk = yijk
)


usethis::use_data(mlm_data2, overwrite = TRUE)
