m <- 100 # number of cluster
n <- 50 # size of each cluster
N <- m * n

set.seed(1083)

x1c <- rnorm(N)
x2 <- rnorm(N)
x2b <- ifelse(x2 <= 0.5, 0, 1)

sigma_u_sq <- 2.5
beta0 <- -1
beta1 <- 1.75
beta2 <- 0.67

uj <- rep(rnorm(m, 0, sqrt(sigma_u_sq)), each = n)
eta_ij <- beta0 + beta1 * x1c + beta2 * x2b + uj
pi_ij <- exp(eta_ij) / (1 + exp(eta_ij))

yij <- rbinom(N, size = 1, prob = pi_ij)

mlm_data1 <- tibble::tibble(
  cluster = rep(seq(m), each = n),
  id = rep(seq(n), times = m),
  X1c = x1c,
  X2b = x2b,
  Yij = yij
)

usethis::use_data(mlm_data1, overwrite = TRUE)
