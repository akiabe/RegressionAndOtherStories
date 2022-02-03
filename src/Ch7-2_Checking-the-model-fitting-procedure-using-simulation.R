library(rstanarm)

hibbs <- read.table("hibbs.dat", header = TRUE)
hibbs

a <- 46.3
b <- 3.0
sigma <- 3.9
x <- hibbs$growth
n <- length(x)

y <- a + b*x + rnorm(n, 0, sigma)
fake <- data.frame(x, y)
fake

fit <- stan_glm(y ~ x, data = fake, refresh = 0)
fit

b_hat <- coef(fit)["x"]
b_se <- se(fit)["x"]

cover_68 <- abs(b - b_hat) < b_se
cover_98 <- abs(b - b_hat) < 2*b_se
cat(paste("68% coverage: ", cover_68))
cat(paste("95% coverage: ", cover_98))

n_fake <- 100
cover_68 <- rep(NA, n_fake)
cover_68 <- rep(NA, n_fake)
for (s in 1:n_fake) {
  y <- a + b*x + rnorm(n, 0, sigma)
  fake <- data.frame(x, y)
  fit <- stan_glm(y ~ x, data = fake, refresh = 0)
  b_hat <- coef(fit)["x"]
  b_se <- se(fit)["x"]
  cover_68[s] <- abs(b - b_hat) < b_se
  cover_98[s] <- abs(b - b_hat) < 2*b_se
}
cat(paste("68% coverage: ", mean(cover_68)))
cat(paste("95% coverage: ", mean(cover_98)))

n_fake <- 100
cover_68 <- rep(NA, n_fake)
cover_68 <- rep(NA, n_fake)
t_68 <- qt(0.84, n - 2)
t_95 <- qt(0.975, n - 2)
for (s in 1:n_fake) {
  y <- a + b*x + rnorm(n, 0, sigma)
  fake <- data.frame(x, y)
  fit <- stan_glm(y ~ x, data = fake, refresh = 0)
  b_hat <- coef(fit)["x"]
  b_se <- se(fit)["x"]
  cover_68[s] <- abs(b - b_hat) < t_68 * b_se
  cover_98[s] <- abs(b - b_hat) < t_95 * b_se
}
cat(paste("68% coverage: ", mean(cover_68)))
cat(paste("95% coverage: ", mean(cover_98)))
