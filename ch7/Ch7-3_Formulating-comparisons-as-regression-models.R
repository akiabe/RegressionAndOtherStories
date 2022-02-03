n_0 <- 20
y_0 <- rnorm(n_0, 2.0, 5.0)
fake_0 <- data.frame(y_0)
y_0

mean(y_0)
sd(y_0)/sqrt(n_0)

library(rstanarm)
fit_0 <- stan_glm(y_0 ~ 1, data = fake_0, refresh = 0, prior_intercept = NULL, prior = NULL, prior_aux = NULL)
fit_0

n_1 <- 30
y_1 <- rnorm(n_1, 8.0, 5.0)

diff <- mean(y_1) - mean(y_0)
se_0 <- sd(y_0)/sqrt(n_0)
se_1 <- sd(y_1)/sqrt(n_1)
se <- sqrt(se_0^2 + se_1^2)

n <- n_0 + n_1
y <- c(y_0, y_1)
x <- c(rep(0, n_0), rep(1, n_1))
fake <- data.frame(x, y)
fit <- stan_glm(y ~ x, data = fake, refresh = 0, prior_intercept = NULL, prior = NULL, prior_aux = NULL)
fit

library(tidyverse)
ggplot(data = fake, mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2])

coef(fit)[1]
coef(fit)[2]
