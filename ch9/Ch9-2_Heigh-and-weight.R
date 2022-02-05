library(rstanarm)
library(tidyverse)

earnings <- read.csv("earnings.csv")
head(earnings)
ggplot(data = earnings, mapping = aes(x = height, y = weight)) +
  geom_point() +
  labs(x = "Height", y = "Weight", title = "")

fit_1 <- stan_glm(weight ~ height, data = earnings, refresh = 0)
fit_1

coefs_1 <- coef(fit_1)
predicted_1 <- coefs_1[1] + coefs_1[2]*66
round(predicted_1, 1)

new <- tibble(height = 66)
pred <- posterior_predict(fit_1, newdata = new)
cat("Predicted weight fot a 66-inch-tall person is", round(mean(pred)),
    "pounds with a sd of", round(sd(pred)))

mean(earnings$height)
earnings$c_height <- earnings$height - 66
fit_2 <- stan_glm(weight ~ c_height, data = earnings, refresh = 0)
fit_2

new <- tibble(c_height = 4.0)
point_pred_2 <- predict(fit_2, newdata = new)
round(point_pred_2, 1)

linpred_2 <- posterior_linpred(fit_2, newdata = new)
hist(linpred_2)
postpred_2 <- posterior_predict(fit_2, newdata = new)
hist(postpred_2)
