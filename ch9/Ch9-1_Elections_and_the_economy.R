library(rstanarm)
library(arm)
library(tidyverse)
library(bayesplot)
library(ggrepel)

d <- read.table("hibbs.dat", header = TRUE)
head(d, n = 5)

d$year_bracket <- paste("(", d$year, ")")
d <- d %>% 
  mutate(tmp  = str_c(d$inc_party_candidate, d$other_candidate, sep = " vs ")) %>% 
  mutate(growth_label = case_when(0.0 >= growth ~ "negative",
                                  growth >= 0.0 & 1.0 >= growth ~ "0% to 1%",
                                  growth >= 1.0 & 2.0 >= growth ~ "1% to 2%",
                                  growth >= 2.0 & 3.0 >= growth ~ "2% to 3%",
                                  growth >= 3.0 & 4.0 >= growth ~ "3% to 4%",
                                  growth >= 4.0 ~ "more than 4%"))
d <- d %>% 
  mutate(vote_label = str_c(d$tmp, d$year_bracket))

ggplot(data = d, mapping = aes(x = vote, y = vote_label, color = growth_label)) +
  geom_point() +
  labs(x = "Vote", y = "Candidate", title = "Forecasting elections from the economy")

ggplot(data = d, mapping = aes(x = growth, y = vote, label = year)) +
  geom_point() +
  geom_text_repel() +
  labs(x = "Growth", y = "Vote", title = "Forecasting elections from the economy")

M1 <- stan_glm(vote ~ growth, data = d, refresh = 0)  
M1

sims <- as.matrix(M1)
Median <- apply(sims, 2, median)
MAD_SD <- apply(sims, 2, mad)
cbind(Median, MAD_SD)

a <- sims[,1]
b <- sims[,2]
par(mfrow = c(1,2))
hist(a, xlab = "a", ylab = "", main = "Posterior simulation \n of the intercept a")
abline(v = median(a), lwd = 2)
arrows(median(a) - 1.483*median(abs(a - median(a))), 550, 
       median(a) + 1.483*median(abs(a - median(a))), 550, 
       length = 0.1, code = 3, lwd = 2)
arrows(median(a) - 2*1.483*median(abs(a - median(a))), 250, 
       median(a) + 2*1.483*median(abs(a - median(a))), 250, 
       length = 0.1, code = 3, lwd = 2)
hist(b, xlab = "b", ylab = "", main = "Posterior simulation \n of the slope b")
abline(v = median(b), lwd = 2)
arrows(median(b) - 1.483*median(abs(b - median(b))), 550, 
       median(b) + 1.483*median(abs(b - median(b))), 550, 
       length = 0.1, code = 3, lwd = 2)
arrows(median(b) - 2*1.483*median(abs(b - median(b))), 250, 
       median(b) + 2*1.483*median(abs(b - median(b))), 250, 
       length = 0.1, code = 3, lwd = 2)
ggplot(data = tibble(a = a, b = b), mapping = aes(x = a, y = b)) +
  geom_point(size = 1) +
  labs(x = "a", y = "b", title = "Posterior drqws of the regression coefficients a, b")
ggplot(data = d, mapping = aes(x = growth, y = vote)) +
  geom_abline(
    intercept = sims[1:100, 1], 
    slope = sims[1:100, 2], 
    size = 0.1
  ) +
  geom_point(color = "white", size = 3) +
  geom_point(color = "black", size = 2) +
  labs(
    x = "Growth", 
    y = "Vote", 
    title = "Data and 100 posterior draws of the line, y = a + bx"
  )

a <- sims[,1]
b <- sims[,2]
z <- a/b
c(median(z), mad(z))

new <- tibble(growth = 2.0)
y_point_pred <- predict(M1, newdata = new)
a_hat <- coef(M1)[1]
b_hat <- coef(M1)[2]
y_point_pred <- a_hat + b_hat*new
y_linpred <- posterior_linpred(M1, newdata = new)

sims <- as.matrix(M1)
a <- sims[,1]
b <- sims[,2]
y_linpred <- a + b*new

y_pred <- posterior_predict(M1, newdata = new)

n_sims <- nrow(sims)
sigma <- sims[,3]
y_pred <- as.numeric(a + b*new) + rnorm(n_sims, 0, sigma)
par(mfrow = c(1,1))
hist(y_pred)

y_pred_median <- median(y_pred)
y_pred_mad <- mad(y_pred)
win_prob <- mean(y_pred > 50)
cat("Predicted Clinton percentage of 2-party vote: ", round(y_pred_median, 1),
    ", with s.e. ", round(y_pred_mad, 1), "\nPr (Clinton win) = ", round(win_prob, 2),
    sep = "")

