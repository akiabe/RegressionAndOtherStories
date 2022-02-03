### Chapter.7 Predicting presidential vote share from the economy ###

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
prior_summary(M1)
help('prior_summary.stanreg') 
summary(M1)
round(posterior_interval(M1), 1)
ggplot(data = d, mapping = aes(x = growth, y = vote, label = year)) +
  geom_point() +
  geom_abline(intercept = coef(M1)[1], slope = coef(M1)[2]) +
  labs(x = "Growth", y = "Vote", title = "Forecasting elections from the economy")

mu <- 52.3
sigma <- 3.9
curve (dnorm(x, mu, sigma), from = 35, to = 70, bty = "n",
       xlab = "Clinton share of the two-party vote", 
       ylab = "",
       main = "Probability forecast of Hillary Clinton vote share in 2016,\nbased on 2% rate of economic growth")
x <- seq (50,65,.1)
polygon(c(min(x), x, max(x)), c(0, dnorm(x, mu, sigma), 0),
        col="darkgray", border="black")
text(50.7, .025, "Predicted\n72% chance\nof Clinton victory", adj=0)

