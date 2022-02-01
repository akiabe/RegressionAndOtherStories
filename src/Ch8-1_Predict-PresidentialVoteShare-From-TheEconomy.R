### Chapter.8 Predicting presidential vote share from the economy ###

library(rstanarm)
library(arm)
library(tidyverse)
library(bayesplot)

d <- read.table("hibbs.dat", header = TRUE)
d

d %>% 
  mutate(vote_label = str_c(d$inc_party_candidate, d$other_candidate, sep = " vs ")) %>% 
  mutate(growth_label = case_when(0.0 >= growth ~ "negative",
                                  growth >= 0.0 & 1.0 >= growth ~ "0% to 1%",
                                  growth >= 1.0 & 2.0 >= growth ~ "1% to 2%",
                                  growth >= 2.0 & 3.0 >= growth ~ "2% to 3%",
                                  growth >= 3.0 & 4.0 >= growth ~ "3% to 4%",
                                  growth >= 4.0 ~ "more than 4%")) %>% 
  

d