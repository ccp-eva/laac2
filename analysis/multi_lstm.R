
library(tidyverse)
library(brms)

long <- read.csv("../data/laac_complete.txt", sep="", na.strings="-99")


Multi_LST <-  brm(code ~ 0 + time_cat + trial + task + time_cat:task + trial:task
                   + (0 + time_cat + time_cat:task || subject) + (0 + task | subject),
                   data = long,
                   family = brmsfamily(family="bernoulli", link="logit"),
                   chains = 4, # to speed up sampling --> increase later
                   cores = 4,
                   backend = "cmdstanr",
                   threads = threading(10),
                   iter=6000) # decreased for model testing
summary(Multi_LST)

saveRDS(Multi_LST, "../saves/multi_lst.rds")