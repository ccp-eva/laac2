
library(tidyverse)
library(brms)

long <- read.csv("../data/laac_complete.txt", sep="", na.strings="-99")

# create categorical variables indicating time point and stable trait level
long$trial <- as.factor(long$trial)
long$time_cat <- as.factor(long$time_point)
long$trait <- 1
# recode search task to binary response
long[long$task == "search", "code"] <- car::recode(long[long$task == "search", "code"], '2=1')



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