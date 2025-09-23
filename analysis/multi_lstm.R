
library(tidyverse)
library(stringr)
library(brms)

long <- read.csv("../data/laac_complete.txt", sep="", na.strings="-99")

# create categorical variables indicating time point and stable trait level
long$trial <- as.factor(long$trial)
long$time_cat <- as.factor(long$time_point)
long$trait <- 1
# recode search task to binary response
long[long$task == "search", "code"] <- car::recode(long[long$task == "search", "code"], '2=1')

d1 <- long%>%filter(task %in% c("cause", "delay", "gaze", "inference", "quant"))

Multi_LST1 <-  brm(code ~ 0 + time_cat + trial + task + time_cat:task + trial:task
                  + (0 + time_cat + time_cat:task || subject) + (0 + task | subject),
                  data = d1,
                  family = brmsfamily(family="bernoulli", link="logit"),
                  chains = 4, # to speed up sampling --> increase later
                  cores = 4,
                  backend = "cmdstanr",
                  threads = threading(10),
                  control = list(adapt_delta = 0.95, max_treedepth = 20),
                  iter=10000) # decreased for model testing

saveRDS(Multi_LST1, "../saves/multi_lst1.rds")
