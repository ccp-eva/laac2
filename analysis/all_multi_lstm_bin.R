
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

long_bin <- long%>%
  filter(!is.na(code))%>%
  group_by(time_point, subject, task, time_cat, trait)%>%
  summarise(n = length(code), 
            sum = sum(code))%>%
  ungroup()

long_bin_all <- long_bin%>%
  filter(!subject %in% c("frank", "hodari"))%>%
  mutate(phase = ifelse(task %in% c("cause", "delay", "gaze", "inference", "quant"), 1, 2))%>%
  mutate(time_point = ifelse(phase == 1, time_point - 4, time_point))%>%
  filter(time_point > 0)%>%
  mutate(time_cat = as.factor(time_point))

Multi_LST_bin <-  brm(sum|trials(n) ~ 0 + time_cat + task + time_cat:task + 
                  (0 + time_cat + time_cat:task || subject) + (0 + task | subject),
                  data = long_bin_all,
                  family = binomial(),
                  chains = 4, # to speed up sampling --> increase later
                  cores = 4,
                  backend = "cmdstanr",
                  threads = threading(10),
                  control = list(adapt_delta = 0.95, max_treedepth = 20),
                  iter=15000) # decreased for model testing

saveRDS(Multi_LST_bin, "../saves/multi_lst_bin_ex_infants.rds")
