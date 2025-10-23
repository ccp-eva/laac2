
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

pairs <- combn(unique(long$task), 2, simplify = F)

dmod <- long%>%filter(task %in% c("vfood", "comm"))


fit_empty <-  brm(code ~ 0 + time_cat + trial + task + time_cat:task + trial:task
                  + (0 + time_cat + time_cat:task || subject) + (0 + task | subject),
                  data = dmod,
                  family = brmsfamily(family="bernoulli", link="logit"),
                  chains = 0) 

pair_LST <- update(fit_empty, recompile = FALSE, 
                    newdata = dmod,
                    chains = 2,
                    iter=1)

# check design matrix of model
stan_dat <- make_standata(fit_empty)

# Edit code
new_code <- capture.output(stancode(pair_LST))

new_code <- paste(c(new_code[1:59],
                    "vector<lower=0>[2] sd_1;",
                    new_code[61:91],
                    "r_1_1 = (sd_1[1] * (z_1[1]));                                                                                                                                                                                                                                                                                         
                r_1_2 = (sd_1[1] * (z_1[2]));
                r_1_3 = (sd_1[1] * (z_1[3]));
                r_1_4 = (sd_1[1] * (z_1[4]));
                r_1_5 = (sd_1[1] * (z_1[5]));
                r_1_6 = (sd_1[1] * (z_1[6]));
                r_1_7 = (sd_1[1] * (z_1[7]));
                r_1_8 = (sd_1[1] * (z_1[8]));
                r_1_9 = (sd_1[1] * (z_1[9]));
                r_1_10 = (sd_1[1] * (z_1[10]));
                r_1_11 = (sd_1[2] * (z_1[11]));                                                                                                                                                                                                                                                                                         
                r_1_12 = (sd_1[2] * (z_1[12]));
                r_1_13 = (sd_1[2] * (z_1[13]));
                r_1_14 = (sd_1[2] * (z_1[14]));
                r_1_15 = (sd_1[2] * (z_1[15]));
                r_1_16 = (sd_1[2] * (z_1[16]));
                r_1_17 = (sd_1[2] * (z_1[17]));
                r_1_18 = (sd_1[2] * (z_1[18]));
                r_1_19 = (sd_1[2] * (z_1[19]));
                r_1_20 = (sd_1[2] * (z_1[20]));",
                    new_code[c(112:167)]),
                  collapse = "\n")

# Replace the model object
pair_LST$model <- structure(new_code, class = c("character", "brmsmodel"))
pair_LST$fit@stanmodel <- rstan::stan_model(model_code = new_code)


pairwise_cor_rasch <- tibble()
  
  
  for (i in pairs) {
    
  print(unlist(i))
    
  # Fit with modified model
  model <- update(pair_LST, recompile = FALSE, 
                newdata = long%>%filter(task %in% unlist(i)),
                chains = 4,
                cores = 4,
                backend = "cmdstanr",
                threads = threading(10),
                control = list(adapt_delta = 0.95, max_treedepth = 20),
                iter=4000)

  
  pair_cor <- summary(model)$random$subject%>%as_tibble(rownames = "param")%>%
    filter(grepl("cor\\(", param))
  
  pairwise_cor_rasch <- bind_rows(pairwise_cor_rasch, pair_cor)

}

saveRDS(pairwise_cor_rasch, "../saves/pairwise_cor_rasch.rds")
