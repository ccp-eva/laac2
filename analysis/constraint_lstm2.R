
library(tidyverse)
library(stringr)
library(brms)

fun_sep_rel_LST <- function(model) {
  pers_params <- ranef(model)$subject[,,"trait"]%>%as_tibble(rownames = "subject")
  var_EAP <- var(pers_params$Estimate)
  Mean_SE <- mean(pers_params$Est.Error^2)
  # nach TAM Paket (bzw. nach Adams unter der Annahme:  var_EAP = true - Mean_SE bzw. true = var_EAP + Mean_SE)
  rel <- var_EAP / (var_EAP + Mean_SE)
  rel
}

long <- read.csv("../data/laac_complete.txt", sep="", na.strings="-99")

# create categorical variables indicating time point and stable trait level
long$trial <- as.factor(long$trial)
long$time_cat <- as.factor(long$time_point)
long$trait <- 1
# recode search task to binary response
long[long$task == "search", "code"] <- car::recode(long[long$task == "search", "code"], '2=1')

d2 <- long%>%filter(!task %in% c("cause", "delay", "gaze", "inference", "quant"))

dmod <- d2%>%filter(task == "vfood")

# Set up brms model code for Latent State Trait model
  fit_empty <- brm(code ~ 0 + time_cat + trial + (0 + time_cat || subject) + (0 + trait | subject),
                   data = dmod,
                   family = brmsfamily(family="bernoulli", link="logit"), 
                   chains = 0)
  
  LST <- update(fit_empty, recompile = FALSE, 
                newdata = dmod,
                chains = 4,
                iter=1)
  
  # Edit code
  new_code <- capture.output(stancode(LST))
  
  new_code <- paste(c(new_code[1:35],
                      "real<lower=0> sd_1;",
                      new_code[37:53],
                      "r_1_1 = (sd_1 * (z_1[1]));                                                                                                                                                                                                                                                                                         
                r_1_2 = (sd_1 * (z_1[2]));
                r_1_3 = (sd_1 * (z_1[3]));
                r_1_4 = (sd_1 * (z_1[4]));
                r_1_5 = (sd_1 * (z_1[5]));
                r_1_6 = (sd_1 * (z_1[6]));
                r_1_7 = (sd_1 * (z_1[7]));
                r_1_8 = (sd_1 * (z_1[8]));
                r_1_9 = (sd_1 * (z_1[9]));
                r_1_10 = (sd_1 * (z_1[10]));",
                      new_code[c(64:96)]),
                    collapse = "\n")
  
  # Replace the model object
  LST$model <- structure(new_code, class = c("character", "brmsmodel"))
  LST$fit@stanmodel <- rstan::stan_model(model_code = new_code)
  
  
  for (i in unique(d2$task)) {
    
    dtask <- d2%>%filter(task == i)
  
  
  # Fit with modified model
  fit <- update(LST, recompile = FALSE, 
                newdata = dtask,
                chains = 4,
                cores = 4,
                backend = "cmdstanr",
                threads = threading(5),
                control = list(adapt_delta = 0.95, max_treedepth = 20),
                iter=8000)

  saveRDS(fit, paste0("../saves/constraint_lst2_",i,".rds"))
  
}

