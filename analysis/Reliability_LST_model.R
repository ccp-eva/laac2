#############################################################################################
# ------------------------------- Compute reliabilities ------------------------------------
#############################################################################################
library(brms)
library(tidyverse)

# read data
long <- read.csv("laac_complete.txt", sep="", na.strings="-99")

#############################################################################################
#----------- "Person separation reliability" for trait factor in LST model ---------------------------
#############################################################################################
# Reliability of the EAP estimate, i.e., adjusted for shrinkage due to Bayesian estimation 
# (theta / ability estimated via plausible values). 
# Formula according to equation 19.111 (S. 496) in Moosbrugger & Kelava (Ed.) (2020). 
# Testtheorie und Fragebogenkonstruktion. 3. Auflage. Springer.
# Also see:
# Adams, R. J. (2005). Reliability as a measurement design effect. Studies in Educational 
# Evaluation, 31(2), 162-172. doi:10.1016/j.stueduc.2005.05.008

fun_sep_rel_LST <- function(model) {
  pers_params <- ranef(model)$subject[,,"trait"]%>%as_tibble(rownames = "subject")
  var_EAP <- var(pers_params$Estimate)
  Mean_SE <- mean(pers_params$Est.Error^2)
  # nach TAM Paket (bzw. nach Adams unter der Annahme:  var_EAP = true - Mean_SE bzw. true = var_EAP + Mean_SE)
  rel <- var_EAP / (var_EAP + Mean_SE)
  rel
}

# Prepare data
long$trial <- as.factor(long$trial)
long$time_cat <- as.factor(long$time_point)
# recode search task to binary response
long[long$task == "search", "code"] <- car::recode(long[long$task == "search", "code"], '2=1')

# create categorical variables indicating time point and stable trait level
long$trial <- as.factor(long$trial)
long$time_cat <- as.factor(long$time_point)
long$trait <- 1
# recode search task to binary response
long[long$task == "search", "code"] <- car::recode(long[long$task == "search", "code"], '2=1')


# Set up results matrix
tasks <- c("attent", "comm", "pop", "reas",  "search",  "vfood")
sep_rel <- data.frame(task= tasks, sep_rel_EAP = rep(NA,length(tasks)))

# Set up brms model code for Latent State Trait model with constraint
fit_empty <- brm(code ~ 0 + time_cat + trial + (0 + time_cat || subject) + (0 + trait | subject),
                 data = task,
                 family = brmsfamily(family="bernoulli", link="logit"), 
                 chains = 0)

LST_model <- update(fit_empty, recompile = FALSE, 
              newdata = task,
              chains = 2,
              iter=1)

new_code <- capture.output(stancode(LST_model))

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
LST_model$model <- structure(new_code, class = c("character", "brmsmodel"))
LST_model$fit@stanmodel <- rstan::stan_model(model_code = new_code)

# Estimate latent trait models and compute reliability
for (t in 1:length(tasks)){

  task <- long %>%
    filter(str_detect(task, tasks[t]))
  
  fit_LST <- update(LST_model, recompile = FALSE, 
                      newdata = task,
                      chains = 2,
                      iter=2000)
  
  sep_rel[sep_rel$task == tasks[t], "sep_rel_EAP"] <-  fun_sep_rel_LST(model = fit_LST)
}

