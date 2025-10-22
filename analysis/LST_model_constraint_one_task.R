##############################################################################################
# ------ Implement constraint of equal state residual variances in LST model in brms  --------
##############################################################################################
library(brms)
library(tidyverse)

##############################################################################
#------------------------------------ Read data -------_---------------------#
##############################################################################
# Combined datasets from two waves
long <- read.csv("laac_complete.txt", sep="", na.strings="-99")

# create categorical variables indicating time point and stable trait level
long$trial <- as.factor(long$trial)
long$time_cat <- as.factor(long$time_point)
long$trait <- 1
# recode search task to binary response
long[long$task == "search", "code"] <- car::recode(long[long$task == "search", "code"], '2=1')


##############################################################################
#---------------- Estimate Rasch LST models using brms (one task) -----------#
##############################################################################

# Select task to be modeled (for single-task models)
task <- long %>%
  filter(str_detect(task, 'vfood'))

# Set up brms model code for Latent State Trait model
fit_empty <- brm(code ~ 0 + time_cat + trial + (0 + time_cat || subject) + (0 + trait | subject),
                 data = task,
                 family = brmsfamily(family="bernoulli", link="logit"), 
                 chains = 0)

LST <- update(fit_empty, recompile = FALSE, 
                         newdata = task,
                         chains = 2,
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

# Fit with modified model
fit <- update(LST, recompile = FALSE, chains = 2, iter = 1000)
# für andere tasks: direkt hier mit Argument "newdata" einen anderen Datensatz nutzen, 
# die vorherigen Schritte müssen nicht wiederholt werden
summary(fit)
