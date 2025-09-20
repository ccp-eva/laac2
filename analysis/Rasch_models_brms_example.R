##############################################################################
#-------------------------------  Preparation -------------------------------#
##############################################################################
library(dplyr)
library(stringr)
library(brms)

# Combine datasets from two waves
# --> Restructure_data.R --> saves combined long-format dataset in file "laac_complete.txt"
long <- read.csv("laac_complete.txt", sep="", na.strings="-99")

# create categorical variables indicating time point and stable trait level
long$trial <- as.factor(long$trial)
long$time_cat <- as.factor(long$time_point)
long$trait <- 1
# recode search task to binary response
long[long$task == "search", "code"] <- car::recode(long[long$task == "search", "code"], '2=1')


##############################################################################
#---------------------- Estimate Rasch models using brms --------------------#
##############################################################################

# Select task to be modeled (for single-task models only)
# here: visible food
task <- long %>%
  filter(str_detect(variable, 'food'))

######################## Ignore visible food
# Latent State model
state <-  brm(code ~ 0 + time_cat + trial + (0 + time_cat | subject),
                 data = task,
                 family = brmsfamily(family="bernoulli", link="logit"),
                 chains = 2, # to speed up sampling --> increase later
                 iter=4000)
# Latent State Trait model
LST <-  brm(code ~ 0 + time_cat + trial + (0 + time_cat || subject) + (0 + trait | subject),
                 data = task,
                 family = brmsfamily(family="bernoulli", link="logit"),
                 chains = 2,
                 iter=4000)
# Latent Growth Curve model
LGC <-  brm(code ~ 0 + time_point + trial + (1 + time_point | subject),
                 data = task,
                 family = brmsfamily(family="bernoulli", link="logit"),
                 chains = 2,
                 iter=2000)

##### Inspect results
plot(state) # check convergence / traceplots; insert respective model

## Latent state model
results <- summary(state)
# Latent state "means" (varying easiness across time) and trial easiness parameters
results$fixed
# latent state correlations across time
results$random$subject[11:55,]
# plot probabilities of correct responses per time point
# (these correspond to prob. of first trial per time point)
conditional_effects(state, "time_cat")
# pattern of probabilities across trials
conditional_effects(state, "variable")

## LST model
results <- summary(LST)
# Compute consistency coefficients (these vary across time points)
con <- results$random$subject[11,1] / ( results$random$subject[11,1] + results$random$subject[,1] )

## LGC model
# Is there a linear trend over time?
results <- summary(LGC)
results$fixed["time_point",]



####################################################################################
#----------  Estimate "multidimenensional" Rasch models for several tasks ----------
####################################################################################

# Latent Trait model (ignore state variability)
# (makes unrealistic assumption of perfectly stable inter-inidividual differences across time)
Multi_Trait <-  brm(code ~ 0 + time_cat + trial + task + time_cat:task + trial:task
                  + (0 + task | subject),
                  data = long,
                  family = brmsfamily(family="bernoulli", link="logit"),
                  chains = 2,
                  iter=1000) # decreased for model testing
summary(Multi_Trait)

# Latent State Trait model

# test LST model for combination of two tasks only
# --> combining all tasks needs VERY long estimation times --> habe ich erstmal abgebrochen
test <- long[long$task %in% c("search","vfood"),]
Multi_LST <-  brm(code ~ 0 + time_cat + trial + task + time_cat:task + trial:task
                  + (0 + time_cat + time_cat:task || subject) + (0 + task | subject),
            data = test,
            family = brmsfamily(family="bernoulli", link="logit"),
            chains = 2,
            iter=1000) # decreased for model testing
summary(Multi_LST)
