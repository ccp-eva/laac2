#############################################################################################
# ------------------------------- Compute reliabilities ------------------------------------
#############################################################################################
library(brms)
library(tidyverse)

# read data
long <- read.csv("laac_complete.txt", sep="", na.strings="-99")

#############################################################################################
#------------------------------------ Functions -----------------------------------
#############################################################################################
# run functions first
fun_KR20 <- function(responses) {
  # Get number of items (N) and individuals
  n_items <- ncol(responses)
  n_persons <- nrow(responses)
  # get p_j for each item
  p <- colMeans(responses, na.rm=T)
  # Get total scores (X)
  x <- rowSums(responses, na.rm = T)
  # observed score variance
  var_x <- var(x) * (n_persons - 1) / n_persons
  # Apply KR-20 formula
  rel <- (n_items / (n_items - 1)) * (1 - sum(p * (1 - p)) / var_x)
  rel
}


fun_sep_rel <- function(model, timepoint) {
  pers_params <- ranef(model)$subject[,,timepoint]%>%as_tibble(rownames = "subject")
  true <- summary(model)$random$subject[timepoint,"Estimate"]^2
  var_EAP <- var(pers_params$Estimate)
  Mean_SE <- mean(pers_params$Est.Error^2)
  # nach TAM Paket (bzw. nach Adams unter der Annahme:  var_EAP = true - Mean_SE bzw. true = var_EAP + Mean_SE)
  rel <- var_EAP / (var_EAP + Mean_SE)
  rel
}


#############################################################################################
#---------------- KR-20 reliability measure for binary items (model-free) -------------------
#############################################################################################

tasks <- c("attent", "cause", "comm", "delay", "gaze", "inference", "pop", "quant", "reas",  "search",  "vfood")
time_point <- seq(1,10,1)
KR20_rel <- data.frame(task= rep(tasks, each =10), time_point = rep(1:10,length(tasks)), KR20 = rep(NA,10*length(tasks)))

# Compute KR-20 per task per time point 
for (i in 1:110){
tp <- long%>%
  filter(task == KR20_rel[i,1] )%>%
  filter(time_point == KR20_rel[i,2] )%>%
  select(subject, trial, code)%>%
  group_by(subject)%>%
  pivot_wider(names_from = trial, values_from = code)%>%
  ungroup()%>%
  select(-subject)

KR20_rel[i,3] <- fun_KR20(tp)
}

# Average reliabilites per task (averaged across time points)
tapply(KR20_rel$KR20,KR20_rel$task, mean)

#############################################################################################
#-------------------------------- "Person separation reliability" ---------------------------
#############################################################################################
# Reliability of the EAP estimate, i.e., adjusted for shrinkage due to Bayesian estimation 
# (theta / ability estimated via plausible values). 
# Formula according to equation 19.111 (S. 496) in Moosbrugger & Kelava (Ed.) (2020). 
# Testtheorie und Fragebogenkonstruktion. 3. Auflage. Springer.
# Also see:
# Adams, R. J. (2005). Reliability as a measurement design effect. Studies in Educational 
# Evaluation, 31(2), 162-172. doi:10.1016/j.stueduc.2005.05.008


# Prepare data
long$trial <- as.factor(long$trial)
long$time_cat <- as.factor(long$time_point)
# recode search task to binary response
long[long$task == "search", "code"] <- car::recode(long[long$task == "search", "code"], '2=1')

# Set up results matrix
Ntp <- 10
timepoints <- seq(1,Ntp,1)
tasks <- c("attent", "comm", "pop", "reas",  "search",  "vfood")
sep_rel <- data.frame(task= rep(tasks, each = Ntp), time_point = rep(1:Ntp,length(tasks)), sep_rel_EAP = rep(NA,Ntp*length(tasks)))

# Estimate latent state models and compute reliability
for (t in 1:length(tasks)){

  task <- long %>%
    filter(str_detect(task, tasks[t]))
  
  state <-  brm(code ~ 0 + time_cat + trial + (0 + time_cat | subject),
                data = task,
                family = brmsfamily(family="bernoulli", link="logit"),
                chains = 2, # to speed up sampling --> increase later
                iter=4000)
  
  for (i in 1:length(timepoints)){
    
    sep_rel[sep_rel$task == tasks[t] & sep_rel$time_point == timepoints[i],
            "sep_rel_EAP"] <-  fun_sep_rel(model = state, timepoint = timepoints[i])
  }
}

psych::describeBy(sep_rel$sep_rel_EAP,sep_rel$task, mat=T)
reliabilities <- merge(KR20_rel, sep_rel, by = c("task", "time_point"), all = T)
save(reliabilities, file = "reliabilities.RData")
