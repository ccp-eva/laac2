library(tidyverse)
library(brms)

t <- c("attent", "comm","pop","reas","search","vfood","cause","delay","gaze","inference","quant")

task <- t[7]

d <- read_csv(paste0("../data/data_",task,".csv"))


data_ind <- d%>%
  filter(subject == unique(d$subject)[1])

# reference model 
bmr <-  brm(sum|trials(n) ~ time_point,
            data = data_ind,
            family = binomial(),
            chains = 3,
            cores = 3,
            iter=2000)

ind_slopes <- tibble()

for (i in unique(d$subject)) {
  
  dx <- d%>%filter(subject == i)
  
  bmij <- update(bmr, newdata = dx)
  
  x <- fixef(bmij)%>%as_tibble(rownames = "param")%>%filter(param == "time_point")%>%mutate(subject = i, task = task)
  
  ind_slopes <- bind_rows(ind_slopes, x)   
  
  saveRDS(ind_slopes, paste0("../saves/id_slopes_",task,".rds"))
  
}