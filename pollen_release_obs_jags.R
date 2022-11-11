# This script is for analyzing the manual observations of pollen release that were assembled in 
# 'pollen_release_obs.R'

# Analysis follows the approach tested out in 'pollen_release_toy_example.R'

Sys.setenv(JAGS_HOME="C:\\Program Files\\JAGS\\JAGS-4.3.0") #\\x64 #a little weird that this was necessary
#install.packages("rjags")
library(rjags)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(here)

#rm(list=ls())

setwd("C:/Users/dsk273/Box")
#setwd("C:/Users/danka/Box")
here::i_am("katz_photo.jpg")




#### prepare data for analysis ############################################################
data_for_model_orig <- read_csv(here("texas", "pheno",  "data_for_daily_release_model_221111.csv"))
data_for_model <- filter(data_for_model_orig, site_name.x == "wade") %>% 
                  filter(tree_xy == "-98.05887 30.82552")



### analyze model ###########################################################

## model

sink("model_a.txt")
cat("  
model{
  
#Likelihood
for(i in 2:30){
#high_release[i] ~ dbern(cones_opening_out[i])
#logit(cones_opening_out[i]) <- cones_opening_in[i]
cones_opening_pred[i] ~ dnorm(cones_opening_in[i], prec)

cones_available[i] <- cones_ripened[i] + cones_ripe_didnt_open[i - 1]
cones_opening_in[i] <- cones_available[i] * alpha * covars[i]
cones_ripe_didnt_open[i] <- cones_available[i] - cones_opening_in[i]
}

#Priors
alpha ~ dunif(0, 1)
prec ~ dgamma(0.001, 0.001)
cones_ripe_didnt_open[1] <- 0



}#end model
    ",fill=TRUE)
sink() 

jags <- jags.model('model_a.txt', 
                   data = list(
                     #high_release = as.matrix(sim_data$high_release), 
                     cones_ripened = sim_data$ripened,
                     covars = sim_data$covars
                     #cones_ripe_didnt_open_1 = 0
                     #cones_ripe_didnt_open = as.vector(c(0, rep(NA, n - 1)))
                   ),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors


#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter=100) 


mcmc_samples_params <- coda.samples(jags, variable.names=c("cones_opening_in"),  n.iter = 1000, thin = 3) #variables to monitor
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_param


results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model

sim_data_results <- sim_data %>% mutate(cones_opening_pred = c(0, results_params2$Mean))
ggplot(sim_data_results, aes(x = day, y = cones_opening)) + geom_point() + scale_color_viridis_c()+
  geom_line(aes(x = day, y = cones_opening_pred), color = "blue")


mcmc_samples_params <- coda.samples(jags, variable.names=c("alpha"),  n.iter = 1000, thin = 3) #variables to monitor
plot(mcmc_samples_params)
