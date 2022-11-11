#the purpose of this script is to simulate data similar to the high pollen release data
#and then to model it using a discrete time differential approach in JAGS

#looking at SIR models was useful for thinking about how to do the indexing and
# to deal with the missing first data point (t-1 at the start time)


#NOTES: I'VE JUST STARTED TO SWITCH THIS MODEL OVER TO THE BINARY OUTCOME INSTEAD OF THE CONTINUOUS OUTCOME
#SO FAR, IT ISN'T GETTING THE RESULTS BACK OUT WELL, NOT QUITE SURE WHY YET... 
#MODEL STRUCTURE PROBLEM? LACK OF DATA?

#NEXT STEPS: MESS AROUND TO GET THIS VERSION WORKING
#ADD IN MULTIPLE TREES
#ADD IN MULTIPLE SITES
#SWITCH OVER TO IMPLEMENTING THIS IN THE pollen_release_obs_jags.R file

Sys.setenv(JAGS_HOME="C:\\Program Files\\JAGS\\JAGS-4.3.0") #\\x64 #a little weird that this was necessary
#install.packages("rjags")
library(rjags)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2) 
library(readr)
#rm(list=ls())


#### simulate some data ############################################################
n <- 30
sim_data <- data.frame(day = 1:n, ripened = round(dnorm(x = 1:n, mean = 15, sd = 5), 3))  %>%
  #  mutate(covars = runif(n, 0, 1))
  mutate(covars = round(rbeta(n, 0.3, 1), 2)) #hist(rbeta(1000, .3, 1))  


cones_available <- as.vector(c(0, rep(NA, n - 1)))
cones_opening <- as.vector(c(0, rep(NA, n - 1)))
cones_ripe_didnt_open <- as.vector(c(0, rep(NA, n - 1)))

test <- for(i in 2:30){
  cones_available[i] <-  (cones_ripe_didnt_open[i-1] + sim_data$ripened[i])
  cones_opening[i]   <-   cones_available[i]* sim_data$covars[i]
  cones_ripe_didnt_open[i] <- cones_available[i] - (cones_available[i]* sim_data$covars[i])
}

sim_data <- sim_data %>% mutate(cones_available = round(cones_available,3),
                                cones_opening = round(cones_opening,3),
                                cones_ripe_didnt_open = round(cones_ripe_didnt_open,3),
                                high_release = case_when(cones_opening > 0.05 ~ 1,
                                                         cones_opening < 0.05 ~ 0)
)

ggplot(sim_data, aes(x = day, y = cones_available, color = covars)) + geom_point() + scale_color_viridis_c()+
  geom_line(aes(x = day, y = cones_opening), color = "blue")+
  geom_line(aes(x = day, y = cones_ripe_didnt_open), color = "red")

colSums(sim_data)




### analyze model ###########################################################

## model

sink("model_a.txt")
cat("  
model{
  
#Likelihood
for(i in 2:30){
high_release[i] ~ dbern(cones_opening_out[i])
logit(cones_opening_out[i]) <- cones_opening_in[i] + beta
#cones_opening_pred[i] ~ dnorm(cones_opening_in[i], prec)

cones_available[i] <- cones_ripened[i] + cones_ripe_didnt_open[i - 1]
cones_opening_in[i] <- cones_available[i] * alpha * covars[i]
cones_ripe_didnt_open[i] <- cones_available[i] - cones_opening_in[i]
}

#Priors
alpha ~ dunif(0, 1)
beta ~ dnorm(0, 0.01)
#prec ~ dgamma(0.001, 0.001)
cones_ripe_didnt_open[1] <- 0



}#end model
    ",fill=TRUE)
sink() 

jags <- jags.model('model_a.txt', 
                   data = list(
                     high_release = sim_data$high_release, 
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


mcmc_samples_params <- coda.samples(jags, variable.names=c("cones_opening_out"),  n.iter = 1000, thin = 3) #variables to monitor
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_param


results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model

sim_data_results <- sim_data %>% mutate(cones_opening_pred = c(0, results_params2$Mean))
ggplot(sim_data_results, aes(x = day, y = cones_opening)) + geom_point() + scale_color_viridis_c()+
  geom_line(aes(x = day, y = cones_opening_pred), color = "blue")


mcmc_samples_params <- coda.samples(jags, variable.names=c("alpha"),  n.iter = 1000, thin = 3) #variables to monitor
plot(mcmc_samples_params)
