#trying out a Gompertz function to model pollen cone opening
#https://en.wikipedia.org/wiki/Gompertz_function



library(rjags)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2) 
#rm(list=ls())

# ### an example with a toy dataset ##############################################
# #https://github.com/xbouteiller/GompertzFit/blob/master/ModelGompertz.txt
# #https://github.com/xbouteiller/GompertzFit
# #file:///C:/Users/dsk856/Downloads/using-automated-sanding-to-homogeneously-break-seed-dormancy-in-black-locust-robinia-pseudoacacia-l-fabaceae.pdf
# 
# test <- matrix(c(0	,	0.004	,
#           4	,	0.03	,
#           5	,	0.302	,
#           6	,	0.584	,
#           7	,	0.758	,
#           8	,	0.804	,
#           9	,	0.84	,
#           10	,	0.86	,
#           11	,	0.884	,
#           12	,	0.89	,
#           13	,	0.9	,
#           14	,	0.904	,
#           15	,	0.908	,
#           16	,	0.91	,
#           18	,	0.91	,
#           19	,	0.912	,
#           20	,	0.912	
# ), ncol = 2, byrow = TRUE)
# data_for_model <- as.data.frame(test)
# names(data_for_model) <- c("t", "G")
# 
# 
# sink("model_b.txt")
# cat("  
# model{
#   
#   #Likelihood
#   for(i in 1:NDAY)  
#   {
#     G[i]<- D  * exp( -exp(-b*(t[i] - tm)))
#     Y[i]~dnorm(G[i], LAMBDA1)
#   }
#   
#   #Priors
#   LAMBDA1~dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   D~dbeta(1, 1)
#   
#   b~dnorm(0, LAMBDA2)
#   LAMBDA2~dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   tm~dnorm(0, LAMBDA3)
#   LAMBDA3~dgamma(0.0001,0.0001) #uninformative gamma prior
# }
#     ",fill=TRUE)
# sink() 
# 
# 
# jags <- jags.model('model_b.txt', 
#                    data = list(
#                      #general stuff
#                      Y = as.numeric(data_for_model$G),  
#                      t = as.numeric(data_for_model$t), 
#                      NDAY = length(data_for_model$t) ),
#                    n.chains = 3,
#                    n.adapt = 100)  # diffuse priors
# 
# #dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
# #Sys.time()
# update(jags,n.iter= 100) #update(jags,n.iter=1000) 
# mcmc_samples_params <- coda.samples(jags, variable.names=c("D", "b", "tm"),  n.iter = 3000, thin = 3) #variables to monitor
# plot(mcmc_samples_params)
# results_param <- summary(mcmc_samples_params)
# 
# mcmc_samples_params2 <- coda.samples(jags, variable.names=c("G"),  n.iter = 1000, thin = 3) #variables to monitor
# plot(mcmc_samples_params2)
# 
# results_param <- summary(mcmc_samples_params)
# results_params <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
# results_params$parameter<-row.names(results_params)
# results_params$param<-substr(results_params$parameter,1,2)
# 
# 
# plot(data_for_model$t, data_for_model$G)





# ### trying to fit an individual tree of mine #######################################
# #p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_data_download_210125_b_edited.csv")
# p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
# day_start <- mdy("12-10-2020")
# p <- p %>%
#   mutate(prop_open = bag_mean,#perc_open/100,
#          #date2 = mdy_hm(date) - hours(6), #correct from GMT
#          date3 = sample_date,
#          site = paste(round(x, 1), round(y, 1)),
#          yday = yday(date3),
#          tree = paste(round(x, 5), round(y, 5))) %>% 
#   filter(!is.na(perc_open)) %>% 
#   #filter(site == "-98 30.1" | site == "-98.2 29.8") %>% 
#   mutate(tree_n = as.numeric(as.factor(tree)),
#          site_n = as.numeric(as.factor(site)),
#          #tree_visit = paste(c(tree_n, as.character(date3))),
#          day_experiment = as.numeric(date3 - day_start)) %>% 
#   arrange(date3, tree_n) %>% 
#   filter( #remove trees that were added because they were late (for pollen platters)
#     tree!= "-98.17164 29.82707" & tree != "-98.17316 29.82818" & tree != "-98.17282 29.82795" & #wilson/comal
#       tree != "-98.05903 30.82545" & tree != "-98.0598 30.82648" & tree != "-98.06041 30.82647" & tree != "-98.06076 30.82632" &#wade/burnett
#       tree != "-98.03122 30.13753" #hays
#   ) #%>% 
# #filter(site != "-97.9 30.7") #this site didnt have any visits after 12/31/20
# 
# 
# # 
# # p <- 
# #   p %>% 
# #   mutate(prop_open = bag_mean,
# #          date2 = mdy_hm(date),
# #          date3 = as_date(date2),
# #          site = paste(round(x, 1), round(y, 1)),
# #          yday = yday(date2),
# #          tree = paste(round(x, 5), round(y, 5)),
# #          day_experiment = as.numeric(date3 - day_start)) 
# 
# #graph of a few individual trees
# p %>% 
#   filter(tree == "-98.0593 30.82629") %>% 
#   #filter(site == "-98.1 30.8") %>% 
#   ggplot(aes(x = sample_date, y = prop_open, group = tree_n)) + geom_point(color = "red") + theme_bw() +
#   geom_line(color = "red", lty = 3) + ylab("sacs open (proportion)") + xlab("")
# 
# # ggsave(filename = "C:/Users/dsk856/Box/texas/pheno/manual_obs_models/fig_single_tree_a_210405.jpeg",
# #        width = 10, height = 6, units = "in", scale = 0.5)
# 
# indiv_tree <- p %>% 
#   filter(tree == "-98.0593 30.82629") %>% 
#   select(day_experiment, prop_open)
# 
# 
# sink("model_b.txt")
# cat("  
# model{
#   
#   #Likelihood
#   for(i in 1:NDAY)  
#   {
#     G[i]<- D  * exp( -exp(-b*(t[i] - tm)))
#     Y[i]~dnorm(G[i], LAMBDA1)
#   }
#   
#   #Priors
#   LAMBDA1~dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   D~dbeta(1, 1)
#   
#   b~dnorm(0, LAMBDA2)
#   LAMBDA2~dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   tm~dnorm(0, LAMBDA3)
#   LAMBDA3~dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   #simulation
#   for(j in 1:MAXDAY){
#   G_hat[j]<- D  * exp( -exp(-b*(j - tm)))
#   }
# }
#     ",fill=TRUE)
# sink() 
# 
# jags <- jags.model('model_b.txt', 
#                    data = list(
#                      #general stuff
#                      Y = as.numeric(indiv_tree$prop_open),  
#                      t = as.numeric(indiv_tree$day_experiment), 
#                      NDAY = length(indiv_tree$day_experiment),
#                      MAXDAY = max(indiv_tree$day_experiment)),
#                    n.chains = 3,
#                    n.adapt = 100)  # diffuse priors
# 
# #dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
# #Sys.time()
# update(jags,n.iter=10000) 
# mcmc_samples_params <- coda.samples(jags, variable.names=c("D", "b", "tm"),  n.iter = 3000, thin = 3) #variables to monitor
# plot(mcmc_samples_params)
# results_param <- summary(mcmc_samples_params)
# 
# #simulation
# mcmc_samples_params2 <- coda.samples(jags, variable.names=c("G_hat"),  n.iter = 3000, thin = 3) #variables to monitor
# #plot(mcmc_samples_params2)
# 
# results_param2 <- summary(mcmc_samples_params2)
# results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
# results_params2$parameter<-row.names(results_params2)
# results_params2$param<-substr(results_params2$parameter,1,1)
# results_params2$day_experiment <- 1:max(indiv_tree$day_experiment)
# 
# ggplot()  + theme_bw() +
#   geom_line(data = results_params2, aes(x = day_experiment, y = Mean), color = "blue") +
#   geom_line(data = results_params2, aes(x = day_experiment, y = X2.5.), lty =2, color = "blue") +
#   geom_line(data = results_params2, aes(x = day_experiment, y = X97.5.), lty = 2, color = "blue") +
#   geom_point(data = indiv_tree, aes(x = day_experiment, y = prop_open), color = "red", size = 2) + 
#    ylab("sacs open (proportion)") + xlab("")
# 
# # ggsave(filename = "C:/Users/dsk856/Box/texas/pheno/manual_obs_models/fig_single_tree_b_210405.jpeg",
# #        width = 10, height = 6, units = "in", scale = 0.5)

### fitting two trees at one site (independently) ##################################################
# p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_data_download_210125_b_edited.csv")
# day_start <- mdy("12-13-2020")
# 
# p <- 
#   p %>% 
#   mutate(prop_open = perc_open/100,
#          date2 = mdy_hm(date),
#          date3 = as_date(date2),
#          site = paste(round(x, 1), round(y, 1)),
#          yday = yday(date2),
#          tree = paste(round(x, 5), round(y, 5)),
#          day_experiment = as.numeric(date3 - day_start)) 
# 
# #graph of two trees at site
# p %>% 
#   filter(tree == "-98.05908 30.82578" | tree == "-98.05905 30.82572") %>% 
#   ggplot(aes(x = date2, y = perc_open, group = tree, color = tree)) + geom_point() + theme_bw() +
#   geom_line() 
# 
# data_for_model_prop_open <- p %>% 
#   filter(tree == "-98.05908 30.82578"  | tree == "-98.05905 30.82572") %>%  #
#   #filter(site == "-98.1 30.8") %>% 
#   mutate(tree_n = as.numeric(as.factor(tree))) %>% 
#   select(tree_n, day_experiment, prop_open) %>% 
#   pivot_wider(id_cols = tree_n, names_from = day_experiment, values_from =prop_open, names_prefix = "d") %>% 
#   arrange(tree_n)%>% 
#   select(-tree_n)
# 
# data_for_model_day_experiment <- p %>% 
#   filter(tree == "-98.05908 30.82578"  | tree == "-98.05905 30.82572") %>%  #
#   #filter(site == "-98.1 30.8") %>% 
#   mutate(tree_n = as.numeric(as.factor(tree))) %>% 
#   select(tree_n, day_experiment, prop_open) %>% 
#   pivot_wider(id_cols = tree_n, names_from = day_experiment, values_from =day_experiment, names_prefix = "d") %>% 
#   arrange(tree_n) %>% 
#   select(-tree_n)
# 
# data_for_model_nobs_per_tree <- p %>% 
#   filter(tree == "-98.05908 30.82578"  | tree == "-98.05905 30.82572") %>%  #
#   mutate(tree_n = as.numeric(as.factor(tree))) %>% 
#   select(tree_n, day_experiment) %>% 
#   arrange(tree_n) %>% 
#   group_by(tree_n) %>% 
#   summarize(nobs_per_tree = n())
# 
# 
# sink("model_b.txt")
# cat("  
# model{
#   
#   #Likelihood
#   
#   for(tree in 1:2){ #NTREE == 2
#   
#   for(i in 1:nobs_per_tree[tree]) #NDAY == c(5,5)
#   {
#     Y_hat[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
#     Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1[tree])
#   }
# 
# 
#   }#end NTREE loop
#   
# #Priors
# for(tree in 1:2){
#   LAMBDA1[tree]~dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   a[tree] ~ dbeta(1, 1) #the asympotote 
#   
#   c[tree] ~ dnorm(0, LAMBDA2[tree]) #the steepness of the curve
#   LAMBDA2[tree] ~ dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   b[tree] ~ dnorm(0, LAMBDA3[tree]) #shifting left and right- When b = log(2), f(0) = a/2, also called the halfway point
#   LAMBDA3[tree] ~ dgamma(0.0001,0.0001) #uninformative gamma prior
# } #end priors NTREE loop
#     
#     
# #simulation
#   for(tree in 1:2){
#   for(i in 1:35){
#     Y_hat_sim[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t_sim[i] - b[tree])))
#   }
#   }
# 
#     
# }#end model
#     ",fill=TRUE)
# sink() 
# 
# jags <- jags.model('model_b.txt', 
#                    data = list(
#                      #general stuff
#                      Y = as.matrix(data_for_model_prop_open),  #data_for_model_prop_open[3,1]
#                      t = as.matrix(data_for_model_day_experiment), #
#                      nobs_per_tree = data_for_model_nobs_per_tree$nobs_per_tree,
#                      t_sim = min(data_for_model_day_experiment):max(data_for_model_day_experiment)
#                      #MAXDAY = max(data_for_model_tree$day_experiment),
#                      #tree_n = data_for_model_tree$tree_n,
#                      #NTREE = length(unique(data_for_model_tree$tree_n))
#                      ),
#                    n.chains = 3,
#                    n.adapt = 100)  # diffuse priors
# 
# #dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
# #Sys.time()
# update(jags,n.iter=10000) 
# mcmc_samples_params <- coda.samples(jags, variable.names=c("a", "b", "c"),  n.iter = 3000, thin = 3) #variables to monitor
# plot(mcmc_samples_params)
# results_param <- summary(mcmc_samples_params)
# 
# #simulation
# mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 3000, thin = 3) #variables to monitor
# #plot(mcmc_samples_params2)
# 
# results_param2 <- summary(mcmc_samples_params2)
# results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
# results_params2$parameter<-row.names(results_params2)
# results_params2$param<-substr(results_params2$parameter,1,1)
# results_params2$day_experiment <- 1:max(data_for_model_day_experiment)
# 
# day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
# results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )
# 
# tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
# results_params2$tree_n <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
# results_params2 <- arrange(results_params2, tree_n, day_experiment)
# 
# ggplot()  + theme_bw() +
#   geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = tree_n, color = tree_n)) +
#   geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
#   geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
#   geom_point(data = p_subset, aes(x = day_experiment, y = prop_open, color = tree_n))
# 
# p_subset <- p %>% 
#   filter(tree == "-98.05908 30.82578" | tree == "-98.05905 30.82572") %>% 
#   mutate(tree_n = as.numeric(as.factor(tree))) 





### fitting all trees at one site (independently) ##################################################
# p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_data_download_210125_b_edited.csv")
# day_start <- mdy("12-13-2020")
# 
# p_site1 <- 
#   p %>%
#   mutate(prop_open = perc_open/100,
#          date2 = mdy_hm(date),
#          date3 = as_date(date2),
#          site = paste(round(x, 1), round(y, 1)),
#          yday = yday(date2),
#          tree = paste(round(x, 5), round(y, 5))) %>% 
#   filter(site == "-98.1 30.8") %>% 
#   filter(!is.na(prop_open)) %>% 
#   mutate(tree_n = as.numeric(as.factor(tree)),
#          day_experiment = as.numeric(date3 - day_start)) %>% 
#   arrange(tree_n, date3) %>% 
#   filter(tree_n < 11)
# 
# #graph of two trees at site
# p_site1 %>% 
#   ggplot(aes(x = date2, y = perc_open, group = tree, color = tree)) + geom_point() + theme_bw() + geom_line() 
# 
# data_for_model_prop_open <- p_site1 %>% 
#   select(tree_n, day_experiment, prop_open) %>% 
#   distinct() %>% 
#   pivot_wider(id_cols = tree_n, names_from = day_experiment, values_from = prop_open, names_prefix = "d") %>% 
#   arrange(tree_n) %>% 
#   select(-tree_n) 
# data_for_model_prop_open_list <- split(as.data.frame(data_for_model_prop_open), seq(nrow(data_for_model_prop_open)))
# data_for_model_prop_open_list <- lapply(data_for_model_prop_open_list, function(x) x[!is.na(x)])
# data_for_model_prop_open_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_prop_open_list, 
#                                                             `length<-`, max(lengths(data_for_model_prop_open_list)))))
# 
# data_for_model_day_experiment <- p_site1 %>% 
#   select(tree_n, day_experiment, prop_open) %>% 
#   distinct() %>% 
#   pivot_wider(id_cols = tree_n, names_from = day_experiment, values_from = day_experiment, names_prefix = "d") %>% 
#   arrange(tree_n) %>% 
#   select(-tree_n) 
# data_for_model_day_experiment_list <- split(as.data.frame(data_for_model_day_experiment), seq(nrow(data_for_model_day_experiment)))
# data_for_model_day_experiment_list <- lapply(data_for_model_day_experiment_list, function(x) x[!is.na(x)])
# data_for_model_day_experiment_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_day_experiment_list, 
#                                                              `length<-`, max(lengths(data_for_model_day_experiment_list)))))
# 
# data_for_model_nobs_per_tree <- p_site1 %>% 
#   select(tree_n, day_experiment) %>% 
#   distinct() %>% 
#   arrange(tree_n) %>% 
#   group_by(tree_n) %>% 
#   summarize(nobs_per_tree = n())
# 
# 
# sink("model_b.txt")
# cat("  
# model{
#   
#   #Likelihood
#   
#   for(tree in 1:n_trees){ 
#   
#   for(i in 1:nobs_per_tree[tree]){
#     Y_hat[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
#     Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1[tree])
#   }
# 
# 
#   }#end NTREE loop
#   
# #Priors
# for(tree in 1:n_trees){
#   LAMBDA1[tree]~dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   a[tree] ~ dbeta(1, 1) #the asympotote 
#   
#   c[tree] ~ dnorm(0, LAMBDA2[tree]) #the steepness of the curve
#   LAMBDA2[tree] ~ dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   b[tree] ~ dnorm(0, LAMBDA3[tree]) #shifting left and right- When b = log(2), f(0) = a/2, also called the halfway point
#   LAMBDA3[tree] ~ dgamma(0.0001,0.0001) #uninformative gamma prior
# } #end priors NTREE loop
#     
#     
# #simulation
#   for(tree in 1:n_trees){
#   for(i in 1:35){
#     Y_hat_sim[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t_sim[i] - b[tree])))
#   }
#   }
# 
#     
# }#end model
#     ",fill=TRUE)
# sink() 
# 
# jags <- jags.model('model_b.txt', 
#                    data = list(
#                      #general stuff
#                      Y = as.matrix(data_for_model_prop_open_ragged),  #data_for_model_prop_open[3,1]
#                      t = as.matrix(data_for_model_day_experiment_ragged), 
#                      nobs_per_tree = data_for_model_nobs_per_tree$nobs_per_tree,
#                      n_trees = nrow(data_for_model_nobs_per_tree),
#                      t_sim = 1:max(data_for_model_day_experiment, na.rm = TRUE)
#                      #MAXDAY = max(data_for_model_tree$day_experiment),
#                      #tree_n = data_for_model_tree$tree_n,
#                      #NTREE = length(unique(data_for_model_tree$tree_n))
#                    ),
#                    n.chains = 3,
#                    n.adapt = 100)  # diffuse priors
# 
# #dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
# #Sys.time()
# update(jags,n.iter=5000) 
# mcmc_samples_params <- coda.samples(jags, variable.names=c("a", "b", "c"),  n.iter = 3000, thin = 3) #variables to monitor
# plot(mcmc_samples_params)
# results_param <- summary(mcmc_samples_params)
# 
# #simulation
# mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 3000, thin = 3) #variables to monitor
# #plot(mcmc_samples_params2)
# 
# results_param2 <- summary(mcmc_samples_params2)
# results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
# results_params2$parameter<-row.names(results_params2)
# results_params2$param<-substr(results_params2$parameter,1,1)
# results_params2$day_experiment <- 1:max(data_for_model_day_experiment, na.rm = TRUE)
# 
# day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
# results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )
# 
# tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
# results_params2$tree_n <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
# results_params2 <- arrange(results_params2, tree_n, day_experiment)
# 
# 
# ggplot()  + theme_bw() +
#   geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = tree_n, color = tree_n)) +
#   # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
#   # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
#   geom_point(data = p_site1, aes(x = day_experiment, y = prop_open), color = "red")





### fitting all trees at one site (hierarchical) ##################################################
# p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
# #p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_data_download_210127.csv")
# day_start <- mdy("12-10-2020")
# 
# p_site1 <- p %>%
#   mutate(prop_open = bag_mean,#perc_open/100,
#          #date2 = mdy_hm(date) - hours(6), #correct from GMT
#          date3 = sample_date,
#          site = paste(round(x, 1), round(y, 1)),
#          yday = yday(date3),
#          tree = paste(round(x, 5), round(y, 5))) %>% 
#   filter(!is.na(perc_open)) %>% 
#   #filter(site == "-98 30.1" | site == "-98.2 29.8") %>% 
#   mutate(tree_n = as.numeric(as.factor(tree)),
#          site_n = as.numeric(as.factor(site)),
#          #tree_visit = paste(c(tree_n, as.character(date3))),
#          day_experiment = as.numeric(date3 - day_start)) %>% 
#   arrange(date3, tree_n) %>% 
#   filter( #remove trees that were added because they were late (for pollen platters)
#     tree!= "-98.17164 29.82707" & tree != "-98.17316 29.82818" & tree != "-98.17282 29.82795" & #wilson/comal
#       tree != "-98.05903 30.82545" & tree != "-98.0598 30.82648" & tree != "-98.06041 30.82647" & tree != "-98.06076 30.82632" &#wade/burnett
#       tree != "-98.03122 30.13753" #hays
#   ) %>% 
#   filter(site == "-98.1 30.8") %>%
#   filter(!is.na(prop_open)) %>%
#   mutate(tree_n = as.numeric(as.factor(tree)),
#          day_experiment = as.numeric(date3 - day_start)) %>%
#   arrange(tree_n, date3)
# 
# 
# # p_site1 <- 
# #   p %>%
# #   mutate(prop_open = perc_open/100,
# #          date2 = mdy_hm(date),
# #          date3 = as_date(date2),
# #          site = paste(round(x, 1), round(y, 1)),
# #          yday = yday(date2),
# #          tree = paste(round(x, 5), round(y, 5))) %>% 
# #   filter(site == "-98.1 30.8") %>% 
# #   filter(!is.na(prop_open)) %>% 
# #   mutate(tree_n = as.numeric(as.factor(tree)),
# #          day_experiment = as.numeric(date3 - day_start)) %>% 
# #   arrange(tree_n, date3)
# # #   filter(tree_n != 5 & tree_n != 14) %>%  #removing a couple trees that didn't open in this time period
# # #   mutate(tree_n = as.numeric(as.factor(tree)))
# # # #%>%  filter(tree_n < 11)
# 
# #graph of two trees at site
# p_site1 %>% 
#   ggplot(aes(x = sample_date, y = perc_open, group = tree, color = tree)) + geom_point() + theme_bw() + geom_line() 
# 
# 
# #graph of a few individual trees
# p_site1%>% 
#   ggplot(aes(x = sample_date, y = prop_open, group = tree_n, color = as.factor(tree_n))) + geom_point(size = 2) + theme_bw() +
#   geom_line() + ylab("sacs open (proportion)") + xlab("") + theme(legend.position = "none")
# 
# # ggsave(filename = "C:/Users/dsk856/Box/texas/pheno/manual_obs_models/fig_single_site_a_210405.jpeg",
# #        width = 10, height = 6, units = "in", scale = 0.5)
# 
# 
# 
# data_for_model_prop_open <- p_site1 %>% 
#   select(tree_n, day_experiment, prop_open) %>% 
#   distinct() %>% 
#   pivot_wider(id_cols = tree_n, names_from = day_experiment, values_from = prop_open, names_prefix = "d") %>% 
#   arrange(tree_n) %>% 
#   select(-tree_n) 
# data_for_model_prop_open_list <- split(as.data.frame(data_for_model_prop_open), seq(nrow(data_for_model_prop_open)))
# data_for_model_prop_open_list <- lapply(data_for_model_prop_open_list, function(x) x[!is.na(x)])
# data_for_model_prop_open_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_prop_open_list, 
#                                                                    `length<-`, max(lengths(data_for_model_prop_open_list)))))
# 
# data_for_model_day_experiment <- p_site1 %>% 
#   select(tree_n, day_experiment, prop_open) %>% 
#   distinct() %>% 
#   pivot_wider(id_cols = tree_n, names_from = day_experiment, values_from = day_experiment, names_prefix = "d") %>% 
#   arrange(tree_n) %>% 
#   select(-tree_n) 
# data_for_model_day_experiment_list <- split(as.data.frame(data_for_model_day_experiment), seq(nrow(data_for_model_day_experiment)))
# data_for_model_day_experiment_list <- lapply(data_for_model_day_experiment_list, function(x) x[!is.na(x)])
# data_for_model_day_experiment_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_day_experiment_list, 
#                                                                         `length<-`, max(lengths(data_for_model_day_experiment_list)))))
# 
# data_for_model_nobs_per_tree <- p_site1 %>% 
#   select(tree_n, day_experiment) %>% 
#   distinct() %>% 
#   arrange(tree_n) %>% 
#   group_by(tree_n) %>% 
#   summarize(nobs_per_tree = n())
# 
# 
# sink("model_b.txt")
# cat("  
# model{
#   
#   #Likelihood
#   
#   for(tree in 1:n_trees){ 
#   
#   for(i in 1:nobs_per_tree[tree]){
#     Y_hat[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
#     Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1[tree])
#   }
# 
# 
#   }#end NTREE loop
#   
# #Priors
# for(tree in 1:n_trees){
#   LAMBDA1[tree]~dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   a[tree] ~ dunif(0.95, 1) #the asympotote #assuming that asymptote is at 1
#   
#   c[tree] ~ dnorm(site_rate, LAMBDA2) #the steepness of the curve
#   
#   b[tree] ~ dnorm(site_halfway_point, LAMBDA3) #shifting left and right- When b = log(2), f(0) = a/2, also called the halfway point
#  
# } #end priors NTREE loop
#   
#   site_halfway_point ~ dnorm(0, 0.001)
#    LAMBDA3 ~ dgamma(0.0001,0.0001) #uninformative gamma prior
#    
#    site_rate ~ dnorm( 0, 0.001)
#    LAMBDA2 ~ dgamma(0.0001,0.0001) #uninformative gamma prior
#     
# #simulation
#   for(tree in 1:n_trees){
#   for(i in 1:max_t){
#     Y_hat_sim[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t_sim[i] - b[tree])))
#   }
#   }
# 
#     
# }#end model
#     ",fill=TRUE)
# sink() 
# 
# jags <- jags.model('model_b.txt', 
#                    data = list(
#                      #general stuff
#                      Y = as.matrix(data_for_model_prop_open_ragged),  #data_for_model_prop_open[3,1]
#                      t = as.matrix(data_for_model_day_experiment_ragged), 
#                      nobs_per_tree = data_for_model_nobs_per_tree$nobs_per_tree,
#                      n_trees = nrow(data_for_model_nobs_per_tree),
#                      t_sim = 1:max(data_for_model_day_experiment, na.rm = TRUE),
#                      max_t = max(data_for_model_day_experiment, na.rm = TRUE)
#                      #MAXDAY = max(data_for_model_tree$day_experiment),
#                      #tree_n = data_for_model_tree$tree_n,
#                      #NTREE = length(unique(data_for_model_tree$tree_n))
#                    ),
#                    n.chains = 3,
#                    n.adapt = 100)  # diffuse priors
# 
# #dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
# #Sys.time()
# update(jags,n.iter=5000) 
# mcmc_samples_params <- coda.samples(jags, variable.names=c("b", "c"),  n.iter = 3000, thin = 3) #variables to monitor
# plot(mcmc_samples_params)
# results_param <- summary(mcmc_samples_params)
# 
# mcmc_samples_params <- coda.samples(jags, variable.names=c("b", "site_halfway_point"),  n.iter = 3000, thin = 3) #variables to monitor
# summary(mcmc_samples_params)
# 
# 
# #simulation
# mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 3000, thin = 3) #variables to monitor
# #plot(mcmc_samples_params2)
# 
# results_param2 <- summary(mcmc_samples_params2)
# results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
# results_params2$parameter<-row.names(results_params2)
# results_params2$param<-substr(results_params2$parameter,1,1)
# results_params2$day_experiment <- 1:max(data_for_model_day_experiment, na.rm = TRUE)
# 
# day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
# results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )
# 
# tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
# results_params2$tree_n <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
# results_params2 <- arrange(results_params2, tree_n, day_experiment)
# 
# 
# ggplot()  + theme_bw() +
#   geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = tree_n, color = as.factor(tree_n))) +
#   # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
#   # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
#   geom_point(data = p_site1, aes(x = day_experiment, y = prop_open, color = as.factor(tree_n))) + 
#   xlab("day of experiment") + ylab("sacs open (proportion)") + theme(legend.position = "none")
# 
# # ggsave(filename = "C:/Users/dsk856/Box/texas/pheno/manual_obs_models/fig_single_site_b_210405.jpeg",
# #        width = 10, height = 6, units = "in", scale = 0.5)
# 



### fitting all trees at all core sites (hierarchical) ##################################################
# #NOTE: NEED TO CHECK ON DISTRIBUTIONS FOR SIGMA
# 
# p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_data_download_210205_b.csv") %>% 
#   mutate(date = case_when( notes_1 == "12/10/2020 12:00 added after field season" ~ "12/10/2020 18:00",
#                            notes_2 == "tree location approx. 1/8/21 ~4pm" ~ "1/8/2021 22:00",
#                            TRUE ~ date))
# 
# day_start <- mdy("12-13-2020")
# 
# p_all_sites <- p %>%
#   mutate(prop_open = perc_open/100,
#          date2 = mdy_hm(date) - hours(6), #correct from GMT
#          date3 = as_date(date2),
#          site = paste(round(x, 1), round(y, 1)),
#          yday = yday(date2),
#          tree = paste(round(x, 5), round(y, 5))) %>% 
#   filter(!is.na(perc_open)) %>% 
#   #filter(site == "-98 30.1" | site == "-98.2 29.8") %>% 
#   mutate(tree_n = as.numeric(as.factor(tree)),
#          site_n = as.numeric(as.factor(site)),
#          day_experiment = as.numeric(date3 - day_start)) %>% 
#   arrange(date3, tree_n) %>% 
#   filter( #remove trees that were added because they were late (for pollen platters)
#     tree!= "-98.17164 29.82707" & tree != "-98.17316 29.82818" & tree != "-98.17282 29.82795" & #wilson/comal
#       tree != "-98.05903 30.82545" & tree != "-98.0598 30.82648" & tree != "-98.06041 30.82647" & tree != "-98.06076 30.82632" &#wade/burnett
#       tree != "-98.03122 30.13753" #hays
#     ) %>% 
#   filter(site != "-97.9 30.7") #this site didnt have any visits after 12/31/20
# 
# # length(unique(p_all_sites$tree))
# # length(unique(p_all_sites$site))
# 
# 
# visits_per_site <- p_all_sites %>%  
#   select(site_n, day_experiment) %>% 
#   distinct() %>% 
#   group_by(site_n) %>% 
#   summarize(n_visits_per_site = n())
# 
# p_all_sites <- left_join(p_all_sites, visits_per_site) %>% 
#   filter(n_visits_per_site > 2) %>% 
#   mutate(tree_n = as.numeric(as.factor(tree)),
#          site_n = as.numeric(as.factor(site)),
#          day_experiment = as.numeric(date3 - day_start)) %>% 
#   arrange(date3, tree_n)
# 
# p_all_sites %>%  ggplot(aes(x = day_experiment, y = prop_open, group = tree)) + geom_line() + facet_wrap(~site)
# #   filter(tree_n != 5 & tree_n != 14) %>%  #removing a couple trees that didn't open in this time period
# #   mutate(tree_n = as.numeric(as.factor(tree)))
# # #%>%  filter(tree_n < 11)
# 
# #graph of trees at all sites
# p_all_sites %>% 
#   ggplot(aes(x = date2, y = perc_open, group = tree, color = site)) + geom_point() + theme_bw() + geom_line() #+ facet_wrap(~site)
# 
# 
# data_for_model_prop_open <- p_all_sites %>% 
#   select(site_n, tree_n, day_experiment, prop_open) %>% 
#   distinct() %>% 
#   group_by(site_n, tree_n, day_experiment) %>% 
#   summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
#   ungroup() %>% 
#   pivot_wider(id_cols = c(site_n, tree_n), names_from = day_experiment, values_from = prop_open, names_prefix = "d") %>% 
#   arrange(tree_n) %>% 
#   select(-tree_n, site_n) %>% 
#   rename(d01 = d1, d02 = d2, d03 = d3, d04 = d4, d09 = d9) %>% 
#   select(sort(tidyselect::peek_vars()))
# data_for_model_prop_open_list <- split(as.data.frame(data_for_model_prop_open), seq(nrow(data_for_model_prop_open)))
# data_for_model_prop_open_list <- lapply(data_for_model_prop_open_list, function(x) x[!is.na(x)])
# data_for_model_prop_open_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_prop_open_list, 
#                                                                    `length<-`, max(lengths(data_for_model_prop_open_list)))))
# 
# data_for_model_day_experiment <- p_all_sites %>% 
#   select(site_n, tree_n, day_experiment, prop_open) %>% 
#   distinct() %>% 
#   group_by(site_n, tree_n, day_experiment) %>% 
#   summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
#   ungroup() %>% 
#   pivot_wider(id_cols = c(site_n, tree_n), names_from = day_experiment, values_from = day_experiment, names_prefix = "d") %>% 
#   arrange(tree_n) %>% 
#   select(-tree_n, site_n) %>% 
#   rename(d01 = d1, d02 = d2, d03 = d3, d04 = d4, d09 = d9) %>% 
#   select(sort(tidyselect::peek_vars()))
# data_for_model_day_experiment_list <- split(as.data.frame(data_for_model_day_experiment), seq(nrow(data_for_model_day_experiment)))
# data_for_model_day_experiment_list <- lapply(data_for_model_day_experiment_list, function(x) x[!is.na(x)])
# data_for_model_day_experiment_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_day_experiment_list, 
#                                                                    `length<-`, max(lengths(data_for_model_day_experiment_list)))))
# 
# data_for_model_nobs_per_tree <- p_all_sites %>% 
#   select(site_n, tree_n, day_experiment) %>% 
#   distinct() %>% 
#   arrange(tree_n, site_n) %>% 
#   group_by(tree_n, site_n) %>% 
#   summarize(nobs_per_tree = n())
# 
# sink("model_b.txt")
# cat("  
# model{
#   
#   #Likelihood
# 
#   for(tree in 1:n_trees){ 
#   
#   for(i in 1:nobs_per_tree[tree]){
#     Y_hat[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
#     Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1[tree])
#   } #end obs loop
#   } #end tree loop
# 
#   
# #Priors
# 
# for(tree in 1:n_trees){
#   LAMBDA1[tree]~dgamma(0.0001,0.0001) #uninformative gamma prior
#   
#   a[tree] ~ dunif(0.95, 1) #the asympotote #assuming that asymptote is at 1
#   
#   c[tree] ~ dnorm(rate_global_mean, rate_global_sigma) #the steepness of the curve
#   
#   b[tree] ~ dnorm(site_halfway_point[site_vector[tree]], LAMBDA3[site_vector[tree]]) #shifting left and right- When b = log(2), f(0) = a/2, also called the halfway point
#  
# } #end priors tree loop
# 
# 
# for(site in 1:n_sites){
#    site_halfway_point[site] ~ dnorm(0, 0.001)
#    LAMBDA3[site] ~ dgamma(0.0001,0.0001) #uninformative gamma prior
# } #end priors site loop
# 
# rate_global_mean ~ dnorm(0, 0.001)
# rate_global_sigma ~ dgamma(0.0001,0.0001)
# c_sim ~ dnorm(rate_global_mean, rate_global_sigma)
# 
# #simulation for each tree
#   for(tree in 1:n_trees){
#     for(i in 1:max_t){
#       Y_hat_sim[tree, i] <- a[tree]  * exp( -exp(-c_sim * (t_sim[i] - b[tree])))
#     }
#   }
# 
# #simulation for each site mean
# for(site in 1:n_sites){
#     for(i in 1:max_t){
#       Y_hat_sim_site[site, i] <- 1 * exp( -exp(-c_sim * (t_sim[i] - b_site_sim[site])))
#     }
#       b_site_sim[site] ~ dnorm(site_halfway_point[site], LAMBDA3[site])
# } #end site sim loop
# 
#     
# }#end model
#     ",fill=TRUE)
# sink() 
# 
# jags <- jags.model('model_b.txt', 
#                    data = list(
#                      Y = as.matrix(data_for_model_prop_open_ragged),  #data_for_model_prop_open[3,1]
#                      t = as.matrix(data_for_model_day_experiment_ragged), 
#                      nobs_per_tree = data_for_model_nobs_per_tree$nobs_per_tree,
#                      n_trees = nrow(data_for_model_nobs_per_tree),
#                      n_sites = max(data_for_model_nobs_per_tree$site_n),
#                      site_vector = data_for_model_nobs_per_tree$site_n,
#                      t_sim = 1:max(data_for_model_day_experiment, na.rm = TRUE),
#                      max_t = max(p_all_sites$day_experiment, na.rm = TRUE)
#                    ),
#                    n.chains = 3,
#                    n.adapt = 100)  # diffuse priors
# 
# #dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
# #Sys.time()
# update(jags,n.iter=5000) 
# mcmc_samples_params <- coda.samples(jags, variable.names=c("b", "c"),  n.iter = 3000, thin = 3) #variables to monitor
# plot(mcmc_samples_params)
# results_param <- summary(mcmc_samples_params)
# 
# mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point"),  n.iter = 5000, thin = 3) #variables to monitor
# summary(mcmc_samples_params)
# 
# 
# #simulation for each tree
# mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 3000, thin = 3) #variables to monitor
# #plot(mcmc_samples_params2)
# 
# results_param2 <- summary(mcmc_samples_params2)
# results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
# results_params2$parameter<-row.names(results_params2)
# results_params2$param<-substr(results_params2$parameter,1,1)
# results_params2$day_experiment <- 1:max(data_for_model_day_experiment, na.rm = TRUE)
# 
# day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
# results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )
# 
# tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
# results_params2$tree_n <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
# results_params2 <- arrange(results_params2, tree_n, day_experiment)
# 
# 
# ggplot()  + theme_bw() +
#   geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = tree_n, color = tree_n)) +
#   # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
#   # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
#   geom_jitter(data = p_all_sites, aes(x = day_experiment, y = prop_open), color = "red") +
#   facet_wrap(~site_n)
# 
# 
# #simulation for each site
# mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim_site"),  n.iter = 3000, thin = 3) #variables to monitor
# #plot(mcmc_samples_params2)
# 
# results_param2 <- summary(mcmc_samples_params2)
# results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
# results_params2$parameter<-row.names(results_params2)
# results_params2$param<-substr(results_params2$parameter,1,1)
# results_params2$day_experiment <- 1:max(data_for_model_day_experiment, na.rm = TRUE)
# 
# day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
# results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )
# 
# site_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
# results_params2$site_n <- as.numeric(gsub("Y_hat_sim_site[", "", site_n_vector, fixed = TRUE) )
# results_params2 <- arrange(results_params2, site_n, day_experiment)
# 
# ggplot()  + theme_bw() +
#   geom_ribbon(data = results_params2, aes(x = day_experiment, y = Mean, ymin = X25., ymax = X75., group = as.factor(site_n), 
#                                           fill = as.factor(site_n)), alpha = 0.1) +
#   geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = site_n, color = as.factor(site_n))) +
#   geom_jitter(data = p_all_sites, aes(x = day_experiment, y = prop_open), color = "red") 
# 
# #site means across space
# tx_boundary <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
# site_locations <- p_all_sites %>% 
#   mutate(site_long = round(x, 1),
#          site_lat = round(y,1)) %>% 
#   select(site_n, site_long, site_lat) %>% distinct() 
# #ggplot(aes(x= site_long, y = site_lat)) + geom_point()
# 
# site_halfway_sim <- coda.samples(jags, variable.names=c("b_site_sim"),  n.iter = 2000, thin = 3) #variables to monitor
# site_halfway_params <- summary(site_halfway_sim)
# site_halfway_params2 <- data.frame(site_halfway_params$statistics, site_halfway_params$quantiles) #multi-var model
# site_halfway_params2$parameter <- row.names(site_halfway_params2)
# site_halfway_params2$site_n <- as.numeric(gsub("[^0-9.-]", "", site_halfway_params2$parameter))
# 
# site_halfway_params2 <- left_join(site_halfway_params2, site_locations)
# 
# site_halfway_params2_filt <-  site_halfway_params2 %>% filter(site_n != 5)
# ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
#   geom_point(aes(x = site_long, y = site_lat, col = Mean),# size = pollen),#col = hilo2), pollen / max_p
#              data = site_halfway_params2_filt, alpha = .7, size = 3)  + 
#   scale_color_continuous(low = "blue", high = "red", name = "halfway point (day)") + 
#   xlab("") + ylab("") + #theme_few() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   coord_sf(datum=NA) #removes sf induced gridlines





### fitting all trees at all core sites and all snapshot sites (hierarchical) #############################


#database was put together in: 
#C:/Users/dsk856/Box/texas/pheno/manual_obs/manual sac counts/sac_count_processing210303.R
p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
#str(p)
day_start <- mdy("12-10-2020")

core_site_list <- c("chumlea", "comal", "hays", "hurst", "jewell", "puccetti",  
                    "sablatura", "shepperd", "wade", "rogers", "menard") #

p_all_sites <- p %>%
  mutate(prop_open = bag_mean,#perc_open/100,
         #date2 = mdy_hm(date) - hours(6), #correct from GMT
         date3 = sample_date,
         site = site_name,
         yday = yday(date3),
         tree = paste(round(x, 5), round(y, 5))) %>% 
  filter(!is.na(perc_open)) %>% 
  mutate(tree_n = as.numeric(as.factor(tree)),
         site_n = as.numeric(as.factor(site)),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(date3, tree_n) %>% 
  filter( #remove trees that were added because they were late (for pollen platters)
    tree!= "-98.17164 29.82707" & tree != "-98.17316 29.82818" & tree != "-98.17282 29.82795" & #wilson/comal
      tree != "-98.05903 30.82545" & tree != "-98.0598 30.82648" & tree != "-98.06041 30.82647" & tree != "-98.06076 30.82632" &#wade/burnett
      tree != "-98.03122 30.13753" #hays
  ) %>% 
  mutate(site_type = case_when(site_name %in% core_site_list ~ "core", TRUE ~ "snap")) %>% 
  mutate(site_name = case_when(site_name == "canyon.lake.dam" & date3 == ymd("2020-12-22") ~ "canyon.lake.dam_visit1",
                               site_name == "rogers" & date3 == ymd("2020-12-14") ~ "rogers_visit1",
                               site_name == "rogers" & date3 == ymd("2020-12-31") ~ "rogers_visit3",
                               site_name == "menard" & date3 == ymd("2020-12-28") ~ "menard_visit_2",
                               site_name == "x1311.near.menard.site" & date3 == ymd("2020-12-28") ~ "x1311.near.menard.site_visit2",
                               site_name == "pass.to.the.w.of.hext" & date3 == ymd("2020-12-28") ~ "pass.to.the.w.of.hext_visit2",
                               TRUE ~ site_name)) #unique(p_all_sites$site_name)

# length(unique(p_all_sites$tree))
# length(unique(p_all_sites$site))
# p_all_sites %>% 
# ggplot(aes(y = perc_open/100, x = bag_mean)) + geom_point(alpha = .3) + theme_bw() + ylab("pollen sacs open (proportion)") + 
#   xlab("cone scales parted (proportion)") + geom_abline(slope = 1, lty = 2) +#geom_smooth(se = FALSE) + 
#   facet_wrap(~site)
# 
# summary(lm(bag_mean ~ perc_open, data = p_all_sites))

#some QAQC: trees where the proportion of open cones went down at next visit?
# test <- p_all_sites %>% arrange(tree_n, date3) %>% 
#   group_by(tree_n) %>% 
#   mutate(dif_cones = lag(bag_mean, n = 1)) %>% 
#   filter(site == "-97.6 32.2") 
# hist(test$dif_cones)

# incorporate some summary information: visits per site 
visits_per_site <- p_all_sites %>%  
  dplyr::select(site_n, day_experiment) %>% 
  distinct() %>% 
  group_by(site_n) %>% 
  summarize(n_visits_per_site = n())

#incorporate some summary information: average cones open per site visit
average_cone_open_per_site <- p_all_sites %>%  
  group_by(site_n, day_experiment) %>% 
  mutate(cone_opening_stage = case_when(prop_open < 0.01 ~ 0,
                                        prop_open > 0.98 ~ 0,
                                        TRUE ~ 1)) %>% 
  summarize(site_mean_visit = mean(prop_open, na.rm = TRUE),
            site_prop_open_visit = mean(cone_opening_stage, na.rm = TRUE))

p_all_sites_summarized <- left_join(p_all_sites, visits_per_site)
p_all_sites_summarized <- left_join(p_all_sites_summarized, average_cone_open_per_site)


p_core_sites <- p_all_sites_summarized %>% 
  filter(site_type == "core") %>% 
  arrange(site, date3) %>% 
  mutate(tree_n_core = as.numeric(forcats::fct_inorder(tree)),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(tree_n_core) %>% #so trees and in sites are both in order
  mutate(site_n_core = as.numeric(forcats::fct_inorder(site)))
length(unique(p_all_sites$site_n))
length(unique(p_core_sites$site_n))

p_snap_sites <- p_all_sites_summarized %>% 
  filter(site_prop_open_visit > 0.025) %>% #remove sites that hadn't started or had already finished
  filter(site_mean_visit > 0.025 & site_mean_visit < 0.975) %>% #remove sites that hadn't started or had already finished
  filter(site_type == "snap") %>% 
  arrange(site, date3) %>% #so trees and in sites are both in order
  mutate(tree_n_snap = as.numeric(forcats::fct_inorder(tree)),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(tree_n_snap) %>% #so trees and in sites are both in order
  mutate(site_n_snap = as.numeric(forcats::fct_inorder(site)))

# 
# p_core_sites %>%  ggplot(aes(x = day_experiment, y = prop_open, group = tree_n)) + geom_line() + facet_wrap(~site)
# 
# filter(p_core_sites, site == "-97.6 32.2") %>% 
#   ggplot(aes(x = day_experiment, y = prop_open, group = tree_n, color = tree_n)) + geom_line() + facet_wrap(~site)
# 
# p_snap_sites %>%  ggplot(aes(x = day_experiment, y = prop_open, group = tree)) + geom_point() + theme_bw()
# #   filter(tree_n != 5 & tree_n != 14) %>%  #removing a couple trees that didn't open in this time period
# #   mutate(tree_n = as.numeric(as.factor(tree)))
# # #%>%  filter(tree_n < 11)
# 
# #graph of cones open vs pollen sacs open
# p_core_sites %>% 
#   ggplot(aes(x = sample_datetime, y = perc_open, group = tree, color = site)) + geom_point() + theme_bw() + geom_line() #+ facet_wrap(~site)
# 
# #graph of cones opening on a single tree
# p_core_sites %>% 
#   ggplot(aes(x = sample_datetime, y = perc_open, group = tree, color = site)) + geom_point() + theme_bw() + geom_line() #+ facet_wrap(~site)

data_for_model_core_prop_open <- p_core_sites %>% 
  dplyr::select(site_n_core, tree_n_core, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n_core, tree_n_core, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n_core, tree_n_core), names_from = day_experiment, values_from = prop_open, names_prefix = "d") %>% 
  arrange(tree_n_core) %>% 
  dplyr::select(-tree_n_core, site_n_core) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  dplyr::select(sort(tidyselect::peek_vars()))
data_for_model_core_prop_open_list <- split(as.data.frame(data_for_model_core_prop_open), seq(nrow(data_for_model_core_prop_open)))
data_for_model_core_prop_open_list <- lapply(data_for_model_core_prop_open_list, function(x) x[!is.na(x)])
data_for_model_core_prop_open_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_prop_open_list, 
                                                                        `length<-`, max(lengths(data_for_model_core_prop_open_list)))))

data_for_model_core_day_experiment <- p_core_sites %>% 
  dplyr::select(site_n_core, tree_n_core, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n_core, tree_n_core, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n_core, tree_n_core), names_from = day_experiment, values_from = day_experiment, names_prefix = "d") %>% 
  arrange(tree_n_core) %>% 
  dplyr::select(-tree_n_core, site_n_core) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  dplyr::select(sort(tidyselect::peek_vars()))
data_for_model_core_day_experiment_list <- split(as.data.frame(data_for_model_core_day_experiment), seq(nrow(data_for_model_core_day_experiment)))
data_for_model_core_day_experiment_list <- lapply(data_for_model_core_day_experiment_list, function(x) x[!is.na(x)])
data_for_model_core_day_experiment_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_day_experiment_list, 
                                                                             `length<-`, max(lengths(data_for_model_core_day_experiment_list)))))

data_for_model_core_nobs_per_tree <- p_core_sites %>% 
  dplyr::select(site_n_core, tree_n_core, day_experiment) %>% 
  distinct() %>% 
  arrange(tree_n_core, site_n_core) %>% 
  group_by(tree_n_core, site_n_core) %>% 
  summarize(nobs_per_tree = n())

data_for_model_snap <- p_snap_sites %>% 
  #filter(is.na(real_data)) %>% 
  dplyr::select(day_experiment, date3, prop_open, site_n_snap, site_name, tree_n_snap) %>% 
  arrange(tree_n_snap, site_n_snap, day_experiment) 

#CURRENT ISSUE: THE VARIANCE ON SITE MEANS IS TOO CONSTRAINED

sink("model_b.txt")
cat("  
model{
  
  #Likelihood

#core trees loop
  for(tree in 1:n_trees_core){ 
  for(i in 1:nobs_per_tree_core[tree]){
    Y_hat[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
    Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1)
  } #end obs loop
  } #end tree loop

#snapshot trees loop
  for(tree_snap in 1:n_trees_snap){
    Y_hat_snap[tree_snap] <- 1  * exp( -exp(-rate_global_mean_snap * (t_snap[tree_snap] - b_snap[tree_snap])))
    Y_snap[tree_snap] ~ dnorm(Y_hat_snap[tree_snap], LAMBDA1_snap)
  } #end tree loop

  
  
#Priors
for(tree in 1:n_trees_core){
  a[tree] ~ dunif(0.95, 1) #assuming that some trees may asymptote before 1
  
  b[tree] ~ dnorm(site_halfway_point_core[site_vector_core[tree]], LAMBDA3_core[site_vector_core[tree]]) #shifting left and right- When b = log(2), f(0) = a/2, also called the halfway point
 
  c[tree] ~ dnorm(rate_global_mean, rate_global_sigma) #the steepness of the curve
} #end priors core trees loop


for(tree in 1:n_trees_snap){
  b_snap[tree] ~ dnorm(site_halfway_point_snap[site_n_snap[tree]], LAMBDA3_snap) #shifting left and right-
} #end priors snap tree loop


for(site in 1:n_sites_core){
   site_halfway_point_core[site] ~ dnorm(0, 0.001)
   LAMBDA3_core[site] ~ dgamma(0.01,0.01) #uninformative gamma prior
} #end priors site loop

for(site in 1:n_sites_snap){
   site_halfway_point_snap[site] ~ dnorm(0, 0.001)
} #end priors site loop

LAMBDA1 ~ dgamma(0.01,0.01) #uninformative gamma prior
LAMBDA1_snap <- 7.3 #using the value from the core trees


LAMBDA3_snap <- 0.049 #using the value from the core trees
  
rate_global_mean ~ dnorm(0, 0.001)
rate_global_sigma ~ dgamma(0.01,0.01)

rate_global_mean_snap <- rate_global_mean

#simulation for each tree core
  for(tree in 1:n_trees_core){
    for(i in 1:max_t){
      Y_hat_sim[tree, i] <- a[tree]  * exp( -exp(-rate_global_mean * (t_sim[i] - b[tree])))
    }
  }

#simulation for each tree snap
  for(tree in 1:n_trees_snap){
    for(i in 1:max_t){
      Y_hat_sim_snap[tree, i] <- 1  * exp( -exp(-rate_global_mean_snap * (t_sim[i] - b_snap[tree])))
    }
  }


# #Site level simulations - not operational after changing LAMBDA3 to global level
# #simulation for each site mean core
# for(site in 1:n_sites_core){
#     for(i in 1:max_t){
#       Y_hat_sim_site[site, i] <- 1 * exp( -exp(-rate_global_mean * (t_sim[i] - b_site_sim_core[site])))
#     }
#       b_site_sim_core[site] ~ dnorm(site_halfway_point_core[site], LAMBDA3_core)
# } #end site sim loop core
# #simulation for each site mean snap
# for(site in 1:n_sites_snap){
#     for(i in 1:max_t){
#       Y_hat_sim_site_snap[site, i] <- 1 * exp( -exp(-rate_global_mean_snap * (t_sim[i] - b_site_sim_snap[site])))
#     }
#       b_site_sim_snap[site] ~ dnorm(site_halfway_point_snap[site], LAMBDA3_snap)
# } #end site sim loop snap
    
}#end model
    ",fill=TRUE)
sink() 

jags <- jags.model('model_b.txt', 
                   data = list(
                     Y = as.matrix(data_for_model_core_prop_open_ragged),  #data_for_model_prop_open[3,1]
                     t = as.matrix(data_for_model_core_day_experiment_ragged), 
                     nobs_per_tree_core = data_for_model_core_nobs_per_tree$nobs_per_tree,
                     n_trees_core = nrow(data_for_model_core_nobs_per_tree),
                     n_sites_core = max(data_for_model_core_nobs_per_tree$site_n_core),
                     site_vector_core = data_for_model_core_nobs_per_tree$site_n_core,
                     t_sim = 1:max(data_for_model_core_day_experiment, na.rm = TRUE),
                     max_t = max(p_core_sites$day_experiment, na.rm = TRUE),
                     
                     #snap sites
                     Y_snap = data_for_model_snap$prop_open,
                     t_snap = data_for_model_snap$day_experiment,
                     #tree_n_snap = data_for_model_snap$tree_n,
                     site_n_snap = data_for_model_snap$site_n_snap,
                     n_trees_snap = nrow(data_for_model_snap),
                     n_sites_snap = max(data_for_model_snap$site_n_snap)
                   ),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter=5000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_snap"),  n.iter = 1000, thin = 3) #variables to monitor
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)

mcmc_samples_params <- coda.samples(jags, variable.names=c("LAMBDA3_core"),  n.iter = 1000, thin = 3) #variables to monitor
plot(mcmc_samples_params)
summary(mcmc_samples_params)



#simulation for each tree core
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 1000, thin = 3) #variables to monitor
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n_core <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n_core, day_experiment) 
site_n_core_join <- dplyr::select(p_core_sites, tree_n_core, site_n_core)
results_params3 <- left_join(results_params2, site_n_core_join)

ggplot()  + theme_bw() +
  geom_line(data = results_params3, aes(x = day_experiment, y = Mean, group = tree_n_core, color = tree_n_core)) +
  geom_jitter(data = p_core_sites, aes(x = day_experiment, y = prop_open), width = 2, color = "red") +
  facet_wrap(~site_n_core)


## simulation for each tree snap
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim_snap"),  n.iter = 1000, thin = 3) #3000 iterations ran out of memory
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n_snap <- as.numeric(gsub("Y_hat_sim_snap[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n_snap, day_experiment) 
site_n_snap_join <- dplyr::select(data_for_model_snap, tree_n_snap, site_n_snap) %>% distinct()
results_params3 <- left_join(results_params2, site_n_snap_join) %>% 
  mutate(date3 = day_start + day_experiment)

results_params4 <- filter(results_params3, date3 > mdy("12-15-2020") & date3 < mdy("02-01-2021")) 
ggplot()  + theme_bw() +
  geom_line(data = results_params4, aes(x = date3, y = Mean, group = tree_n_snap)) +
  geom_jitter(data = data_for_model_snap, aes(x = date3, y = prop_open), width = 2, color = "red") +
  facet_wrap(~site_n_snap) + xlab("day of experiment") + ylab("cones open (proportion)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### extract site means for each site: core
mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_core"),  n.iter = 10000, thin = 3) 
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$site_n_core <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
site_n_core_join <- dplyr::select(p_core_sites, site_name, site_n_core, x, y) %>% 
  group_by(site_name, site_n_core) %>% 
  summarize(x_site = mean(x, na.rm = TRUE),
            y_site = mean(y, na.rm = TRUE)) %>% distinct()
site_halfway_point_core <- left_join(results_params2, site_n_core_join)

### extract site means for each site: snap
mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_snap"),  n.iter = 10000, thin = 3) 
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$site_n_snap <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
site_n_snap_join <- dplyr::select(p_snap_sites, site_name, site_n_snap, x, y) %>% 
  group_by(site_name, site_n_snap) %>% 
  summarize(x_site = mean(x, na.rm = TRUE),
            y_site = mean(y, na.rm = TRUE)) %>% distinct()
site_halfway_point_snap <- left_join(results_params2, site_n_snap_join)

site_export_df <- bind_rows(site_halfway_point_core, site_halfway_point_snap)  %>% 
  filter(SD < 5) #remove the sites that I didn't have enough info from


## visualize site means: forest plot
site_export_df %>% 
  mutate(site_name = as.factor(site_name),
         site_name = reorder(site_name, Mean)) %>% 
  ggplot(aes(x = site_name, y = Mean + day_start, ymin = Mean - SD + day_start, ymax = Mean + SD + day_start))  + 
  theme_bw() + geom_pointrange() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ylab("Halfway point of season (mean +/- SD")+ xlab("site")

## visualize site means across space
tx_boundary <- sf::read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = x_site, y = y_site, col = Mean),# size = pollen),#col = hilo2), pollen / max_p
             data = site_export_df, alpha = .7, size = 3)  + 
  scale_color_continuous(low = "blue", high = "red", name = "halfway point (day)") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines

## export site means data
readr::write_csv(site_export_df, "C:/Users/dsk856/Box/texas/pheno/fs20_21_site_halfway_sacs_210909.csv")


#export an idealized opening trajectory from all core sites

######### save idealized opening trajectory across all core sites 
#get b (midpoint) for each core tree
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("b"),  n.iter = 3000, thin = 3) #variables to monitor
results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter <- row.names(results_params2)
results_params2$tree_n_core <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
tree_b <- results_params2
tree_b_join <- tree_b %>% 
              dplyr::select(b_mean = Mean,
                            b_sd = SD,
                            tree_n_core)

#get Y_hat for each core tree on each day
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 3000, thin = 3) #variables to monitor
results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n_core <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n_core, day_experiment) 
site_n_core_join <- dplyr::select(p_core_sites, tree_n_core, site_n_core)
results_params3 <- left_join(results_params2, site_n_core_join)
yhat_join <- results_params3 %>% distinct() %>% 
              dplyr::select(yhat_mean = Mean,
                            yhat_sd = SD,
                            day_experiment, tree_n_core, site_n_core)

idealized_sac_opening_curve <- left_join(yhat_join, tree_b_join) %>% 
  mutate(day_before_peak = round(day_experiment - b_mean), 0) %>% 
  group_by(day_before_peak) %>% 
  filter(site_n_core < 5 | site_n_core > 8) %>% 
  summarize(yhat_median = median(yhat_mean, na.rm = TRUE)) %>% 
  # mutate(yhat_median = case_when(day_before_peak < -15 ~ 0,
  #                                day_before_peak > 35 ~ 0.9715,TRUE ~ yhat_median)) %>%  # a manual fix for some weird noise
  mutate(sac_opening_day = yhat_median - lag(yhat_median)) %>% 
  mutate(sac_opening_day = case_when(sac_opening_day < 0 ~ 0, TRUE ~ sac_opening_day))
  
ggplot(idealized_sac_opening_curve, aes(x = day_before_peak, y = sac_opening_day)) + geom_point() + theme_bw()
#write_csv(idealized_sac_opening_curve, "C:/Users/dsk856/Box/texas/pheno/manual_obs/idealized_sac_opening_curve_210910.csv")




### pollen cones, not pollen sacs: fitting all trees at all core sites and all snapshot sites (hierarchical) #############################

#database was put together in: 
#C:/Users/dsk856/Box/texas/pheno/manual_obs/manual sac counts/sac_count_processing210303.R
p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
length(unique(paste(p$x, p$y)))

p %>% group_by(pollen_rel) %>% summarize(n = n()/ (142+231 + 540 + 169))
summarize(p$pollen_rel)

length(na.omit(p$bag_mean[p$bag_mean > 0.99]))/nrow(p)

length(p$bag_cones_opening[p$bag_cones_opening > 0])
length(p$bag_cones_opening[p$bag_cones_unopened > 0])
length(p$perc_open[p$perc_open > 96])/nrow(p)

#str(p)
day_start <- mdy("12-10-2020")

core_site_list <- c("chumlea", "comal", "hays", "hurst", "jewell", "puccetti",  
                    "sablatura", "shepperd", "wade") #"rogers", "menard"

p_all_sites <- p %>%
  mutate(prop_open = perc_open/100,
         #date2 = mdy_hm(date) - hours(6), #correct from GMT
         date3 = sample_date,
         site = site_name, #paste(round(x, 1), round(y, 1)),
         doy = yday(date3),
         tree = paste(round(x, 5), round(y, 5))) %>% 
  filter(!is.na(perc_open)) %>% 
  mutate(tree_n = as.numeric(as.factor(tree)),
         site_n = as.numeric(as.factor(site)),
         #tree_visit = paste(c(tree_n, as.character(date3))),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(date3, tree_n) %>% 
  filter( #remove trees that were added because they were late (for pollen platters)
    tree!= "-98.17164 29.82707" & tree != "-98.17316 29.82818" & tree != "-98.17282 29.82795" & #wilson/comal
      tree != "-98.05903 30.82545" & tree != "-98.0598 30.82648" & tree != "-98.06041 30.82647" & tree != "-98.06076 30.82632" &#wade/burnett
      tree != "-98.03122 30.13753" #hays
  ) %>% 
  mutate(site_type = case_when(site_name %in% core_site_list ~ "core",
                               TRUE ~ "snap")) %>% 
  mutate(prop_open  = case_when(prop_open >= 0.99 ~ 0.99, #I don't think the number of open cones above 99% is robust due to cones that will
                                prop_open < 0.99 ~ prop_open)) %>% #never open and dropping of other cones. So I'm trying 95% as the asymptote
  #changing site name for duplicate sites (That didn't have enough data to be considered core sites)
  mutate(site_name = case_when(site_name == "canyon.lake.dam" & date3 == ymd("2020-12-22") ~ "canyon.lake.dam_visit1",
                                      site_name == "rogers" & date3 == ymd("2020-12-14") ~ "rogers_visit1",
                                      site_name == "rogers" & date3 == ymd("2020-12-31") ~ "rogers_visit3",
                                      site_name == "menard" & date3 == ymd("2020-12-28") ~ "menard_visit_2",
                                      site_name == "x1311.near.menard.site" & date3 == ymd("2020-12-28") ~ "x1311.near.menard.site_visit2",
                                      site_name == "pass.to.the.w.of.hext" & date3 == ymd("2020-12-28") ~ "pass.to.the.w.of.hext_visit2",
                                      TRUE ~ site_name)) #unique(p_all_sites$site_name)
#  filter(duplicate_visits == "single visit") #find duplicate visits at what should be snapshot sites and remove them 
#%>% group_by(site_name, tree_n) %>% summarize(n_visits = n()) 

#average number of trees per site
#p_all_sites %>% dplyr::select(site, tree) %>% distinct() %>% group_by(site)%>% summarize(n = n()) %>% ungroup() %>% summarize(mean_n = mean(n))


#adding in the assumption that all cones were closed on Dec 1 and open on March 1
p_all_sites_bound_start <- p_all_sites %>% 
  dplyr::select(tree, tree_n, site, site_n, site_name, site_type) %>% 
  distinct() %>% 
  mutate(prop_open = 0,
         date3 = mdy("12-01-2020"),
         day_experiment = as.numeric(date3 - day_start),
         doy = yday(date3),
         real_data = "not real data")
p_all_sites_bound_end <- p_all_sites %>% 
  dplyr::select(tree, tree_n, site, site_n, site_name, site_type) %>% 
  distinct() %>% 
  mutate(prop_open = 0.99,
         date3 = mdy("03-01-2021"),
         day_experiment = as.numeric(date3 - day_start),
         doy = yday(date3),
         real_data = "not real data")

p_all_sites <- bind_rows(p_all_sites, p_all_sites_bound_start, p_all_sites_bound_end)



# length(unique(p_all_sites$tree))
# length(unique(p_all_sites$site))

# #compare open pollen cones to open pollen sacs
# p_all_sites %>%
# ggplot(aes(y = perc_open/100, x = bag_mean)) + geom_point(alpha = .3) + theme_bw() + ylab("pollen sacs open (proportion)") +
#   xlab("cone scales parted (proportion)") + geom_abline(slope = 1, lty = 2) +#geom_smooth(se = FALSE) +
#   facet_wrap(~site)

# summary(lm(bag_mean ~ perc_open, data = p_all_sites))

#some QAQC: trees where the proportion of open cones went down at next visit?
# test <- p_all_sites %>% arrange(tree_n, date3) %>%  
#   group_by(tree_n) %>% 
#   mutate(dif_cones = lag(bag_mean, n = 1)) %>% 
#   filter(site == "-97.6 32.2") 
# hist(test$dif_cones)


# incorporate some summary information: visits per site 
visits_per_site <- p_all_sites %>%  
  dplyr::select(site_n, day_experiment) %>% 
  distinct() %>% 
  group_by(site_n) %>% 
  summarize(n_visits_per_site = n())

#incorporate some summary information: average cones open per site visit
average_cone_open_per_site <- p_all_sites %>%  
  group_by(site_n, day_experiment) %>% 
  mutate(cone_opening_stage = case_when(prop_open < 0.01 ~ 0,
                                        prop_open > 0.98 ~ 0,
                                        TRUE ~ 1)) %>% 
  summarize(site_mean_visit = mean(prop_open, na.rm = TRUE),
            site_prop_open_visit = mean(cone_opening_stage, na.rm = TRUE))

p_all_sites_summarized <- left_join(p_all_sites, visits_per_site)
p_all_sites_summarized <- left_join(p_all_sites_summarized, average_cone_open_per_site)

p_core_sites <- p_all_sites_summarized %>% 
  filter(site_type == "core") %>% 
  arrange(site, date3) %>% 
  mutate(tree_n_core = as.numeric(forcats::fct_inorder(tree)),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(tree_n_core) %>% #so trees and in sites are both in order
  mutate(site_n_core = as.numeric(forcats::fct_inorder(site)))
length(unique(p_all_sites$site_n))
length(unique(p_core_sites$site_n))


p_core_sites_tree_key <- p_core_sites %>% 
  dplyr::select(site_name, tree_n_core, x, y) %>% 
  group_by(tree_n_core, site_name) %>%  #not sure why 'distinct' wasn't working for coordinates here
  summarize(x = mean(x, na.rm = TRUE),
            y = mean(y, na.rm = TRUE))


p_snap_sites <- p_all_sites_summarized %>% 
  filter(site_prop_open_visit > 0.025) %>% #remove sites that hadn't started or had already finished
  filter(site_mean_visit > 0.025 & site_mean_visit < 0.975) %>% #remove sites that hadn't started or had already finished
  filter(site_type == "snap") %>% 
  arrange(site, date3) %>% #so trees and in sites are both in order
  mutate(tree_n_snap = as.numeric(forcats::fct_inorder(tree)),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(tree_n_snap) %>% #so trees and in sites are both in order
  mutate(site_n_snap = as.numeric(forcats::fct_inorder(site)))


#export sites for map in arcgis fig1_pheno_sites_210811.mxd
#p_core_sites %>% dplyr::select(site_name) %>% distinct()
# p_core_sites %>% group_by(site_name) %>% 
#   summarize(x = mean(x),
#             y = mean(y)) %>% 
#   dplyr::select(x, y, site_name) %>% distinct() %>% 
#   st_as_sf(., coords = c("x", "y")) %>% 
#   st_set_crs(., 4326) %>% 
#   st_write(.,  "C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_core_sites8.shp")

# p_snap_sites %>% group_by(site_name) %>% 
#   summarize(x = mean(x),
#             y = mean(y)) %>% 
#   dplyr::select(x, y, site_name) %>% distinct() %>% 
#   st_as_sf(., coords = c("x", "y")) %>% 
#   st_set_crs(., 4326) %>% 
#   st_write(.,  "C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_snap_sites8.shp")

#can cone drop be used to indicate time since cones opened? #doesn't look great
# p_core_sites %>% 
#   ggplot(aes(x = day_experiment, y = prop_open, color = cone_drop, group = tree_n)) + geom_jitter() + 
#   theme_bw() + geom_line()+ facet_wrap(~site_name)
# 
# 
# p_core_sites %>%  ggplot(aes(x = day_experiment, y = prop_open, group = tree_n_core)) + geom_line() + facet_wrap(~site_name)
# 
# filter(p_core_sites, site == "comal") %>% 
#   ggplot(aes(x = day_experiment, y = prop_open, group = tree_n_core, color = tree_n_core)) + geom_line() + facet_wrap(~site)
# 
# p_snap_sites %>%  ggplot(aes(x = day_experiment, y = prop_open, group = tree_n_snap)) + geom_point() + theme_bw()
# 
# #graph of cones opening on a single tree
# p_core_sites %>% 
#   ggplot(aes(x = sample_datetime, y = prop_open, group = tree, color = site)) + geom_point() + theme_bw() + geom_line() #+ facet_wrap(~site)


data_for_model_core_prop_open <- p_core_sites %>% 
  dplyr::select(site_n_core, tree_n_core, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n_core, tree_n_core, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n_core, tree_n_core), names_from = day_experiment, values_from = prop_open, names_prefix = "d") %>% 
  arrange(tree_n_core) %>% 
  dplyr::select(-tree_n_core, site_n_core) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  dplyr::select(sort(tidyselect::peek_vars()))
data_for_model_core_prop_open_list <- split(as.data.frame(data_for_model_core_prop_open), seq(nrow(data_for_model_core_prop_open)))
data_for_model_core_prop_open_list <- lapply(data_for_model_core_prop_open_list, function(x) x[!is.na(x)])
data_for_model_core_prop_open_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_prop_open_list, 
                                                                        `length<-`, max(lengths(data_for_model_core_prop_open_list)))))

data_for_model_core_day_experiment <- p_core_sites %>% 
  dplyr::select(site_n_core, tree_n_core, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n_core, tree_n_core, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n_core, tree_n_core), names_from = day_experiment, values_from = day_experiment, names_prefix = "d") %>% 
  arrange(tree_n_core) %>% 
  dplyr::select(-tree_n_core, site_n_core) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  dplyr::select(sort(tidyselect::peek_vars()))
data_for_model_core_day_experiment_list <- split(as.data.frame(data_for_model_core_day_experiment), seq(nrow(data_for_model_core_day_experiment)))
data_for_model_core_day_experiment_list <- lapply(data_for_model_core_day_experiment_list, function(x) x[!is.na(x)])
data_for_model_core_day_experiment_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_day_experiment_list, 
                                                                             `length<-`, max(lengths(data_for_model_core_day_experiment_list)))))

data_for_model_core_nobs_per_tree <- p_core_sites %>% 
  dplyr::select(site_n_core, tree_n_core, day_experiment) %>% 
  distinct() %>% 
  arrange(tree_n_core, site_n_core) %>% 
  group_by(tree_n_core, site_n_core) %>% 
  summarize(nobs_per_tree = n())

data_for_model_snap <- p_snap_sites %>% 
  #filter(is.na(real_data)) %>% 
  dplyr::select(day_experiment, date3, prop_open, site_n_snap, site_name, tree_n_snap) %>% 
  arrange(tree_n_snap, site_n_snap, day_experiment) 

# # Assuming that no cones were open on Dec 10 and that all cones had opened by Mar. 1
# Y_snap_df_ass <- bind_cols(rep(x = 0, times = nrow(data_for_model_snap)),
#                                data_for_model_snap$prop_open,
#                                rep(x = 0.99, times = nrow(data_for_model_snap)))
# t_snap_df_ass <- bind_cols(rep(x = 1, times = nrow(data_for_model_snap)),
#                            data_for_model_snap$prop_open,
#                            rep(x = 60, times = nrow(data_for_model_snap)))

sink("model_b.txt")
cat("  
model{
  
##Likelihood

#core trees loop
  for(tree in 1:n_trees_core){ 
    for(i in 1:nobs_per_tree_core[tree]){
      Y_hat[tree, i] <- 0.99  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
      Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1[tree])
    } #end obs loop
  } #end tree loop

#snapshot trees loop
  for(tree_snap in 1:n_trees_snap){
      Y_hat_snap[tree_snap] <- 0.99  * exp( -exp(-rate_global_mean_snap * (t_snap[tree_snap] -
                                          b_snap[tree_snap])))
      Y_snap[tree_snap] ~ dnorm(Y_hat_snap[tree_snap], LAMBDA1_snap[tree_snap])
  } #end tree loop

  
#Priors
for(tree in 1:n_trees_core){
  LAMBDA1[tree]~dgamma(0.01,0.01) #uninformative gamma prior
  
  #a[tree] ~ dunif(0.99, 1) #the asympotote #assuming that asymptote is at 1
  b[tree] ~ dnorm(site_halfway_point_core[site_vector_core[tree]], LAMBDA3_core) #shifting left and right-  #
                                                                  #LAMBDA3_core[site_vector_core[tree]]
                                                                      #When b = log(2), f(0) = a/2, also called the halfway point
  c[tree] ~ dnorm(rate_global_mean, rate_global_sigma) #the steepness of the curve
} #end priors tree loop: core

for(tree in 1:n_trees_snap){
  LAMBDA1_snap[tree] <- 250 #~dgamma(0.01,0.01) #uninformative gamma prior
  #b_snap[tree] <- max(b_snap_orig[tree], 0) #keeping the halfway point for each tree above Dec 10
  b_snap[tree] ~ dnorm(site_halfway_point_snap[site_n_snap[tree]], LAMBDA3_snap) #[site_n_snap[tree]]
} #end priors tree loop: snap


for(site in 1:n_sites_core){
   site_halfway_point_core[site] ~ dnorm(0, 0.001)
} #end priors site loop

for(site in 1:n_sites_snap){
   site_halfway_point_snap[site] ~ dnorm(0, 0.001)
} #end priors site loop

rate_global_mean ~ dnorm(0, 0.001)
rate_global_mean_snap <- 1.02 #max(rate_global_mean, 0)#preventing backflow of information #prevent it from wandering negative

rate_global_sigma ~ dgamma(0.01,0.01)
rate_global_sigma_snap <- rate_global_sigma #preventing backflow of information

LAMBDA3_core ~ dgamma(0.01,0.01)
LAMBDA3_snap <- 0.031 #LAMBDA3_core
# LAMBDA3_snap ~ dgamma(0.01,0.01) #uninformative gamma prior 
# c_sim ~ dnorm(rate_global_mean, rate_global_sigma)
# c_sim_snap ~ dnorm(rate_global_mean_snap, rate_global_sigma_snap)

#simulation for each tree core
  for(tree in 1:n_trees_core){
    for(i in 1:max_t){
      Y_hat_sim[tree, i] <- 0.99  * exp( -exp(-c[tree] * (t_sim[i] - b[tree])))
    }
  }

#simulation for each tree snap
  for(tree in 1:n_trees_snap){
    for(i in 1:max_t){
      Y_hat_sim_snap[tree, i] <- 0.99  * exp( -exp(-rate_global_mean_snap * (t_sim[i] - b_snap[tree])))
    }
  }


#simulation for each site mean core
for(site in 1:n_sites_core){
    for(i in 1:max_t){
      Y_hat_sim_site[site, i] <- 0.99 * exp( -exp(-rate_global_mean * (t_sim[i] - b_site_sim_core[site])))
    }
      b_site_sim_core[site] ~ dnorm(site_halfway_point_core[site], LAMBDA3_core) #LAMBDA3_core[site]) 
} #end site sim loop

#simulation for each site mean snap
for(site in 1:n_sites_snap){
    for(i in 1:max_t){
      Y_hat_sim_site_snap[site, i] <- 0.99 * exp( -exp(-rate_global_mean_snap * (t_sim[i] -
                                                   b_site_sim_snap[site])))
    }
      b_site_sim_snap[site] ~ dnorm(site_halfway_point_snap[site], LAMBDA3_snap)
} #end site sim loop
    
}#end model
    ",fill=TRUE)
sink() 
sink() 

jags <- jags.model('model_b.txt', 
                   data = list(
                     #core sites
                     Y = as.matrix(data_for_model_core_prop_open_ragged),  #data_for_model_prop_open[3,1]
                     t = as.matrix(data_for_model_core_day_experiment_ragged), 
                     nobs_per_tree_core = data_for_model_core_nobs_per_tree$nobs_per_tree,
                     n_trees_core = nrow(data_for_model_core_nobs_per_tree),
                     n_sites_core = max(data_for_model_core_nobs_per_tree$site_n_core),
                     site_vector_core = data_for_model_core_nobs_per_tree$site_n_core,
                     t_sim = 1:max(data_for_model_core_day_experiment, na.rm = TRUE),
                     max_t = max(p_core_sites$day_experiment, na.rm = TRUE),
                     
                     #snap sites
                     Y_snap = data_for_model_snap$prop_open, #Y_snap_df_ass
                     t_snap = data_for_model_snap$day_experiment,
                     #tree_n_snap = data_for_model_snap$tree_n_snap,
                     site_n_snap = data_for_model_snap$site_n_snap,
                     #site_vector_snap = unique(data_for_model_snap$site_n_snap),
                     n_trees_snap = max(data_for_model_snap$tree_n_snap),
                     n_sites_snap = max(data_for_model_snap$site_n_snap)
                   ),
                   n.chains = 3,
                   n.adapt = 1000)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter = 9000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_snap"),  n.iter = 40000, thin = 10) #variables to monitor #"b", "c" "b_snap"
plot(mcmc_samples_params)
gelman.plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$tree <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
hist(results_params2$Mean)


#simulation for each tree core
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 40000, thin = 10) #variables to monitor
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n_core <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n_core, day_experiment) 
site_n_core_join <- dplyr::select(p_core_sites, tree_n_core, site_n_core)
results_params3 <- left_join(results_params2, site_n_core_join)

ggplot()  + theme_bw() +
  geom_line(data = results_params3, aes(x = day_experiment, y = Mean, group = tree_n_core, color = tree_n_core)) +
  geom_jitter(data = p_core_sites, aes(x = day_experiment, y = prop_open), width = 2, color = "red") +
  facet_wrap(~site_n_core)

### panel for example for paper
results_param_fig <- results_params3 %>%  filter(site_n_core == 7) 
p_core_sites_fig <- p_core_sites %>% filter(site_n_core == 7) %>% dplyr::select(day_experiment, date, tree_n_core, prop_open)
p_core_site_results_param_fig <- left_join(results_param_fig, p_core_sites_fig) %>% 
  mutate(date_sample = day_experiment + day_start)
library(ggthemes)
panel_a_core <- ggplot(p_core_site_results_param_fig)  + theme_few() +
  geom_line(aes(x = date_sample, y = Mean, group = tree_n_core),color = "gray50") +
  geom_point(aes(x = date, y = prop_open), color = "red") +
  xlab("date") + ylab("opened cones (proportion)") + theme(legend.position = "none") +
  scale_x_date(limits = c(mdy("12-15-2020"), mdy("01-25-2021")))+
  geom_pointrange(y = 0.5, xmin = day_start + 16.853922, x = day_start + 20.53876, xmax = day_start + 24.19277, 
                  col = rgb(152/255, 0/255, 197/255), lwd = 1.3) #based on mean of that site



## simulation for each tree snap
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim_snap"),  n.iter = 1000, thin = 3) #3000 iterations ran out of memory
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n_snap <- as.numeric(gsub("Y_hat_sim_snap[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n_snap, day_experiment) 
site_n_snap_join <- dplyr::select(data_for_model_snap, tree_n_snap, site_n_snap) %>% distinct()
results_params3 <- left_join(results_params2, site_n_snap_join) %>% 
  mutate(date3 = day_start + day_experiment)

results_params4 <- filter(results_params3, date3 > mdy("12-15-2020") & date3 < mdy("02-01-2021")) 
ggplot()  + theme_bw() +
  geom_line(data = results_params4, aes(x = date3, y = Mean, group = tree_n_snap)) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
  geom_jitter(data = data_for_model_snap, aes(x = date3, y = prop_open), width = 2, color = "red") +
  facet_wrap(~site_n_snap) + xlab("day of experiment") + ylab("cones open (proportion)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


### panel for example for paper
results_param_fig <- results_params3 %>%  filter(site_n_snap == 7) 
p_snap_sites_fig <- p_snap_sites %>% filter(site_n_snap == 7) %>% dplyr::select(day_experiment, date, tree_n_snap, prop_open)
p_snap_site_results_param_fig <- left_join(results_param_fig, p_snap_sites_fig) %>% 
  mutate(date_sample = day_experiment + day_start)

panel_b_snap <- ggplot(p_snap_site_results_param_fig)  + theme_few() +
  geom_line(aes(x = date_sample, y = Mean, group = tree_n_snap),color = "gray50") +
  geom_point(aes(x = date, y = prop_open), color = "red") +
  xlab("date") + ylab("opened cones (proportion)") + theme(legend.position = "none") + ylab(NULL)+ 
  scale_x_date(limits = c(mdy("12-15-2020"), mdy("01-25-2021")))+
  geom_pointrange(y = 0.5, xmin = day_start + 23.9, x = day_start + 27.9, xmax = day_start + 31.8, 
                  col = rgb(199/255, 0/255, 140/255), lwd = 1.3) #based on mean of that site

cowplot::plot_grid(panel_a_core, panel_b_snap) #for Fig 2, panels B & C


### extract site means for each site: core
mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_core"),  n.iter = 10000, thin = 3) 
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$site_n_core <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
site_n_core_join <- select(p_core_sites, site_name, site_n_core, x, y) %>% 
                        group_by(site_name, site_n_core) %>% 
                        summarize(x_site = mean(x, na.rm = TRUE),
                                  y_site = mean(y, na.rm = TRUE)) %>% distinct()
site_halfway_point_core <- left_join(results_params2, site_n_core_join)

### extract site means for each site: snap
mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_snap"),  n.iter = 10000, thin = 3) 
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$site_n_snap <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
site_n_snap_join <- select(p_snap_sites, site_name, site_n_snap, x, y) %>% 
  group_by(site_name, site_n_snap) %>% 
  summarize(x_site = mean(x, na.rm = TRUE),
            y_site = mean(y, na.rm = TRUE)) %>% distinct()
site_halfway_point_snap <- left_join(results_params2, site_n_snap_join)

site_export_df <- bind_rows(site_halfway_point_core, site_halfway_point_snap)  %>% 
                  filter(SD < 5) #remove the sites that I didn't have enough info from


## visualize site means: forest plot
site_export_df %>% 
  mutate(site_name = as.factor(site_name),
         site_name = reorder(site_name, Mean)) %>% 
ggplot(aes(x = site_name, y = Mean + day_start, ymin = Mean - SD + day_start, ymax = Mean + SD + day_start))  + 
  theme_bw() + geom_pointrange() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ylab("Halfway point of season (mean +/- SD")+ xlab("site")
  
## visualize site means across space
tx_boundary <- sf::read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = x_site, y = y_site, col = Mean),# size = pollen),#col = hilo2), pollen / max_p
             data = site_export_df, alpha = .7, size = 3)  + 
  scale_color_continuous(low = "blue", high = "red", name = "halfway point (day)") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines

## export site means data
readr::write_csv(site_export_df, "C:/Users/dsk856/Box/texas/pheno/fs20_21_site_halfway_cones_210904.csv")




######### save idealized opening trajectory across all core sites 
#get b (midpoint) for each core tree
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("b"),  n.iter = 3000, thin = 3) #variables to monitor
results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter <- row.names(results_params2)
results_params2$tree_n_core <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
tree_b <- results_params2
tree_b_join <- tree_b %>% 
  dplyr::select(b_mean = Mean,
                b_sd = SD,
                tree_n_core)
tree_b_save <- left_join(tree_b_join, p_core_sites_tree_key) 
#write_csv(tree_b_save, "C:/Users/dsk856/Box/texas/pheno/manual_obs/modeled_cone_peak_day_param_b_210910.csv")



#get Y_hat for each core tree on each day
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 3000, thin = 3) #variables to monitor
results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n_core <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n_core, day_experiment) 
site_n_core_join <- dplyr::select(p_core_sites, tree_n_core, site_n_core)
results_params3 <- left_join(results_params2, site_n_core_join)
yhat_join <- results_params3 %>% distinct() %>% 
  dplyr::select(yhat_mean = Mean,
                yhat_sd = SD,
                day_experiment, tree_n_core, site_n_core)

idealized_cone_opening_curve <- left_join(yhat_join, tree_b_join) %>% 
  mutate(day_before_peak = round(day_experiment - b_mean), 0) %>% 
  group_by(day_before_peak) %>% 
  filter(site_n_core < 5 | site_n_core > 8) %>% 
  summarize(yhat_median = median(yhat_mean, na.rm = TRUE)) %>% 
  # mutate(yhat_median = case_when(day_before_peak < -15 ~ 0,
  #                                day_before_peak > 35 ~ 0.9715,TRUE ~ yhat_median)) %>%  # a manual fix for some weird noise
  mutate(cone_opening_day = yhat_median - lag(yhat_median)) %>% 
  mutate(cone_opening_day = case_when(cone_opening_day < 0 ~ 0, TRUE ~ cone_opening_day))

ggplot(idealized_cone_opening_curve, aes(x = day_before_peak, y = cone_opening_day * 100)) + geom_point() + theme_bw() +
  xlab("day from peak") + ylab("cones opening on day (%)")
#write_csv(idealized_cone_opening_curve, "C:/Users/dsk856/Box/texas/pheno/manual_obs/idealized_cone_opening_curve_210910.csv")


