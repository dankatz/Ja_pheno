#trying out a Gompertz function to model pollen cone opening
#https://en.wikipedia.org/wiki/Gompertz_function



library(rjags)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2) 

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
#switching over to percent of cones opened from cone processing
#NOTE: NEED TO CHECK ON DISTRIBUTIONS FOR SIGMA

#database was put together in: 
#C:/Users/dsk856/Box/texas/pheno/manual_obs/manual sac counts/sac_count_processing210303.R
p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
#str(p)
day_start <- mdy("12-10-2020")

p_all_sites <- p %>%
  mutate(prop_open = bag_mean,#perc_open/100,
         #date2 = mdy_hm(date) - hours(6), #correct from GMT
         date3 = sample_date,
         site = paste(round(x, 1), round(y, 1)),
         yday = yday(date3),
         tree = paste(round(x, 5), round(y, 5))) %>% 
  filter(!is.na(perc_open)) %>% 
  #filter(site == "-98 30.1" | site == "-98.2 29.8") %>% 
  mutate(tree_n = as.numeric(as.factor(tree)),
         site_n = as.numeric(as.factor(site)),
         #tree_visit = paste(c(tree_n, as.character(date3))),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(date3, tree_n) %>% 
  filter( #remove trees that were added because they were late (for pollen platters)
    tree!= "-98.17164 29.82707" & tree != "-98.17316 29.82818" & tree != "-98.17282 29.82795" & #wilson/comal
      tree != "-98.05903 30.82545" & tree != "-98.0598 30.82648" & tree != "-98.06041 30.82647" & tree != "-98.06076 30.82632" &#wade/burnett
      tree != "-98.03122 30.13753" #hays
  ) #%>% 
#filter(site != "-97.9 30.7") #this site didnt have any visits after 12/31/20

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


visits_per_site <- p_all_sites %>%  
  select(site_n, day_experiment) %>% 
  distinct() %>% 
  group_by(site_n) %>% 
  summarize(n_visits_per_site = n())

p_core_sites <- left_join(p_all_sites, visits_per_site) %>% 
  filter(n_visits_per_site > 1) %>% 
  mutate(tree_n = as.numeric(as.factor(tree)),
         site_n = as.numeric(as.factor(site)),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(date3, tree_n)
length(unique(p_all_sites$site_n))
length(unique(p_core_sites$site_n))

p_snap_sites <- left_join(p_all_sites, visits_per_site) %>% 
  filter(n_visits_per_site < 2) %>% 
  mutate(tree_n = as.numeric(as.factor(tree)),
         site_n = as.numeric(as.factor(site)),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(date3, tree_n)
length(unique(p_snap_sites$site_n))

p_core_sites %>%  ggplot(aes(x = day_experiment, y = prop_open, group = tree_n)) + geom_line() + facet_wrap(~site)

filter(p_core_sites, site == "-97.6 32.2") %>% 
  ggplot(aes(x = day_experiment, y = prop_open, group = tree_n, color = tree_n)) + geom_line() + facet_wrap(~site)

p_snap_sites %>%  ggplot(aes(x = day_experiment, y = prop_open, group = tree)) + geom_point() + theme_bw()
#   filter(tree_n != 5 & tree_n != 14) %>%  #removing a couple trees that didn't open in this time period
#   mutate(tree_n = as.numeric(as.factor(tree)))
# #%>%  filter(tree_n < 11)

#graph of cones open vs pollen sacs open
p_core_sites %>% 
  ggplot(aes(x = sample_datetime, y = perc_open, group = tree, color = site)) + geom_point() + theme_bw() + geom_line() #+ facet_wrap(~site)

#graph of cones opening on a single tree
p_core_sites %>% 
  ggplot(aes(x = sample_datetime, y = perc_open, group = tree, color = site)) + geom_point() + theme_bw() + geom_line() #+ facet_wrap(~site)

#
data_for_model_core_prop_open <- p_core_sites %>% 
  select(site_n, tree_n, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n, tree_n, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n, tree_n), names_from = day_experiment, values_from = prop_open, names_prefix = "d") %>% 
  arrange(tree_n) %>% 
  select(-tree_n, site_n) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  select(sort(tidyselect::peek_vars()))
data_for_model_core_prop_open_list <- split(as.data.frame(data_for_model_core_prop_open), seq(nrow(data_for_model_core_prop_open)))
data_for_model_core_prop_open_list <- lapply(data_for_model_core_prop_open_list, function(x) x[!is.na(x)])
data_for_model_core_prop_open_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_prop_open_list, 
                                                                        `length<-`, max(lengths(data_for_model_core_prop_open_list)))))

data_for_model_core_day_experiment <- p_core_sites %>% 
  select(site_n, tree_n, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n, tree_n, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n, tree_n), names_from = day_experiment, values_from = day_experiment, names_prefix = "d") %>% 
  arrange(tree_n) %>% 
  select(-tree_n, site_n) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  select(sort(tidyselect::peek_vars()))
data_for_model_core_day_experiment_list <- split(as.data.frame(data_for_model_core_day_experiment), seq(nrow(data_for_model_core_day_experiment)))
data_for_model_core_day_experiment_list <- lapply(data_for_model_core_day_experiment_list, function(x) x[!is.na(x)])
data_for_model_core_day_experiment_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_day_experiment_list, 
                                                                             `length<-`, max(lengths(data_for_model_core_day_experiment_list)))))

data_for_model_core_nobs_per_tree <- p_core_sites %>% 
  select(site_n, tree_n, day_experiment) %>% 
  distinct() %>% 
  arrange(tree_n, site_n) %>% 
  group_by(tree_n, site_n) %>% 
  summarize(nobs_per_tree = n())

data_for_model_snap <- p_snap_sites %>% 
  select(day_experiment, date3, prop_open, site_n, tree_n) %>% 
  arrange(site_n, day_experiment)

sink("model_b.txt")
cat("  
model{
  
  #Likelihood

#core trees loop
  for(tree in 1:n_trees_core){ 
  for(i in 1:nobs_per_tree_core[tree]){
    Y_hat[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
    Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1[tree])
  } #end obs loop
  } #end tree loop

#snapshot trees loop
  for(tree_snap in 1:n_trees_snap){
    Y_hat_snap[tree_snap] <- 1  * exp( -exp(-c_sim * (t_snap[tree_snap] - b_snap[tree_snap])))
    Y_snap[tree_snap] ~ dnorm(Y_hat_snap[tree_snap], LAMBDA1_snap[tree_snap])
  } #end tree loop

  
  
#Priors
for(tree in 1:n_trees_core){
  LAMBDA1[tree]~dgamma(0.0001,0.0001) #uninformative gamma prior
  
  a[tree] ~ dunif(0.95, 1) #the asympotote #assuming that asymptote is at 1
  
  c[tree] ~ dnorm(rate_global_mean, rate_global_sigma) #the steepness of the curve
  
  b[tree] ~ dnorm(site_halfway_point_core[site_vector_core[tree]], LAMBDA3_core[site_vector_core[tree]]) #shifting left and right- When b = log(2), f(0) = a/2, also called the halfway point
 
} #end priors tree loop

for(tree in 1:n_trees_snap){
  LAMBDA1_snap[tree]~dgamma(0.0001,0.0001) #uninformative gamma prior

  b_snap[tree] ~ dnorm(site_halfway_point_snap[site_n_snap[tree]], LAMBDA3_snap[site_n_snap[tree]]) #shifting left and right- When b = log(2), f(0) = a/2, also called the halfway point

} #end priors tree loop


for(site in 1:n_sites_core){
   site_halfway_point_core[site] ~ dnorm(0, 0.001)
   LAMBDA3_core[site] ~ dgamma(0.0001,0.0001) #uninformative gamma prior
} #end priors site loop

for(site in 1:n_sites_snap){
   site_halfway_point_snap[site] ~ dnorm(0, 0.001)
   LAMBDA3_snap[site] ~ dgamma(0.0001,0.0001) #uninformative gamma prior
} #end priors site loop

rate_global_mean ~ dnorm(0, 0.001)
rate_global_sigma ~ dgamma(0.0001,0.0001)
c_sim ~ dnorm(rate_global_mean, rate_global_sigma)

#simulation for each tree core
  for(tree in 1:n_trees_core){
    for(i in 1:max_t){
      Y_hat_sim[tree, i] <- a[tree]  * exp( -exp(-c_sim * (t_sim[i] - b[tree])))
    }
  }

#simulation for each tree snap
  for(tree in 1:n_trees_snap){
    for(i in 1:max_t){
      Y_hat_sim_snap[tree, i] <- 1  * exp( -exp(-c_sim * (t_sim[i] - b_snap[tree])))
    }
  }


#simulation for each site mean core
for(site in 1:n_sites_core){
    for(i in 1:max_t){
      Y_hat_sim_site[site, i] <- 1 * exp( -exp(-c_sim * (t_sim[i] - b_site_sim_core[site])))
    }
      b_site_sim_core[site] ~ dnorm(site_halfway_point_core[site], LAMBDA3_core[site])
} #end site sim loop

#simulation for each site mean snap
for(site in 1:n_sites_snap){
    for(i in 1:max_t){
      Y_hat_sim_site_snap[site, i] <- 1 * exp( -exp(-c_sim * (t_sim[i] - b_site_sim_snap[site])))
    }
      b_site_sim_snap[site] ~ dnorm(site_halfway_point_snap[site], LAMBDA3_snap[site])
} #end site sim loop
    
}#end model
    ",fill=TRUE)
sink() 

jags <- jags.model('model_b.txt', 
                   data = list(
                     Y = as.matrix(data_for_model_core_prop_open_ragged),  #data_for_model_prop_open[3,1]
                     t = as.matrix(data_for_model_core_day_experiment_ragged), 
                     nobs_per_tree_core = data_for_model_core_nobs_per_tree$nobs_per_tree,
                     n_trees_core = nrow(data_for_model_core_nobs_per_tree),
                     n_sites_core = max(data_for_model_core_nobs_per_tree$site_n),
                     site_vector_core = data_for_model_core_nobs_per_tree$site_n,
                     t_sim = 1:max(data_for_model_core_day_experiment, na.rm = TRUE),
                     max_t = max(p_core_sites$day_experiment, na.rm = TRUE),
                     
                     #snap sites
                     Y_snap = data_for_model_snap$prop_open,
                     t_snap = data_for_model_snap$day_experiment,
                     #tree_n_snap = data_for_model_snap$tree_n,
                     site_n_snap = data_for_model_snap$site_n,
                     n_trees_snap = nrow(data_for_model_snap),
                     n_sites_snap = max(data_for_model_snap$site_n)
                   ),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter=1000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("b", "c"),  n.iter = 3000, thin = 3) #variables to monitor
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)

mcmc_samples_params <- coda.samples(jags, variable.names=c("b_snap"),  n.iter = 1000, thin = 3) #variables to monitor
summary(mcmc_samples_params)


#simulation for each tree snap
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim_snap"),  n.iter = 3000, thin = 3) #variables to monitor
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)
results_params2$day_experiment <- 1:max(data_for_model_day_experiment, na.rm = TRUE)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n <- as.numeric(gsub("Y_hat_sim_snap[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n, day_experiment) 
site_n_snap_join <- select(data_for_model_snap, tree_n, site_n)
results_params3 <- left_join(results_params2, site_n_snap_join)


ggplot()  + theme_bw() +
  geom_line(data = results_params3, aes(x = day_experiment, y = Mean, group = tree_n, color = tree_n)) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
  geom_jitter(data = p_snap_sites, aes(x = day_experiment, y = prop_open), width = 2, color = "red") +
  facet_wrap(~site_n)



#simulation for each site
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim_site"),  n.iter = 3000, thin = 3) #variables to monitor
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)
results_params2$day_experiment <- 1:max(data_for_model_day_experiment, na.rm = TRUE)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

site_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$site_n <- as.numeric(gsub("Y_hat_sim_site[", "", site_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, site_n, day_experiment)

ggplot()  + theme_bw() +
  geom_ribbon(data = results_params2, aes(x = day_experiment, y = Mean, ymin = X25., ymax = X75., group = as.factor(site_n), 
                                          fill = as.factor(site_n)), alpha = 0.1) +
  geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = site_n, color = as.factor(site_n))) +
  geom_jitter(data = p_core_sites, aes(x = day_experiment, y = prop_open), color = "red") 

#site means across space
tx_boundary <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
site_locations <- p_core_sites %>% 
  mutate(site_long = round(x, 1),
         site_lat = round(y,1)) %>% 
  select(site_n, site_long, site_lat) %>% distinct() 
#ggplot(aes(x= site_long, y = site_lat)) + geom_point()

site_halfway_sim <- coda.samples(jags, variable.names=c("b_site_sim"),  n.iter = 2000, thin = 3) #variables to monitor
site_halfway_params <- summary(site_halfway_sim)
site_halfway_params2 <- data.frame(site_halfway_params$statistics, site_halfway_params$quantiles) #multi-var model
site_halfway_params2$parameter <- row.names(site_halfway_params2)
site_halfway_params2$site_n <- as.numeric(gsub("[^0-9.-]", "", site_halfway_params2$parameter))

site_halfway_params2 <- left_join(site_halfway_params2, site_locations)

site_halfway_params2_filt <-  site_halfway_params2 %>% filter(site_n != 5)
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = site_long, y = site_lat, col = Mean),# size = pollen),#col = hilo2), pollen / max_p
             data = site_halfway_params2_filt, alpha = .7, size = 3)  + 
  scale_color_continuous(low = "blue", high = "red", name = "halfway point (day)") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines


#simulation for each tree core
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 3000, thin = 3) #variables to monitor
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)
results_params2$day_experiment <- 1:max(data_for_model_day_experiment, na.rm = TRUE)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n, day_experiment)
results_tree_core_sim_join <- select(results_params2, tree_n, day_experiment, Mean, SD)
results_tree_core_sim <- left_join(p_core_sites, results_tree_core_sim_join) %>% 
  mutate(pollen_release = factor(pollen_release, levels = c("NA","none", "little", "some", "lots")),
         pol_bin = case_when(pollen_release == "none" ~ "none",
                             pollen_release == "little" ~ "release",
                             pollen_release == "some" ~ "release",
                             pollen_release == "lots" ~ "release",
                             pollen_release == "NA" ~ "none"),
         pol_cont = case_when(pollen_release == "none" ~ 0,
                              pollen_release == "little" ~ 0.1,
                              pollen_release == "some" ~ 0.3,
                              pollen_release == "lots" ~ 1,
                              pollen_release == "NA" ~ 0))

ggplot()  + theme_bw() +
  geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = tree_n, color = tree_n)) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
  geom_jitter(data = p_all_sites_core, aes(x = day_experiment, y = prop_open), color = "red") +
  facet_wrap(~site_n)


#a little analysis of pollen release
results_tree_core_sim %>% 
  ggplot(aes(x = date2, y = Mean, color = pollen_release)) + geom_jitter(size = 4, alpha = .7) + theme_bw() +
  scale_color_viridis_d(direction = -1) + ylab("cones opened (proportion)")



### pollen cones, not pollen sacs: fitting all trees at all core sites and all snapshot sites (hierarchical) #############################
#switching over to percent of cones opened from cone processing


#database was put together in: 
#C:/Users/dsk856/Box/texas/pheno/manual_obs/manual sac counts/sac_count_processing210303.R
p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
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
  mutate(prop_open  = case_when(prop_open >= 0.95 ~ 0.95, #I don't think the number of open cones above 95% is robust due to cones that will
                                prop_open < 0.95 ~ prop_open)) %>% #never open and dropping of other cones. So I'm trying 95% as the asymptote
  mutate(duplicate_visits = case_when(site_name == "canyon.lake.dam" & date3 == ymd("2020-12-22") ~ "duplicate",
                                      site_name == "rogers" & date3 == ymd("2020-12-14") ~ "duplicate",
                                      site_name == "rogers" & date3 == ymd("2020-12-31") ~ "duplicate",
                                      site_name == "menard" & date3 == ymd("2020-12-28") ~ "duplicate",
                                      site_name == "x1311.near.menard.site" & date3 == ymd("2020-12-28") ~ "duplicate",
                                      site_name == "pass.to.the.w.of.hext" & date3 == ymd("2020-12-28") ~ "duplicate",
                                      TRUE ~ "single visit")) %>% 
  filter(duplicate_visits == "single visit") #find duplicate visits at what should be snapshot sites and remove them 
#%>% group_by(site_name, tree_n) %>% summarize(n_visits = n()) 



#adding in the assumption that all cones were closed on Dec 1 and open on March 1
p_all_sites_bound_start <- p_all_sites %>% 
  select(tree, tree_n, site, site_n, site_name, site_type) %>% 
  distinct() %>% 
  mutate(prop_open = 0,
         date3 = mdy("12-01-2020"),
         day_experiment = as.numeric(date3 - day_start),
         doy = yday(date3),
         real_data = "not real data")
p_all_sites_bound_end <- p_all_sites %>% 
  select(tree, tree_n, site, site_n, site_name, site_type) %>% 
  distinct() %>% 
  mutate(prop_open = 0.95,
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
  select(site_n, day_experiment) %>% 
  distinct() %>% 
  group_by(site_n) %>% 
  summarize(n_visits_per_site = n())

#incorporate some summary information: average cones open per site visit
average_cone_open_per_site <- p_all_sites %>%  
  group_by(site_n, day_experiment) %>% 
  mutate(cone_opening_stage = case_when(prop_open < 0.01 ~ 0,
                                        prop_open > 0.94 ~ 0,
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
  filter(site_prop_open_visit > 0.1) %>% #remove sites that hadn't started or had already finished
  filter(site_mean_visit > 0.05 & site_mean_visit < 0.90) %>% #remove sites that hadn't started or had already finished
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
  select(site_n_core, tree_n_core, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n_core, tree_n_core, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n_core, tree_n_core), names_from = day_experiment, values_from = prop_open, names_prefix = "d") %>% 
  arrange(tree_n_core) %>% 
  select(-tree_n_core, site_n_core) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  select(sort(tidyselect::peek_vars()))
data_for_model_core_prop_open_list <- split(as.data.frame(data_for_model_core_prop_open), seq(nrow(data_for_model_core_prop_open)))
data_for_model_core_prop_open_list <- lapply(data_for_model_core_prop_open_list, function(x) x[!is.na(x)])
data_for_model_core_prop_open_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_prop_open_list, 
                                                                        `length<-`, max(lengths(data_for_model_core_prop_open_list)))))

data_for_model_core_day_experiment <- p_core_sites %>% 
  select(site_n_core, tree_n_core, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n_core, tree_n_core, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n_core, tree_n_core), names_from = day_experiment, values_from = day_experiment, names_prefix = "d") %>% 
  arrange(tree_n_core) %>% 
  select(-tree_n_core, site_n_core) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  select(sort(tidyselect::peek_vars()))
data_for_model_core_day_experiment_list <- split(as.data.frame(data_for_model_core_day_experiment), seq(nrow(data_for_model_core_day_experiment)))
data_for_model_core_day_experiment_list <- lapply(data_for_model_core_day_experiment_list, function(x) x[!is.na(x)])
data_for_model_core_day_experiment_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_day_experiment_list, 
                                                                             `length<-`, max(lengths(data_for_model_core_day_experiment_list)))))

data_for_model_core_nobs_per_tree <- p_core_sites %>% 
  select(site_n_core, tree_n_core, day_experiment) %>% 
  distinct() %>% 
  arrange(tree_n_core, site_n_core) %>% 
  group_by(tree_n_core, site_n_core) %>% 
  summarize(nobs_per_tree = n())

data_for_model_snap <- p_snap_sites %>% 
  #filter(is.na(real_data)) %>% 
  select(day_experiment, date3, prop_open, site_n_snap, site_name, tree_n_snap) %>% 
  arrange(tree_n_snap, site_n_snap, day_experiment) 

max(data_for_model_snap$tree_n_snap)

sink("model_b.txt")
cat("  
model{
  
  #Likelihood

#core trees loop
  for(tree in 1:n_trees_core){ 
    for(i in 1:nobs_per_tree_core[tree]){
      Y_hat[tree, i] <- 0.95  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
      Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1[tree])
    } #end obs loop
  } #end tree loop

#snapshot trees loop
  for(tree_snap in 1:n_trees_snap){
    Y_hat_snap[tree_snap] <- 0.95  * exp( -exp(-rate_global_mean_snap * (t_snap[tree_snap] -
                                          b_snap[tree_snap])))
    Y_snap[tree_snap] ~ dnorm(Y_hat_snap[tree_snap], LAMBDA1_snap[tree_snap])
  } #end tree loop

  
  
#Priors
for(tree in 1:n_trees_core){
  LAMBDA1[tree]~dgamma(0.001,0.001) #uninformative gamma prior
  
  #a[tree] ~ dunif(0.95, 1) #the asympotote #assuming that asymptote is at 1
  b[tree] ~ dnorm(site_halfway_point_core[site_vector_core[tree]], LAMBDA3_core) #shifting left and right-  #
                                                                  #LAMBDA3_core[site_vector_core[tree]]
                                                                      #When b = log(2), f(0) = a/2, also called the halfway point
  c[tree] ~ dnorm(rate_global_mean, rate_global_sigma) #the steepness of the curve
} #end priors tree loop: core

for(tree in 1:n_trees_snap){
  LAMBDA1_snap[tree]~dgamma(0.001,0.001) #uninformative gamma prior
  b_snap[tree] ~ dnorm(site_halfway_point_snap[site_n_snap[tree]], LAMBDA3_snap) #[site_n_snap[tree]]
} #end priors tree loop: snap


for(site in 1:n_sites_core){
   site_halfway_point_core[site] ~ dnorm(0, 0.001)
  # LAMBDA3_core[site] ~ dgamma(core_site_halfway_var_gamma1, core_site_halfway_var_gamma2) 
} #end priors site loop

for(site in 1:n_sites_snap){
   site_halfway_point_snap[site] ~ dnorm(0, 0.001)
   #LAMBDA3_snap[site] ~ dgamma(0.001,0.001) #uninformative gamma prior
} #end priors site loop

rate_global_mean ~ dnorm(0, 0.001)
rate_global_mean_snap <- max(rate_global_mean, 0)#preventing backflow of information #prevent it from wandering negative

rate_global_sigma ~ dgamma(0.01,0.01)
rate_global_sigma_snap <- rate_global_sigma #preventing backflow of information

LAMBDA3_core ~ dgamma(0.001,0.001)
LAMBDA3_snap <- LAMBDA3_core
# LAMBDA3_snap ~ dgamma(0.001,0.001) #uninformative gamma prior 
# core_site_halfway_var_gamma1 ~ dgamma(0.001,0.001)
# core_site_halfway_var_gamma2 ~ dgamma(0.001,0.001)
# c_sim ~ dnorm(rate_global_mean, rate_global_sigma)
# c_sim_snap ~ dnorm(rate_global_mean_snap, rate_global_sigma_snap)

#simulation for each tree core
  for(tree in 1:n_trees_core){
    for(i in 1:max_t){
      Y_hat_sim[tree, i] <- 0.95  * exp( -exp(-c[tree] * (t_sim[i] - b[tree])))
    }
  }

# #simulation for each tree snap
#   for(tree in 1:n_trees_snap){
#     for(i in 1:max_t){
#       Y_hat_sim_snap[tree, i] <- 0.95  * exp( -exp(-rate_global_mean_snap * (t_sim[i] - b_snap[tree])))
#     }
#   }


#simulation for each site mean core
for(site in 1:n_sites_core){
    for(i in 1:max_t){
      Y_hat_sim_site[site, i] <- 0.95 * exp( -exp(-rate_global_mean * (t_sim[i] - b_site_sim_core[site])))
    }
      b_site_sim_core[site] ~ dnorm(site_halfway_point_core[site], LAMBDA3_core) #LAMBDA3_core[site]) 
} #end site sim loop

# #simulation for each site mean snap
# for(site in 1:n_sites_snap){
#     for(i in 1:max_t){
#       Y_hat_sim_site_snap[site, i] <- 0.95 * exp( -exp(-rate_global_mean_snap * (t_sim[i] - 
#                                                    b_site_sim_snap[site])))
#     }
#       b_site_sim_snap[site] ~ dnorm(site_halfway_point_snap[site], LAMBDA3_snap)
# } #end site sim loop
    
}#end model
    ",fill=TRUE)
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
                     Y_snap = data_for_model_snap$prop_open,
                     t_snap = data_for_model_snap$day_experiment,
                     #tree_n_snap = data_for_model_snap$tree_n_snap,
                     site_n_snap = data_for_model_snap$site_n_snap,
                     #site_vector_snap = unique(data_for_model_snap$site_n_snap),
                     n_trees_snap = max(data_for_model_snap$tree_n_snap),
                     n_sites_snap = max(data_for_model_snap$site_n_snap)
                   ),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
#update(jags,n.iter = 5000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_snap"),  n.iter = 15000, thin = 3) #variables to monitor #"b", "c" "b_snap"
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$tree <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
hist(results_params2$Mean)



#simulation for each tree core
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 100, thin = 3) #variables to monitor
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
site_n_core_join <- select(p_core_sites, tree_n_core, site_n_core)
results_params3 <- left_join(results_params2, site_n_core_join)

ggplot()  + theme_bw() +
  geom_line(data = results_params3, aes(x = day_experiment, y = Mean, group = tree_n_core, color = tree_n_core)) +
  geom_jitter(data = p_core_sites, aes(x = day_experiment, y = prop_open), width = 2, color = "red") +
  facet_wrap(~site_n_core)


## simulation for each tree snap
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim_snap"),  n.iter = 100, thin = 3) #3000 iterations ran out of memory
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
site_n_snap_join <- select(data_for_model_snap, tree_n_snap, site_n_snap) %>% distinct()
results_params3 <- left_join(results_params2, site_n_snap_join)

ggplot()  + theme_bw() +
  geom_line(data = results_params3, aes(x = day_experiment, y = Mean, group = tree_n_snap, color = tree_n_snap)) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
  geom_jitter(data = data_for_model_snap, aes(x = day_experiment, y = prop_open), width = 2, color = "red") +
  facet_wrap(~site_n_snap)



#simulation for each site
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim_site"),  n.iter = 1000, thin = 3) #variables to monitor
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)
#results_params2$day_experiment <- 1:max(data_for_model_day_experiment, na.rm = TRUE)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

site_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$site_n <- as.numeric(gsub("Y_hat_sim_site[", "", site_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, site_n, day_experiment)

ggplot()  + theme_bw() +
  geom_ribbon(data = results_params2, aes(x = day_experiment, y = Mean, ymin = X25., ymax = X75., group = as.factor(site_n), 
                                          fill = as.factor(site_n)), alpha = 0.1) +
  geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = site_n, color = as.factor(site_n))) +
  geom_jitter(data = p_core_sites, aes(x = day_experiment, y = prop_open), color = "red") 

#site means across space
tx_boundary <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
site_locations <- p_core_sites %>% 
  mutate(site_long = round(x, 1),
         site_lat = round(y,1)) %>% 
  select(site_n, site_long, site_lat) %>% distinct() 
#ggplot(aes(x= site_long, y = site_lat)) + geom_point()

site_halfway_sim <- coda.samples(jags, variable.names=c("b_site_sim"),  n.iter = 2000, thin = 3) #variables to monitor
site_halfway_params <- summary(site_halfway_sim)
site_halfway_params2 <- data.frame(site_halfway_params$statistics, site_halfway_params$quantiles) #multi-var model
site_halfway_params2$parameter <- row.names(site_halfway_params2)
site_halfway_params2$site_n <- as.numeric(gsub("[^0-9.-]", "", site_halfway_params2$parameter))

site_halfway_params2 <- left_join(site_halfway_params2, site_locations)

site_halfway_params2_filt <-  site_halfway_params2 %>% filter(site_n != 5)
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = site_long, y = site_lat, col = Mean),# size = pollen),#col = hilo2), pollen / max_p
             data = site_halfway_params2_filt, alpha = .7, size = 3)  + 
  scale_color_continuous(low = "blue", high = "red", name = "halfway point (day)") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines


#simulation for each tree core
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 3000, thin = 3) #variables to monitor
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)
results_params2$day_experiment <- 1:max(data_for_model_core_day_experiment, na.rm = TRUE)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n, day_experiment)
results_params2 <- mutate(results_params2, site_n = as.numeric(gsub("Y_hat_sim[", "", site_n_vector, fixed = TRUE) ))

results_tree_core_sim_join <- select(results_params2, tree_n, day_experiment, Mean, SD)
results_tree_core_sim <- left_join(p_core_sites, results_tree_core_sim_join) 
# mutate(pollen_release = factor(pollen_release, levels = c("NA","none", "little", "some", "lots")),
#        pol_bin = case_when(pollen_release == "none" ~ "none",
#                            pollen_release == "little" ~ "release",
#                            pollen_release == "some" ~ "release",
#                            pollen_release == "lots" ~ "release",
#                            pollen_release == "NA" ~ "none"),
#        pol_cont = case_when(pollen_release == "none" ~ 0,
#                             pollen_release == "little" ~ 0.1,
#                             pollen_release == "some" ~ 0.3,
#                             pollen_release == "lots" ~ 1,
#                             pollen_release == "NA" ~ 0))

ggplot()  + theme_bw() +
  geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = tree_n, color = tree_n)) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
  geom_jitter(data = p_core_sites, aes(x = day_experiment, y = prop_open), color = "red") +
  facet_wrap(~site_n)


#a little analysis of pollen release
results_tree_core_sim %>% 
  ggplot(aes(x = date2, y = Mean, color = pollen_release)) + geom_jitter(size = 4, alpha = .7) + theme_bw() +
  scale_color_viridis_d(direction = -1) + ylab("cones opened (proportion)")




### hacky approach: fit params on core sites, non-hierarchical model for all trees based on that, average of d #############################
#database was put together in: 
#C:/Users/dsk856/Box/texas/pheno/manual_obs/manual sac counts/sac_count_processing210303.R
p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
#str(p)

# p %>% 
#   group_by(pollen_rel) %>% 
#   filter(!is.na(pollen_rel)) %>% 
#   mutate(pollen_rel = factor(pollen_rel, levels = c("none", "little", "some", "lots"))) %>% 
#   #summarize(opening_mean = mean(bag_cones_opening, na.rm = TRUE)) %>% 
# ggplot(aes(x = pollen_rel, y = bag_cones_opening)) + geom_boxplot(outlier.shape=NA) + #geom_jitter(width = 0.1) + 
#   theme_bw() +
#   xlab("pollen release") + ylab("opening sacs (proportion)")


day_start <- mdy("12-10-2020")

p_all_sites <- p %>%
  mutate(prop_open = bag_mean,#perc_open/100,
         #date2 = mdy_hm(date) - hours(6), #correct from GMT
         date3 = sample_date,
         site = site_name,#paste(round(x, 1), round(y, 1)),
         yday = yday(date3),
         tree = paste(round(x, 5), round(y, 5))) %>% 
  filter(!is.na(perc_open) & !is.na(prop_open)) %>% 
  filter( #remove trees that were added because they were late (for pollen platters)
    tree!= "-98.17164 29.82707" & tree != "-98.17316 29.82818" & tree != "-98.17282 29.82795" & #wilson/comal
      tree != "-98.05903 30.82545" & tree != "-98.0598 30.82648" & tree != "-98.06041 30.82647" & tree != "-98.06076 30.82632" &#wade/burnett
      tree != "-98.03122 30.13753") %>%  #hays 
  filter(site_name != "bulverde.park" & #these sites don't have enough info to be useful
           site_name != "evans"&
           site_name != "k.allison.ranch" &
           site_name != "speegleville") %>%  
  #filter(site == "-98 30.1" | site == "-98.2 29.8") %>%  #-97.85830	30.69633
  mutate(tree_n = as.numeric(as.factor(tree)),
         site_n = as.numeric(as.factor(site)),
         #tree_visit = paste(c(tree_n, as.character(date3))),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(tree_n, date3)

#p_all_sites %>% select(tree, tree_n, site, site_n) %>% distinct() -> test


visits_per_site <- p_all_sites %>%  
  select(site_n, day_experiment) %>% 
  distinct() %>% 
  group_by(site_n) %>% 
  summarize(n_visits_per_site = n())

visits_per_tree <- p_all_sites %>%  
  select(tree_n, day_experiment) %>% 
  distinct() %>% 
  group_by(tree_n) %>% 
  summarize(n_visits_per_tree = n())

p_core_sites <- left_join(p_all_sites, visits_per_tree) %>% 
  filter(n_visits_per_tree > 2) %>% 
  filter(site_name != "rogers") %>%  
  mutate(tree_n = as.numeric(as.factor(tree)),
         site_n = as.numeric(as.factor(site)),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(date3, tree_n) %>% 
  group_by(tree_n) %>% 
  mutate(prop_open_lag1 = lag(prop_open, 1), #assume that the proportion of cones open on a twig can't go down on next visit
         prop_open_dif = prop_open - prop_open_lag1,
         prop_open_c = case_when(prop_open <  prop_open_lag1 ~ prop_open_lag1,
                                 prop_open >= prop_open_lag1 ~ prop_open,
                                 is.na(prop_open_lag1) ~ prop_open)) %>% 
  ungroup()

length(unique(p_all_sites$site_n))
length(unique(p_core_sites$site_n))
p_core_sites %>%  ggplot(aes(x = day_experiment, y = prop_open_c, group = tree_n)) + geom_line() #+ facet_wrap(~site)

p_snap_sites <- left_join(p_all_sites, visits_per_site) %>%
  filter(n_visits_per_site < 3 | site_name == "rogers") %>% #rogers didn't end up being a core site
  #filter() #remove the sites that have too little info (prop_open  < X or > X)
  #filter(site_name == "rr.337...rimrock") %>% 
  #DIRTY HACK: AVERGE ALL TREES AT A SITE
  # group_by(site_n, site_name, date3, day_experiment) %>% summarize(prop_open = mean(prop_open, na.rm = TRUE),
  #                                                       x = mean(x),
  #                                                       y = mean(y)) %>% 
  #filter(prop_open > 0.03 & prop_open < 0.97) %>% 
  mutate(tree_n = as.numeric(as.factor(tree)),
         site_n = as.numeric(as.factor(site)),
         day_experiment = as.numeric(date3 - day_start)) 
#arrange(date3, tree_n)
length(unique(p_snap_sites$tree_n))
p_snap_sites %>%  ggplot(aes(x = prop_open, group = tree_n)) + geom_histogram() +xlab("average proportion of sacs open") + theme_bw()#
#test <- p_snap_sites %>% group_by(site_n) %>% summarize(site_mean = mean(prop_open, na.rm = TRUE))
#test <- filter(p_snap_sites, site_name == "rr.337...rimrock")

#p_snap_sites %>% select(site, site_n, site_name) %>% distinct() -> test

#filter(p_core_sites, site == "-97.6 32.2") %>% 
# ggplot(aes(x = day_experiment, y = prop_open, group = tree_n, color = tree_n)) + geom_line() + facet_wrap(~site)

# p_snap_sites %>%  ggplot(aes(x = day_experiment, y = prop_open, group = tree)) + geom_point() 
#   filter(tree_n != 5 & tree_n != 14) %>%  #removing a couple trees that didn't open in this time period
#   mutate(tree_n = as.numeric(as.factor(tree)))
# #%>%  filter(tree_n < 11)


data_for_model_core_prop_open <- p_core_sites %>% 
  select(site_n, tree_n, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n, tree_n, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n, tree_n), names_from = day_experiment, values_from = prop_open, names_prefix = "d") %>% 
  arrange(tree_n) %>% 
  select(-tree_n, site_n) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  select(sort(tidyselect::peek_vars()))
data_for_model_core_prop_open_list <- split(as.data.frame(data_for_model_core_prop_open), seq(nrow(data_for_model_core_prop_open)))
data_for_model_core_prop_open_list <- lapply(data_for_model_core_prop_open_list, function(x) x[!is.na(x)])
data_for_model_core_prop_open_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_prop_open_list, 
                                                                        `length<-`, max(lengths(data_for_model_core_prop_open_list)))))

data_for_model_core_day_experiment <- p_core_sites %>% 
  select(site_n, tree_n, day_experiment, prop_open) %>% 
  distinct() %>% 
  group_by(site_n, tree_n, day_experiment) %>% 
  summarize(prop_open = mean(prop_open)) %>% #combining sequential measurements on the same tree on the same day
  ungroup() %>% 
  pivot_wider(id_cols = c(site_n, tree_n), names_from = day_experiment, values_from = day_experiment, names_prefix = "d") %>% 
  arrange(tree_n) %>% 
  select(-tree_n, site_n) %>% 
  rename(d04 = d4, d05 = d5, d06 = d6, d07 = d7) %>% 
  select(sort(tidyselect::peek_vars()))
data_for_model_core_day_experiment_list <- split(as.data.frame(data_for_model_core_day_experiment), seq(nrow(data_for_model_core_day_experiment)))
data_for_model_core_day_experiment_list <- lapply(data_for_model_core_day_experiment_list, function(x) x[!is.na(x)])
data_for_model_core_day_experiment_ragged <- data.frame(Reduce(rbind, lapply(data_for_model_core_day_experiment_list, 
                                                                             `length<-`, max(lengths(data_for_model_core_day_experiment_list)))))

data_for_model_core_nobs_per_tree <- p_core_sites %>% 
  select(site_n, tree_n, day_experiment) %>% 
  distinct() %>% 
  arrange(tree_n, site_n) %>% 
  group_by(tree_n, site_n) %>% 
  summarize(nobs_per_tree = n())

data_for_model_snap <- p_snap_sites %>% 
  filter(!is.na(prop_open))%>% 
  select(day_experiment, date3, prop_open, site_n, site_name, tree_n, x, y) %>% 
  mutate(tree_n2 = as.numeric(as.factor(paste(tree_n, date3)))) %>% #count trees separately
  arrange(tree_n2, day_experiment) 

sink("model_b.txt")
cat("  
model{
  
  #Likelihood

#core loop #for trees that were visited two or more times 
  for(tree in 1:n_trees_core){ 
  for(i in 1:nobs_per_tree_core[tree]){
    Y_hat[tree, i] <- a[tree]  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
    Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1[tree])
  } #end obs loop
  } #end tree loop

#snapshot trees loop
  for(tree_snap in 1:n_trees_snap){
    Y_hat_snap[tree_snap] <- 1  * exp( -exp(-c_hardcode * (t_snap[tree_snap] - b_snap[tree_snap])))
    Y_snap[tree_snap] ~ dnorm(Y_hat_snap[tree_snap], 7000)
  } #end tree loop

  
#Priors
for(tree in 1:n_trees_core){
  LAMBDA1[tree]~dgamma(0.0001,0.0001) #uninformative gamma prior
  
  a[tree] ~ dunif(min_topout, 1) #the asympotote #assuming that asymptote is at 1
  #a[tree] ~ dbeta(0.95, 1) #the asympotote #assuming that asymptote is at 1
  
  c[tree] ~ dnorm(rate_global_mean, rate_global_sigma) #the steepness of the curve
  
  #b[tree] ~ dnorm(halfway_point_global_mean, halfway_point_global_sigma) # hierarchical
   b[tree] ~ dnorm(site_halfway_point_core[site_vector_core[tree]], LAMBDA3_core) #shifting left and right-
    #b[tree] ~ dnorm(0, 0.0001) #as a fixed effect
} #end priors tree loop

for(site in 1:n_sites_core){
site_halfway_point_core[site] ~ dnorm(0, 0.0001)
#LAMBDA3_core[site] ~ dgamma(0.0001, 0.0001)
}

for(tree in 1:n_trees_snap){
  #LAMBDA1_snap[tree]~dgamma(0.0001,0.0001) #uninformative gamma prior
  b_snap[tree] ~ dnorm(site_halfway_point_snap[site_n_snap[tree]], Lambda3_snap) # hierarchical
  #b_snap[tree] ~ dnorm(0, 0.001) #non-hierarchical
} #end priors tree loop

for(site in 1:n_sites_snap){
site_halfway_point_snap[site] ~ dnorm(0, 0.001)
#Lambda3_snap[site] ~ dgamma(0.0001, 0.0001)
}

LAMBDA3_core ~ dgamma(0.0001, 0.0001)
Lambda3_snap <- LAMBDA3_core
rate_global_mean ~ dnorm(0, 0.001)
rate_global_sigma ~ dgamma(0.0001, 0.0001)

c_sim ~ dnorm(rate_global_mean, rate_global_sigma)
c_sim_snap <- c_sim
c_hardcode <- 0.46
min_topout ~ dunif(0.001, 0.999)

a_sim ~ dunif(min_topout, 1)

#global_halfway_point_snap ~ dnorm(0, 0.001)
halfway_point_global_mean ~ dnorm(0, 0.001)
halfway_point_global_sigma ~ dgamma(0.0001,0.0001)
b_sim ~ dnorm(halfway_point_global_mean, halfway_point_global_sigma)

# snap_halfway_point_global_mean ~ dnorm(0, 0.001)
# snap_halfway_point_global_sigma ~ dgamma(0.0001,0.0001)

# for(snap_site in 1:n_sites_snap){
# snap_site_b_sim[snap_site] ~ dnorm(site_halfway_point_snap[snap_site], Lambda3_snap)
# }

# #simulation for each tree core
  for(tree in 1:n_trees_core){
    for(i in 1:max_t){
      Y_hat_sim[tree, i] <- a[tree]  * exp( -exp(-c_sim * (t_sim[i] - b[tree])))
    }
  }

#simulation for each tree snap
  for(tree in 1:n_trees_snap){
    for(i in 1:max_t){
      Y_hat_sim_snap[tree, i] <-  1 * exp( -exp(-c_hardcode * (t_sim[i] - b_snap[tree])))
    }
  }
     
}#end model
    ",fill=TRUE)
sink() 

jags <- jags.model('model_b.txt', 
                   data = list(
                     Y = as.matrix(data_for_model_core_prop_open_ragged),  #data_for_model_prop_open[3,1]
                     t = as.matrix(data_for_model_core_day_experiment_ragged), 
                     nobs_per_tree_core = data_for_model_core_nobs_per_tree$nobs_per_tree,
                     n_trees_core = nrow(data_for_model_core_nobs_per_tree),
                     n_sites_core = max(data_for_model_core_nobs_per_tree$site_n),
                     site_vector_core = data_for_model_core_nobs_per_tree$site_n,
                     t_sim = 1:max(data_for_model_core_day_experiment, na.rm = TRUE),
                     max_t = max(p_core_sites$day_experiment, na.rm = TRUE),
                     
                     #snap sites
                     Y_snap = data_for_model_snap$prop_open,
                     t_snap = data_for_model_snap$day_experiment,
                     #tree_n_snap = data_for_model_snap$tree_n,
                     site_n_snap = data_for_model_snap$site_n,
                     n_trees_snap = nrow(data_for_model_snap), #max(unique(data_for_model_snap$tree_n))
                     n_sites_snap = max(data_for_model_snap$site_n)
                   ),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter=2000) 

mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_snap"),  n.iter = 3000, thin = 3) #variables to monitor
plot(mcmc_samples_params)

mcmc_samples_params <- coda.samples(jags, variable.names=c("a_sim"),  n.iter = 1000, thin = 3) #variables to monitor
plot(mcmc_samples_params)

mcmc_samples_params <- coda.samples(jags, variable.names=c("Lambda3_snap"),  n.iter = 1000, thin = 3) #variables to monitor
plot(mcmc_samples_params)
summary(mcmc_samples_params)

mcmc_samples_params <- coda.samples(jags, variable.names=c("Y_hat_snap"),  n.iter = 1000, thin = 3) #variables to monitor
summary(mcmc_samples_params)

mcmc_samples_params <- coda.samples(jags, variable.names=c("b"),  n.iter = 1000, thin = 3) #variables to monitor
summary(mcmc_samples_params)

mcmc_samples_params <- coda.samples(jags, variable.names=c("b_snap"),  n.iter = 4000, thin = 3) #variables to monitor
summary(mcmc_samples_params)
plot(mcmc_samples_params)





## core: simulation for each tree ---------------------------------------------------------------------------
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 5000, thin = 2) #variables to monitor
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)
#results_params2$day_experiment <- 1:max(data_for_model_day_experiment, na.rm = TRUE)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n <- as.numeric(gsub("Y_hat_sim[", "", tree_n_vector, fixed = TRUE) )
results_params2 <- arrange(results_params2, tree_n, day_experiment) 
core_join <- select(p_core_sites, tree_n, site_n, x, y)
results_params3 <- left_join(results_params2, core_join) %>%  distinct()

ggplot()  + theme_bw() +
  geom_line(data = results_params3, aes(x = day_experiment, y = Mean, group = tree_n, color = tree_n)) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
  geom_point(data = p_core_sites, aes(x = day_experiment, y = prop_open), alpha = .5, color = "red") +
  facet_wrap(~site_n)

p_core_sites_halfway_day <- results_params3 %>% 
  group_by(tree_n) %>%
  mutate(sample_date = day_start + day_experiment) %>% 
  slice(which.min(abs(Mean - 0.50))) 





## simulation for each tree snap ---------------------------------------------------------------------------
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim_snap"),  n.iter = 5000, thin = 3) #variables to monitor
#plot(mcmc_samples_params2)

results_param2 <- summary(mcmc_samples_params2)
results_params2 <- data.frame(results_param2$statistics, results_param2$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$param<-substr(results_params2$parameter,1,1)

day_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 2) 
results_params2$day_experiment <- as.numeric(gsub("]", "", day_n_vector, fixed = TRUE) )

tree_n_vector <- purrr::map(strsplit(results_params2$parameter, split = ","), 1) 
results_params2$tree_n2 <- as.numeric(gsub("Y_hat_sim_snap[", "", tree_n_vector, fixed = TRUE) )

results_params2 <- arrange(results_params2, tree_n2, day_experiment) 
site_n_snap_join <- select(data_for_model_snap, tree_n2, site_n, site_name, x, y) %>% arrange(tree_n2) #%>% distinct()
results_params3 <- left_join(results_params2, site_n_snap_join)

# str(results_params2)
# str(site_n_snap_join)
# data_for_model_snap %>% select(site_n, site_name) %>% distinct() -> test

ggplot()  + theme_bw() +
  geom_line(data = results_params3, aes(x = day_experiment, y = Mean, group = tree_n2, color = tree_n2)) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
  geom_point(data = p_snap_sites, aes(x = day_experiment, y = prop_open), alpha = .2, color = "red") +
  facet_wrap(~site_n)

p_snap_sites_halfway_day <- results_params3 %>% 
  group_by(tree_n2) %>%
  mutate(sample_date = day_start + day_experiment) %>% 
  slice(which.min(abs(Mean - 0.50))) 

### an average of the day that the experiment
p_core_mean_day <-
  p_core_sites_halfway_day %>% select(day_experiment, tree_n, site_n, x, y) %>% 
  group_by(site_n) %>% summarize(site_mean = mean(day_experiment),
                                 x = mean(x),
                                 y = mean(y))

p_snap_mean_day <- 
  p_snap_sites_halfway_day %>% select(day_experiment, tree_n2,site_n, site_name, x, y) %>% 
  group_by(site_n, site_name) %>% summarize(site_mean = mean(day_experiment),
                                            x = mean(x),
                                            y = mean(y)) 
trees_mean_day <- bind_rows(p_core_mean_day, p_snap_mean_day)
readr::write_csv(trees_mean_day, "C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_gompertz_210416.csv")


tree_mean_day_sf <- st_as_sf(trees_mean_day, coords = c("x", "y")) %>% 
  select(d = site_mean)
# ggplot(tree_mean_day_sf) +   geom_sf(data = tree_mean_day_sf, colour = "black", fill = NA)
st_write(tree_mean_day_sf, "C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_gompertz_210416.shp")

#site means across space #library(sf)
tx_boundary <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")

ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = x, y = y, col = site_mean),# size = pollen),#col = hilo2), pollen / max_p
             data = trees_mean_day, alpha = .7, size = 3)  + 
  scale_color_continuous(low = "blue", high = "red", name = "halfway point (day)") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines




# 
# #simulation for each tree core
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
# results_tree_core_sim_join <- select(results_params2, tree_n, day_experiment, Mean, SD)
# results_tree_core_sim <- left_join(p_core_sites, results_tree_core_sim_join) %>% 
#   mutate(pollen_release = factor(pollen_release, levels = c("NA","none", "little", "some", "lots")),
#          pol_bin = case_when(pollen_release == "none" ~ "none",
#                              pollen_release == "little" ~ "release",
#                              pollen_release == "some" ~ "release",
#                              pollen_release == "lots" ~ "release",
#                              pollen_release == "NA" ~ "none"),
#          pol_cont = case_when(pollen_release == "none" ~ 0,
#                               pollen_release == "little" ~ 0.1,
#                               pollen_release == "some" ~ 0.3,
#                               pollen_release == "lots" ~ 1,
#                               pollen_release == "NA" ~ 0))
# 
# ggplot()  + theme_bw() +
#   geom_line(data = results_params2, aes(x = day_experiment, y = Mean, group = tree_n, color = tree_n)) +
#   # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
#   # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
#   geom_jitter(data = p_all_sites_core, aes(x = day_experiment, y = prop_open), color = "red") +
#   facet_wrap(~site_n)
# 
# 
# #a little analysis of pollen release
# results_tree_core_sim %>% 
#   ggplot(aes(x = date2, y = Mean, color = pollen_release)) + geom_jitter(size = 4, alpha = .7) + theme_bw() +
#   scale_color_viridis_d(direction = -1) + ylab("cones opened (proportion)")


### download and extract met data from Daymet and PRISM ###############################################################
library(daymetr)
library(sf)
library(magrittr)
library(zoo)
library(readr)


manual_obs_coords <- p_all_sites %>% #group_by(site, y, x) %>%  ungroup() %>% 
  mutate(x = round(x, 1), y = round(y, 1)) %>% 
  dplyr::select(site = site, lat = y, long = x) %>% distinct() %>% filter(!is.na(site)) 


#DAYMET ISNT A GOOD CHOICE BECAUSE YOU CANT DOWNLOAD CURRENT YEAR
# write_csv(manual_obs_coords, "C:/Users/dsk856/Box/texas/pheno/met_data/fs20_21_coords_daymetr210406.csv")
# weather_at_stations <- download_daymet_batch(file_location = "C:/Users/dsk856/Box/texas/pheno/met_data/fs20_21_coords_daymetr210406.csv",
#                                              start =2018, end = 2021, simplify = TRUE)
# write_csv(weather_at_stations, "C:/Users/dsk856/Box/texas/pheno/met_data/weather_at_sites_2018_2020_210406.csv")
# unique(weather_at_stations$measurement)
# weather_at_stations <-
#   read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/weather_at_sites_2018_2020_210203.csv") %>%
#   mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
#   mutate(measurement = gsub(pattern = ".", replacement = "", x = measurement, fixed = TRUE)) %>%
#   dplyr::select(site = site, date, measurement, value) %>%
# group_by(site, date, measurement) %>%
#   summarise(value = mean(value, na.rm = TRUE)) %>%
#     distinct() %>%
#   pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")
# head(weather_at_stations); summary(weather_at_stations)


weather_at_stations <- 
  readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/prism_ppt_fs20_21_2019_2021.csv") %>% 
  mutate(date = ymd(date)) %>%
  #mutate(measurement = gsub(pattern = ".", replacement = "", x = data, fixed = TRUE)) %>%
  dplyr::select(site = site, date, measurement = metvar, value = data) %>% 
  group_by(site, date, measurement) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")

weather_at_stations_tmean <- 
  readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/prism_tmean_fs20_21_2019_2021.csv") %>% 
  mutate(date = ymd(date)) %>%
  #mutate(measurement = gsub(pattern = ".", replacement = "", x = data, fixed = TRUE)) %>%
  dplyr::select(site = site, date, measurement = metvar, value = data) %>% 
  group_by(site, date, measurement) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")

weather_at_stations_vpdmin <- 
  readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/prism_fs20_21_vpdmin_2019_2021.csv") %>% 
  mutate(date = ymd(date)) %>%
  #mutate(measurement = gsub(pattern = ".", replacement = "", x = data, fixed = TRUE)) %>%
  dplyr::select(site = site, date, measurement = metvar, value = data) %>% 
  group_by(site, date, measurement) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")

weather_at_stations_vpdmax <- 
  readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/prism_fs20_21_vpdmax_2019_2021.csv") %>% 
  mutate(date = ymd(date)) %>%
  #mutate(measurement = gsub(pattern = ".", replacement = "", x = data, fixed = TRUE)) %>%
  dplyr::select(site = site, date, measurement = metvar, value = data) %>% 
  group_by(site, date, measurement) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  distinct() %>% 
  pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")



weather_at_stations <- left_join(weather_at_stations, weather_at_stations_tmean)
weather_at_stations <- left_join(weather_at_stations, weather_at_stations_tmean)

#creating some indices for weather conditions around the time of release
library(zoo)
weather_at_stations3 <- weather_at_stations %>% group_by(site) %>% 
  #filter(date > ymd("2020 - 11 - 01")) %>% 
  mutate(doy = yday(date),
         year = year(date),
         mo = month(date),
         season = case_when(doy > 320 ~ paste("s", year, (year + 1)),
                            doy < 41  ~ paste("s", (year - 1), year),
                            doy < 321 & doy > 40  ~ "not Ja season"),
         doy_m = case_when(doy > 320 ~ doy - 365,
                           doy < 320 ~ doy),
         # met_tmaxdegc_l1 = lag(met_tmaxdegc, 1),
         # met_tmindegc_l1 = lag(met_tmindegc, 1),
         # #met_tavgdegc_l1 = lag(met_tmindegc + met_tmindegc )/2) 
         met_prcpmmday_l1 =  dplyr::lag(x = met_ppt, k = 1),
         
         # met_vpPa_l1 = lag(met_vpPa, 1),
         # # met_vpdmin_l1 = lag(met_vpdmin, 1),
         # # met_vpdmax_l1 = lag(met_vpdmax, 1),
         # # 
         # matmax_7 =rollapply(met_tmaxdegc, 7,mean,align='right',fill=NA),
         # matmin_7 =rollapply(met_tmindegc, 7,mean,align='right',fill=NA),
         matavg_7 = rollapply(met_tmean, 7, mean,align='right',fill=NA),
         maprcp_7 = rollapply(met_ppt, 7, mean,align='right',fill=NA),
         # mavp_7 = rollapply(met_vpPa, 7,mean,align='right',fill=NA),
         # # mavpdmin_7 = rollapply(met_vpdmin, 7,mean,align='right',fill=NA),
         # # mavpdmax_7 = rollapply(met_vpdmax, 7,mean,align='right',fill=NA),
         # # 
         # matmax_14 =rollapply(met_tmaxdegc, 14,mean,align='right',fill=NA),
         # matmin_14 =rollapply(met_tmindegc, 14,mean,align='right',fill=NA),
         matavg_14 =rollapply(((met_tmindegc + met_tmindegc )/2), 14,mean,align='right',fill=NA),
         maprcp_14 = rollapply(met_ppt, 14,mean,align='right',fill=NA),
         # masrad_14 = rollapply(met_sradWm2, 14,mean,align='right',fill=NA),
         # mavp_14 = rollapply(met_vpPa, 14,mean,align='right',fill=NA),
         # # mavpdmin_14 = rollapply(met_vpdmin, 14,mean,align='right',fill=NA),
         # # mavpdmax_14 = rollapply(met_vpdmax, 14,mean,align='right',fill=NA),
         # # 
         # matmax_21 =rollapply(met_tmaxdegc, 21,mean,align='right',fill=NA),
         # matmin_21 =rollapply(met_tmindegc, 21,mean,align='right',fill=NA),
         matavg_21 =rollapply(((met_tmindegc + met_tmindegc )/2), 21,mean,align='right',fill=NA),
         maprcp_21 = rollapply(met_ppt, 21,mean,align='right',fill=NA),
         # masrad_21 = rollapply(met_sradWm2, 21,mean,align='right',fill=NA),
         # mavp_21 = rollapply(met_vpPa, 21,mean,align='right',fill=NA),
         # # mavpdmin_21 = rollapply(met_vpdmin, 21,mean,align='right',fill=NA),
         # # mavpdmax_21 = rollapply(met_vpdmax, 21,mean,align='right',fill=NA),
         # 
         # matmax_28 =rollapply(met_tmaxdegc, 28,mean,align='right',fill=NA),
         # matmin_28 =rollapply(met_tmindegc, 28,mean,align='right',fill=NA),
         matavg_28 =rollapply(((met_tmindegc + met_tmindegc )/2), 28,mean,align='right',fill=NA),
         maprcp_28 = rollapply(met_ppt, 28,mean,align='right',fill=NA),
         # masrad_28 = rollapply(met_sradWm2, 28,mean,align='right',fill=NA),
         # mavp_28 = rollapply(met_vpPa, 28,mean,align='right',fill=NA),
         # # mavpdmin_28 = rollapply(met_vpdmin, 28,mean,align='right',fill=NA),
         # # mavpdmax_28 = rollapply(met_vpdmax, 28,mean,align='right',fill=NA),
         # 
         # matmax_35 =rollapply(met_tmaxdegc, 35,mean,align='right',fill=NA),
         # matmin_35 =rollapply(met_tmindegc, 35,mean,align='right',fill=NA),
         matavg_35 =rollapply(((met_tmindegc + met_tmindegc )/2), 35,mean,align='right',fill=NA),
         maprcp_35 = rollapply(met_ppt, 35,mean,align='right',fill=NA)
         # masrad_35 = rollapply(met_sradWm2, 35,mean,align='right',fill=NA),
         # mavp_35 = rollapply(met_vpPa, 35,mean,align='right',fill=NA),
         # mavpdmin_35 = rollapply(met_vpdmin, 35,mean,align='right',fill=NA),
         # mavpdmax_35 = rollapply(met_vpdmax, 35,mean,align='right',fill=NA)
  )

#creating some summary weather data
weather_at_stations_summary_dec <- weather_at_stations3 %>% 
  group_by(site, season) %>% 
  filter(mo == 12) %>% 
  summarize(#matmax_dec = mean(met_tmaxdegc),
    #matmin_dec = mean(met_tmindegc),
    mactmean_dec = mean(met_tmean),
    macprcp_dec = mean(met_ppt)
    #mavpda_dec = mean(met_vpPa)
    # mavpdmin_dec = mean(met_vpdmin),
    # mavpdmax_dec = mean(met_vpdmax)
  )

weather_at_stations_summary_jan <- weather_at_stations3 %>%
  group_by(site, season) %>%
  filter(mo == 1) %>%
  summarize(#matmax_jan = mean(met_tmaxdegc),
    #matmin_jan = mean(met_tmindegc),
    mactmean_jan = mean(met_tmean),
    macprcp_jan = mean(met_ppt))
#mavpdmin_jan = mean(met_vpdmin),
#mavpdmax_jan = mean(met_vpdmax))

weather_at_stations_summary_fall <- weather_at_stations3 %>%
  mutate( season = case_when(doy > 274 ~ paste("s", year, (year + 1)),
                             doy < 41  ~ paste("s", (year - 1), year),
                             doy < 275 & doy > 40  ~ "not Ja season")) %>%
  group_by(site, season) %>%
  filter(mo > 9 & season != "not Ja season") %>%
  summarize(#matmax_fall = mean(met_tmaxdegc),
    #matmin_fall = mean(met_tmindegc),
    mactmean_fall = mean(met_tmean),
    macprcp_fall = mean(met_ppt))
#mavpdmin_fall = mean(met_vpdmin),
#mavpdmax_fall = mean(met_vpdmax))
# 
# #joining weather data with pollen
# pw <- left_join(p, weather_at_stations3)
# pw <- left_join(pw, weather_at_stations_summary_dec)
# pw <- left_join(pw, weather_at_stations_summary_jan)
# pw <- left_join(pw, weather_at_stations_summary_fall)



# site_halfway_params2_filt <-  site_halfway_params2 %>% #filter(site_n != 5) %>% 
#   mutate(site = paste(site_long, site_lat),
#          date = day_start + Mean,
#          dif_mean = Mean - 17.3) 
global_mean_50p <- mean(trees_mean_day$site_mean)

site_halfway_params2_filt <-  trees_mean_day %>% #filter(!is.na(site_n)) %>% 
  mutate(site = paste(round(x, 1), round(y, 1)),
         dif_mean = site_mean - global_mean_50p,
         date = day_start + site_mean) 


site_halfway_params2_filt <- left_join(site_halfway_params2_filt, weather_at_stations_summary_dec)
site_halfway_params2_filt<- left_join(site_halfway_params2_filt, weather_at_stations_summary_jan)
site_halfway_params2_filt<- left_join(site_halfway_params2_filt, weather_at_stations_summary_fall)

ggplot(site_halfway_params2_filt, aes(x = macprcp_dec, y= site_mean)) + geom_point(size = 3) + theme_bw() + 
  geom_smooth(method = "lm", se = F) +
  xlab("mean daily temperature in Jan (C)") + 
  #xlab("mean precipitation/day in the fall (mm)") + 
  ylab("date") + scale_color_viridis_c()

fit <- lm(site_mean ~ macprcp_dec , data = site_halfway_params2_filt)
summary(fit)
# fit$residuals
# str(fit)
# site_halfway_params2_filt <- site_halfway_params2_filt %>% 
#   mutate(resid = fit$residuals)



ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = x, y = y, col = date),# size = pollen),#col = hilo2), pollen / max_p
             data = site_halfway_params2_filt, alpha = .7, size = 3)  + 
  scale_color_continuous(low = "blue", high = "red", name = "halfway point (day)", labels=as.Date) + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines


ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = site_long, y = site_lat, col = resid),# size = pollen),#col = hilo2), pollen / max_p
             data = site_halfway_params2_filt, alpha = .7, size = 3)  + 
  scale_color_continuous(low = "blue", high = "red") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines

ggplot(weather_at_stations3, aes(x = date, y = met_ppt)) + geom_point() + facet_wrap(~site) + theme_bw() +
  geom_point(data = site_halfway_params2_filt, aes(x = date, y = -1, color = dif_mean), size = 5) +
  scale_color_viridis_c() +
  scale_x_date(limits = c(mdy("12-15-2020"), mdy("1-20-2021")))

mean(site_halfway_params2_filt$Mean)




ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = x, y = y, col = mactmean_dec),# size = pollen),#col = hilo2), pollen / max_p
             data = site_halfway_params2_filt, alpha = .7, size = 3)  + 
  scale_color_continuous(low = "blue", high = "red") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines





sms %>% 
  filter(sms_date > ymd("20/03/20")) %>% 
  filter(sms_date < ymd("20/06/01")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
  ggplot(aes(x = sms_date, y = sms, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
  scale_color_viridis_c(labels=as.Date, name = "peak date") +
  xlab("date") + ylab("surface soil moisture (mm)")
summary(sms$sms)

formula <- y ~ x 
#formula <- y ~ x + I(x^2)
library(ggpmisc)
sms %>% 
  filter(sms_date > ymd("20/04/1")) %>% 
  filter(sms_date < ymd("20/05/1")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
  group_by(site, d, d2) %>% 
  summarize(sms_mean = mean(sms)) %>% #%T>% => test 
  ggplot(aes(x = sms_mean, y = d2)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
               formula = formula, parse = TRUE, label.x = .9) +
  theme_bw() + xlab("surface soil moisture (mm)") + ylab("50% pollen released (day)")


