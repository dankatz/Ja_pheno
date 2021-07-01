# This script is for the Dec 2019 - Jan 2020 Juniperus ashei field season
# Data assembly (observations I took via Nature's Notebook and via Collector for ArcGIS) and QA/QC
# Analysis to find the cone-opening season midpoint using a Gompertz function

#set up work environment
library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)


### load data and perform QA/QC #########################################################################
#load in cone %open observations I took via Collector for ArcGIS
arc_data_dk_raw <- read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_GIScollector200207.csv") %>% 
  rename(x = POINT_X,
         y = POINT_Y) %>% 
  mutate(date_hm = mdy_hm(mdy_hm_chr),
        date = date(date_hm),
        site = paste(round(x, 1), round(y, 1)),
        yday = yday(date),
        tree = paste(round(x, 5), round(y, 5))) 

obs_to_exclude <- c("{b1180487-06cf-41f5-9e3e-5d8d2270397b}", #incomplete datapoint that was double-entered
                    "{7b4fcd5e-e551-45eb-bb42-709bfa32fa5f}", #incomplete datapoint that was double-entered
                    "{18861c6e-0c7f-4665-9d10-f2d6b1c949f5}", #incomplete datapoint that was double-entered
                    "{b438d625-5240-4193-882b-2cef56f4d573}", #incomplete datapoint that was double-entered
                    "{2c10c452-fba4-442c-af45-5904aa07abb0}", #incomplete datapoint that was double-entered
                    "{6569d35d-0393-4527-bc4b-e887e742cf20}", #incomplete datapoint that was double-entered
                    "{d4c4b2a0-d806-463a-bd4e-00578dc9e8d3}", #incomplete datapoint that was double-entered
                    "{4a86352f-9579-425d-96b1-65b917eedcdc}", #incomplete datapoint that was double-entered
                    "{67c2e5d2-6e4c-498a-99cf-8d24679b62f8}", #incomplete datapoint that was double-entered
                    "{17adfa4c-bf28-434c-a11b-3c9b810159cb}", #incomplete datapoint that was double-entered
                    "{19f62d31-981c-4077-80e2-f4b28a5a0af4}", #incomplete datapoint that was double-entered
                    "{de5b3194-647e-4a24-ab4f-f6e2f8af01da}", #incomplete datapoint that was double-entered
                    "{05c3cbb7-cff9-4ff1-8cf0-bf8db9b4b359}", #incomplete datapoint that was double-entered
                    "{4a86352f-9579-425d-96b1-65b917eedcdc}", #incomplete datapoint that was double-entered
                    "{2cf55121-2dd1-4757-a336-d2c1d9e8b734}", #incomplete datapoint that was double-entered
                    "{2cedb4a3-7609-49d0-9486-4d491a54bf25}", #incomplete datapoint that was double-entered
                    "{21600119-4d78-41d0-83a9-a46ba6357d38}", #incomplete datapoint that was double-entered
                    "{f37926fa-d4f4-4626-a7d2-607681157190}", #incomplete datapoint that was double-entered
                    "{9f39240c-3127-41c1-882d-1ce9d6eb8624}", #incomplete datapoint that was double-entered
                    "{5fa5e271-924b-4e03-9047-cf6b8ce0ff8f}", #incomplete datapoint that was double-entered
                    "{75db4db5-76af-4af9-b64f-f511bba2b086}", #incomplete datapoint that was double-entered
                    "{aa65299a-9af3-4ef7-baa8-d0586fec5bc7}") #incomplete datapoint that was double-entered

arc_data_dk <- arc_data_dk_raw %>% 
  filter(pollen_con > 0 & !is.na(pollen_con)) %>% 
  filter(pc_open_pc != 999) %>% 
  filter(!GlobalID %in% obs_to_exclude) #incomplete datapoint that was double-entered

#correct some datapoints that seem to be incorrect data entry
arc_data_dk$pc_open_pc[arc_data_dk$GlobalID == "{65d73664-2a14-4f74-a599-49401be87e98}" & !is.na(arc_data_dk$GlobalID)] <- 100 
 #This is an error- unopened cones don't release pollen and I remember cones being open on that tree on that visit 

#looking around for trees where Collector double entered
# potential_duplicates <- arc_data_dk %>% 
#   #mutate(tree_obs = paste(mdy_hm_chr, tree)) %>% 
#   dplyr::select(mdy_hm_chr, tree) %>% 
#   group_by(mdy_hm_chr, tree) %>% 
#   summarize(n = n())
# test <- left_join(arc_data_dk, potential_duplicates)

#time series for each site
arc_data_dk %>% 
  ggplot(aes(x =date, y =pc_open_pc)) + geom_jitter() + facet_wrap(~site)

#time series for each tree at a site
arc_data_dk %>% 
  filter(site == "-98.2 29.9") %>% 
  ggplot(aes(x =date, y =pc_open_pc)) + geom_point(alpha = 0.5) + facet_wrap(~tree) + theme_bw()


ggplot(arc_data_dk, aes(x = x, y = y)) + geom_point() + theme_bw()

#map of Texas with data I collected via Nature's Notebook app
tx_boundary <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = x, y = y), #size = Observation_Date,#col = hilo2), pollen / max_p
             data = arc_data_dk, alpha = .7, size = 2, col = "red")  + #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines



#load in cone %open observations I took via Nature's Notebook and convert to same format
npn_data_dk <- read_csv("C:/Users/dsk856/Box/texas/pheno/npn_cit_sci/npn_data_downloads/datasheet_210630/status_intensity_observation_data_dk.csv") %>% 
  rename(x = Longitude,
         y = Latitude,
         tree = Individual_ID,
         GlobalID = Observation_ID) %>% 
  mutate(date = ymd(Observation_Date),
         date_hm = ymd_hms(paste(Observation_Date, Observation_Time)),
         site = paste(round(x, 1), round(y, 1)),
         yday = yday(date),
         pc_open_pc = case_when(Intensity_Value == "-9999" ~ 0,
                                Intensity_Value == "Less than 5%" ~ 2.5,
                                Intensity_Value == "5-24%" ~ 14.5,
                                Intensity_Value == "25-49%" ~ 37,
                                Intensity_Value == "50-74%" ~ 62,
                                Intensity_Value == "75-94%" ~ 84.5,
                                Intensity_Value == "95% or more" ~ 97.5)) %>% 
  filter(date > ymd("2019-12-01") & date < ymd("2020-03-01")) %>% 
  dplyr::select(date, date_hm, Observation_Time, yday, site, tree, x, y, pc_open_pc, GlobalID) %>% 
  arrange(site, date_hm)


npn_data_dk %>% 
  filter(site == "-97.7 30.3") %>% 
  ggplot(aes(x = date, y = pc_open_pc, color = as.factor(tree))) + geom_point() + geom_line()  + theme_bw()

#need to make sure the tree order changes get fixed 


### Gompertz model ##################################################################################
#adopt this from the FS2020_2021 model:

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

test <- filter(p_core_sites, site == "-97.6 32.2") %>% 
  ggplot(aes(x = day_experiment, y = prop_open, group = tree_n, color = tree_n)) + geom_line() + facet_wrap(~site)

p_snap_sites %>%  ggplot(aes(x = day_experiment, y = prop_open, group = tree)) + geom_point() 
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
