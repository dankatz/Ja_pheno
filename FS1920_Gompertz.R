# This script is for the Dec 2019 - Jan 2020 Juniperus ashei field season
# Data assembly (observations I took via Nature's Notebook and via Collector for ArcGIS) and QA/QC
# Analysis to find the cone-opening season midpoint using a Gompertz function

#set up work environment
#rm(list=ls())
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(rjags)


### load data and perform QA/QC #########################################################################
site_names_shp <- st_read("C:/Users/dsk856/Box/texas/pheno/site_names_fs2019_20.shp") #load site names

#load in cone %open observations I took via Collector for ArcGIS
arc_data_dk_raw <- read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_GIScollector200207.csv") %>% 
  rename(x = POINT_X,
         y = POINT_Y) %>% 
  mutate(date_hm = mdy_hm(mdy_hm_chr),
        dates = date(date_hm),
        ydays = yday(dates),
        site_placeholder = paste(round(x, 1), round(y, 1)),
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
  filter(!GlobalID %in% obs_to_exclude) %>% #incomplete datapoint that was double-entered
  mutate(tree = case_when(tree == "-99.39506 30.02735" & ydays == 14 ~ "-99.395063, 30.027340", #correcting a location error 
                          tree == "-99.3948 30.03148" & ydays == 14 ~ "-99.39481 30.03148", #correcting a location error
                          TRUE ~ tree))

# arc_data_dk_raw$tree[arc_data_dk_raw$tree == "-99.39506 30.02735"]
# arc_data_dk$tree2[arc_data_dk$tree2 == "aaaaa"]

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

# #time series for each site
# arc_data_dk %>% 
#   ggplot(aes(x =dates, y = pc_open_pc, group = tree)) + geom_jitter() + geom_line() + facet_wrap(~site_placeholder) + theme_bw()
# 
# #time series for each tree at a site
# arc_data_dk %>%
#   filter(site_name == "Jewell") %>%
#   ggplot(aes(x =dates, y =pc_open_pc)) + geom_point(alpha = 0.5) + geom_line() + facet_wrap(~tree) + theme_bw()
# 

#map of Texas with data I collected via Nature's Notebook app
#ggplot(arc_data_dk, aes(x = x, y = y)) + geom_point() + theme_bw()
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
         y = Latitude) %>% 
         # tree = Individual_ID,
         # GlobalID = Observation_ID
  mutate(GlobalID = as.character(Observation_ID),
         tree = as.character(Individual_ID),
         dates = ymd(Observation_Date),
         date_hm = ymd_hms(paste(Observation_Date, Observation_Time)),
         site = paste(round(x, 1), round(y, 1)),
         ydays = yday(dates),
         pc_open_pc = case_when(Intensity_Value == "-9999" ~ 0,
                                Intensity_Value == "Less than 5%" ~ 2.5,
                                Intensity_Value == "5-24%" ~ 14.5,
                                Intensity_Value == "25-49%" ~ 37,
                                Intensity_Value == "50-74%" ~ 62,
                                Intensity_Value == "75-94%" ~ 84.5,
                                Intensity_Value == "95% or more" ~ 97.5)) %>% 
  filter(dates > ymd("2019-12-01") & dates < ymd("2020-03-01")) %>% 
  dplyr::select(dates, date_hm, Observation_Time, ydays, site, tree, x, y, pc_open_pc, GlobalID) %>% 
  arrange(site, date_hm) %>% 
  filter(tree != 222846) %>%  #not actually Juniperus ashei
  filter(!tree %in% c(222847, 222848, 222851, 222852, 222853, 223224, 223225)) #one time observations 

npn_data_dk %>% 
  #filter(site == "-97.7 30.3") %>% 
  ggplot(aes(x = dates, y = pc_open_pc, color = as.factor(tree))) + geom_point() + geom_line()  + theme_bw() +
  facet_wrap(~tree)

### combine the data I collected via Collector for ArcGIS and via Nature's Notebook
arc_data_dk
npn_data_dk
day_start_1920 <- mdy("12-08-2019")

pheno <- bind_rows(arc_data_dk, npn_data_dk) %>% 
          mutate(day_experiment = as.numeric(dates - day_start_1920))
                 #tree = paste(round(x, 5), round(y, 5)))
#write_csv(pheno, "C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs19_20_database_210813.csv")
#pheno <- read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs19_20_database_210813.csv")

site_names_shp <- st_read("C:/Users/dsk856/Box/texas/pheno/site_names_fs2019_20.shp")
pheno_sf <- st_as_sf(pheno, coords = c("x", "y"), remove = FALSE) %>% 
  st_set_crs(., 4326) %>% st_transform(., st_crs(site_names_shp))

pheno_sf <- st_join(pheno_sf, site_names_shp)
#unique(pheno_sf$site_name)

core_sites_fs1920 <- c("Hurst", "Puccetti", "Jewell", "hyde_park")
pheno_sf <- mutate(pheno_sf, site_type = case_when(site_name %in% core_sites_fs1920 ~ "core",
                                                   TRUE ~ "snap"))

#test <- pheno_sf %>% group_by(site_name, site_type) %>% summarize(n = n()) 

pheno_sf %>%  ggplot(aes(x = day_experiment, y = pc_open_pc, group = tree, color = site_type)) + geom_line() + 
  facet_wrap(~site_name) +theme_bw() + geom_point()

# pheno_sf %>% 
#   st_drop_geometry() %>% #need to drop the geometry to average the site coordinates
#   filter(site_type == "core") %>% 
#   group_by(site_name) %>% 
#   summarize(x = mean(x),
#             y = mean(y)) %>% 
#   dplyr::select(x, y, site_name) %>% distinct() %>% 
#   st_as_sf(., coords = c("x", "y")) %>% 
#   st_set_crs(., 4326) %>% 
#   st_write(.,  "C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs19_20_core_sites2.shp")
# 
# pheno_sf %>% st_drop_geometry() %>% #need to drop the geometry to average the site coordinates
#   filter(site_type == "snap") %>% 
#   group_by(site_name) %>% 
#   summarize(x = mean(x),
#             y = mean(y)) %>% 
#   dplyr::select(x, y, site_name) %>% distinct()  %>% 
#   st_as_sf(., coords = c("x", "y")) %>% 
#   st_set_crs(., 4326) %>% 
#   st_write(.,  "C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs19_20_snap_sites2.shp")
# 
#save file
pheno_sf %>% st_drop_geometry() %>%
  write_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_clean_fs19_20_210907.csv")

### some data exploration #############################################################################
p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_clean_fs19_20_210907.csv") 

#time series for each site
p %>% 
  ggplot(aes(x =dates, y = pc_open_pc, group = tree, color = site_type)) + geom_jitter() + 
  geom_line() + facet_wrap(~site_name) + theme_bw()

#time series for each tree at a site
p %>%
  filter(site_name == "Jewell") %>%
#  filter(tree == "-99.39506 30.02735") %>% 
  ggplot(aes(x =dates, y =pc_open_pc)) + geom_point(alpha = 0.5) + geom_line() + facet_wrap(~tree) + theme_bw()



### Gompertz model for pollen cone opening fs19_20 ####################################################
#adopted from the FS2020_2021 model in ja_ecolab_Gompertz_jags.R
#using parameter values from the FS2020_2021 model too, including for core sites

p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_clean_fs19_20_210907.csv") 


#str(p)
day_start <- mdy("12-10-2019")


p_all_sites <- p %>%
  mutate(prop_open = pc_open_pc/100,
         #date2 = mdy_hm(date) - hours(6), #correct from GMT
         date3 = dates,
         site = site_name, #paste(round(x, 1), round(y, 1)),
         ydays = yday(date3)
         #tree = paste(round(x, 5), round(y, 5))
         ) %>% 
  filter(!is.na(prop_open)) %>% 
  mutate(tree_n = as.numeric(as.factor(tree)),
         site_n = as.numeric(as.factor(site)),
         day_experiment = as.numeric(date3 - day_start)) %>% 
  arrange(date3, tree_n) %>% 
  mutate(prop_open  = case_when(prop_open >= 0.99 ~ 0.99, #I don't think the number of open cones above 99% is robust due to cones that will
                                prop_open < 0.99 ~ prop_open)) #never open and dropping of other cones. So I'm trying 95% as the asymptote

  
# #adding in the assumption that all cones were closed on Dec 1 and open on March 1
p_all_sites_bound_start <- p_all_sites %>%
  select(tree, tree_n, site, site_n, site_name, site_type) %>%
  distinct() %>%
  mutate(prop_open = 0,
         date3 = mdy("12-01-2019"),
         day_experiment = as.numeric(date3 - day_start),
         doy = yday(date3),
         real_data = "not real data")
p_all_sites_bound_end <- p_all_sites %>%
  select(tree, tree_n, site, site_n, site_name, site_type) %>%
  distinct() %>%
  mutate(prop_open = 0.99,
         date3 = mdy("03-01-2020"),
         day_experiment = as.numeric(date3 - day_start),
         doy = yday(date3),
         real_data = "not real data")

p_all_sites <- bind_rows(p_all_sites, p_all_sites_bound_start, p_all_sites_bound_end)



# length(unique(p_all_sites$tree))
# length(unique(p_all_sites$site))

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
                                        prop_open > 0.974 ~ 0,
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
#   st_write(.,  "C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs19_20_core_sites8.shp")

# p_snap_sites %>% group_by(site_name) %>% 
#   summarize(x = mean(x),
#             y = mean(y)) %>% 
#   dplyr::select(x, y, site_name) %>% distinct() %>% 
#   st_as_sf(., coords = c("x", "y")) %>% 
#   st_set_crs(., 4326) %>% 
#   st_write(.,  "C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs19_20_snap_sites8.shp")

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
  LAMBDA1[tree] <- 250 #~ dgamma(0.01,0.01) #uninformative gamma prior
  
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

rate_global_mean <- 1.02 #dnorm(0, 0.001)
rate_global_mean_snap <- 1.02 #max(rate_global_mean, 0)#preventing backflow of information #prevent it from wandering negative

rate_global_sigma <- 27.2 #~ dgamma(0.01,0.01)
rate_global_sigma_snap <- rate_global_sigma #preventing backflow of information

LAMBDA3_core <- 0.031 #~ dgamma(0.01,0.01)
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
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter = 4000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_core"),  n.iter = 1000, thin = 3) #variables to monitor #"b", "c" "b_snap"
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$tree <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
hist(results_params2$Mean)


#simulation for each tree core
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("Y_hat_sim"),  n.iter = 5000, thin = 3) #variables to monitor
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
site_n_snap_join <- select(data_for_model_snap, tree_n_snap, site_n_snap) %>% distinct()
results_params3 <- left_join(results_params2, site_n_snap_join) %>% 
  mutate(date3 = day_start + day_experiment)

results_params4 <- filter(results_params3, date3 > mdy("12-15-2019") & date3 < mdy("02-01-2020")) 
ggplot()  + theme_bw() +
  geom_line(data = results_params4, aes(x = date3, y = Mean, group = tree_n_snap)) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X2.5., group = tree_n,color = tree_n), lty =2) +
  # geom_line(data = results_params2, aes(x = day_experiment, y = X97.5., group = tree_n,color = tree_n), lty =2) +
  geom_jitter(data = data_for_model_snap, aes(x = date3, y = prop_open), width = 2, color = "red") +
  facet_wrap(~site_n_snap) + xlab("day of experiment") + ylab("cones open (proportion)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


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
readr::write_csv(site_export_df, "C:/Users/dsk856/Box/texas/pheno/fs19_20_site_halfway_cones_210907.csv")
