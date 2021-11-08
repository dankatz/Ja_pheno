#J. ashei ripening vs airborne juniper pollen in Texas


library(readr)
library(dplyr)
library(tidyr)
library(imputeTS)
library(ggpmisc)
library(lubridate)
library(sf)

### NAB data in Texas ############################################################
# load in NAB data 
nab <- readr::read_csv("C:/Users/dsk856/Box/texas/NAB/NAB_pollen_modeled_linear_interp_201002.csv") %>%
  # filter(NAB_station != "Houston") %>%
  # filter(NAB_station != "College Station") %>%
  # filter(NAB_station != "Dallas") %>%
  # filter(NAB_station != "Flower Mound") %>%
  filter(mo < 4 | mo > 11) %>%
  mutate(day_exp = case_when(mo < 4 ~ yday(date) + 21,
                             mo > 10 ~ yday(date) -  yday(dmy(paste(10, 12, year, sep = "/")))),
         yr_exp = case_when(mo < 4 ~ paste0("yr_", (year(date) -1 ), "_", year(date)),
                            mo > 10 ~ paste0("yr_", year(date), "_", (year(date) + 1))))
           

#map of Texas with NAB data
tx_boundary <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
NAB_subset <- nab %>% filter(date == "2019-01-08")
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = Long, y = Lat, color = Cupressaceae), #size = Observation_Date,#col = hilo2), pollen / max_p
             data = NAB_subset, alpha = .95, size = 5)  + #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  scale_color_viridis_c(name = "airborne pollen (grains/m3)", option = "viridis") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines


    
#detect low data years 
n_na <- nab %>%
  group_by(yr_exp, NAB_station) %>%
  filter(day_exp > 10 & day_exp < 60) %>%
  filter(!is.na(Cupressaceae))%>%
  summarize(n_non_NA = n(),
            n_NA = 49 - n_non_NA)

nab <- left_join(nab, n_na)

nab %>% 
  #filter(n_NA < 40) %>%
  ggplot(aes(x = day_exp, y = Cupressaceae + 1)) + geom_point() + theme_bw() + facet_grid(yr_exp~NAB_station) +
  scale_y_log10() + 
  geom_line(aes(x = day_exp, y = Cupressaceae_m + 1), color = "red", lwd = 0.5)


#filter out low data years
nab_low_na <- nab %>% filter(n_NA < 50)


#calculate cumulative pollen and relative position in accumulated pollen season
cup_m_season_total <- nab_low_na %>%
  group_by(NAB_station, yr_exp) %>%
  summarize(cup_m_total = sum(Cupressaceae_m))

nab_low_na <- left_join(nab_low_na, cup_m_season_total)  

nab_low_na <- nab_low_na %>%
  arrange(NAB_station, yr_exp, day_exp) %>%
  group_by(NAB_station, yr_exp) %>%
  mutate(cup_m_cumu = cumsum(Cupressaceae_m),
         cup_m_cumu_dist = cup_m_cumu/cup_m_total)

#median and 50% and 95% pollen season
nearest_25 <- nab_low_na %>% group_by(NAB_station, yr_exp) %>% 
  filter(abs(cup_m_cumu_dist - 0.25) == min(abs(cup_m_cumu_dist - 0.25)))

nearest_50 <- nab_low_na %>% group_by(NAB_station, yr_exp) %>% 
  filter(abs(cup_m_cumu_dist - 0.5) == min(abs(cup_m_cumu_dist - 0.5)))

nearest_75 <- nab_low_na %>% group_by(NAB_station, yr_exp) %>% 
  filter(abs(cup_m_cumu_dist - 0.75) == min(abs(cup_m_cumu_dist - 0.75))) %>%
  slice(1)

  
#some data visualization
ggplot(nab_low_na, aes(x = day_exp, y = Cupressaceae_m + 1, color = cup_m_cumu_dist)) + geom_line() + theme_bw() + facet_grid(yr_exp~NAB_station) +
  #scale_y_log10() + 
  scale_color_viridis_c() +
  geom_point(data = nearest_25, aes(x= day_exp, y = Cupressaceae_m + 1 ), col = "red") +
  geom_point(data = nearest_50, aes(x= day_exp, y = Cupressaceae_m + 1 ), col = "red") +
  geom_point(data = nearest_75, aes(x= day_exp, y = Cupressaceae_m + 1 ), col = "red") 

ggplot(nab_low_na, aes(x = day_exp, y = cup_m_cumu_dist + 1)) + geom_line() + theme_bw() + facet_grid(yr_exp~NAB_station) +
  #scale_y_log10() + 
  scale_color_viridis_c() +
  geom_point(data = nearest_25, aes(x= day_exp, y = cup_m_cumu_dist + 1 ), col = "red") +
  geom_point(data = nearest_50, aes(x= day_exp, y = cup_m_cumu_dist + 1 ), col = "red") +
  geom_point(data = nearest_75, aes(x= day_exp, y = cup_m_cumu_dist + 1 ), col = "red") 




### cone opening preds compared to NAB #########################################################################
pheno_preds_NAB <- read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs_models/NAB_opening_preds_jan_dec_prcp_25km.csv") %>%
                    rename(day_exp = day_experiment,
                           yr_exp = fs_year) %>%
                    dplyr::select(-ID, -lat, -long) %>%
                    filter(!is.na(rel_opening)) %>%
                    filter(NAB_station == "Georgetown" | NAB_station == "San Antonio A" | NAB_station == "San Antonio B"|
                           NAB_station == "Waco A" | NAB_station == "Waco B" | NAB_station == "College Station" |
                           NAB_station == "Flower Mound" | NAB_station == "Dallas" | NAB_station == "Houston")

pheno_season_total <- pheno_preds_NAB %>%
  group_by(NAB_station, yr_exp) %>%
  summarize(opening_total = sum(rel_opening))

pheno_preds_NAB <- left_join(pheno_preds_NAB, pheno_season_total)  

pheno_preds_NAB <- pheno_preds_NAB %>%
  arrange(NAB_station, yr_exp, day_exp) %>%
  group_by(NAB_station, yr_exp) %>%
  mutate(opening_cumu = cumsum(rel_opening),
         opening_dist = opening_cumu/opening_total)

#25 and 50% and 75% pollen season
nearest_25_pheno <- pheno_preds_NAB %>% group_by(NAB_station, yr_exp) %>% 
  filter(abs(opening_dist - 0.25) == min(abs(opening_dist - 0.25)))%>%
  rename(day_exp_p = day_exp)

nearest_50_pheno <- pheno_preds_NAB %>% group_by(NAB_station, yr_exp) %>% 
  filter(abs(opening_dist - 0.5) == min(abs(opening_dist - 0.5))) %>%
  rename(day_exp_p = day_exp)

nearest_75_pheno <- pheno_preds_NAB %>% group_by(NAB_station, yr_exp) %>% 
  filter(abs(opening_dist - 0.75) == min(abs(opening_dist - 0.75))) %>%
  slice(1)%>%
  rename(day_exp_p = day_exp)


pheno_preds_NAB
nab_low_na

nab_pheno_c <- left_join(nab_low_na, pheno_preds_NAB)

#visualize phenology model at NAB stations
nab_pheno_c %>%
  ggplot(aes(x = day_exp, y = rel_opening)) + geom_line() + facet_grid(NAB_station ~ yr_exp) + theme_bw()


#compare time series to NAB
nab_pheno_c %>% #filter(env_year > 2008) %>%

  ggplot(aes(x = day_exp, y = Cupressaceae_m + 1, color = opening_dist)) + geom_line() + theme_bw() + 
  facet_grid(NAB_station ~ year, scales = "free_y") +
  #scale_y_log10() + 
  scale_color_viridis_c() +
  geom_point(data = nearest_25, aes(x= day_exp, y = Cupressaceae_m + 1 ), col = "red") +
  geom_point(data = nearest_50, aes(x= day_exp, y = Cupressaceae_m + 1 ), col = "red") +
  geom_point(data = nearest_75, aes(x= day_exp, y = Cupressaceae_m + 1 ), col = "red") +
  geom_point(data = nearest_25_pheno, aes(x= day_exp_p, y = 1 ), col = "blue") +
  geom_point(data = nearest_50_pheno, aes(x= day_exp_p, y = 1 ), col = "blue") +
  geom_point(data = nearest_75_pheno, aes(x= day_exp_p, y = 1 ), col = "blue") 

#put both time series on the same fig
nab_pheno_c %>%
  ggplot(aes(x = day_exp, y = rel_opening)) + geom_line() + 
  facet_grid(NAB_station ~ yr_exp, scales = "free_y") + ggthemes::theme_few() +
  geom_point(data = nab_pheno_c, aes(x = day_exp, y = Cupressaceae/50000), alpha = 0.1, col = "red") + 
  geom_line(data = nab_pheno_c, aes(x = day_exp, y = Cupressaceae/50000), alpha = 0.5, col = "red") 


#compare season mid-points directly
nearest_50_nab_pheno <- left_join(nearest_50, nearest_50_pheno)

nearest_50_nab_pheno %>%
  #filter(yr_exp != "yr_2015_2016") %>%
ggplot(aes(x = day_exp_p, y = day_exp, color = NAB_station)) + geom_point() + theme_bw() + 
  geom_abline(slope = 1, intercept = 0, lty =2) +
  geom_smooth(method = "lm", se = FALSE)

summary(lm(day_exp ~ day_exp_p, data = nearest_50_nab_pheno))




### Landon's pollen data in Texas ############################################################
# load in NAB data 
lb <- readr::read_csv("C:/Users/dsk856/Box/texas/NASA_project/landon_daily_measurements_2009_2011.csv") %>%
  mutate(date = ymd(date),
         mo = month(date),
         year = year(date),
         day_exp = case_when(mo < 4 ~ yday(date) + 21,
                             mo > 10 ~ yday(date) -  yday(dmy(paste(10, 12, year, sep = "/")))),
         yr_exp = case_when(mo < 4 ~ paste0("yr_", (year(date) -1 ), "_", year(date)),
                            mo > 10 ~ paste0("yr_", year(date), "_", (year(date) + 1))))%>%
  arrange(site, date)

#data imputation
lb_imputation <- imputeTS::na_interpolation(lb$concentration)
lb <- lb %>%  
  mutate(concentration_m = lb_imputation) %>%
  group_by(site, yr_exp) %>%
  mutate(concentration_m_7d = slider::slide_dbl(concentration_m, mean, .before = 3, .after = 3))


lb %>% 
  #filter(n_NA < 20) %>%
  ggplot(aes(x = day_exp, y = concentration + 1)) + geom_point() + theme_bw() + facet_grid(yr_exp~site) +
  #scale_y_log10() + 
  geom_line(aes(x = day_exp, y = concentration_m_7d + 1), color = "red", lwd = 0.5)



#calculate cumulative pollen and relative position in accumulated pollen season
cup_m_season_total <- lb %>%
  group_by(site, yr_exp) %>%
  summarize(cup_m_total = sum(concentration_m))

lb <- left_join(lb, cup_m_season_total)  %>%
  arrange(site, yr_exp, day_exp) %>%
  group_by(site, yr_exp) %>%
  mutate(cup_m_cumu = cumsum(concentration_m),
         cup_m_cumu_dist = cup_m_cumu/cup_m_total)

#median and 50% and 95% pollen season
lb_nearest_25 <- lb %>% group_by(site, yr_exp) %>% 
  filter(abs(cup_m_cumu_dist - 0.25) == min(abs(cup_m_cumu_dist - 0.25)))

lb_nearest_50 <- lb %>% group_by(site, yr_exp) %>% 
  filter(abs(cup_m_cumu_dist - 0.5) == min(abs(cup_m_cumu_dist - 0.5)))

lb_nearest_75 <- lb %>% group_by(site, yr_exp) %>% 
  filter(abs(cup_m_cumu_dist - 0.75) == min(abs(cup_m_cumu_dist - 0.75))) %>%
  slice(1)

lb_peak <- lb %>% group_by(site, yr_exp) %>% slice_max(order_by = concentration_m_7d, n = 1)




#some data visualization
ggplot(lb, aes(x = day_exp, y = concentration + 1, color = cup_m_cumu_dist)) + geom_line() + geom_point()+ theme_bw() + 
  facet_grid(site~yr_exp, scales = "free_y") +
  #scale_y_log10() + 
  scale_color_viridis_c() +
  geom_point(data = lb_nearest_25, aes(x= day_exp, y = -100 ), col = "red") +
  geom_point(data = lb_nearest_50, aes(x= day_exp, y = -100 ), col = "red") +
  geom_point(data = lb_nearest_75, aes(x= day_exp, y = -100 ), col = "red") +
  geom_point(data = lb_peak, aes(x= day_exp, y = -100 ), col = "blue") 

ggplot(lb, aes(x = day_exp, y = cup_m_cumu_dist + 1)) + geom_line() + theme_bw() + facet_grid(yr_exp~site) +
  scale_y_log10() + scale_color_viridis_c() +
  geom_point(data = lb_nearest_25, aes(x= day_exp, y = cup_m_cumu_dist + 1 ), col = "red") +
  geom_point(data = lb_nearest_50, aes(x= day_exp, y = cup_m_cumu_dist + 1 ), col = "red") +
  geom_point(data = lb_nearest_75, aes(x= day_exp, y = cup_m_cumu_dist + 1 ), col = "red") 


### cone opening preds compared to landon's data #########################################################################
pheno_preds_lb <- read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs_models/NAB_opening_preds_jan_dec_prcp_25km.csv") %>%
  rename(day_exp = day_experiment,
         yr_exp = fs_year,
         site = NAB_station) %>%
  dplyr::select(-ID, -lat, -long) %>%
  filter(!is.na(rel_opening))  %>%
  filter(yr_exp == "yr_2009_2010" | yr_exp == "yr_2010_2011") %>%
  filter(site == "balcones" | site == "dallas" | site ==  "junction" | site == "san marcos" | site == "sonora")

pheno_season_total <- pheno_preds_lb %>%
  group_by(site, yr_exp) %>%
  summarize(opening_total = sum(rel_opening))

pheno_preds_lb <- left_join(pheno_preds_lb, pheno_season_total)  

pheno_preds_lb <- pheno_preds_lb %>%
  arrange(site, yr_exp, day_exp) %>%
  group_by(site, yr_exp) %>%
  mutate(opening_cumu = cumsum(rel_opening),
         opening_dist = opening_cumu/opening_total) 

#25 and 50% and 75% pollen season
nearest_25_pheno_lb <- pheno_preds_lb %>% group_by(site, yr_exp) %>% 
  filter(abs(opening_dist - 0.25) == min(abs(opening_dist - 0.25)))%>%
  rename(day_exp_p = day_exp)

nearest_50_pheno_lb <- pheno_preds_lb %>% group_by(site, yr_exp) %>% 
  filter(abs(opening_dist - 0.5) == min(abs(opening_dist - 0.5))) %>%
  rename(day_exp_p = day_exp)


nearest_75_pheno_lb <- pheno_preds_lb %>% group_by(site, yr_exp) %>% 
  filter(abs(opening_dist - 0.75) == min(abs(opening_dist - 0.75))) %>%
  slice(1)%>%
  rename(day_exp_p = day_exp)

lb_pheno_c <- left_join(lb, pheno_preds_lb)

#visualize phenology model at LB's sites
pheno_preds_lb %>%
  ggplot(aes(x = day_exp, y = rel_opening)) + geom_line() + facet_grid(yr_exp~ site) + theme_bw()


#compare time series to lb
lb_pheno_c %>% filter(yr_exp == "yr_2009_2010" | yr_exp == "yr_2010_2011") %>%
  ggplot(aes(x = day_exp, y = concentration_m + 1, color = opening_dist)) + geom_line() + theme_bw() + 
  facet_grid(yr_exp~site) +
  #scale_y_log10() + 
  scale_color_viridis_c() +
  geom_point(data = lb_nearest_25, aes(x= day_exp, y = concentration_m + 1 ), col = "red") +
  geom_point(data = lb_nearest_50, aes(x= day_exp, y = concentration_m + 1 ), col = "red") +
  geom_point(data = lb_nearest_75, aes(x= day_exp, y = concentration_m + 1 ), col = "red") +
  geom_point(data = nearest_25_pheno_lb, aes(x= day_exp_p, y = 1 ), col = "blue") +
  geom_point(data = nearest_50_pheno_lb, aes(x= day_exp_p, y = 1 ), col = "blue") +
  geom_point(data = nearest_75_pheno_lb, aes(x= day_exp_p, y = 1 ), col = "blue") +
  geom_point(data = lb_peak, aes(x= day_exp, y = concentration_m_7d ), col = "black") 

#put both time series on the same fig
pheno_preds_lb %>%
  ggplot(aes(x = day_exp, y = rel_opening)) + geom_line() + facet_grid(site ~ yr_exp) + theme_bw() +
  geom_point(data = lb, aes(x = day_exp, y = concentration/50000), alpha = 0.5) + 
  geom_line(data = lb, aes(x = day_exp, y = concentration/50000), alpha = 0.5) +
  xlab("day") + ylab("scaled pollen release and scaled airborne pollen")



#compare season mid-points directly
lb_nearest_50_join <- lb_nearest_50 %>% dplyr::select(day_exp,  site, yr_exp, concentration_m_7d)
nearest_50_pheno_lb_join <- nearest_50_pheno_lb %>% dplyr::select(site, yr_exp, day_exp_p, rel_opening)
nearest_50_lb_pheno <- left_join(lb_nearest_50_join, nearest_50_pheno_lb_join)

nearest_50_lb_pheno %>%
  #filter(yr_exp != "yr_2015_2016") %>%
  ggplot(aes(x = day_exp_p, y = day_exp)) + geom_point(aes(color = site, shape = yr_exp)) + theme_bw() + 
  geom_abline(slope = 1, intercept = 0, lty =2) + geom_smooth(method = "lm", se = FALSE) + 
  xlab("season midpoint predicted by phenological model (day)") +  ylab("observed peak in airborne pollen (day)")
season_midpoint_fit <- lm(day_exp ~ day_exp_p, data = nearest_50_lb_pheno)
summary(season_midpoint_fit)
mean(abs(season_midpoint_fit$residuals))
mean(abs(nearest_50_lb_pheno$day_exp - nearest_50_lb_pheno$day_exp_p), na.rm = TRUE)

#compare day of highest pollen measurement with midpoint of sac opening
nearest_50_lb_pheno <- left_join(lb_peak, nearest_50_pheno_lb_join)
nearest_50_lb_pheno %>%
  #filter(yr_exp != "yr_2015_2016") %>%
  ggplot(aes(x = day_exp_p, y = day_exp, color = site)) + geom_point() + theme_bw() + 
  geom_abline(slope = 1, intercept = 0, lty =2)
summary(lm(day_exp ~ day_exp_p, data = nearest_50_nab_pheno))


