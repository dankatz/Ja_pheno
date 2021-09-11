library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(forcats)

#load in the idealized cone opening curve (from 'ja_ecolab_Gompertz_jags.R')
idealized_cone_opening_curve <- read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/idealized_cone_opening_curve_210910.csv")

#load in the observations
day_start <- mdy("12-10-2020")
p <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") %>% 
  mutate(sample_hours = hour(sample_datetime),
         prop_open = perc_open/100) %>% 
  filter(!is.na(pollen_rel))
  # dplyr::select(site_name, sample_datetime, date, x, y, bag_mean, perc_open) %>% 
  # mutate(prop_open = perc_open/100,
  #        date3 = sample_date,
  #        site = site_name, #paste(round(x, 1), round(y, 1)),
  #        doy = yday(date3),
  #        tree = paste(round(x, 5), round(y, 5)))


#use the idealized cone curve look-up table to estimate where each tree is in its curve
day_before_peak_v <- rep(x = NA, times = nrow(p))
sac_opening_day_v <- rep(x = NA, times = nrow(p))
  
for(i in 1:nrow(p)){
  perc_open_i <- p$prop_open[i]
  row_i <- which(abs(idealized_cone_opening_curve$yhat_median - perc_open_i) == 
             min(abs(idealized_cone_opening_curve$yhat_median - perc_open_i)))
  day_before_peak_v[i] <- idealized_cone_opening_curve$day_before_peak[row_i]
  sac_opening_day_v[i] <- idealized_cone_opening_curve$sac_opening_day[row_i]
}

p <- p %>% 
  mutate(day_from_peak = day_before_peak_v,
         sac_opening_day = sac_opening_day_v)

# SHOULD WORK MORE ON THE SAC MODEL, BECAUSE THAT IDEALIZED SAC CURVE IS SUSPECT
# #use the idealized sac curve look-up table to estimate where each tree is in its curve
# day_before_peak_v <- rep(x = NA, times = nrow(p))
# sac_opening_day_v <- rep(x = NA, times = nrow(p))
# 
# for(i in 1:nrow(p)){
#   perc_open_i <- round(p$bag_mean, 3)[i]
#   row_i <- which(abs(idealized_cone_opening_curve$yhat_median - perc_open_i) == 
#                    min(abs(idealized_cone_opening_curve$yhat_median - perc_open_i)))
#   day_before_peak_v[i] <- idealized_cone_opening_curve$day_before_peak[row_i]
#   sac_opening_day_v[i] <- idealized_cone_opening_curve$sac_opening_day[row_i]
# }
# 
# p <- p %>% 
#   mutate(day_from_peak = day_before_peak_v,
#          sac_opening_day = sac_opening_day_v)

### adding in environmental data ###########################################

vpd <- read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/DAYMET_vp_2018_2020_download.csv",
                    na = "No data") %>%  #names(vpd_raw) #str(vpd_raw)
  pivot_longer(cols = contains("00:00")) %>% 
  rename(site_coords = .geo) %>% 
  mutate(site_coords = substr(site_coords, 32, 71),
         site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
  separate(site_coords, sep = ",", c("long", "lat"))  %>%  #unique(test$lat)
  mutate(lat = round(as.numeric(lat), 1), 
         long = round(as.numeric(long), 1),
         site = paste(long, lat)) %>% 
  rename(vpd = value) %>% 
  mutate(vpd_date = lubridate::ymd_hms(name),
         d2 = case_when(years == "19-20" ~ d + day_start_1920,
                        years == "20-21" ~ d + day_start_2021)) %>% 
  dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
  dplyr::select(-c(name)) %>% 
  dplyr::select(site_name, sample_date = vpd_date, vpd)


vs <- read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/GRIDMET_vs_2018_2021_download.csv",
                na = "No data") %>%  #names(vpd_raw) #str(vpd_raw)
  pivot_longer(cols = contains("00:00")) %>% 
  rename(site_coords = .geo) %>% 
  mutate(site_coords = substr(site_coords, 32, 71),
         site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
  separate(site_coords, sep = ",", c("long", "lat"))  %>%  #unique(test$lat)
  mutate(lat = round(as.numeric(lat), 1), 
         long = round(as.numeric(long), 1),
         site = paste(long, lat)) %>% 
  rename(vs = value) %>% 
  mutate(vs_date = lubridate::ymd_hms(name),
         vs_date = date(vs_date),
         d2 = case_when(years == "19-20" ~ d + day_start_1920,
                        years == "20-21" ~ d + day_start_2021)) %>% 
  dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
  dplyr::select(-c(name)) %>% 
  dplyr::select(site_name, sample_date = vs_date, vs)


### data exploration #######################################################
p2 <- p %>% mutate(pollen_lots = case_when(pollen_rel == "lots" ~ 1, TRUE ~ 0),
                  pollen_rel = fct_relevel(pollen_rel, "none", "little", "some","lots"))

p2 <- left_join(p2, vpd)
p2 <- left_join(p2, vs)

unique(vpd$site_name)
#
p2 %>% group_by(pollen_rel) %>% 
  summarize(sac_opening_mean = mean(sac_opening_day)) %>% 
ggplot(aes(x = pollen_rel, y = sac_opening_mean)) + geom_bar(stat = "identity") + theme_bw() +
  xlab("pollen release category") + ylab("sacs opening that day (proportion)")

# by hour of the day
p2 %>% group_by(sample_hours) %>% 
  summarize(pollen_lots_mean = mean(pollen_lots),
            n = n()) %>% 
  ggplot(aes(x = sample_hours, y = pollen_lots_mean * 100)) + geom_bar(stat = "identity") + theme_bw() +
  geom_text(aes(x = sample_hours, y = -2, label = n))+
  xlab("time of day (hour)") + ylab("observations with high pollen release (%)")

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

#by vpd
p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = vpd, y = pollen_lots, col = sac_opening_day * 100)) + geom_jitter(height = 0.05, width = 5) + theme_bw() +
  xlab("vapor pressure deficit (kPa)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

#by vs
p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = vs, y = pollen_lots, col = sac_opening_day * 100)) + geom_jitter(height = 0.05, width = .5) + theme_bw() +
  xlab("wind speed (m/s)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")


fit <- glm(pollen_lots ~ vs + sac_opening_day + vpd + sample_hours, data = p2, family = "binomial")
summary(fit)
