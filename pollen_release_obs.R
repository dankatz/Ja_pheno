library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(forcats)
library(slider)
library(purrr)
library(here)

setwd("C:/Users/dsk273/Box")
#setwd("C:/Users/danka/Box")
here::i_am("katz_photo.jpg")

day_start_1920 <- mdy("12-10-2019")
day_start_2021 <- mdy("12-10-2020")


### load in the observations (which now include a filled out date x tree grid and predicted sac open on focal day)#########################
p_core <- read_csv(here("texas", "pheno",  "p_core_sites_Y_sim_220926.csv")) %>% 
  mutate(sample_datetime = ymd_hms(sample_datetime, tz = "US/Central"),
         sample_hours = hour(sample_datetime),
         sample_date = date(sample_datetime),
         prop_open = perc_open/100) # %>%  filter(!is.na(pollen_rel))

p_snap <- read_csv(here("texas", "pheno",  "p_snap_sites_Y_sim_220926.csv")) %>% 
  mutate(sample_datetime = ymd_hms(sample_datetime, tz = "US/Central"),
         sample_hours = hour(sample_datetime),
         sample_date = date(sample_datetime),
         prop_open = perc_open/100) # %>%  filter(!is.na(pollen_rel))

p <- bind_rows(p_core, p_snap) %>% 
  mutate(y_hat_release_mean = (lead(Y_hat_mean) - lag(Y_hat_mean))/2,
         y_hat_release_mean = case_when(y_hat_release_mean < 0 ~ 0, TRUE ~ y_hat_release_mean),
         tree_id2 = paste0(site_n_core, site_n_snap, tree_n_core, tree_n_snap),
         date4 = day_experiment + day_start_2021)

#filling in the variables that are not linked to time: tree coordinates
tree_id_vars <- p %>% dplyr::select(tree_n_core, tree_n_snap, site_n_core, tree_n_snap, tree, site_name, site_type) %>% distinct() %>% filter(!is.na(tree) ) %>% 
              rename(tree_id3 = tree, site_name2 = site_name, site_type2 = site_type)

p <- left_join(p, tree_id_vars) %>% filter(!is.na(tree_id3)) %>% 
      mutate(tree_xy = tree_id3, site_name = site_name2, site = site_name2, site_type = site_type2)

ggplot(p, aes(x = date4, y = y_hat_release_mean, group = tree_id2)) + geom_point() + geom_line() + facet_wrap(~site_n_snap) + theme_bw()
ggplot(p, aes(x = y_hat_release_mean, y = pollen_rel)) + geom_boxplot() 


# ############################################
# 
# #load in the idealized cone opening curve (from 'ja_ecolab_Gompertz_jags.R')
# idealized_cone_opening_curve <- read_csv(here("texas", "pheno", "manual_obs", "idealized_cone_opening_curve_210910.csv"))
# 
# #load in the observations
# 
# day_start <- mdy("12-10-2020")
# p <- read_csv(here("texas", "pheno", "manual_obs", "pheno_fs20_21_database_210402.csv")) %>% 
#   mutate(sample_datetime = ymd_hms(sample_datetime, tz = "US/Central"),
#          sample_hours = hour(sample_datetime),
#          sample_date = date(sample_datetime),
#          prop_open = perc_open/100) %>% 
#   filter(!is.na(pollen_rel))
#   # dplyr::select(site_name, sample_datetime, date, x, y, bag_mean, perc_open) %>% 
#   # mutate(prop_open = perc_open/100,
#   #        date3 = sample_date,
#   #        site = site_name, #paste(round(x, 1), round(y, 1)),
#   #        doy = yday(date3),
#   #        tree = paste(round(x, 5), round(y, 5)))
# 
# 
# #use the idealized cone curve look-up table to estimate where each tree is in its curve
# day_before_peak_v <- rep(x = NA, times = nrow(p))
# sac_opening_day_v <- rep(x = NA, times = nrow(p))
#   
# for(i in 1:nrow(p)){
#   perc_open_i <- p$prop_open[i]
#   row_i <- which(abs(idealized_cone_opening_curve$yhat_median - perc_open_i) == 
#              min(abs(idealized_cone_opening_curve$yhat_median - perc_open_i)))
#   day_before_peak_v[i] <- idealized_cone_opening_curve$day_before_peak[row_i]
#   sac_opening_day_v[i] <- idealized_cone_opening_curve$sac_opening_day[row_i]
# }
# 
# p <- p %>% 
#   mutate(day_from_peak = day_before_peak_v,
#          sac_opening_day = sac_opening_day_v)
# 
# # SHOULD WORK MORE ON THE SAC MODEL, BECAUSE THAT IDEALIZED SAC CURVE IS SUSPECT
# # #use the idealized sac curve look-up table to estimate where each tree is in its curve
# # day_before_peak_v <- rep(x = NA, times = nrow(p))
# # sac_opening_day_v <- rep(x = NA, times = nrow(p))
# # 
# # for(i in 1:nrow(p)){
# #   perc_open_i <- round(p$bag_mean, 3)[i]
# #   row_i <- which(abs(idealized_cone_opening_curve$yhat_median - perc_open_i) == 
# #                    min(abs(idealized_cone_opening_curve$yhat_median - perc_open_i)))
# #   day_before_peak_v[i] <- idealized_cone_opening_curve$day_before_peak[row_i]
# #   sac_opening_day_v[i] <- idealized_cone_opening_curve$sac_opening_day[row_i]
# # }
# # 
# # p <- p %>% 
# #   mutate(day_from_peak = day_before_peak_v,
# #          sac_opening_day = sac_opening_day_v)
# 
# ggplot(p, aes(x = sac_opening_day_v, y = pollen_rel)) + geom_boxplot() 

### adding in environmental data: daily ###########################################

daily_files <- list.files(path='C:/Users/danka/Box/texas/pheno/met_data/GEE_pheno_site_downloads/', pattern='GRIDMET', 
                           full.names = TRUE)
daily_files <- daily_files[!grepl("drought", daily_files)] #remove the drought files which have a different format
#if(grepl("drought", daily_files[1]) == TRUE){hourly_files_names <- substr(daily_file_name, 82, 87)} #for drought files

#function to read in the daily data (GRIDMET for now)
daily_read_fun <- function(daily_file_name){
  hourly_files_names <- substr(daily_file_name, 74, 78)  #daily_file_name <- daily_files[2]
  hourly_files_names <- gsub('[[:digit:][:punct:]]+', '', hourly_files_names) #remove all digits and punctuation
output_df <- read_csv(daily_file_name, na = "No data") %>%  
  pivot_longer(cols = contains("00:00")) %>% 
  rename(site_coords = .geo) %>% 
  mutate(site_coords = substr(site_coords, 32, 71),
         site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
  separate(site_coords, sep = ",", c("long", "lat"))  %>%  #unique(test$lat)
  mutate(lat = round(as.numeric(lat), 1), 
         long = round(as.numeric(long), 1),
         site = paste(long, lat)) %>% 
  mutate(sample_date = lubridate::ymd_hms(name),
         d2 = case_when(years == "19-20" ~ d + day_start_1920,
                        years == "20-21" ~ d + day_start_2021)) %>% 
  dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
  dplyr::select(-c(name)) %>% 
  dplyr::select(site_name, sample_date, value) %>% 
  rename({{hourly_files_names}} :=  value) #using the glue syntax
return(output_df)
}

GRIDMET <- purrr::map_dfc(daily_files, daily_read_fun)
GRIDMET <- dplyr::select(GRIDMET, site_name = site_name...1, sample_date = sample_date...2, !contains("..."))


daily_files

summary(GRIDMET)
filter(GRIDMET, sample_date > ymd_hms("2020-12-15 00:00:00") &
                sample_date < ymd_hms("2021-02-22 00:00:00")) %>% 
  ggplot(aes(x = sample_date, y = (tmmx - 273.15) * 9/5 + 32, color = vpd, group = site_name)) + geom_line()+ theme_bw() + scale_color_viridis_c()
#+   theme(legend.position = "none") 

#assuming NA values for precip are no precip
GRIDMET <- mutate(GRIDMET, pr = case_when(is.na(pr) ~ 0, TRUE ~ pr)) #summary(GRIDMET2)

#changing sample date to just be date, not date time
GRIDMET <- mutate(GRIDMET, sample_date = date(sample_date)) #summary(GRIDMET2)

#changing site names to play nice with p3
GRIDMET <- mutate(GRIDMET, site_name = tolower(site_name)) #
unique(GRIDMET$site_name) %in% unique(p$site_name)
sort(unique(GRIDMET$site_name))
sort(unique(p$site_name))

#interpolating missing values (with a maximum gap of 3 days)
summary(GRIDMET) 
GRIDMET <- group_by(GRIDMET, site_name)
GRIDMET$th <- imputeTS::na_interpolation(x = GRIDMET$th, option = "linear", maxgap = 3) #doesn't work to do this within mutate
GRIDMET$vpd <- imputeTS::na_interpolation(x = GRIDMET$vpd, option = "linear", maxgap = 3) #doesn't work to do this within mutate
GRIDMET$pr <- imputeTS::na_interpolation(x = GRIDMET$pr, option = "linear", maxgap = 3) #doesn't work to do this within mutate
GRIDMET$rmax <- imputeTS::na_interpolation(x = GRIDMET$rmax, option = "linear", maxgap = 3) #doesn't work to do this within mutate
GRIDMET$rmin <- imputeTS::na_interpolation(x = GRIDMET$rmin, option = "linear", maxgap = 3) #doesn't work to do this within mutate
GRIDMET$srad <- imputeTS::na_interpolation(x = GRIDMET$srad, option = "linear", maxgap = 3) #doesn't work to do this within mutate
summary(GRIDMET)

# #creating some derived variables: Average over last 3/7/14 days
# mean_3day_fun <- function(x){slide_dbl(x, function(y){mean(y, na.rm = TRUE)}, .before = 2, .after = 0)}
# mean_7day_fun <- function(x){slide_dbl(x, function(y){mean(y, na.rm = TRUE)}, .before = 6, .after = 0)}
# mean_14day_fun <- function(x){slide_dbl(x, function(y){mean(y, na.rm = TRUE)}, .before = 13, .after = 0)}
# 
# GRIDMET_mean_3day <- 
#   purrr::map_dfc(GRIDMET[, 3:7], mean_3day_fun) %>% #apply the mean_3day_fun to all data columns
#   set_names(paste0(names(GRIDMET[, 3:7]), "_mean_3day")) #create new names
# 
# GRIDMET_mean_7day <- 
#   purrr::map_dfc(GRIDMET[, 3:7], mean_7day_fun) %>% 
#   set_names(paste0(names(GRIDMET[, 3:7]), "_mean_7day")) 
# 
# GRIDMET_mean_14day <- 
#   purrr::map_dfc(GRIDMET[, 3:7], mean_14day_fun) %>% 
#   set_names(paste0(names(GRIDMET[, 3:7]), "_mean_14day")) 
# 
# 
# GRIDMET2 <- bind_cols(GRIDMET, GRIDMET_mean_3day, GRIDMET_mean_7day, GRIDMET_mean_14day) %>% 
#   mutate(sample_date = date(sample_date),
#          pr_dif_1 = pr - lag(pr, 1),
#          pr_dif_mean3 = pr - pr_mean_3day,
#          pr_dif_mean7 = pr - pr_mean_7day,
#          rmi_dif_1 = rmi - lag(rmi, 1),
#          rmi_dif_mean3 = rmi - rmi_mean_3day,
#          rmi_dif_mean7 = rmi - rmi_mean_7day,
#          th_dif_1 = th - lag(th, 1),
#          th_dif_mean3 = th - th_mean_3day,
#          th_dif_mean7 = th - th_mean_7day,
#          vpd_dif_1 = vpd - lag(vpd, 1),
#          vpd_dif_mean3 = vpd - vpd_mean_3day,
#          vpd_dif_mean7 = vpd - vpd_mean_7day,
#          vs_dif_1 = vs - lag(vs, 1),
#          vs_dif_mean3 = vs - vs_mean_3day,
#          vs_dif_mean7 = vs - vs_mean_7day
#          )


# GRIDMET %>% 
#   filter()
#   ggplot(aes(x = ))

### adding in environmental data: hourly ###########################################
hourly_files <- list.files(path='C:/Users/danka/Box/texas/pheno/met_data/GEE_pheno_site_downloads/', pattern='RTMA', 
                           full.names = TRUE)

hourly_read_fun <- function(hourly_files){
  hourly_files_names <- substr(hourly_files, 71, 74) #need to switch here if going back and forth between lab computer and personal laptop
  #hourly_files_names_focal2 <- hourly_files_names_focal
  env_var_hourly <- read_csv(hourly_files,
                             na = "No data") %>%  #names(vpd_raw) #str(vpd_raw)
    pivot_longer(cols = contains("00:00")) %>% 
    rename(site_coords = .geo) %>% 
    mutate(site_coords = substr(site_coords, 32, 71),
           site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
    separate(site_coords, sep = ",", c("long", "lat"))  %>%  #unique(test$lat)
    mutate(lat = round(as.numeric(lat), 1), 
           long = round(as.numeric(long), 1),
           site = paste(long, lat)) %>% 
    mutate(focal_date = lubridate::ymd_hms(name, tz = "UTC"), #note: native time zone = UTC NOT local time
           focal_date = with_tz(focal_date, "US/Central"), #change to local time zone
           d2 = case_when(years == "19-20" ~ d + day_start_1920,
                          years == "20-21" ~ d + day_start_2021)) %>% 
    dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
    dplyr::select(-c(name)) %>% 
    dplyr::select(site_name, focal_date, value) %>% 
    rename({{hourly_files_names}} :=  value) #using the glue syntax
          #https://stackoverflow.com/questions/49650394/how-to-rename-a-variable-using-a-dynamic-name-and-dplyr
  return(env_var_hourly)
}

#getting each study year separately, as that's how I downloaded the data from GEE
RTMA_2019 <- purrr::map_dfc(hourly_files[grep(pattern = "2019", x = hourly_files)], hourly_read_fun)
RTMA_2019b <- dplyr::select(RTMA_2019, site_name = site_name...1, focal_date = focal_date...2, !contains("..."))


RTMA_2020 <- purrr::map_dfc(hourly_files[grep(pattern = "2021", x = hourly_files)], hourly_read_fun)
RTMA_2020b <- dplyr::select(RTMA_2020, site_name = site_name...1, focal_date = focal_date...2, !contains("..."))

RTMA <- bind_rows(RTMA_2019b, RTMA_2020b) %>% 
  rename(sample_datetime_rounded = focal_date,
         TMP = TMP_)
         # GUST = UST_,
         # wind = ind_,
         # TCDC = CDC_,
         # TMP = MP_2,
         # PRES = RES_,
         # SPFH = PFH_)

RTMA$sample_datetime_rounded
summary(RTMA)

RTMA %>% filter( sample_datetime_rounded > ymd_hms("2020-12-15 00:00:00") &
             sample_datetime_rounded < ymd_hms("2020-12-18 00:00:00")) %>% 
ggplot(aes(x = sample_datetime_rounded, y = TMP, color = site_name)) + geom_line()+ theme_bw() + 
  theme(legend.position = "none")

#interpolating missing values (with a maximum gap of 3 hours)
summary(RTMA) 
RTMA <- group_by(RTMA, site_name)
RTMA$GUST <- imputeTS::na_interpolation(x = RTMA$GUST, option = "linear", maxgap = 3) #doesn't work to do this within mutate
RTMA$TCDC <- imputeTS::na_interpolation(x = RTMA$TCDC, option = "linear", maxgap = 3) #doesn't work to do this within mutate
RTMA$wind <- imputeTS::na_interpolation(x = RTMA$wind, option = "linear", maxgap = 3) #doesn't work to do this within mutate
summary(RTMA)

#creating some derived variables: Average over last 3/6/12 hours
mean_3hr_fun <- function(x){slide_dbl(x, function(y){mean(y, na.rm = TRUE)}, .before = 2, .after = 0)}
mean_6hr_fun <- function(x){slide_dbl(x, function(y){mean(y, na.rm = TRUE)}, .before = 5, .after = 0)}
mean_12hr_fun <- function(x){slide_dbl(x, function(y){mean(y, na.rm = TRUE)}, .before = 11, .after = 0)}
mean_24hr_fun <- function(x){slide_dbl(x, function(y){mean(y, na.rm = TRUE)}, .before = 23, .after = 0)}

RTMA_mean_3hr <- 
  purrr::map_dfc(RTMA[, 3:8], mean_3hr_fun) %>% #apply the mean_3hr_fun to all data columns
  set_names(paste0(names(RTMA[, 3:8]), "_mean_3hr")) #create new names

RTMA_mean_6hr <- 
  purrr::map_dfc(RTMA[, 3:8], mean_6hr_fun) %>% 
  set_names(paste0(names(RTMA[, 3:8]), "_mean_6hr")) 

RTMA_mean_12hr <- 
  purrr::map_dfc(RTMA[, 3:8], mean_12hr_fun) %>% 
  set_names(paste0(names(RTMA[, 3:8]), "_mean_12hr")) 

RTMA_mean_24hr <- 
  purrr::map_dfc(RTMA[, 3:8], mean_24hr_fun) %>% 
  set_names(paste0(names(RTMA[, 3:8]), "_mean_24hr")) 

RTMA2 <- bind_cols(RTMA, RTMA_mean_3hr, RTMA_mean_6hr, RTMA_mean_12hr, RTMA_mean_24hr) %>% 
        mutate(GUST_dif = GUST_mean_3hr - GUST_mean_12hr,
               TCDC_dif = TCDC_mean_3hr - TCDC_mean_12hr,
               wind_dif = wind_mean_3hr - wind_mean_12hr,
               PRES_dif = PRES_mean_3hr - PRES_mean_12hr,
               SPFH_dif = SPFH_mean_3hr - SPFH_mean_12hr,
               TMP_dif = TMP_mean_3hr - TMP_mean_12hr)


filter(RTMA2, sample_datetime_rounded > ymd_hms("2020-12-15 00:00:00") &
         sample_datetime_rounded < ymd_hms("2021-01-15 00:00:00")) %>% 
  ggplot(aes(x = sample_datetime_rounded, y = TMP_mean_3hr, color = site_name)) + geom_line()+ theme_bw() + 
  theme(legend.position = "none") + xlab("date") + ylab("temperature (C)")

# GGally::ggcorr(RTMA_mean_24hr[1:6], label = TRUE)

### add in time since sunrise and angle of sun  ##############################
library(suncalc)
#for manual observation times
sun_df <- data.frame(date = p$sample_date, lat = p$y, lon = p$x)
p_sun <- getSunlightTimes(data = sun_df, keep = c("sunrise", "solarNoon"), tz = "Etc/GMT+6") #I thought it should have been GMT+6, but this is what works

# #for full  time series
# sun_df_full <- data.frame(date = GRIDMET$sample_date, lat = GRIDMET$y, lon = GRIDMET$x)
# p_sun <- getSunlightTimes(data = sun_df, keep = c("sunrise", "solarNoon"), tz = "Etc/GMT+6") #I thought it should have been GMT+6, but this is what works


### data exploration #######################################################
p2 <- p %>% mutate(pollen_lots = case_when(pollen_rel == "lots" ~ 1, TRUE ~ 0),
                  pollen_rel = fct_relevel(pollen_rel, "none", "little", "some","lots"),
                  sample_datetime_rounded = round_date(sample_datetime, unit = "hour")) %>% 
  mutate(sunrise = p_sun$sunrise, solarNoon = p_sun$solarNoon,
         time_after_sunrise = round(as.numeric(time_length(sample_datetime - sunrise, unit = "hours")), 2),
         time_after_noon =   round(as.numeric(time_length(sample_datetime - solarNoon, unit = "hours")), 2))   #in hours




p2 <- left_join(p2, GRIDMET)
p2 <- left_join(p2, (RTMA2))

p2 %>% group_by(pollen_rel) %>% 
  summarize(sac_opening_mean = mean(y_hat_release_mean)) %>% 
ggplot(aes(x = pollen_rel, y = sac_opening_mean)) + geom_bar(stat = "identity") + theme_bw() +
  xlab("pollen release category") + ylab("sacs opening that day (proportion)")

# by hour of the day
p2 %>% group_by(sample_hours) %>% 
  summarize(pollen_lots_mean = mean(pollen_lots),
            n = n()) %>% 
  filter(sample_hours > 8 & sample_hours < 19) %>% 
  ggplot(aes(x = sample_hours, y = pollen_lots_mean * 100)) + geom_bar(stat = "identity") + theme_bw() +
  geom_text(aes(x = sample_hours, y = -2, label = n))+
  xlab("time of day (hour)") + ylab("observations with high pollen release (%)")

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

### by solar 
p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = time_after_sunrise, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .1) + theme_bw() +
  xlab("time after sunrise (hr)") + ylab("observations with high pollen release (%)") +
  scale_color_viridis_c(name = "sacs opening that day (%)") + geom_smooth(method = "loess")

p2 %>% arrange(time_after_noon) %>%  #group_by(sample_hours) %>% 
  ggplot(aes(x = time_after_noon, y = pollen_lots)) + geom_jitter(height = 0.05, width = .1) + theme_bw() +
  xlab("time after solar noon (hr)") + ylab("observations with high pollen release (%)") +
  geom_line(aes(y=zoo::rollmean(pollen_lots, 100, na.pad=TRUE))) 


### by GRIDMET vars
p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = pr_mean_7day, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .1) + theme_bw() +
  xlab("precipitation (mm)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = vpd, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .001) + theme_bw() +
  xlab("vapor pressure deficit (kPa)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = rmax, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .001) + theme_bw() +
  xlab("minimum relative humidity (%)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = th, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .001) + theme_bw() +
  xlab("wind direction") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

#by vs
p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = vs, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .5) + theme_bw() +
  xlab("wind speed (m/s)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

### by RTMA vars
p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = wind_dif, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .1) + theme_bw() +
  xlab("wind speed (m/s)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = GUST_mean_3hr, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .5) + theme_bw() +
  xlab("gust wind speed (m/s)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = PRES_dif, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .5) + theme_bw() +
  xlab("change in pressure (Pa)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = SPFH, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .001) + theme_bw() +
  xlab("specific humidity (kg/kg)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = TCDC_mean_3hr, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .5) + theme_bw() +
  xlab("total cloud cover (%)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")

p2 %>% #group_by(sample_hours) %>% 
  ggplot(aes(x = TMP_dif, y = pollen_lots, col = y_hat_release_mean * 100)) + geom_jitter(height = 0.05, width = .5) + theme_bw() +
  xlab("change in temperature (C)") + ylab("observations with high pollen release (%)") +
  binomial_smooth(se = FALSE) + scale_color_viridis_c(name = "sacs opening that day (%)")


summary(p2)
fit <- glm(pollen_lots ~ y_hat_release_mean + sample_hours + time_after_noon + # vpd 
             #pr + rmi + th + vpd + vs +
             wind_mean_12hr + PRES_dif +  TMP_mean_24hr, # + TCDC_mean_3hr, #TCDC + SPFH: no effect
            
             data = p2, family = "binomial")
summary(fit)

hist(p2$y_hat_release_mean, breaks = 200)


fit <- glm(pollen_lots ~  PRES_dif, # + TCDC_mean_3hr, #TCDC + SPFH: no effect
           data = p2, family = "binomial")
summary(fit)




### setting up analysis with a distributed lags model using dlnm package #######################################
library(dlnm)
library(splines)
library(MASS)

#make a dataframe that has all of the environmental data
p3 <- p %>% mutate(pollen_lots = case_when(pollen_rel == "lots" ~ 1, 
                                           pollen_rel == "some" ~ 0, 
                                           pollen_rel == "little" ~ 0, 
                                           pollen_rel == "none" ~ 0, 
                                           is.na(pollen_rel) ~ as.numeric(NA)), #need to maintain consistency in output type for case_when
                   pollen_rel = fct_relevel(pollen_rel, "none", "little", "some","lots"),
                   sample_datetime_rounded = round_date(sample_datetime, unit = "hour")) %>% 
      dplyr::select(site_type, site_name, sample_date = date4, sample_datetime, tree_xy, prop_open, y_hat_release_mean, pollen_lots, 
                    sample_datetime_rounded) 
p3 <- mutate(p3, sunrise = p_sun$sunrise, solarNoon = p_sun$solarNoon,
             time_after_sunrise = round(as.numeric(time_length(sample_datetime - sunrise, unit = "hours")), 2),
             time_after_noon =   round(as.numeric(time_length(sample_datetime - solarNoon, unit = "hours")), 2))   #in hours



tree_list <- dplyr::select(p3, site_name, tree_xy) %>% distinct()

GRIDMET_tree <- filter(GRIDMET, sample_date > ymd("2020-11-01") & sample_date < ymd("2021-03-01")) %>% 
                    full_join(., tree_list) %>% 
                    arrange(site_name, tree_xy, sample_date)

data_for_model <- left_join(GRIDMET_tree, p3, by = c("sample_date", "tree_xy")) %>% 
  filter(tree_xy != "NA NA")


## set up dlnm crossbasis object for use in glm
max_lag <- 14
vpd_lag <- crossbasis(data_for_model$vpd, lag = 14, 
                      #argvar=list(fun = "ns"), #"poly", degree = 3), #shape of response curve
                      argvar = list(fun = "lin"), #"poly", degree = 3), #shape of response curve
                      arglag = list(fun = "ns")) #shape of lag
                      #arglag = list(fun = "integer")) #shape of lag 
                      #arglag = list(fun = "poly", degree = 1)) #shape of lag
                      #arglag = list(fun = "lin")) #shape of lag

srad_lag <- crossbasis(data_for_model$srad, lag = 14, 
                      argvar = list(fun = "ns"), #"poly", degree = 3), #shape of response curve
                      arglag = list(fun = "ns")) #shape of lag

rmax_lag <- crossbasis(data_for_model$rmax, lag = 13, 
                       argvar = list(fun = "ns"), #"poly", degree = 3), #shape of response curve
                       arglag = list(fun = "ns")) #shape of lag

tmmx_lag <- crossbasis(data_for_model$tmmx, lag = 15, 
                       argvar = list(fun = "ns"), #"poly", degree = 3), #shape of response curve
                       arglag = list(fun = "ns")) #shape of lag

vs_lag <- crossbasis(data_for_model$vs, lag =20, 
                       argvar = list(fun = "ns"), #"poly", degree = 3), #shape of response curve
                       arglag = list(fun = "ns")) #shape of lag


#binomial glm with included variables
model1 <- glm(pollen_lots ~  #number of cases at a station on an observed day
                y_hat_release_mean + 
                #prop_open + 
                
                vpd_lag +
                vpd +
                srad_lag +
                srad +
                rmax_lag +
                rmax +
                tmmx_lag +
                tmmx +
                vs_lag,
                #vs +
                
                
               # ns(time_after_noon, df = 3) ,
              #  ns(time_after_sunrise, df = 3), 
                
              family = "binomial", 
              #link = "logit", #(link = "logit")(link="logit"),
              data = data_for_model)  

model1
summary(model1) #str(model1)


### visualize effects of vpd
pred1_vpd <- crosspred(vpd_lag,  model1, #at = 1,
                       at = seq(from = min(data_for_model$vpd, na.rm = TRUE), to = max(data_for_model$vpd, na.rm = TRUE), by = 0.10), 
                       bylag = 1, cen = 0, cumul = TRUE) #str(pred1_cup)

vpd_lag_RR <-
  as.data.frame(pred1_vpd$cumfit) %>% mutate(vpd = pred1_vpd$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag))) %>% 
  ggplot(aes(x = vpd, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_bw() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR")    #automatically bins and turns to factor
vpd_lag_RR


### visualize effects of env vars: srad
pred1_srad <- crosspred(srad_lag,  model1, #at = 1,
                       at = seq(from = min(data_for_model$srad, na.rm = TRUE), to = max(data_for_model$srad, na.rm = TRUE), by = 0.10), 
                       bylag = 1, cen = 0, cumul = TRUE) #str(pred1_cup)

srad_lag_RR <-
  as.data.frame(pred1_srad$cumfit) %>% mutate(srad = pred1_srad$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag))) %>% 
  ggplot(aes(x = srad, y = lag, z = RR)) + geom_contour_filled(bins = 5) + theme_bw() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR")    #automatically bins and turns to factor
srad_lag_RR


### visualize effects of rmax
pred1_rmax <- crosspred(rmax_lag,  model1, #at = 1,
                       at = seq(from = min(data_for_model$rmax, na.rm = TRUE), to = max(data_for_model$rmax, na.rm = TRUE), by = 0.10), 
                       bylag = 1, cen = 0, cumul = TRUE) #str(pred1_cup)

rmax_lag_RR <-
  as.data.frame(pred1_rmax$cumfit) %>% mutate(rmax = pred1_rmax$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag))) %>% 
  ggplot(aes(x = rmax, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_bw() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR")    #automatically bins and turns to factor
rmax_lag_RR

### visualize effects of tmmx
pred1_tmmx <- crosspred(tmmx_lag,  model1, #at = 1,
                       at = seq(from = min(data_for_model$tmmx, na.rm = TRUE), to = max(data_for_model$tmmx, na.rm = TRUE), by = 0.10), 
                       bylag = 1, cen = 0, cumul = TRUE) #str(pred1_cup)

tmmx_lag_RR <-
  as.data.frame(pred1_tmmx$cumfit) %>% mutate(tmmx = pred1_tmmx$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag))) %>% 
  ggplot(aes(x = tmmx, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_bw() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR")    #automatically bins and turns to factor

tmmx_lag_RR

### visualize effects of vs
pred1_vs <- crosspred(vs_lag,  model1, #at = 1,
                       at = seq(from = min(data_for_model$vs, na.rm = TRUE), to = max(data_for_model$vs, na.rm = TRUE), by = 0.10), 
                       bylag = 1, cen = 0, cumul = TRUE) #str(pred1_cup)

vs_lag_RR <-
  as.data.frame(pred1_vs$cumfit) %>% mutate(vs = pred1_vs$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag))) %>% 
  ggplot(aes(x = vs, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_bw() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR")    #automatically bins and turns to factor
vs_lag_RR


### exploring predictions #######################
data_for_model_results <- data_for_model %>% 
  mutate(pollen_lots_pred = predict(object = model1, data_for_model, type = "response"))

#test <- predict(object = model1, data_for_model, type = "response")
hist(model1$fitted.values, n = 100)
hist(data_for_model_results$pollen_lots_pred, n = 100)

data_for_model_results %>% #unique(data_for_model_results$site_name.x)
  filter(site_name.x == "san.antonio.hospital" ) %>% 
ggplot(aes(x = sample_date, y = pollen_lots_pred, group = tree_xy)) + geom_line(alpha = 0.2) + theme_bw() +
  scale_x_date(limits = c(mdy("12-1-2020"), mdy("2-1-2021")))
         
#for all the field sites near San Antonio 
#an example for ESA pres 2024
panel_c_SanA <- data_for_model_results %>% 
  mutate(lat = as.numeric(stringr::str_split_i(tree_xy, " ", 2)),
         long = as.numeric(stringr::str_split_i(tree_xy, " ", 1))) %>% 
  #unique(data_for_model_results$site_name.x)
  #filter(site_name.x == "san.antonio.hospital" ) %>% 
  filter(lat < 29.8 & long > -99.2) %>% 
  ggplot(aes(x = sample_date, y = pollen_lots_pred, group = tree_xy)) + geom_line(alpha = 0.2) + theme_bw() +
  scale_x_date(limits = c(mdy("12-17-2020"), mdy("2-1-2021"))) +
  theme(text = element_text(size = 18)) + xlab("date")


pROC::auc(data_for_model_results$pollen_lots, data_for_model_results$pollen_lots_pred)
pROC::plot.roc(data_for_model_results$pollen_lots, data_for_model_results$pollen_lots_pred)
#creating predictions as if time was noon


?predict


# # include the 1-day lagged residual in the model
# #resid_model1 <- c(rep(NA, max_lag), residuals(model1, type = "deviance"))
# resid_model1 <- c(rep(NA, 0), residuals(model1, type = "deviance"))#for the model version when pollen isn't included
# model2 <- update(model1, .~. + tsModel::Lag(resid_model1, 1))  #length(resid_model1) #length(residuals(model1, type = "deviance"))

# hist(model1$fitted.values, n = 100)
# hist(model2$fitted.values, n = 200)
# hist(data_for_model$n_cases, n = 100)


# ### model diagnotistic plots
# #deviance residuals over time
# data_for_model %>%
#   ungroup() %>%
#   mutate(resid = c(rep(NA, 15), residuals(model2, type = "deviance"))) %>%
#   ggplot(aes(x = date, y = resid)) + theme_bw() +
#   geom_point() + facet_wrap(~NAB_station) + ylab("Deviance residuals")
# 
# #partial autocorrelation plots of the deviance residuals
# pacf(residuals(model1, type = "deviance"), na.action=na.omit,main="From original model")
# pacf(residuals(model2, type = "deviance"), na.action=na.omit,main="From model adjusted for residual autocorrelation")
# 
# summary(model1)
# summary(model2)


### investigate residuals and time series from a model without pollen #############################
# some basic exploration of lags and correlations
# data_for_model %>% ungroup() %>%  
#   mutate(cuplag6 = cup_lag[,7]) %>% 
#   group_by(NAB_station) %>% 
#   # dplyr::select(pbir, cuplag6, NAB_station) %>% 
#   # filter(!is.na(cuplag6)) %>% 
#   summarize(correlation = cor(cuplag6, pbir))
# ggplot(aes(x = cuplag6, y = pbir)) + geom_point(alpha = 0.2) + facet_wrap(~NAB_station) + geom_smooth(se  = FALSE) + theme_bw()
# 
# str(cup_lag)
# 

# #data exploration figure for correlations between residuals and pollen
# test <- NA
# test2 <- NA
# for(i in 1:max_lag){
#   test <- data_for_model %>% ungroup() %>%
#     mutate(focal_pol_lag = cup_lag[,i]) %>%
#     mutate(nopol_resid = residuals(model1, type = "deviance")) %>%  #model1
#     #mutate(nopol_resid = c(1, residuals(model2, type = "deviance"))) %>%  # model2 has the lagged residuals included
#     #mutate(months = month(date)) %>% filter(months == 12 | months == 1 | months == 2) %>% #filter(months != 1 & months != 2) %>%
#     #mutate(years = year(date)) %>% filter(years == 2017) %>%
#     group_by(NAB_station) %>%
#     dplyr::select(nopol_resid, focal_pol_lag, NAB_station) %>%
#     filter(!is.na(focal_pol_lag)) %>%
#     summarize(correlation = cor(focal_pol_lag, nopol_resid)) %>%
#     mutate(lag = i - 1)
#   if(i == 1){test2 <- test}
#   if(i > 1){test2 <- bind_rows(test, test2)}  #test2 <- test
# }
# test2 %>% ggplot(aes(x = lag, y = correlation, color = NAB_station)) + geom_line(lwd = 2) + theme_bw() + 
#   ylab("correlation between Cupressaceae pollen and residuals")

#data exploration figure for correlations between residuals and pollen when model is only in Ja season
resid_df <- NA
resid_df2 <- NA

data_for_model2 <- dplyr::select(data_for_model2, date, NAB_station, cup_all_lm, trees_lm)
for(i in 1:max_lag){
  data_for_model3 <- mutate(data_for_model2, focal_pol_lag = lag(cup_all_lm, i))
  resid_df <- left_join(data_for_model, data_for_model3) %>% ungroup() %>%
    mutate(nopol_resid = residuals(model1, type = "deviance")) %>%  #model1
    #mutate(nopol_resid = c(1, residuals(model2, type = "deviance"))) %>%  # model2 has the lagged residuals included
    #mutate(months = month(date)) %>% filter(months == 12 | months == 1 | months == 2) %>% #filter(months != 1 & months != 2) %>%
    #mutate(years = year(date)) %>% filter(years == 2017) %>%
    group_by(NAB_station) %>%
    dplyr::select(nopol_resid, focal_pol_lag, NAB_station) %>%
    filter(!is.na(focal_pol_lag)) %>%
    summarize(correlation = cor(focal_pol_lag, nopol_resid)) %>%
    mutate(lag = i - 1)
  if(i == 1){resid_df2 <- resid_df}
  if(i > 1){resid_df2 <- bind_rows(resid_df, resid_df2)}  #test2 <- test
}
resid_df2 %>% ggplot(aes(x = lag, y = correlation, color = NAB_station)) + geom_line(lwd = 2) + theme_bw() + 
  ylab("correlation between Cupressaceae pollen and residuals")

#residuals vs pollen
resid_df <- NA
resid_df2 <- NA

for(i in 1:max_lag){
  data_for_model3 <- mutate(data_for_model2, focal_pol_lag = lag(cup_all_lm, i))
  resid_df <- left_join(data_for_model, data_for_model3) %>% ungroup() %>%
    mutate(nopol_resid = residuals(model1, type = "deviance")) %>%  #model1
    group_by(NAB_station) %>%
    filter(!is.na(focal_pol_lag))
  if(i == 1){resid_df2 <- resid_df}
  if(i > 1){resid_df2 <- bind_rows(resid_df, resid_df2)}  #test2 <- test
}
resid_df2_ts <- resid_df2 %>% 
  mutate(doy = yday(date),
         doy2 = case_when(doy > 300 ~ doy -365,
                          doy < 301 ~ doy )) %>% 
  filter(doy2 < 60)  
resid_df2_ts %>% ggplot(aes(x = doy2, y = nopol_resid)) + geom_point(alpha = 0.002) + theme_bw() + 
  geom_line(aes(y=rollmean(nopol_resid , 7, na.pad=TRUE)), lwd = 1) +
  ylab("correlation between Cupressaceae pollen and residuals") + xlab("Julian day") + facet_grid(season~NAB_station) + 
  geom_line(aes(y=rollmean(cup_all_lm, 7, na.pad=TRUE)), color = "red") +
  geom_line(aes(y=rollmean(trees_lm, 7, na.pad=TRUE)), color = "blue")






#data exploration figure for pollen vs residuals 
test <- NA
test2 <- NA
for(i in 1:max_lag){
  test <- data_for_model %>% ungroup() %>%
    mutate(focal_pol_lag = cup_lag[,i]) %>%
    mutate(nopol_resid = residuals(model1, type = "deviance")) %>%  #model1
    #mutate(nopol_resid = c(1, residuals(model2, type = "deviance"))) %>%  # model2 has the lagged residuals included
    mutate(months = month(date)) %>% #filter(months == 12 | months == 1 | months == 2) %>% #filter(months != 1 & months != 2) %>%
    mutate(years = year(date)) %>% #filter(years == 2017) %>%
    group_by(NAB_station) %>%
    dplyr::select(nopol_resid, focal_pol_lag, NAB_station, months, years) %>%
    filter(!is.na(focal_pol_lag)) %>%
    #summarize(correlation = cor(focal_pol_lag, nopol_resid)) %>%
    mutate(lag = i - 1)
  if(i == 1){test2 <- test}
  if(i > 1){test2 <- bind_rows(test, test2)}  #test2 <- test
}

#


resid_df2 %>% 
  filter(!is.na(nopol_resid)) %>% 
  filter(!is.na(focal_pol_lag)) %>% 
  filter(lag == 28) %>% 
  ggplot(aes(x = focal_pol_lag, y = nopol_resid,  color = months)) + geom_point() + theme_bw() + 
  ylab("residual") + xlab("pollen concentration")+ facet_wrap(~NAB_station) + geom_smooth(method = "lm", se = FALSE)




resid_explor <- bind_cols(data_for_model, resid = c(rep(NA, 0), residuals(model1, type = "deviance")))#for the model version when pollen isn't included
resid_explor <- bind_cols(data_for_model, resid = c(rep(NA, max_lag), residuals(model1, type = "deviance")))# with pollen
resid_explor %>% #str(resid_explor)
  ungroup() %>% 
  mutate(doy = yday(date),
         syear = year(date)) %>%
  mutate(cuplag6 = cup_lag[,1]) %>% 
  ##filter(NAB_station == "San Antonio A") %>%
  #filter(date > ymd("2017-02-01") & date < ymd("2017-05-15")) %>%
  
  ggplot(aes(x = date, y = resid , col = resid)) +  theme_bw() +
  scale_x_date(breaks = pretty(resid_explor$date, n = 22)) + scale_color_viridis_c() +
  scale_y_continuous("residuals", sec.axis = sec_axis(~ ., name = "Cup pollen")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.y.right = element_text(color = "red")) +
  facet_wrap(~NAB_station)+
  geom_point(col = "black", alpha = 0.1) +
  geom_point(aes(x = date, y = cuplag6), col = "red", alpha = 0.1)+
  geom_line(aes(y=rollmean(resid , 7, na.pad=TRUE)), alpha = 0.9, col = "black") +
  # geom_line(aes(y=rollmean(resid , 14, na.pad=TRUE)), alpha = 0.9, lwd = 2) +
  #geom_line(aes(y=rollmean( pbir * 3, 7, na.pad=TRUE)), col = "black") +
  geom_line(aes(y=rollmean(cuplag6 , 7, na.pad=TRUE)), alpha = 0.9, col = "red") 
# ♫
# geom_line(aes(y=rollmean( cuplag6 * .5, 1, na.pad=TRUE, align = "left")), col = "red") +
# geom_line(aes(y=rollmean( trees_lm * .5, 1, na.pad=TRUE, align = "left")), col = "green") +
# geom_line(aes(y=rollmean( pol_other_lm * .5, 1, na.pad=TRUE, align = "left")), col = "yellow") +
# geom_line(aes(y=rollmean( v_pos_rel_adj_Rhinovirus_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "blue") +
# geom_line(aes(y=rollmean( v_pos_rel_adj_corona_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "brown") +
# geom_line(aes(y=rollmean( v_pos_rel_adj_RSV_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "purple") +
# geom_line(aes(y=rollmean( v_pos_rel_adj_flu_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "orange")


# resid_explor %>%
#   mutate(doy = yday(date),
#          syear = year(date)) %>%
#   ggplot(aes(x = v_pos_rel_adj_flu_m14, y = resid, col = doy)) + geom_point() + theme_bw() + geom_smooth(se = FALSE, color = "red") +
#   facet_wrap(~NAB_station) + scale_color_viridis_c()
#   # geom_line(aes(y=rollmean(cup_all_lm, 7, na.pad=TRUE, align = "left")), col = "red") +
#   # geom_line(aes(y=rollmean(cup_all_lm, 14, na.pad=TRUE, align = "left")), col = "orange") +
#   # geom_line(aes(y=rollmean(cup_all_lm, 21, na.pad=TRUE, align = "left")), col = "yellow")


# resid_explor %>%
#   mutate(doy = yday(date)) %>%
#   mutate(cup_all_lm_14d = rollmean(cup_all_lm, 14, na.pad=TRUE)) %>%
#   #filter(date < ymd("2016-07-15")) %>%
#   filter(doy < 70 | doy > 330) %>%
#   ggplot(aes(x = cup_all_lm_14d, y = resid)) + geom_point() + theme_bw() + facet_wrap(~NAB_station) +
#   geom_smooth(se = FALSE)

#difference between years in PBIR
data_for_model %>% 
  mutate(years = year(date),
         jday = yday(date)) %>% 
  dplyr::select(years, jday, pbir, NAB_station) %>% 
  ggplot(aes(x=jday, y = pbir, color = as.factor(years))) + theme_bw()+ facet_wrap(~NAB_station)+
  geom_line(aes(y=rollmean(pbir, 7, na.pad=TRUE, align = "left"))) + #scale_y_carte(limits = c(0, 50))
  coord_cartesian(ylim = c(0,60))



### Fig 2,3,4: visualize effects of pollen ###############################################################
pred1_cup <- crosspred(cup_lag,  model2, #at = 1,
                       at = seq(from = 0, to = max(data_for_model$cup_all_lm), by = 0.10), 
                       bylag = 1, cen = 0, cumul = TRUE) #str(pred1_cup)

child_RR_x_cup_25km <- 
  data.frame(pol_conc = 10^(pred1_cup$predvar), 
             mean = pred1_cup$allRRfit,
             lower = pred1_cup$allRRlow,
             upper = pred1_cup$allRRhigh) %>% 
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10() + 
  annotation_logticks(sides = "b")  
#ggtitle(paste0("ages ", age_low, "-", age_hi, "  n_cases = ", sum(data_for_model$n_cases))) 

child_RR_x_cup_25km <- child_RR_x_cup_25km + geom_rug(data = data_for_model, aes(x = cup_all_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)

child_lag_RR_x_cup_25km <-
  as.data.frame(exp(pred1_cup$cumfit)) %>% mutate(pol_conc = pred1_cup$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_few() +
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR") + #, begin = 0.3, end = 1)  #automatically bins and turns to factor
  annotation_logticks(sides = "b")  
#ggtitle(paste0("  n_pop = ", sum(pop_near_NAB_agegroup_x$agegroup_x_pop)))
cowplot::plot_grid(child_RR_x_cup_25km, child_lag_RR_x_cup_25km)

## trees #
pred1_trees <- crosspred(trees_lag,  model2, 
                         at = seq(from = 0, to = max(data_for_model$trees_lm), by = .10),
                         bylag = 1, cen = 0, cumul = TRUE)

# plot(pred1_trees, "overall", ci = "lines", #ylim = c(0.95, 4), lwd = 2,
#      xlab = expression(paste("log10(tree pollen grains m"^"3",")")), 
#      ylab = "RR", main = "Overall effect of tree pollen")
child_RR_x_trees_25km <- 
  data.frame(pol_conc = 10^(pred1_trees$predvar), 
             mean = pred1_trees$allRRfit,
             lower = pred1_trees$allRRlow,
             upper = pred1_trees$allRRhigh) %>% 
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("trees (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10()+ 
  annotation_logticks(sides = "b")  

child_RR_x_trees_25km <-  child_RR_x_trees_25km +  geom_rug(data = data_for_model, aes(x = trees_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)

# plot.crosspred(pred1_trees, "contour", cumul = TRUE,
#      plot.title = title(xlab = expression(paste("log10(tree pollen grains m"^"3",")")),
#                         ylab = "Lag", main = "Cumulative RR across lags for trees"), key.title = title("RR"))
child_lag_RR_x_trees_25km <-
  as.data.frame(exp(pred1_trees$cumfit)) %>% mutate(pol_conc = pred1_trees$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_few() +
  xlab(expression(paste("tree pollen (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR") + 
  annotation_logticks(sides = "b")  


## pol_other
pred1_pol_other <- crosspred(pol_other_lag,  model2, 
                             at = seq(from = 0, to = max(data_for_model$pol_other_lm), by = .10),
                             bylag = 1, cen = 0, cumul = TRUE)
child_RR_x_pol_other_25km <-
  data.frame(pol_conc = 10^(pred1_pol_other$predvar),
             mean = pred1_pol_other$allRRfit,
             lower = pred1_pol_other$allRRlow,
             upper = pred1_pol_other$allRRhigh) %>%
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("other pollen (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10() + 
  annotation_logticks(sides = "b")  

child_RR_x_pol_other_25km <- child_RR_x_pol_other_25km +geom_rug(data = data_for_model, aes(x = pol_other_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)


child_lag_RR_x_pol_other_25km <-
  as.data.frame(exp(pred1_pol_other$cumfit)) %>% mutate(pol_conc = pred1_pol_other$predvar) %>%
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>%
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>%
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_few() +
  xlab(expression(paste("other pollen (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR") + 
  annotation_logticks(sides = "b")  


#saving the figs 
fig234 <-
  cowplot::plot_grid(child_RR_x_cup_25km, child_lag_RR_x_cup_25km, child_RR_x_trees_25km, child_lag_RR_x_trees_25km,
                     child_RR_x_pol_other_25km, child_lag_RR_x_pol_other_25km,
                     ncol = 2, labels = c("  A) Cupressaceae pollen", 
                                          "B) Cupressaceae pollen by lag", 
                                          "  C) tree pollen", 
                                          "D) tree pollen by lag",
                                          "    E) other pollen",
                                          "F) other pollen by lag"),
                     rel_widths = c(0.8, 1, 0.8, 1),
                     label_size = 11, label_x = 0.14, label_y = 0.9, hjust = 0, vjust = 0)
#fig234
fig_234_name <- paste0("C:/Users/dsk856/Desktop/thcic_analysis/results/",
                       "fig234_pol_ages",age_low,"_",age_hi,"_dist_", NAB_min_dist_threshold, "_",Sys.Date(),".jpg")
ggsave(file = fig_234_name, plot = fig234,
       height = 25, width = 21, units = "cm", dpi = 300)


