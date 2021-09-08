#script for analyzing site-level phenology as a function of environmental data
library(daymetr)
library(sf)
library(magrittr)
library(zoo)
library(readr)
library(dplyr)
library(climwin)
#rm(list=ls())

#load in site level-estimates: cones for field season 2021
c_site_1920 <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/fs19_20_site_halfway_cones_210907.csv") %>% 
  mutate(years = "19-20")
c_site_2021 <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/fs20_21_site_halfway_cones_210904.csv") %>% 
  mutate(years = "20-21")

c_sites <- bind_rows(c_site_1920, c_site_2021)

#get coordinates for sites that are included in analysis
pheno_site_mean_gompertz_1920_2021 <- c_sites %>% 
  mutate(d = round(Mean, 2),
         sd = round(SD,2),
         x = x_site,
         y = y_site) %>% 
  select(d, sd, x, y, site_name, years)
write_csv(pheno_site_mean_gompertz_1920_2021, "C:/Users/dsk856/Box/texas/pheno/fs20_21_site_coords_210907.csv")



## visualize site means across space
tx_boundary <- sf::read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_jitter(aes(x = x_site, y = y_site, col = Mean),# size = pollen),#col = hilo2), pollen / max_p
             data = c_sites, alpha = .7, size = 3)  + facet_wrap(~years)+
  scale_color_continuous(low = "blue", high = "red", name = "halfway point (day)") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines

### SMAP: loading in surface soil moisture (sms) ########################################
#SMAP download script is Google Earth Engine in: TX_Ja_pheno

sms_raw <- read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/SMAP_SMS_2019_2021_download_210907.csv")

sms <- 
  sms_raw %>% 
  pivot_longer(cols = contains("T12:00:00")) %>% 
  rename(site_coords = .geo) %>% 
  mutate(site_coords = substr(site_coords, 32, 70),
         site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
  separate(site_coords, sep = ",", c("lat", "long")) %>% 
  mutate(lat = round(as.numeric(lat), 1), 
         long = round(as.numeric(long), 1),
         site = paste(long, lat)) %>% 
  rename(sms = value) %>% 
  mutate(sms_date = lubridate::ymd_hms(name),
         d2 = case_when(years == "19-20" ~ d + mdy("12-08-2019"),
                        years == "20-21" ~ d + mdy("12-10-2020"))) %>% 
  dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
  dplyr::select(-c(name))


#19-20 field season
sms %>% 
  filter(years == "19-20") %>% 
  filter(sms_date > ymd("19/01/20")) %>% 
  filter(sms_date < ymd("19/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
  ggplot(aes(x = sms_date, y = sms, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
  scale_color_viridis_c(name = "peak date", trans = "date") +
  xlab("date") + ylab("surface soil moisture (mm)")


#20-21 field season
sms %>% 
  filter(years == "20-21") %>% 
  filter(sms_date > ymd("20/01/20")) %>% 
  filter(sms_date < ymd("20/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
  ggplot(aes(x = sms_date, y = sms, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
  scale_color_viridis_c(name = "peak date", trans = "date") +
  xlab("date") + ylab("surface soil moisture (mm)")
summary(sms$sms)

formula <- y ~ x 
#formula <- y ~ x + I(x^2)
library(ggpmisc)
sms %>% 
  filter(years == "19-20") %>% 
  filter(sms_date > ymd("19/04/1")) %>% 
  filter(sms_date < ymd("19/06/1")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
  group_by(site, site_name, d, d2) %>% 
  summarize(sms_mean = mean(sms)) %>%  #-> test2 #%>% #%T>% => test 
  ggplot(aes(x = sms_mean, y = d2, label = site_name)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = formula, se = FALSE) + geom_label() + 
  stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
               formula = formula, parse = TRUE, label.x = .9) +
  theme_bw() + xlab("surface soil moisture (mm)") + ylab("50% pollen released (day)") 

sms %>% 
  filter(years == "20-21") %>% 
  filter(sms_date > ymd("19/04/1")) %>% 
  filter(sms_date < ymd("19/06/1")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
  group_by(site, site_name, d, d2) %>% 
  summarize(sms_mean = mean(sms))%>%  #-> test # #%T>% => test 
  ggplot(aes(x = sms_mean, y = d2, label = site_name)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = formula, se = FALSE) + geom_label() + 
  stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
               formula = formula, parse = TRUE, label.x = .9) +
  theme_bw() + xlab("surface soil moisture (mm)") + ylab("50% pollen released (day)")

### trying out the climwin package ###############################################
sms_2021 <- filter(sms, years == "20-21") %>% 
  mutate(d3 = paste(day(sms_date), "/", month(sms_date),"/", year(sms_date), sep = ""), #climwin wants date in ddmmyyyy format
         placeholder_date = "31/12/2020")
MassWin <- slidingwin(xvar = list(Temp = sms_2021$sms),
                      cdate = sms_2021$d3,
                      bdate = sms_2021$placeholder_date,
                      baseline = lm(d ~ 1, data = sms_2021),
                      cinterval = "week",
                      range = c(50, 0),
                      type = "absolute", 
                      refday = c(31, 12), #dd mm of reference day
                      stat = "mean",
                      func = "lin", 
                      spatial = list(sms_2021$site_name, sms_2021$site_name))
head(MassWin[[1]]$Dataset)
MassWin[[1]]$BestModel

MassRand <- randwin(  repeats = 100,
                      xvar = list(Temp = sms_2021$sms),
                      cdate = sms_2021$d3,
                      bdate = sms_2021$placeholder_date,
                      baseline = lm(d ~ 1, data = sms_2021),
                      cinterval = "week",
                      range = c(50, 0),
                      type = "absolute", 
                      refday = c(31, 12), #dd mm of reference day
                      stat = "mean",
                      func = "lin", 
                      spatial = list(sms_2021$site_name, sms_2021$site_name))

pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "C", 
       sample.size = length(unique(sms_2021$site_name))) #using the number of different sites as n
MassOutput <- MassWin[[1]]$Dataset
MassRand <- MassRand[[1]]
plotdelta(dataset = MassOutput)
plotweights(dataset = MassOutput)
plotbetas(dataset = MassOutput)
plotwin(dataset = MassOutput)
plotall(dataset = MassOutput,
        datasetrand = MassRand,
        bestmodel = MassSingle$BestModel, 
        bestmodeldata = MassSingle$BestModelData)



### loading in surface soil moisture (sms) ########################################
susm_raw <- read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_SMAP_SUSM_2019_2021_download_210419.csv")

susm <- 
  susm_raw %>% 
  pivot_longer(cols = contains("T12:00:00")) %>% 
  rename(site_coords = .geo) %>% 
  mutate(site_coords = substr(site_coords, 32, 70),
         site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
  separate(site_coords, sep = ",", c("lat", "long")) %>% 
  mutate(lat = round(as.numeric(lat), 1), 
         long = round(as.numeric(long), 1),
         site = paste(long, lat)) %>% 
  rename(susm = value) %>% 
  mutate(susm_date = lubridate::ymd_hms(name),
         d2 = d + mdy("12-10-2020")) %>% 
  dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
  dplyr::select(-c(name))

susm %>% 
  filter(susm_date > ymd("20/01/01")) %>% 
  filter(susm_date < ymd("20/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
  ggplot(aes(x = susm_date, y = susm, group = site, color = d2)) + geom_line() + theme_bw()+
  scale_color_viridis_c(labels=as.Date, name = "peak date") +
  xlab("date") + ylab("sub-surface soil moisture (mm)")
summary(susm$susm)


formula <- y ~ x 
formula <- y ~ x + I(x^2)
susm %>% 
  filter(susm_date > ymd("20/04/1")) %>% 
  filter(susm_date < ymd("20/06/01")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
  group_by(site, d, d2) %>% 
  summarize(susm_mean = mean(susm)) %>% #%T>% => test 
  ggplot(aes(x = susm_mean, y = d2)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
               formula = formula, parse = TRUE, label.x = .9) +
  theme_bw() + xlab("surface soil moisture (mm)") + ylab("50% pollen released (day)")

#   
#   
# test <- left_join(test, fs2020_2021_coords)          
# unique(test$site)
# unique(fs2020_2021_coords$site)
# ?pivot_wider
# fs2020_2021_coords <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/fs20_21_coords_daymetr210406.csv")
# tree_mean_day_sf <- st_read("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_gompertz_210416.shp")


### download and extract met data from Daymet and PRISM ###############################################################
manual_obs_coords <- c_sites %>% #group_by(site, y, x) %>%  ungroup() %>% 
  mutate(x = round(x_site, 1), y = round(y_site, 1)) %>% 
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


