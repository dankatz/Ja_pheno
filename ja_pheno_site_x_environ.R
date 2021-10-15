#script for analyzing site-level phenology as a function of environmental data
library(daymetr)
library(sf)
library(magrittr)
library(zoo)
library(readr)
library(dplyr)
library(tidyr)
#library(climwin)
library(imputeTS)
library(ggpmisc)
library(raster)
library(tmap)
library(lubridate)
#rm(list=ls())


### load in observational data from year 1 and year 2 ####################################################

#load in site level-estimates: cones for field season 2021
c_site_1920 <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/fs19_20_site_halfway_cones_210910.csv") %>% 
  mutate(years = "19-20")
 c_site_2021 <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/fs20_21_site_halfway_cones_210904.csv") %>%
   mutate(years = "20-21") %>% 
    mutate(organ = "cones")
# c_site_2021 <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/fs20_21_site_halfway_sacs_210909.csv") %>% 
#   mutate(years = "20-21") %>% 
#   mutate(organ = "pollen sacs")
# 
# day_start <- mdy("12-10-2020")
# compare_sac_cones <- bind_rows(c_site_2021, c_site_2021_cones)
# compare_sac_cones %>% 
#   mutate(site_name = as.factor(site_name),
#          site_name = reorder(site_name, Mean)) %>% 
#   ggplot(aes(x = site_name, y = Mean + day_start, ymin = Mean - SD + day_start, ymax = Mean + SD + day_start, color = organ))  + 
#   theme_bw() + geom_pointrange(position = position_dodge(width = 0.4)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   ylab("Halfway point of season (mean +/- SD")+ xlab("site")

day_start_1920 <- mdy("12-10-2019")
day_start_2021 <- mdy("12-10-2020")

c_sites <- bind_rows(c_site_1920, c_site_2021)

#get coordinates for sites that are included in analysis
pheno_site_mean_gompertz_1920_2021 <- c_sites %>% 
  mutate(d = round(Mean, 2),
         sd = round(SD,2),
         x = x_site,
         y = y_site) %>% 
  dplyr::select(d, sd, x, y, site_name, years)
#write_csv(pheno_site_mean_gompertz_1920_2021, "C:/Users/dsk856/Box/texas/pheno/fs20_21_site_coords_210907.csv")



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

### making an empirical function for pollen release as a function of time difference from pheno modeled site mean ##############
p_2021 <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
c_site_2021_join <- c_site_2021 %>% dplyr::select(site_name, site_mean_d = Mean)
p_2021 <- left_join(p_2021, c_site_2021_join) %>% 
  mutate(site_mean_date = site_mean_d + mdy("12-10-2020"),
         site_mean_dif = round(as.numeric(sample_date - site_mean_date), 0),
         bag_cones_opening = case_when(is.na(bag_cones_opening) & bag_mean == 0 ~ 0,
                                      TRUE ~ bag_cones_opening),
         pol_release = case_when(pollen_rel == "none" ~ 0, 
                                 pollen_rel == "little" ~ 1, 
                                 pollen_rel == "some" ~ 1, 
                                 pollen_rel == "lots" ~ 1)) #unique(p_2021$pollen_rel)

ggplot(p_2021, aes(x = site_mean_dif, y = bag_cones_opening)) + geom_jitter() + theme_bw() +
  geom_smooth(method =)

cones_d_empir <- p_2021 %>% 
  group_by(site_mean_dif) %>% 
  summarize(bag_cones_opening_mean = mean(bag_cones_opening, na.rm = TRUE),
            pol_release_mean = mean(pol_release, na.rm = TRUE))
site_mean_dif_df <- data.frame(site_mean_dif = -100:100)
cones_d_empir <- left_join(site_mean_dif_df, cones_d_empir) %>% 
                  mutate(bag_cones_opening_mean = case_when(site_mean_dif > 34 ~ 0,
                                                            site_mean_dif < -18 ~ 0,
                                                            TRUE ~ bag_cones_opening_mean),
                         pol_release_mean = case_when(site_mean_dif > 34 ~ 0,
                                                      site_mean_dif < -18 ~ 0,
                                                      TRUE ~ pol_release_mean)) %>% 
                  mutate(bag_cones_opening_mean_m = zoo::rollapply(bag_cones_opening_mean, 7, #one week moving average
                                                                   mean, na.rm = TRUE, partial = TRUE, align='center'),
                         pol_release_mean_m = zoo::rollapply(pol_release_mean, 7, 
                                                                   mean, na.rm = TRUE, partial = TRUE, align='center'))

ggplot(cones_d_empir, aes(x = site_mean_dif, y = bag_cones_opening_mean)) + geom_point()+
  geom_line(aes(x = site_mean_dif, y = bag_cones_opening_mean_m)) + theme_bw() + 
  xlab( "difference from modeled site mean (days)") + ylab("opening sacs (proportion of observations)") +
  coord_cartesian(xlim = c(-25, 40) )

ggplot(cones_d_empir, aes(x = site_mean_dif, y = pol_release_mean)) + geom_point()+
  geom_line(aes(x = site_mean_dif, y = pol_release_mean_m)) + theme_bw() + 
  xlab( "difference from modeled site mean (days)") + ylab("pollen released during observation (proportion of observations)")+
  coord_cartesian(xlim = c(-25, 40) )

#write_csv(cones_d_empir, "C:/Users/dsk856/Box/texas/pheno/manual_obs/empirical_cone_sac_function_from_site_mean_day_210909.csv")


# ### SMAP: loading in surface soil moisture (sms) ########################################
# #SMAP download script is Google Earth Engine in: TX_Ja_pheno
# 
# sms_raw <- read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/SMAP10KM_ssm_2018_2020_download.csv",
#                     na = "No data")
# 
# sms <- 
#   sms_raw %>%  #names(sms_raw) #str(sms_raw)
#   pivot_longer(cols = contains("00:00")) %>% 
#   rename(site_coords = .geo) %>% 
#   mutate(site_coords = substr(site_coords, 32, 71),
#          site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
#   separate(site_coords, sep = ",", c("long", "lat")) %>% 
#   mutate(lat = round(as.numeric(lat), 1), 
#          long = round(as.numeric(long), 1),
#          site = paste(long, lat)) %>% 
#   rename(sms = value) %>% 
#   mutate(sms_date = lubridate::ymd_hms(name),
#          d2 = case_when(years == "19-20" ~ d + day_start_1920,
#                         years == "20-21" ~ d + day_start_2021)) %>% 
#   dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
#   dplyr::select(-c(name))
# 
# 
# #19-20 field season
# sms %>% 
#   filter(years == "19-20") %>% 
#   filter(sms_date > ymd("19/01/20")) %>% 
#   filter(sms_date < ymd("19/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   ggplot(aes(x = sms_date, y = sms, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
#   scale_color_viridis_c(name = "peak date", trans = "date") +
#   xlab("date") + ylab("surface soil moisture (mm)")
# 
# 
# #20-21 field season
# sms %>% 
#   filter(years == "20-21") %>% 
#   filter(sms_date > ymd("20/01/01")) %>% 
#   filter(sms_date < ymd("20/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   ggplot(aes(x = sms_date, y = sms, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
#   scale_color_viridis_c(name = "peak date", trans = "date") +
#   xlab("date") + ylab("surface soil moisture (mm)")
# 
# #summary(sms$sms)
# 
# formula <- y ~ x 
# #formula <- y ~ x + I(x^2)
# 
# sms %>% 
#   filter(years == "19-20") %>% 
#   filter(sms_date > ymd("19/04/1")) %>% 
#   filter(sms_date < ymd("19/06/1")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, d2) %>% 
#   summarize(sms_mean = mean(sms)) %>%  #-> test2 #%>% #%T>% => test 
#   ggplot(aes(x = sms_mean, y = d2, label = site_name)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
#   geom_smooth(method = "lm", formula = formula, se = FALSE) + #geom_label() + 
#   stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
#                formula = formula, parse = TRUE, label.x = .9) +
#   theme_bw() + xlab("surface soil moisture (mm)") + ylab("50% pollen released (day)") 
# 
# sms %>% 
#   filter(years == "20-21") %>% 
#   filter(sms_date > ymd("20/04/01")) %>% 
#   filter(sms_date < ymd("20/06/01")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, d2) %>% 
#   summarize(sms_mean = mean(sms))%>%  #-> test # #%T>% => test 
#   ggplot(aes(x = sms_mean, y = d2, label = site_name)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
#   geom_smooth(method = "lm", formula = formula, se = FALSE) + #geom_label() + 
#   stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
#                formula = formula, parse = TRUE, label.x = .9) +
#   theme_bw() + xlab("surface soil moisture (mm)") + ylab("50% pollen released (day)")
# 
# smap_spring19 <- 
#   sms %>% 
#   filter(sms_date > ymd("19/04/01")) %>% 
#   filter(sms_date < ymd("19/06/01")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, d2) %>% 
#   summarize(sms_mean = mean(sms))
# 
# smap_spring20 <- 
#   sms %>% 
#   filter(years == "20-21") %>% 
#   filter(sms_date > ymd("20/04/01")) %>% 
#   filter(sms_date < ymd("20/06/01")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, d2) %>% 
#   summarize(sms_mean = mean(sms)) 
#   
# 
# #plotting raw sms rasters
# ssm_2019_spring <- raster::raster("C:/Users/dsk856/Box/texas/pheno/met_data/SMAP_ssm_TX_2021_spring.tif")
# tm_shape(ssm_2019_spring) + 
#   tm_raster(title = "spring soil moisture",  
#             legend.reverse = TRUE,
#             breaks = c(0, 5, 10, 15, 20, 25, 30),
#             style = "cont", #style = "log10",
#             palette = "-viridis") +
#   tm_legend(outside = TRUE, legend.text.size = 1.0) + tm_compass() +
#   tm_layout(legend.position = c("left", "top"), legend.title.size = 1.2,
#             title= "April & May 2021",
#             title.size = 1.2, title.position = c('left', 'top') ) +
#   tm_shape(tx_boundary) +  tm_polygons(col = "white", alpha = 0) 
# 
# ssm_2020_spring <- raster::raster("C:/Users/dsk856/Box/texas/pheno/met_data/SMAP_ssm_TX_2020_spring.tif")
# tm_shape(ssm_2020_spring) + 
#   tm_raster(title = "spring soil moisture",  
#             legend.reverse = TRUE,
#             breaks = c(0, 5, 10, 15, 20, 25, 30),
#             style = "cont", #style = "log10",
#             palette = "-viridis") +
#   tm_legend(outside = TRUE, legend.text.size = 1.0) + tm_compass() +
#   tm_layout(legend.position = c("left", "top"), legend.title.size = 1.2,
#             title= "April & May 2020",
#             title.size = 1.2, title.position = c('left', 'top') ) +
#   tm_shape(tx_boundary) +  tm_polygons(col = "white", alpha = 0) 
# 
# 
# 
# ### vpd: loading in vapor pressure deficit (from daymet) ########################################
# #download script is Google Earth Engine in: TX_Ja_pheno
# 
# vpd_raw <- read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/DAYMET_vp_2018_2020_download.csv",
#                     na = "No data")
# 
# vpd <- 
#   vpd_raw %>%  #names(vpd_raw) #str(vpd_raw)
#   pivot_longer(cols = contains("00:00")) %>% 
#   rename(site_coords = .geo) %>% 
#   mutate(site_coords = substr(site_coords, 32, 71),
#          site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
#   separate(site_coords, sep = ",", c("long", "lat"))  %>%  #unique(test$lat)
#   mutate(lat = round(as.numeric(lat), 1), 
#          long = round(as.numeric(long), 1),
#          site = paste(long, lat)) %>% 
#   rename(vpd = value) %>% 
#   mutate(vpd_date = lubridate::ymd_hms(name),
#          d2 = case_when(years == "19-20" ~ d + day_start_1920,
#                         years == "20-21" ~ d + day_start_2021)) %>% 
#   dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
#   dplyr::select(-c(name))
# 
# 
# #19-20 field season
# vpd %>% 
#   filter(years == "19-20") %>% 
#   filter(vpd_date > ymd("19/01/20")) %>% 
#   filter(vpd_date < ymd("19/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   ggplot(aes(x = vpd_date, y = vpd, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
#   scale_color_viridis_c(name = "peak date", trans = "date") +
#   xlab("date") + ylab("vapor pressure deficit (kPa)")
# 
# 
# #20-21 field season
# vpd %>% 
#   filter(years == "20-21") %>% 
#   filter(vpd_date > ymd("20/05/01")) %>% 
#   filter(vpd_date < ymd("20/08/01")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   ggplot(aes(x = vpd_date, y = vpd, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
#   scale_color_viridis_c(name = "peak date", trans = "date") +
#   xlab("date") + ylab("vapor pressure deficit (kPa)")
# 
# 
# formula <- y ~ x #formula <- y ~ x + I(x^2)
# 
# vpd %>% 
#   filter(years == "19-20") %>% 
#   filter(vpd_date > ymd("19/07/1")) %>% 
#   filter(vpd_date < ymd("19/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, d2) %>% 
#   summarize(vpd_mean = mean(vpd)) %>%  #-> test2 #%>% #%T>% => test 
#   ggplot(aes(x = vpd_mean, y = d2, label = site_name)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
#   geom_smooth(method = "lm", formula = formula, se = FALSE) + #geom_label() + 
#   stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
#                formula = formula, parse = TRUE, label.x = .9) +
#   theme_bw() + xlab("VPD (kPa)") + ylab("50% pollen released (day)") 
# 
# vpd %>% 
#   filter(years == "20-21") %>% 
#   filter(vpd_date > ymd("20/07/01")) %>% 
#   filter(vpd_date < ymd("20/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, d2) %>% 
#   summarize(vpd_mean = mean(vpd))%>%  #-> test # #%T>% => test 
#   ggplot(aes(x = vpd_mean, y = d2, label = site_name)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
#   geom_smooth(method = "lm", formula = formula, se = FALSE) + #geom_label() + 
#   stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
#                formula = formula, parse = TRUE, label.x = .9) +
#   theme_bw() + xlab("VPD (kPa)") + ylab("50% pollen released (day)") 
# 
# vpd_summerfall19 <- 
#   vpd %>% 
#   filter(vpd_date > ymd("19/07/01")) %>% 
#   filter(vpd_date < ymd("19/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, d2) %>% 
#   summarize(vpd_mean = mean(vpd))
# fit_vpd_summerfall19 <- lm(d ~ vpd_mean, data = vpd_summerfall19); summary(fit_vpd_summerfall19)
# 
# 
# vpd_summerfall20 <- 
#   vpd %>% 
#   filter(years == "20-21") %>% 
#   filter(vpd_date > ymd("20/07/01")) %>% 
#   filter(vpd_date < ymd("20/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by( site_name, lat, long, d, d2) %>% 
#   summarize(vpd_mean = mean(vpd))
# fit_vpd_summerfall20 <- lm(d ~ vpd_mean, data = vpd_summerfall20); summary(fit_vpd_summerfall19)
# 
# ### combining different variables ##################################################
# # vpd_spring19
# # vpd_summerfall19
# tx_boundary <- sf::read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
# 
# 
# 
# yr2_env <- left_join(smap_spring20, vpd_summerfall20)
# ggplot(yr2_env, aes(x = sms_mean, y = vpd_mean)) + geom_point() + theme_bw() + geom_smooth(method ="lm")
# ggplot(yr2_env, aes(x = sms_mean, y = d)) + geom_point() + theme_bw() + geom_smooth(method ="lm")
# fit_sms_spring2020 <- lm(d ~ sms_mean, data = yr2_env)
# summary(fit_sms_spring2020)
# str(fit_sms_spring2020)
# 
# sms_resids <- yr2_env %>% 
#   dplyr::select(site, site_name, lat, long, d, d2, sms_mean, vpd_mean) %>% 
#   ungroup() %>% 
#   mutate(sms_resid = as.vector(fit_sms_spring2020$residuals),
#          years = "20-21")
# 
# ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
#   geom_point(data = sms_resids, aes(x = long, y = lat, color = sms_resid), size = 2, alpha = 0.8) + theme_bw() +
#   scale_color_gradient2(low = "red", mid = "gray", high = "blue", name = "residuals") 
#   #scale_color_viridis_c(option = "magma", direction = -1)
# 
# 
# sms_resids %>% 
#   ggplot(aes(x = long, y = sms_resid)) + geom_point() + theme_bw() + geom_smooth(method = "lm") +
#   scale_color_gradient2()#scale_color_viridis_c() 
# 
# 
# sms_resids %>% 
#   ggplot(aes(x = sms_mean, y = d, color = sms_resid)) + geom_point() + theme_bw() + geom_smooth(method = "lm") +
#   scale_color_gradient2()#scale_color_viridis_c() 
# 
# 
# ### predicting 2019-2020 mean day based on 2020-2021 regression ####################################################
# day_start_1920
# day_start_2021
# 
# # SMS from SMAP
# #19-20 field season
# sms_1920 <- sms %>% 
#   filter(years == "19-20") %>% 
#   filter(sms_date > ymd("19/04/01")) %>% 
#   filter(sms_date < ymd("19/06/01")) %>% 
#   # ggplot(aes(x = sms_date, y = sms, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
#   # scale_color_viridis_c(name = "peak date", trans = "date") +
#   # xlab("date") + ylab("surface soil moisture (mm)")
#   group_by(site_name, long, lat) %>% 
#   summarize(sms_mean = mean(sms),
#             d = mean(d), #accounting for different start dates
#             sd = mean(sd),
#             d2 = mean(d2)) %>% 
#   ungroup()
# 
# fit_sms_spring2020
# fit_sms_spring2020_on_1920data <- data.frame(predict.lm(fit_sms_spring2020, newdata = sms_1920, 
#                                                         interval = "prediction", level = 0.68)) #0.68/34 ~ 1 sd on standard normal 
# sms_1920 <- sms_1920 %>% 
#   mutate(d1920_pred2021_mean = fit_sms_spring2020_on_1920data$fit, #= sms_mean * fit_sms_spring2020$coefficients[2] + fit_sms_spring2020$coefficients[1])
#          d1920_pred2021_lwr25 = fit_sms_spring2020_on_1920data$lwr,
#          d1920_pred2021_upr75 = fit_sms_spring2020_on_1920data$upr)
# 
# ggplot(sms_1920, aes(y = d + day_start_1920, ymin = d - sd + day_start_1920, ymax = d + sd + day_start_1920, label = site_name, #color = sms_mean,
#                      x = d1920_pred2021_mean + day_start_1920 , xmin = d1920_pred2021_lwr25 + day_start_1920, 
#                      xmax = d1920_pred2021_upr75 + day_start_1920)) + 
#   geom_pointrange() + geom_errorbarh() + theme_bw() + geom_smooth(method = "lm") +
#   #coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) + 
#   geom_abline(slope = 1, intercept = 0, lty = 2) +
#   xlab("predicted midpoint(based on applying year 2 model to year 1 soil moisture)")+
#   ylab("observed midpoint (site mean based on cone model)")
#   #geom_label()
# 
# summary(lm(d ~  d1920_pred2021_mean , data = sms_1920))
# 
# #map of residuals
# ggplot(sms_1920, aes(x = long, y = lat, color = as.numeric((d + day_start_1920) - (d1920_pred2021_mean + day_start_1920)))) + 
#   geom_point(size = 4) + theme_bw() + scale_color_viridis_c(name = "days underpredicted")
# 
# ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
#   geom_jitter(aes(x = long, y = lat, 
#                   col = as.numeric((d + day_start_1920) - (d1920_pred2021_mean + day_start_1920))),# size = pollen),#col = hilo2), pollen / max_p
#               data = sms_1920, alpha = .7, size = 3)  + 
#   scale_color_viridis_c(name = "days underpredicted") +
#   xlab("") + ylab("") + #theme_few() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   coord_sf(datum=NA) #removes sf induced gridlines
# 
# 
# 
# # vpd from daymet
# #19-20 field season
# vpd_1920 <- vpd %>% 
#   filter(years == "19-20") %>% 
#   filter(vpd_date > ymd("19/04/01")) %>% 
#   filter(vpd_date < ymd("19/06/01")) %>% 
#   # ggplot(aes(x = vpd_date, y = vpd, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
#   # scale_color_viridis_c(name = "peak date", trans = "date") +
#   # xlab("date") + ylab("surface soil moisture (mm)")
#   group_by(site_name, long, lat) %>% 
#   summarize(vpd_mean = mean(vpd),
#             d = mean(d), #accounting for different start dates
#             sd = mean(sd),
#             d2 = mean(d2)) %>% 
#   ungroup()
# 
# vpd_summerfall20
# vpd_summerfall2020_on_1920data <- data.frame(predict.lm(fit_vpd_summerfall20, newdata = vpd_1920, 
#                                                         interval = "prediction", level = 0.68)) #0.68/34 ~ 1 sd on standard normal 
# vpd_1920 <- vpd_1920 %>% 
#   mutate(d1920_pred2021_mean = vpd_summerfall2020_on_1920data$fit, #= vpd_mean * fit_vpd_spring2020$coefficients[2] + fit_vpd_spring2020$coefficients[1])
#          d1920_pred2021_lwr25 = vpd_summerfall2020_on_1920data$lwr,
#          d1920_pred2021_upr75 = vpd_summerfall2020_on_1920data$upr)
# 
# ggplot(vpd_1920, aes(y = d + day_start_1920, ymin = d - sd + day_start_1920, ymax = d + sd + day_start_1920, label = site_name, #color = vpd_mean,
#                      x = d1920_pred2021_mean + day_start_1920 , xmin = d1920_pred2021_lwr25 + day_start_1920, 
#                      xmax = d1920_pred2021_upr75 + day_start_1920)) + 
#   geom_pointrange() + geom_errorbarh() + theme_bw() + geom_smooth(method = "lm") +
#   #coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) + 
#   geom_abline(slope = 1, intercept = 0, lty = 2) +
#   xlab("predicted midpoint(based on applying year 2 model to year 1 soil moisture)")+
#   ylab("observed midpoint (site mean based on cone model)")
# #geom_label()
# 
# 
# summary(lm(d ~  d1920_pred2021_mean, data = vpd_1920))
# sum()
# 
# #map of residuals
# ggplot(vpd_1920, aes(x = long, y = lat, color = as.numeric((d + day_start_1920) - (d1920_pred2021_mean + day_start_1920)))) + 
#   geom_point(size = 4) + theme_bw() + scale_color_viridis_c(name = "days underpredicted")
# 
# ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
#   geom_jitter(aes(x = long, y = lat, 
#                   col = as.numeric((d + day_start_1920) - (d1920_pred2021_mean + day_start_1920))),# size = pollen),#col = hilo2), pollen / max_p
#               data = vpd_1920, alpha = .7, size = 3)  + 
#   scale_color_viridis_c(name = "days underpredicted") +
#   xlab("") + ylab("") + #theme_few() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   coord_sf(datum=NA) #removes sf induced gridlines
# 
# 
# 
# 
# ### loop exploration through the other datasets ########################################
# #download script is Google Earth Engine in: TX_Ja_pheno
# env_data_list <- dir("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/", full.names = TRUE)
# pheno_site_mean_gompertz_1920_2021 <- read_csv("C:/Users/dsk856/Box/texas/pheno/fs20_21_site_coords_210907.csv")
# 
# for(i in 1:5){
# env_raw <- read_csv(env_data_list[i], na = "No data") #i <- 8 #str(env_raw)
# focal_dataset_name <- gsub(x = dir("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/"),
#                       "_download.csv", "")[i]
# 
# env <- 
#   env_raw %>%  #names(vpd_raw) #str(vpd_raw)
#   pivot_longer(cols = contains("00:00")) %>% 
#   rename(site_coords = .geo) %>% 
#   mutate(site_coords = substr(site_coords, 32, 70),
#          site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
#   separate(site_coords, sep = ",", c("lat", "long")) %>% 
#   mutate(lat = round(as.numeric(lat), 1), 
#          long = round(as.numeric(long), 1),
#          site = paste(long, lat)) %>% 
#   rename(env = value) %>% 
#   mutate(env_date = lubridate::ymd_hms(name)) %>% 
#          
#   dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
#   dplyr::select(-c(name, d, sd))
# 
# env <- left_join(env, pheno_site_mean_gompertz_1920_2021) %>% 
#         mutate( d2 = case_when(years == "19-20" ~ d + mdy("12-08-2019"),
#                                years == "20-21" ~ d + mdy("12-10-2020"))) 
# #length(unique(env$site_name))
# 
# #20-21 field season
# env_summary_plot_yr2 <-
#   env %>% 
#   filter(years == "20-21") %>% 
#   filter(env_date > ymd("20/01/01")) %>% 
#   filter(env_date < ymd("20/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   ggplot(aes(x = env_date, y = env, group = site, color = d)) + geom_line(lwd = 1, alpha =0.5) + theme_bw()+
#   scale_color_viridis_c(name = "peak day") +
#   xlab("date") + ylab(focal_dataset_name)
# 
# ggsave(plot = env_summary_plot_yr2, 
#        filename = paste0("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/loop_explor/", 
#                          focal_dataset_name, "yr2.png"),
#        height = 10,
#        width = 14,
#        units = "in")
# 
# #19-20 field season
# env_summary_plot_yr2 <-
#   env %>% 
#   filter(years == "19-20") %>% 
#   filter(env_date > ymd("19/01/01")) %>% 
#   filter(env_date < ymd("19/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   ggplot(aes(x = env_date, y = env, group = site, color = d)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
#   scale_color_viridis_c(name = "peak day") +
#   xlab("date") + ylab(focal_dataset_name)
# 
# ggsave(plot = env_summary_plot_yr2, 
#        filename = paste0("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/loop_explor/", 
#                          focal_dataset_name, "yr1.png"),
#        height = 10,
#        width = 14,
#        units = "in")
# 
# }
# 
# 
# 
# formula <- y ~ x 
# #formula <- y ~ x + I(x^2)
# 
# # env %>% 
# #   filter(years == "19-20") %>% 
# #   filter(env_date > ymd("19/07/1")) %>% 
# #   filter(env_date < ymd("19/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
# #   group_by(site, site_name, d, d2) %>% 
# #   summarize(env_mean = mean(env)) %>%  #-> test2 #%>% #%T>% => test 
# #   ggplot(aes(x = env_mean, y = d2, label = site_name)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
# #   geom_smooth(method = "lm", formula = formula, se = FALSE) + #geom_label() + 
# #   stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
# #                formula = formula, parse = TRUE, label.x = .9) +
# #   theme_bw() + xlab("env (kPa)") + ylab("50% pollen released (day)") 
# 
# env %>% 
#   filter(years == "20-21") %>% 
#   filter(env_date > ymd("20/08/01")) %>% 
#   filter(env_date < ymd("20/08/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, sms_resid) %>% 
#   summarize(env_mean = mean(env))%>%  #-> test # #%T>% => test 
#   ggplot(aes(x = env_mean, y = sms_resid, label = site_name)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
#   geom_smooth(method = "lm", formula = formula, se = FALSE) + #geom_label() + 
#   stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
#                formula = formula, parse = TRUE, label.x = .9) +
#   theme_bw() + xlab("environmental variable") + ylab("residual of sms: 50% pollen released (day)") 
# 
# env_summerfall19 <- 
#   env %>% 
#   filter(env_date > ymd("19/07/01")) %>% 
#   filter(env_date < ymd("19/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, d2) %>% 
#   summarize(env_mean = mean(env))
# 
# env_summerfall20 <- 
#   env %>% 
#   filter(years == "20-21") %>% 
#   filter(env_date > ymd("20/07/01")) %>% 
#   filter(env_date < ymd("20/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, site_name, d, d2) %>% 
#   summarize(env_mean = mean(env))
# test <- lm(d ~ env_mean, data = env_summerfall20); summary(test)
# 

### using climwin to select the best window for each ~daily env variable ###############################################

#modified function for extracting the info that is displayed in plotwin (median model open and close dates)
plotwin2 <- function(dataset, cw = 0.95){
  #Order models by weight#
  dataset    <- dataset[order(-dataset$ModWeight), ]
  #Firstly, check if the top model has a weight > cw. If so, just use the top model.
  if(dataset$ModWeight[1] > cw){
    datasetcw <- dataset[1, ]
    warning(paste0("Top window has a weight greater than ", cw, ". Plotting single best window only."))
  } else {
    dataset$cw <- as.numeric(cumsum(dataset$ModWeight) <= cw)
    datasetcw  <- subset(dataset, cw == 1) 
  }
  keep=c("Closest", "WindowClose", "WindowOpen")
  datasetcw                  <- datasetcw[keep]
  datasetcw                  <- reshape::melt(datasetcw, id = "Closest")
  datasetcw$variable         <- factor(datasetcw$variable, levels = c("WindowOpen", "WindowClose"))
  levels(datasetcw$variable) <- c("Window Open", "Window Close")
  p_meds <- data.frame(variable = levels(datasetcw$variable), value = as.numeric(tapply(datasetcw$value, datasetcw$variable, median)))
  return(p_meds)
}

#environmental data available at all sites
env_data_list <- dir("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/", full.names = TRUE)
pheno_site_mean_gompertz_1920_2021 <- read_csv("C:/Users/dsk856/Box/texas/pheno/fs20_21_site_coords_210907.csv") %>% 
  mutate(b_date_peak = case_when(years == "19-20" ~ d + mdy("12-10-2019"),
                                 years == "20-21" ~ d + mdy("12-10-2020")),
         b_date_placeholder = case_when(years == "19-20" ~ "28/12/2019", #some issue with data download from GEE?
                                        years == "20-21" ~ "28/12/2020"))

#only select the ~daily met data
env_data_list2 <- stringr::str_subset(env_data_list, pattern = "GRIDMET|DAYMET|SMAP10KM")
env_table <- data.frame(matrix(nrow = length(env_data_list2), ncol = 9))
names(env_table) <- c("env_var", "window_open", "window_close", "window_open_jul", "window_close_jul", 
                      "R2", "p", "MAE", "RMSE")

#loop through each met dataset to assess optimal open and close dates and model fit
for(i in 20:20){ #length(env_data_list2) #i <- 20 #str(env_raw)
env_raw <- read_csv(env_data_list2[i], na = "No data") 
focal_dataset_name <- gsub(x = env_data_list2[i],
                           pattern = ("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/|_download.csv"),
                           replacement = "")
print(focal_dataset_name)

env <- 
  env_raw %>%  #names(vpd_raw) #str(vpd_raw)
  pivot_longer(cols = contains("00:00")) %>% 
  rename(site_coords = .geo) %>% 
  mutate(site_coords = substr(site_coords, 32, 70),
         site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
  separate(site_coords, sep = ",", c("lat", "long")) %>% 
  mutate(lat = round(as.numeric(lat), 1), 
         long = round(as.numeric(long), 1),
         site = paste(long, lat)) %>% 
  rename(env = value) %>% 
  mutate(env_date = lubridate::ymd_hms(name)) %>% 
  dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
  dplyr::select(-c(name, d, sd)) %>% 
  mutate( env_c = env, #- 273.15,
          env_date2 = format(env_date, "%d/%m/%Y")) #climwin needs this specific date format 

#convert NAs to 0 for precip
if(focal_dataset_name == "DAYMET_prcp_2018_2020" |
   focal_dataset_name == "GRIDMET_pr_2018_2021" ){
  env <- env %>% mutate(env_c = replace_na(env_c, 0)) #for precipitation
}

#convert temperature to C from K
if(focal_dataset_name == "DAYMET_tmax_2018_2020" |
   focal_dataset_name == "DAYMET_tmin_2018_2020" |
   focal_dataset_name == "GRIDMET_tmmn_2018_2021"|
   focal_dataset_name == "GRIDMET_tmmx_2018_2021"){
  env <- env %>% mutate(env_c = env_c - 273.15) 
}

#interpolate any further missing data
if(length(env$env_c[is.na(env$env_c)]) > 0){
  print(paste0("there were ", length(env$env_c[is.na(env$env_c)]), " NA values interpolated"))
  env$env_c <- imputeTS::na_interpolation(x = env$env_c, option = "linear") #doesn't work to do this within mutate
}


#convert the once every three day measurements to a daily time-step by interpolating sms
if(focal_dataset_name == "SMAP10KM_ssm_2018_2020" | focal_dataset_name == "SMAP10KM_susm_2018_2020"){
date_fill_join <- expand_grid(sms_date = seq(min(date(env$env_date)), max(date(env$env_date)), by = "1 day"),
                              site_name = unique(env$site_name)) %>%
                  mutate(env_date2 = format(sms_date, "%d/%m/%Y")) 

env <- left_join(date_fill_join, env) %>% arrange(site_name, sms_date)

sms_imputed <- imputeTS::na_interpolation(x = env$env_c, option = "linear", maxgap = 3) #doesn't work to do this within mutate
env <- env %>%
      mutate(env_c = sms_imputed) %>%
      fill(years, lat, long, site, .direction = "downup")
}

####
MassWin <- slidingwin(xvar = list(Temp = env$env_c),
                      cdate = env$env_date2,
                      bdate = pheno_site_mean_gompertz_1920_2021$b_date_placeholder, 
                      baseline = lm(d ~ 1, data = pheno_site_mean_gompertz_1920_2021),
                      cinterval = "month",
                      range = c(12, 0),
                      type = "absolute",
                      refday = c(28, 12), #dd mm of reference day
                      stat = "mean",
                      func = "lin",
                      spatial = list(pheno_site_mean_gompertz_1920_2021$site_name, #biological dataset #unique(pheno_site_mean_gompertz_1920_2021$site_name)
                                     env$site_name)) #climate dataset #unique(env$site_name)
head(MassWin[[1]]$Dataset)
MassWin[[1]]$BestModel
MassOutput <- MassWin[[1]]$Dataset

plotdelta(dataset = MassOutput)
plotweights(dataset = MassOutput)
#plotbetas(dataset = MassOutput)
plotwin(dataset = MassOutput)

#best fit model
#plot(MassWin[[1]]$BestModelData$climate, MassWin[[1]]$BestModelData$yvar)
#summary(lm(MassWin[[1]]$BestModelData$yvar ~ MassWin[[1]]$BestModelData$climate))

plotwin_oc <- plotwin2(MassOutput) #plotwin_oc$value[1]

#plot(MassWin[[1]]$BestModelData$climate, MassWin[[1]]$BestModelData$yvar)

env_month <- env %>% mutate(months = month(env_date),
               years = year(env_date)) %>%
  group_by(site_name, years, months)%>%
  summarize(env_month = mean(env_c))

env_month_join <- env_month %>% 
  filter(months >= 12 - plotwin_oc$value[1],
         months <= 12 - plotwin_oc$value[2]) %>%
  group_by(site_name, years)%>%
  summarize(env_mo_mean = mean(env_month))%>%
  mutate(years = case_when(years == "2019" ~ "19-20",
                           years == "2020" ~ "20-21"))

site_mean_env_mo <- left_join(pheno_site_mean_gompertz_1920_2021, env_month_join)
site_mean_env_mo_fit <- lm(d ~ env_mo_mean, data = site_mean_env_mo)
site_mean_env_mo_fit_sum <- summary(site_mean_env_mo_fit)

fig_save <- ggplot(site_mean_env_mo, aes(x = env_mo_mean, y = d)) + 
  geom_abline(intercept = site_mean_env_mo_fit_sum$coefficients[1], slope = site_mean_env_mo_fit_sum$coefficients[2]) +
  geom_point(aes(color = years)) +
  xlab(focal_dataset_name) + theme_bw() + ggtitle(paste0("R2=", round(site_mean_env_mo_fit_sum$r.squared, 3)))

print(fig_save)
file_name_save <- paste0("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/climwin_loop_explor_mo/",
                         focal_dataset_name,".jpg")
ggsave(filename = file_name_save, plot = fig_save)

#save the selected window's environmental data too (site_mean_env_mo) 
write_csv(site_mean_env_mo, file = paste0("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/climwin_loop_explor_mo/env_mo_csvs/",
                                          focal_dataset_name,"_mo.csv"))

# str(MassOutput)
# (MassOutput$WindowOpen)
# ?plotwin
# test <- plotwin(dataset = MassOutput)
# str(test)
# median(test$data$value)
# .05 * nrow(MassWin[[1]]$Dataset)
# median(MassWin[[1]]$Dataset$WindowClose[1:1016])
# ??plotwin
# plotwin2(dataset = MassOutput)

#manual double check I'm getting "best fit" when I do the regression myself
# test_env <- env %>% filter(years == "20-21") %>% 
#   group_by(site_name) %>% 
#   filter(env_date2 == "22/12/2020") #start date of dec 28 - 6 days
# test_env_d <- left_join(pheno_site_mean_gompertz_1920_2021, test_env)
# plot(test_env_d$env_c, test_env_d$d) #yes, it works fine
# 
# MassSingle <- singlewin(xvar = list(Temp = env$env_c),
#                         cdate = env$env_date2,
#                         bdate = pheno_site_mean_gompertz_1920_2021$b_date_placeholder, 
#                         baseline = lm(d ~ 1, data = pheno_site_mean_gompertz_1920_2021),
#                         cinterval = "month",
#                         range = c(plotwin_oc$value[1], plotwin_oc$value[2]),
#                         #range = c(176, 91),
#                         type = "absolute",
#                         refday = c(28, 12), #dd mm of reference day
#                         stat = "mean",
#                         func = "lin",
#                         spatial = list(pheno_site_mean_gompertz_1920_2021$site_name, #biological dataset #unique(pheno_site_mean_gompertz_1920_2021$site_name)
#                                        env$site_name))
# #summary(MassSingle$BestModel)
# medianoc_fit <- summary(lm( MassSingle$BestModelData$yvar ~ MassSingle$BestModelData$climate))
# 
# medianoc_df <- data.frame(env = MassSingle$BestModelData$climate, yvar = MassSingle$BestModelData$yvar,
#                           site_name = pheno_site_mean_gompertz_1920_2021$site_name, years = pheno_site_mean_gompertz_1920_2021$years)
# 
# fig_save <- ggplot(medianoc_df, aes(x = env, y = yvar)) + geom_point(aes(color = years)) + geom_smooth(method = "lm", se = F) + 
#   xlab(focal_dataset_name) + theme_bw() + ggtitle(paste0("R2=", round(medianoc_fit$r.squared, 3)))
# fig_save
# file_name_save <- paste0("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/climwin_loop_explor/",
#                          focal_dataset_name,".jpg")
# ggsave(filename = file_name_save, plot = fig_save)

#plot(MassSingle$BestModelData$climate, MassSingle$BestModelData$yvar) #yes, I am getting what I should from MassSingle
# plotbest(dataset = MassOutput,
#          bestmodel = MassSingle$BestModel,
#          bestmodeldata = MassSingle$BestModelData)

### save results in the output table
env_table$env_var[i] <- focal_dataset_name
env_table$window_open[i] <- as.character(12 - plotwin_oc$value[1])
env_table$window_close[i] <- as.character(12 - plotwin_oc$value[2]) #as.character(mdy("12/28/2020") - plotwin_oc$value[2])
env_table$window_open_jul[i] <- plotwin_oc$value[1]#yday(mdy("12/28/2020") - plotwin_oc$value[1])
env_table$window_close_jul[i] <- plotwin_oc$value[2]#yday(mdy("12/28/2020") - plotwin_oc$value[2])
env_table$R2[i] <- round(site_mean_env_mo_fit_sum$r.squared, 3)
env_table$p[i] <- site_mean_env_mo_fit_sum$coefficients[8]
env_table$MAE[i] <- round(mean(abs(site_mean_env_mo_fit_sum$residuals)), 3)
env_table$RMSE[i] <- round(sqrt(mean(site_mean_env_mo_fit_sum$residuals^2)), 3)

} #end env var table creation loop

write.table(env_table, "clipboard", sep="\t", row.names=FALSE)
# plotall(dataset = MassOutput,
#         datasetrand = MassRand,
#         bestmodel = MassSingle$BestModel,
#         bestmodeldata = MassSingle$BestModelData)
# 
# plot(MassSingle$BestModelData)

MassRand <- randwin(  repeats = 5,
                      xvar = list(Temp = env$env_c),
                      cdate = env$env_date2,
                      bdate = pheno_site_mean_gompertz_1920_2021$b_date_placeholder, 
                      baseline = lm(d ~ 1, data = pheno_site_mean_gompertz_1920_2021),
                      cinterval = "day",
                      range = c(360, 1),
                      type = "absolute",
                      refday = c(31, 12), #dd mm of reference day
                      stat = "mean",
                      func = "lin",
                      spatial = list(pheno_site_mean_gompertz_1920_2021$site_name, #biological dataset #unique(pheno_site_mean_gompertz_1920_2021$site_name)
                                     env$site_name)) #climate dataset #unique(env$site_name)
MassRand[[1]]
pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "C",
       sample.size = 55) #using the number of different sites x years as n

plothist(dataset = MassOutput, datasetrand = MassRand[[1]]) #hist(MassRand[[1]])



### sequentially adding in a second environmental variable with climwin #####################################
#result: adding in the best variable (gridMET SRAD only added an R2 of 0.06)

#adding in the SMAP SSM data
ssm <- read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/climwin_loop_explor_mo/env_mo_csvs/SMAP10KM_ssm_2018_2020_mo.csv")
ssm <- ssm %>% mutate(ssm = env_mo_mean)

#loop through each met dataset to assess optimal open and close dates and model fit
for(i in 1:21){ #length(env_data_list2) #i <- 20 #str(env_raw)
  env_raw <- read_csv(env_data_list2[i], na = "No data") 
  focal_dataset_name <- gsub(x = env_data_list2[i],
                             pattern = ("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/|_download.csv"),
                             replacement = "")
  print(focal_dataset_name)
  
  env <- 
    env_raw %>%  #names(vpd_raw) #str(vpd_raw)
    pivot_longer(cols = contains("00:00")) %>% 
    rename(site_coords = .geo) %>% 
    mutate(site_coords = substr(site_coords, 32, 70),
           site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
    separate(site_coords, sep = ",", c("lat", "long")) %>% 
    mutate(lat = round(as.numeric(lat), 1), 
           long = round(as.numeric(long), 1),
           site = paste(long, lat)) %>% 
    rename(env = value) %>% 
    mutate(env_date = lubridate::ymd_hms(name)) %>% 
    dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
    dplyr::select(-c(name, d, sd)) %>% 
    mutate( env_c = env, #- 273.15,
            env_date2 = format(env_date, "%d/%m/%Y")) #climwin needs this specific date format 
  
  #convert NAs to 0 for precip
  if(focal_dataset_name == "DAYMET_prcp_2018_2020" |
     focal_dataset_name == "GRIDMET_pr_2018_2021" ){
    env <- env %>% mutate(env_c = replace_na(env_c, 0)) #for precipitation
  }
  
  #convert temperature to C from K
  if(focal_dataset_name == "DAYMET_tmax_2018_2020" |
     focal_dataset_name == "DAYMET_tmin_2018_2020" |
     focal_dataset_name == "GRIDMET_tmmn_2018_2021"|
     focal_dataset_name == "GRIDMET_tmmx_2018_2021"){
    env <- env %>% mutate(env_c = env_c - 273.15) 
  }
  
  #interpolate any further missing data
  if(length(env$env_c[is.na(env$env_c)]) > 0){
    print(paste0("there were ", length(env$env_c[is.na(env$env_c)]), " NA values interpolated"))
    env$env_c <- imputeTS::na_interpolation(x = env$env_c, option = "linear") #doesn't work to do this within mutate
  }
  
  
  #convert the once every three day measurements to a daily time-step by interpolating sms
  if(focal_dataset_name == "SMAP10KM_ssm_2018_2020" | focal_dataset_name == "SMAP10KM_susm_2018_2020"){
    date_fill_join <- expand_grid(sms_date = seq(min(date(env$env_date)), max(date(env$env_date)), by = "1 day"),
                                  site_name = unique(env$site_name)) %>%
      mutate(env_date2 = format(sms_date, "%d/%m/%Y")) 
    
    env <- left_join(date_fill_join, env) %>% arrange(site_name, sms_date)
    
    sms_imputed <- imputeTS::na_interpolation(x = env$env_c, option = "linear", maxgap = 3) #doesn't work to do this within mutate
    env <- env %>%
      mutate(env_c = sms_imputed) %>%
      fill(years, lat, long, site, .direction = "downup")
  }
  
  ####
  MassWin <- slidingwin(xvar = list(Temp = env$env_c),
                        cdate = env$env_date2,
                        bdate = pheno_site_mean_gompertz_1920_2021$b_date_placeholder, 
                        baseline = lm(d ~ 1 + ssm$ssm, data = pheno_site_mean_gompertz_1920_2021),
                        cinterval = "month",
                        range = c(12, 0),
                        type = "absolute",
                        refday = c(28, 12), #dd mm of reference day
                        stat = "mean",
                        func = "lin",
                        spatial = list(pheno_site_mean_gompertz_1920_2021$site_name, #biological dataset #unique(pheno_site_mean_gompertz_1920_2021$site_name)
                                       env$site_name)) #climate dataset #unique(env$site_name)
  head(MassWin[[1]]$Dataset)
  MassWin[[1]]$BestModel
  MassOutput <- MassWin[[1]]$Dataset
  
  plotdelta(dataset = MassOutput)
  plotweights(dataset = MassOutput)
  #plotbetas(dataset = MassOutput)
  plotwin(dataset = MassOutput)
  
  plotwin_oc <- plotwin2(MassOutput) #plotwin_oc$value[1]
  
  plot(MassWin[[1]]$BestModelData$climate, MassWin[[1]]$BestModelData$yvar)
  
  env_month <- env %>% mutate(months = month(env_date),
                              years = year(env_date)) %>%
    group_by(site_name, years, months)%>%
    summarize(env_month = mean(env_c))
  
  env_month_join <- env_month %>% 
    filter(months >= 12 - plotwin_oc$value[1],
           months <= 12 - plotwin_oc$value[2]) %>%
    group_by(site_name, years)%>%
    summarize(env_mo_mean = mean(env_month))%>%
    mutate(years = case_when(years == "2019" ~ "19-20",
                             years == "2020" ~ "20-21"))
  
  site_mean_env_mo <- left_join(pheno_site_mean_gompertz_1920_2021, env_month_join)
  site_mean_env_mo <- bind_cols(site_mean_env_mo, ssm = ssm$ssm)
  site_mean_env_mo_fit <- lm(d ~ env_mo_mean + ssm, data = site_mean_env_mo)
  site_mean_env_mo_fit_sum <- summary(site_mean_env_mo_fit)
  
  fig_save <- ggplot(site_mean_env_mo, aes(x = env_mo_mean, y = d)) + 
    #geom_abline(intercept = site_mean_env_mo_fit_sum$coefficients[1], slope = site_mean_env_mo_fit_sum$coefficients[2]) +
    geom_smooth(method = "lm")+
    geom_point(aes(shape = years, color = ssm)) + scale_color_viridis_c()+
    xlab(focal_dataset_name) + theme_bw() + 
    ggtitle(paste0("total R2= ", round(site_mean_env_mo_fit_sum$r.squared, 3), 
                   ", extra var p = ", round(site_mean_env_mo_fit_sum$coefficients[11], 3)))
  
  fig_save
  file_name_save <- paste0("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/climwin_loop_explor_mo/with_sms/",
                           focal_dataset_name,".jpg")
  ggsave(filename = file_name_save, plot = fig_save)

  ### save results in the output table
  env_table$env_var[i] <- focal_dataset_name
  env_table$window_open[i] <- as.character(12 - plotwin_oc$value[1])
  env_table$window_close[i] <- as.character(12 - plotwin_oc$value[2]) #as.character(mdy("12/28/2020") - plotwin_oc$value[2])
  env_table$window_open_jul[i] <- plotwin_oc$value[1]#yday(mdy("12/28/2020") - plotwin_oc$value[1])
  env_table$window_close_jul[i] <- plotwin_oc$value[2]#yday(mdy("12/28/2020") - plotwin_oc$value[2])
  env_table$R2[i] <- round(site_mean_env_mo_fit_sum$r.squared, 3)
  env_table$p[i] <- site_mean_env_mo_fit_sum$coefficients[8]
  env_table$MAE[i] <- round(mean(abs(site_mean_env_mo_fit_sum$residuals)), 3)
  env_table$RMSE[i] <- round(sqrt(mean(site_mean_env_mo_fit_sum$residuals^2)), 3)
  
} #end env var table creation loop

write.table(env_table, "clipboard", sep="\t", row.names=FALSE)





### the best single variable: SSM in April ########################################

#load in pheno data
pheno_site_mean_gompertz_1920_2021 <- read_csv("C:/Users/dsk856/Box/texas/pheno/fs20_21_site_coords_210907.csv") %>% 
  mutate(b_date_peak = case_when(years == "19-20" ~ d + mdy("12-10-2019"),
                                 years == "20-21" ~ d + mdy("12-10-2020")),
         b_date_placeholder = case_when(years == "19-20" ~ "28/12/2019", #some issue with data download from GEE?
                                        years == "20-21" ~ "28/12/2020"))

#load in environmental data: SSM from April 
ssm_plots <- read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_site_downloads/SMAP10KM_ssm_2018_2020_download.csv",
                      na = "No data")

env_ssm <- 
    ssm_plots %>%  #names(vpd_raw) #str(vpd_raw)
    pivot_longer(cols = contains("00:00")) %>% 
    rename(site_coords = .geo) %>% 
    mutate(site_coords = substr(site_coords, 32, 70),
           site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
    separate(site_coords, sep = ",", c("lat", "long")) %>% 
    mutate(lat = round(as.numeric(lat), 1), 
           long = round(as.numeric(long), 1),
           site = paste(long, lat)) %>% 
    rename(env = value) %>% 
    mutate(env_date = lubridate::ymd_hms(name)) %>% 
    dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
    dplyr::select(-c(name, d, sd)) %>% 
    mutate( env_c = env,
            sms_date = date(env_date)) #climwin needs this specific date format 

  #convert the once every three day measurements to a daily time-step by interpolating sms
    date_fill_join <- expand_grid(sms_date = seq(min(date(env_ssm$env_date)), max(date(env_ssm$env_date)), by = "1 day"),
                                  site_name = unique(env_ssm$site_name)) 
  
   env_ssm <- left_join(date_fill_join, env_ssm) %>% arrange(site_name, sms_date)
    
    sms_imputed <- imputeTS::na_interpolation(x = env_ssm$env_c, option = "linear", maxgap = 3) #doesn't work to do this within mutate
    env_ssm <- env_ssm %>%
      mutate(env_c = sms_imputed) %>%
      fill(years, lat, long, site, .direction = "downup")

    env_month_ssm <- env_ssm %>% mutate(months = month(env_date),
                                years = year(env_date)) %>%
    group_by(site_name, years, months)%>%
    summarize(env_month = mean(env_c))
  
  env_month_join_ssm <- env_month_ssm %>% 
    filter(months == 4 ) %>%
    group_by(site_name, years)%>%
    summarize(env_mo_mean = mean(env_month))%>%
    mutate(years = case_when(years == "2019" ~ "19-20",
                             years == "2020" ~ "20-21"))
  
  site_mean_ssm_apr <- left_join(pheno_site_mean_gompertz_1920_2021, env_month_join_ssm) %>%
    mutate(b_date_peak_noyr = case_when(years == "19-20" ~ b_date_peak + 366,
                                        years == "20-21" ~ b_date_peak)) #set everything to be in the same year
  
  site_mean_ssm_apr_fit <- lm(d ~ env_mo_mean, data = site_mean_ssm_apr)
  site_mean_ssm_apr_fit_sum <- summary(site_mean_ssm_apr_fit)
  site_mean_ssm_apr_fit_sum
  site_mean_ssm_apr <- site_mean_ssm_apr %>% mutate(d_ssm = site_mean_ssm_apr_fit$fitted.values,
                                                    d_resid = site_mean_ssm_apr_fit$residuals)  

  ggplot(site_mean_ssm_apr, aes(x= d_ssm, y = d, color = d_ssm)) + geom_point()
  
### figure for manuscript: SSM vs peak date
  fig_save <- ggplot(site_mean_ssm_apr, aes(x = env_mo_mean, y = b_date_peak_noyr)) + 
    #geom_abline(intercept = site_mean_env_mo_fit_sum$coefficients[1], slope = site_mean_env_mo_fit_sum$coefficients[2]) +
    geom_smooth(method = "lm", se = FALSE, color="black") +
    geom_point(aes(color = years)) +
    scale_y_date()+ ylab("season midpoint (day)") +
    xlab("surface soil moisture (mm)") + ggthemes::theme_few() + 
    annotate("text", x=17, y= mdy("1-23-2021"), label= "r^2 == 0.63", parse=TRUE) +
    annotate("text", x=17, y= mdy("1-21-2021"), label= "p < 0.001", parse=FALSE) 
  print(fig_save)
  # ggsave(filename = "C:/Users/dsk856/Box/texas/writing/pheno/fig3b_site_ssm_vs_peak.jpg", 
  #        plot = fig_save, width = 7, height = 6, units ="in")

  
### empirical function for pollen release as a function of sms in April ##############
  site_mean_ssm_apr #need to run the previous section first
  #p_1920 <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_clean_fs19_20_210910.csv") 
  p_2021_raw <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
  
  site_mean_ssm_apr_join <- site_mean_ssm_apr %>% dplyr::select(site_name, site_mean_d = d_ssm)
  
  #### NEED TO CHECK that site names line up so data isn't lost in the join
  
  p_2021 <- left_join(p_2021_raw, site_mean_ssm_apr_join) %>% 
    mutate(site_mean_ssm_pred = site_mean_d + mdy("12-10-2020"),
           site_mean_dif = round(as.numeric(sample_date - site_mean_ssm_pred), 0),
           bag_cones_opening = case_when(is.na(bag_cones_opening) & bag_mean == 0 ~ 0,
                                         TRUE ~ bag_cones_opening),
           pol_release = case_when(pollen_rel == "none" ~ 0, 
                                   pollen_rel == "little" ~ 0, 
                                   pollen_rel == "some" ~ 1, 
                                   pollen_rel == "lots" ~ 1)) #unique(p_2021$pollen_rel)
  
  ggplot(p_2021, aes(x = site_mean_dif, y = bag_cones_opening)) + geom_jitter() + theme_bw() +
    geom_smooth()
  
  cones_d_empir <- p_2021 %>% 
    group_by(site_mean_dif) %>% 
    summarize(bag_cones_opening_mean = mean(bag_cones_opening, na.rm = TRUE),
              pol_release_mean = mean(pol_release, na.rm = TRUE))
  site_mean_dif_df <- data.frame(site_mean_dif = -100:100)
  cones_d_empir <- left_join(site_mean_dif_df, cones_d_empir) %>% 
    mutate(bag_cones_opening_mean = case_when(site_mean_dif > 34 ~ 0,
                                              site_mean_dif < -18 ~ 0,
                                              TRUE ~ bag_cones_opening_mean),
           pol_release_mean = case_when(site_mean_dif > 34 ~ 0,
                                        site_mean_dif < -18 ~ 0,
                                        TRUE ~ pol_release_mean)) %>% 
    mutate(bag_cones_opening_mean_m = zoo::rollapply(bag_cones_opening_mean, 7, #one week moving average
                                                     mean, na.rm = TRUE, partial = TRUE, align='center'),
           pol_release_mean_m = zoo::rollapply(pol_release_mean, 7, 
                                               mean, na.rm = TRUE, partial = TRUE, align='center'))
  
  ggplot(cones_d_empir, aes(x = site_mean_dif, y = bag_cones_opening_mean)) + geom_point()+
    geom_line(aes(x = site_mean_dif, y = bag_cones_opening_mean_m)) + theme_bw() + 
    xlab( "difference from modeled site mean (days)") + ylab("opening sacs (proportion of observations)") +
    coord_cartesian(xlim = c(-25, 40) )
  
  ggplot(cones_d_empir, aes(x = site_mean_dif, y = pol_release_mean)) + geom_point()+
    geom_line(aes(x = site_mean_dif, y = pol_release_mean_m)) + theme_bw() + 
    xlab( "difference from modeled site mean (days)") + ylab("pollen released during observation (proportion of observations)")+
    coord_cartesian(xlim = c(-25, 40) )
  
  #write_csv(cones_d_empir, "C:/Users/dsk856/Box/texas/pheno/manual_obs/empirical_cone_sac_function_from_site_mean_ssm_day_211015.csv")
  
  
  
  


### create an empirical model/animation of opening sacs in fs 2020/2021 #######################################
library(tmap)
library(magick)

tx_boundary <- sf::read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")

#LOAD IN SSM
ssm_2020_spring <- raster::raster("C:/Users/dsk856/Box/texas/pheno/met_data/SMAP_ssm_TX_2020_april.tif")
par(mfrow = c(1,1))
plot(ssm_2020_spring) #par(mfrow=c(1,1))

#load in the Ja abundance map
ja_ba_orig <- raster("C:/Users/dsk856/Box/texas/statewide_abundance/USFS_TreeSpeciesMetrics/Ashe_juniper_basal_area_3km.tif")
ja_ba <- projectRaster(from = ja_ba_orig, to = ssm_2020_spring)
plot(ja_ba); plot(tx_boundary, add = TRUE, col = NA)
ja_ba_rel <- ja_ba/98.40555  #max(ja_ba$Ashe_juniper_basal_area_3km)
ja_ba_pres <- ja_ba
ja_ba_pres[ja_ba_pres > 0] <- 1 #plot(ja_ba_pres)



#using the best fit of soil moisture in spring 2020 to predict time of maximum pollen release in 2020
site_mean_ssm_apr_fit
ssm_2020_peak_d <- ssm_2020_spring * site_mean_ssm_apr_fit$coefficients[2] + site_mean_ssm_apr_fit$coefficients[1]
#d is with a start date of mdy("12-10-2020")
plot(ssm_2020_peak_d)

#empirical function of pollen release based on difference from peak date (sms regression)
cones_d_empir <- read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/empirical_cone_sac_function_from_site_mean_ssm_day_211015.csv") 
cones_d_empir2 <- cones_d_empir %>% mutate(days_from_site_mean_low = site_mean_dif - 0.5,
         days_from_site_mean_hi = site_mean_dif + 0.5) %>% 
  dplyr::select(days_from_site_mean_low, days_from_site_mean_hi, bag_cones_opening_mean_m)


#loop for creating rasters and map frames
opening_cones_stack <- stack()
for(i in 98:179){ #100 = Dec. 9th # 180 = Feb 27
  
  #create opening cones rasters
  focal_day <- round(cones_d_empir$site_mean_dif[i], 0)
  focal_day_date <- focal_day + mdy("12-10-2020")
  days_from_peak_rast <- focal_day - ssm_2020_peak_d  #plot(days_from_peak_rast)
  opening_on_day_x_rast <- reclassify(days_from_peak_rast, cones_d_empir2) #plot(opening_on_day_x_rast)
  names(opening_on_day_x_rast) <- paste("opening_", focal_day, sep = "")
   
  opening_cones_day_x_rast <- opening_on_day_x_rast * ja_ba_pres #ja_ba_rel #multiply daily release by ja_basal area (scaled to max)
  plot(opening_cones_day_x_rast, main = paste("date:", focal_day_date, "focal day: ", focal_day))
  opening_cones_pheno_stack <- stack(opening_cones_stack, opening_cones_day_x_rast) #plot(opening_cones_pheno_stack)

  #save each frame as a png
  date_title <- paste("opening cones on:\n", focal_day_date,"\n ") #convert Julian to date
  
  png(filename=paste("C:/Users/dsk856/Box/texas/pheno/manual_obs/animations/cones_opening_smspred_211015/",
    "TX_opening_", i,".png", sep = ""), width = 1200, height = 800, units = "px")

print(
  tm_shape(tx_boundary) +  tm_polygons(col = "white") + 
    tm_shape(opening_cones_day_x_rast * 100) + 
    tm_raster(title = "relative cone opening/day (%)",  #have to use unicode to sneak in the superscript
              legend.reverse = TRUE,
              #legend.is.portrait = FALSE,
              breaks = c(0, 5, 10, 15, 20, 25),
              style = "cont", #style = "log10",
              #labels = c("0.01","0.05","0.10","0.50", "1.0"),
              palette = "-viridis") +
   
    tm_legend(outside = TRUE, legend.text.size = 1.0) + 
    tm_compass() +
    tm_scale_bar(breaks = c(0,2), text.size = 1) +
    tm_layout(scale = 1.2,
              legend.position = c("left", "top"), 
              legend.title.size = 1.5,
              title= date_title,
              title.size = 1.2,
              title.position = c('left', 'top') )
)
dev.off()
} #end day loop


#creating an animation
list.files(path = "C:/Users/dsk856/Box/texas/pheno/manual_obs/animations/cones_opening_smspred_211015/", 
           pattern = "*.png", full.names = T) %>%
  purrr::map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("C:/Users/dsk856/Box/texas/pheno/manual_obs/animations/cones_opening_smspred_211015/cone_opening_fs2021_v2.gif") 




### create an empirical model/animation of opening sacs in fs 2019/2020 #######################################
ssm_2019_spring <- raster::raster("C:/Users/dsk856/Box/texas/pheno/met_data/SMAP_ssm_TX_2019_april.tif")
par(mfrow = c(1,1))
plot(ssm_2019_spring) #par(mfrow=c(1,1))

#check that this box includes all of TX
# tx_boundary <- sf::read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
# plot(tx_boundary, add = TRUE, col = NA)

#using the best fit of soil moisture to predict time of maximum pollen release
site_mean_ssm_apr_fit
ssm_2019_peak_d <- ssm_2019_spring * site_mean_ssm_apr_fit$coefficients[2] + site_mean_ssm_apr_fit$coefficients[1]
#d is with a start date of mdy("12-10-2020")
plot(ssm_2019_peak_d)



#loop for creating rasters and map frames
opening_cones_stack <- stack()
#for(i in 110:112){ #100 = Dec. 9th # 180 = Feb 27
for(i in 98:179){ #100 = Dec. 9th # 180 = Feb 27  
  #create opening cones rasters
  focal_day <- round(cones_d_empir$site_mean_dif[i], 0)
  focal_day_date <- focal_day + mdy("12-10-2019")
  days_from_peak_rast <- focal_day - ssm_2019_peak_d  #plot(days_from_peak_rast)
  opening_on_day_x_rast <- reclassify(days_from_peak_rast, cones_d_empir2) #plot(opening_on_day_x_rast)
  names(opening_on_day_x_rast) <- paste("opening_", focal_day, sep = "")
  
  opening_cones_day_x_rast <- opening_on_day_x_rast * ja_ba_pres #ja_ba_rel #multiply daily release by ja_basal area (scaled to max)
  plot(opening_cones_day_x_rast, main = paste("date:", focal_day_date, "focal day: ", focal_day))
  opening_cones_stack <- stack(opening_cones_stack, opening_cones_day_x_rast) #plot(opening_cones_pheno_stack)
  
  #save each frame as a png
  date_title <- paste("opening cones on:\n", focal_day_date,"\n ") #convert Julian to date
  
  png(filename=paste("C:/Users/dsk856/Box/texas/pheno/manual_obs/animations/cones_opening_1920_smspred_211015/",
                     "TX_opening_", i,".png", sep = ""), width = 1200, height = 800, units = "px")
  
  print(
    tm_shape(tx_boundary) +  tm_polygons(col = "white") + 
      tm_shape(opening_cones_day_x_rast * 100) + 
      tm_raster(title = "relative cone opening/day (%)",  #have to use unicode to sneak in the superscript
                legend.reverse = TRUE,
                #legend.is.portrait = FALSE,
                breaks = c(0, 5, 10, 15, 20, 25),
                style = "cont", #style = "log10",
                #labels = c("0.01","0.05","0.10","0.50", "1.0"),
                palette = "-viridis") +
      
      tm_legend(outside = TRUE, legend.text.size = 1.0) + 
      tm_compass() +
      tm_scale_bar(breaks = c(0,2), text.size = 1) +
      tm_layout(scale = 1.2,
                legend.position = c("left", "top"), 
                legend.title.size = 1.5,
                title= date_title,
                title.size = 1.2,
                title.position = c('left', 'top') )
  )
  dev.off()
} #end day loop


#creating an animation
list.files(path = "C:/Users/dsk856/Box/texas/pheno/manual_obs/animations/cones_opening_1920_smspred_211015/", 
           pattern = "*.png", full.names = T) %>%
  purrr::map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("C:/Users/dsk856/Box/texas/pheno/manual_obs/animations/cones_opening_1920_smspred_211015/cone_opening_fs1920.gif") 



### raster stack of opening sacs from 2015 - 2021 #######################################
ssm_file_list <- dir("C:/Users/dsk856/Box/texas/pheno/met_data/", full = TRUE)
ssm_file_list <- stringr::str_subset(ssm_file_list, pattern = "april") #just select the ones for april

focal_year <- gsub("[^0-9-]", "", ssm_file_list[1]) 
focal_year <- gsub("856", "", focal_year)
ssm_focal_rast <- raster::raster(ssm_file_list[1])
par(mfrow = c(1,1))
plot(ssm_focal_rast) #par(mfrow=c(1,1))

#using the best fit of soil moisture in spring 2020 to predict time of maximum pollen release in 2020
site_mean_ssm_apr_fit
ssm_focal_yr_peak_d <- ssm_focal_rast * site_mean_ssm_apr_fit$coefficients[2] + site_mean_ssm_apr_fit$coefficients[1]
plot(ssm_focal_yr_peak_d)

#loop for creating rasters and map frames
opening_cones_stack <- stack()
#for(i in 110:112){ #100 = Dec. 9th # 180 = Feb 27
for(i in 98:179){ #100 = Dec. 9th # 180 = Feb 27  
  #create opening cones rasters
  focal_day <- round(cones_d_empir$site_mean_dif[i], 0)
  focal_day_date <- focal_day + mdy(paste("12-10-", focal_year))#focal_day + mdy("12-10-2019")
  days_from_peak_rast <- focal_day - ssm_focal_yr_peak_d  #plot(days_from_peak_rast)
  opening_on_day_x_rast <- reclassify(days_from_peak_rast, cones_d_empir2) #plot(opening_on_day_x_rast)
  names(opening_on_day_x_rast) <- paste("opening_", focal_day, sep = "")
  
  opening_cones_day_x_rast <- opening_on_day_x_rast * ja_ba_pres #ja_ba_rel #multiply daily release by ja_basal area (scaled to max)
# plot(opening_cones_day_x_rast, main = paste("date:", focal_day_date, "focal day: ", focal_day))
  opening_cones_stack <- stack(opening_cones_stack, opening_cones_day_x_rast) #plot(opening_cones_pheno_stack)
  
} #end day loop

focal_year_stack_name_terra <- terra::rast(opening_cones_stack)
focal_year_stack_name <- paste0("C:/Users/dsk856/Box/texas/pheno/manual_obs_models/sac_opening_smsapril_stacks/",
                                "sac_opening_yr", focal_year, ".tif")
terra::writeRaster(focal_year_stack_name_terra, focal_year_stack_name, overwrite = TRUE)
#opening_cones_stack_terra <- terra::rast(focal_year_stack_name)

#test extract values within region of a 




### load in NAB data ############################################################

### extract sac opening time series from raster ###############################
library(terra)
nab_station_coords_raw <- read_csv("C:/Users/dsk856/Box/texas/NAB/NAB_tx_coords.csv")
#nab_station_coords <- st_as_sf(nab_station_coords_raw, coords = c("long","lat"))

nab_station_coords <- vect(nab_station_coords_raw, geom=c("long", "lat"), #crs=crs(r, proj=T),
                           type = "points")

test <- terra::extract(focal_year_stack_name_terra, nab_station_coords[1,])


### compare sac opening time series with NAB time series #########################

# ### create an empirical model/animation of cones opening in fs 2020/2021 as a function of sms regression########
# 
# 
# #predicted site mean date based on sms in spring 2020
# yr2_env$d_sms <- fit_sms_spring2020$fitted.values
# 
# 
# ### making an empirical function for pollen release as a function of time difference from modeled site mean ~ sms spring 2020
# p_2021 <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
# c_site_2021_join <- yr2_env %>% dplyr::select(site_name, site_mean_d = d_sms)
# p_2021 <- left_join(p_2021, c_site_2021_join) %>% 
#   mutate(site_mean_date = site_mean_d + mdy("12-10-2020"),
#          site_mean_dif = round(as.numeric(sample_date - site_mean_date), 0),
#          bag_cones_opening = case_when(is.na(bag_cones_opening) & bag_mean == 0 ~ 0,
#                                        TRUE ~ bag_cones_opening),
#          pol_release = case_when(pollen_rel == "none" ~ 0, 
#                                  pollen_rel == "little" ~ 1, 
#                                  pollen_rel == "some" ~ 1, 
#                                  pollen_rel == "lots" ~ 1)) #unique(p_2021$pollen_rel)
# 
# ggplot(p_2021, aes(x = site_mean_dif, y = bag_cones_opening)) + geom_jitter() + theme_bw() +
#   geom_smooth()
# 
# cones_d_empir <- p_2021 %>% 
#   group_by(site_mean_dif) %>% 
#   summarize(bag_cones_opening_mean = mean(bag_cones_opening, na.rm = TRUE),
#             pol_release_mean = mean(pol_release, na.rm = TRUE))
# site_mean_dif_df <- data.frame(site_mean_dif = -100:100)
# cones_d_empir <- left_join(site_mean_dif_df, cones_d_empir) %>% 
#   mutate(bag_cones_opening_mean = case_when(site_mean_dif > 34 ~ 0,
#                                             site_mean_dif < -18 ~ 0,
#                                             TRUE ~ bag_cones_opening_mean),
#          pol_release_mean = case_when(site_mean_dif > 34 ~ 0,
#                                       site_mean_dif < -18 ~ 0,
#                                       TRUE ~ pol_release_mean)) %>% 
#   mutate(bag_cones_opening_mean_m = zoo::rollapply(bag_cones_opening_mean, 7, #one week moving average
#                                                    mean, na.rm = TRUE, partial = TRUE, align='center'),
#          pol_release_mean_m = zoo::rollapply(pol_release_mean, 7, 
#                                              mean, na.rm = TRUE, partial = TRUE, align='center'))
# 
# ggplot(cones_d_empir, aes(x = site_mean_dif, y = bag_cones_opening_mean)) + geom_point()+
#   geom_line(aes(x = site_mean_dif, y = bag_cones_opening_mean_m)) + theme_bw() + 
#   xlab( "difference from modeled site mean (days)") + ylab("opening sacs (proportion of observations)") +
#   coord_cartesian(xlim = c(-25, 40) )
# 
# ggplot(cones_d_empir, aes(x = site_mean_dif, y = pol_release_mean)) + geom_point()+
#   geom_line(aes(x = site_mean_dif, y = pol_release_mean_m)) + theme_bw() + 
#   xlab( "difference from modeled site mean (days)") + ylab("pollen released during observation (proportion of observations)")+
#   coord_cartesian(xlim = c(-25, 40) )
# 
# #write_csv(cones_d_empir, "C:/Users/dsk856/Box/texas/pheno/manual_obs/empirical_cone_sac_function_from_sms_pred_site_mean_day_210913.csv")
# 
# 
# library(tmap)
# library(magick)
# 
# #load in the Ja abundance map
# ja_ba_orig <- raster("C:/Users/dsk856/Box/texas/statewide_abundance/USFS_TreeSpeciesMetrics/Ashe_juniper_basal_area_3km.tif")
# ja_ba <- projectRaster(from = ja_ba_orig, to = ssm_2020_spring)
# plot(ja_ba); plot(tx_boundary, add = TRUE, col = NA)
# ja_ba_rel <- ja_ba/98.40555  #max(ja_ba$Ashe_juniper_basal_area_3km)
# ja_ba_pres <- ja_ba
# ja_ba_pres[ja_ba_pres > 0] <- 1 #plot(ja_ba_pres)
# 
# ssm_2020_spring <- raster::raster("C:/Users/dsk856/Box/texas/pheno/met_data/SMAP_ssm_TX_2020_spring.tif")
# par(mfrow = c(1,1))
# plot(ssm_2020_spring) #par(mfrow=c(1,1))
# 
# #check that this box includes all of TX
# # tx_boundary <- sf::read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
# # plot(tx_boundary, add = TRUE, col = NA)
# 
# #using the best fit of soil moisture in spring 2020 to predict time of maximum pollen release in 2020
# fit_sms_spring2020
# ssm_2020_peak_d <- ssm_2020_spring * fit_sms_spring2020$coefficients[2] + fit_sms_spring2020$coefficients[1]
# #d is with a start date of mdy("12-10-2020")
# plot(ssm_2020_peak_d)
# 
# #empirical function of pollen release
# #cones_d_empir <- read_csv("C:/Users/dsk856/Box/texas/pheno/manual_obs/empirical_cone_sac_function_from_site_mean_day_210909.csv") 
# cones_d_empir2 <- cones_d_empir %>% mutate(days_from_site_mean_low = site_mean_dif - 0.5,
#                                            days_from_site_mean_hi = site_mean_dif + 0.5) %>% 
#   dplyr::select(days_from_site_mean_low, days_from_site_mean_hi, bag_cones_opening_mean_m)
# 
# 
# #loop for creating rasters and map frames
# 
# opening_cones_stack <- stack()
# for(i in 98:179){ #100 = Dec. 9th # 180 = Feb 27
#   
#   #create opening cones rasters
#   focal_day <- round(cones_d_empir$site_mean_dif[i], 0)
#   focal_day_date <- focal_day + mdy("12-10-2020")
#   days_from_peak_rast <- focal_day - ssm_2020_peak_d  #plot(days_from_peak_rast)
#   opening_on_day_x_rast <- reclassify(days_from_peak_rast, cones_d_empir2) #plot(opening_on_day_x_rast)
#   names(opening_on_day_x_rast) <- paste("opening_", focal_day, sep = "")
#   
#   opening_cones_day_x_rast <- opening_on_day_x_rast * ja_ba_rel #ja_ba_rel #multiply daily release by ja_basal area (scaled to max)
#   plot(opening_cones_day_x_rast, main = paste("date:", focal_day_date, "focal day: ", focal_day))
#   opening_cones_pheno_stack <- stack(opening_cones_stack, opening_cones_day_x_rast) #plot(opening_cones_pheno_stack)
#   
#   #save each frame as a png
#   date_title <- paste("opening cones on:\n", focal_day_date,"\n ") #convert Julian to date
#   
#   png(filename=paste("C:/Users/dsk856/Box/texas/pheno/manual_obs/animations/cones_opening_smspred_210913/",
#                      "TX_opening_", i,".png", sep = ""), width = 1200, height = 800, units = "px")
#   
#   print(
#     tm_shape(tx_boundary) +  tm_polygons(col = "white") + 
#       tm_shape(opening_cones_day_x_rast * 100) + 
#       tm_raster(title = "relative cone opening/day (%)",  #have to use unicode to sneak in the superscript
#                 legend.reverse = TRUE,
#                 #legend.is.portrait = FALSE,
#                 breaks = c(0, 5, 10, 15, 20, 25),
#                 style = "cont", #style = "log10",
#                 #labels = c("0.01","0.05","0.10","0.50", "1.0"),
#                 palette = "-viridis") +
#       
#       tm_legend(outside = TRUE, legend.text.size = 1.0) + 
#       tm_compass() +
#       tm_scale_bar(breaks = c(0,2), text.size = 1) +
#       tm_layout(scale = 1.2,
#                 legend.position = c("left", "top"), 
#                 legend.title.size = 1.5,
#                 title= date_title,
#                 title.size = 1.2,
#                 title.position = c('left', 'top') )
#   )
#   dev.off()
# } #end day loop
# 
# 
# #creating an animation
# list.files(path = "C:/Users/dsk856/Box/texas/pheno/manual_obs/animations/cones_opening_smspred_210913/", 
#            pattern = "*.png", full.names = T) %>%
#   purrr::map(image_read) %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=1) %>% # animates, can opt for number of loops
#   image_write("C:/Users/dsk856/Box/texas/pheno/manual_obs/animations/cones_opening_smspred_210913/cone_opening_fs2021_v3.gif") 
# 





# ### loading in surface soil moisture (sms) ########################################
# susm_raw <- read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_SMAP_SUSM_2019_2021_download_210419.csv")
# 
# susm <- 
#   susm_raw %>% 
#   pivot_longer(cols = contains("T12:00:00")) %>% 
#   rename(site_coords = .geo) %>% 
#   mutate(site_coords = substr(site_coords, 32, 70),
#          site_coords = gsub(pattern = "]}", replacement = "", x = site_coords)) %>% 
#   separate(site_coords, sep = ",", c("lat", "long")) %>% 
#   mutate(lat = round(as.numeric(lat), 1), 
#          long = round(as.numeric(long), 1),
#          site = paste(long, lat)) %>% 
#   rename(susm = value) %>% 
#   mutate(susm_date = lubridate::ymd_hms(name),
#          d2 = d + mdy("12-10-2020")) %>% 
#   dplyr::select(-1) %>%  #getting rid of the bad name that contained a ":"
#   dplyr::select(-c(name))
# 
# susm %>% 
#   filter(susm_date > ymd("20/01/01")) %>% 
#   filter(susm_date < ymd("20/12/31")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   ggplot(aes(x = susm_date, y = susm, group = site, color = d2)) + geom_line() + theme_bw()+
#   scale_color_viridis_c(labels=as.Date, name = "peak date") +
#   xlab("date") + ylab("sub-surface soil moisture (mm)")
# summary(susm$susm)
# 
# 
# formula <- y ~ x 
# formula <- y ~ x + I(x^2)
# susm %>% 
#   filter(susm_date > ymd("20/04/1")) %>% 
#   filter(susm_date < ymd("20/06/01")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, d, d2) %>% 
#   summarize(susm_mean = mean(susm)) %>% #%T>% => test 
#   ggplot(aes(x = susm_mean, y = d2)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
#   geom_smooth(method = "lm", formula = formula, se = FALSE) +
#   stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
#                formula = formula, parse = TRUE, label.x = .9) +
#   theme_bw() + xlab("surface soil moisture (mm)") + ylab("50% pollen released (day)")
# 
# #   
# #   
# # test <- left_join(test, fs2020_2021_coords)          
# # unique(test$site)
# # unique(fs2020_2021_coords$site)
# # ?pivot_wider
# # fs2020_2021_coords <- readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/fs20_21_coords_daymetr210406.csv")
# # tree_mean_day_sf <- st_read("C:/Users/dsk856/Box/texas/pheno/manual_obs/pheno_gompertz_210416.shp")


# ### download and extract met data from Daymet and PRISM ###############################################################
# manual_obs_coords <- c_sites %>% #group_by(site, y, x) %>%  ungroup() %>% 
#   mutate(x = round(x_site, 1), y = round(y_site, 1)) %>% 
#   dplyr::select(site = site, lat = y, long = x) %>% distinct() %>% filter(!is.na(site)) 
# 
# 
# #DAYMET ISNT A GOOD CHOICE BECAUSE YOU CANT DOWNLOAD CURRENT YEAR
# # write_csv(manual_obs_coords, "C:/Users/dsk856/Box/texas/pheno/met_data/fs20_21_coords_daymetr210406.csv")
# # weather_at_stations <- download_daymet_batch(file_location = "C:/Users/dsk856/Box/texas/pheno/met_data/fs20_21_coords_daymetr210406.csv",
# #                                              start =2018, end = 2021, simplify = TRUE)
# # write_csv(weather_at_stations, "C:/Users/dsk856/Box/texas/pheno/met_data/weather_at_sites_2018_2020_210406.csv")
# # unique(weather_at_stations$measurement)
# # weather_at_stations <-
# #   read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/weather_at_sites_2018_2020_210203.csv") %>%
# #   mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
# #   mutate(measurement = gsub(pattern = ".", replacement = "", x = measurement, fixed = TRUE)) %>%
# #   dplyr::select(site = site, date, measurement, value) %>%
# # group_by(site, date, measurement) %>%
# #   summarise(value = mean(value, na.rm = TRUE)) %>%
# #     distinct() %>%
# #   pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")
# # head(weather_at_stations); summary(weather_at_stations)
# 
# 
# weather_at_stations <- 
#   readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/prism_ppt_fs20_21_2019_2021.csv") %>% 
#   mutate(date = ymd(date)) %>%
#   #mutate(measurement = gsub(pattern = ".", replacement = "", x = data, fixed = TRUE)) %>%
#   dplyr::select(site = site, date, measurement = metvar, value = data) %>% 
#   group_by(site, date, measurement) %>% 
#   summarise(value = mean(value, na.rm = TRUE)) %>% 
#   distinct() %>% 
#   pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")
# 
# weather_at_stations_tmean <- 
#   readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/prism_tmean_fs20_21_2019_2021.csv") %>% 
#   mutate(date = ymd(date)) %>%
#   #mutate(measurement = gsub(pattern = ".", replacement = "", x = data, fixed = TRUE)) %>%
#   dplyr::select(site = site, date, measurement = metvar, value = data) %>% 
#   group_by(site, date, measurement) %>% 
#   summarise(value = mean(value, na.rm = TRUE)) %>% 
#   distinct() %>% 
#   pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")
# 
# weather_at_stations_vpdmin <- 
#   readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/prism_fs20_21_vpdmin_2019_2021.csv") %>% 
#   mutate(date = ymd(date)) %>%
#   #mutate(measurement = gsub(pattern = ".", replacement = "", x = data, fixed = TRUE)) %>%
#   dplyr::select(site = site, date, measurement = metvar, value = data) %>% 
#   group_by(site, date, measurement) %>% 
#   summarise(value = mean(value, na.rm = TRUE)) %>% 
#   distinct() %>% 
#   pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")
# 
# weather_at_stations_vpdmax <- 
#   readr::read_csv("C:/Users/dsk856/Box/texas/pheno/met_data/prism_fs20_21_vpdmax_2019_2021.csv") %>% 
#   mutate(date = ymd(date)) %>%
#   #mutate(measurement = gsub(pattern = ".", replacement = "", x = data, fixed = TRUE)) %>%
#   dplyr::select(site = site, date, measurement = metvar, value = data) %>% 
#   group_by(site, date, measurement) %>% 
#   summarise(value = mean(value, na.rm = TRUE)) %>% 
#   distinct() %>% 
#   pivot_wider(id_cols = c(site, date), names_from = measurement, values_from = value, names_prefix = "met_")
# 
# 
# 
# weather_at_stations <- left_join(weather_at_stations, weather_at_stations_tmean)
# 
# 
# #creating some indices for weather conditions around the time of release
# library(zoo)
# weather_at_stations3 <- weather_at_stations %>% group_by(site) %>% 
#   #filter(date > ymd("2020 - 11 - 01")) %>% 
#   mutate(doy = yday(date),
#          year = year(date),
#          mo = month(date),
#          season = case_when(doy > 320 ~ paste("s", year, (year + 1)),
#                             doy < 41  ~ paste("s", (year - 1), year),
#                             doy < 321 & doy > 40  ~ "not Ja season"),
#          doy_m = case_when(doy > 320 ~ doy - 365,
#                            doy < 320 ~ doy),
#          # met_tmaxdegc_l1 = lag(met_tmaxdegc, 1),
#          # met_tmindegc_l1 = lag(met_tmindegc, 1),
#          # #met_tavgdegc_l1 = lag(met_tmindegc + met_tmindegc )/2) 
#          met_prcpmmday_l1 =  dplyr::lag(x = met_ppt, k = 1),
#          
#          # met_vpPa_l1 = lag(met_vpPa, 1),
#          # # met_vpdmin_l1 = lag(met_vpdmin, 1),
#          # # met_vpdmax_l1 = lag(met_vpdmax, 1),
#          # # 
#          # matmax_7 =rollapply(met_tmaxdegc, 7,mean,align='right',fill=NA),
#          # matmin_7 =rollapply(met_tmindegc, 7,mean,align='right',fill=NA),
#          matavg_7 = rollapply(met_tmean, 7, mean,align='right',fill=NA),
#          maprcp_7 = rollapply(met_ppt, 7, mean,align='right',fill=NA),
#          # mavp_7 = rollapply(met_vpPa, 7,mean,align='right',fill=NA),
#          # # mavpdmin_7 = rollapply(met_vpdmin, 7,mean,align='right',fill=NA),
#          # # mavpdmax_7 = rollapply(met_vpdmax, 7,mean,align='right',fill=NA),
#          # # 
#          # matmax_14 =rollapply(met_tmaxdegc, 14,mean,align='right',fill=NA),
#          # matmin_14 =rollapply(met_tmindegc, 14,mean,align='right',fill=NA),
#          matavg_14 =rollapply(((met_tmindegc + met_tmindegc )/2), 14,mean,align='right',fill=NA),
#          maprcp_14 = rollapply(met_ppt, 14,mean,align='right',fill=NA),
#          # masrad_14 = rollapply(met_sradWm2, 14,mean,align='right',fill=NA),
#          # mavp_14 = rollapply(met_vpPa, 14,mean,align='right',fill=NA),
#          # # mavpdmin_14 = rollapply(met_vpdmin, 14,mean,align='right',fill=NA),
#          # # mavpdmax_14 = rollapply(met_vpdmax, 14,mean,align='right',fill=NA),
#          # # 
#          # matmax_21 =rollapply(met_tmaxdegc, 21,mean,align='right',fill=NA),
#          # matmin_21 =rollapply(met_tmindegc, 21,mean,align='right',fill=NA),
#          matavg_21 =rollapply(((met_tmindegc + met_tmindegc )/2), 21,mean,align='right',fill=NA),
#          maprcp_21 = rollapply(met_ppt, 21,mean,align='right',fill=NA),
#          # masrad_21 = rollapply(met_sradWm2, 21,mean,align='right',fill=NA),
#          # mavp_21 = rollapply(met_vpPa, 21,mean,align='right',fill=NA),
#          # # mavpdmin_21 = rollapply(met_vpdmin, 21,mean,align='right',fill=NA),
#          # # mavpdmax_21 = rollapply(met_vpdmax, 21,mean,align='right',fill=NA),
#          # 
#          # matmax_28 =rollapply(met_tmaxdegc, 28,mean,align='right',fill=NA),
#          # matmin_28 =rollapply(met_tmindegc, 28,mean,align='right',fill=NA),
#          matavg_28 =rollapply(((met_tmindegc + met_tmindegc )/2), 28,mean,align='right',fill=NA),
#          maprcp_28 = rollapply(met_ppt, 28,mean,align='right',fill=NA),
#          # masrad_28 = rollapply(met_sradWm2, 28,mean,align='right',fill=NA),
#          # mavp_28 = rollapply(met_vpPa, 28,mean,align='right',fill=NA),
#          # # mavpdmin_28 = rollapply(met_vpdmin, 28,mean,align='right',fill=NA),
#          # # mavpdmax_28 = rollapply(met_vpdmax, 28,mean,align='right',fill=NA),
#          # 
#          # matmax_35 =rollapply(met_tmaxdegc, 35,mean,align='right',fill=NA),
#          # matmin_35 =rollapply(met_tmindegc, 35,mean,align='right',fill=NA),
#          matavg_35 =rollapply(((met_tmindegc + met_tmindegc )/2), 35,mean,align='right',fill=NA),
#          maprcp_35 = rollapply(met_ppt, 35,mean,align='right',fill=NA)
#          # masrad_35 = rollapply(met_sradWm2, 35,mean,align='right',fill=NA),
#          # mavp_35 = rollapply(met_vpPa, 35,mean,align='right',fill=NA),
#          # mavpdmin_35 = rollapply(met_vpdmin, 35,mean,align='right',fill=NA),
#          # mavpdmax_35 = rollapply(met_vpdmax, 35,mean,align='right',fill=NA)
#   )
# 
# #creating some summary weather data
# weather_at_stations_summary_dec <- weather_at_stations3 %>% 
#   group_by(site, season) %>% 
#   filter(mo == 12) %>% 
#   summarize(#matmax_dec = mean(met_tmaxdegc),
#     #matmin_dec = mean(met_tmindegc),
#     mactmean_dec = mean(met_tmean),
#     macprcp_dec = mean(met_ppt)
#     #mavpda_dec = mean(met_vpPa)
#     # mavpdmin_dec = mean(met_vpdmin),
#     # mavpdmax_dec = mean(met_vpdmax)
#   )
# 
# weather_at_stations_summary_jan <- weather_at_stations3 %>%
#   group_by(site, season) %>%
#   filter(mo == 1) %>%
#   summarize(#matmax_jan = mean(met_tmaxdegc),
#     #matmin_jan = mean(met_tmindegc),
#     mactmean_jan = mean(met_tmean),
#     macprcp_jan = mean(met_ppt))
# #mavpdmin_jan = mean(met_vpdmin),
# #mavpdmax_jan = mean(met_vpdmax))
# 
# weather_at_stations_summary_fall <- weather_at_stations3 %>%
#   mutate( season = case_when(doy > 274 ~ paste("s", year, (year + 1)),
#                              doy < 41  ~ paste("s", (year - 1), year),
#                              doy < 275 & doy > 40  ~ "not Ja season")) %>%
#   group_by(site, season) %>%
#   filter(mo > 9 & season != "not Ja season") %>%
#   summarize(#matmax_fall = mean(met_tmaxdegc),
#     #matmin_fall = mean(met_tmindegc),
#     mactmean_fall = mean(met_tmean),
#     macprcp_fall = mean(met_ppt))
# #mavpdmin_fall = mean(met_vpdmin),
# #mavpdmax_fall = mean(met_vpdmax))
# # 
# # #joining weather data with pollen
# # pw <- left_join(p, weather_at_stations3)
# # pw <- left_join(pw, weather_at_stations_summary_dec)
# # pw <- left_join(pw, weather_at_stations_summary_jan)
# # pw <- left_join(pw, weather_at_stations_summary_fall)
# 
# 
# 
# # site_halfway_params2_filt <-  site_halfway_params2 %>% #filter(site_n != 5) %>% 
# #   mutate(site = paste(site_long, site_lat),
# #          date = day_start + Mean,
# #          dif_mean = Mean - 17.3) 
# global_mean_50p <- mean(trees_mean_day$site_mean)
# 
# site_halfway_params2_filt <-  trees_mean_day %>% #filter(!is.na(site_n)) %>% 
#   mutate(site = paste(round(x, 1), round(y, 1)),
#          dif_mean = site_mean - global_mean_50p,
#          date = day_start + site_mean) 
# 
# 
# site_halfway_params2_filt <- left_join(site_halfway_params2_filt, weather_at_stations_summary_dec)
# site_halfway_params2_filt<- left_join(site_halfway_params2_filt, weather_at_stations_summary_jan)
# site_halfway_params2_filt<- left_join(site_halfway_params2_filt, weather_at_stations_summary_fall)
# 
# ggplot(site_halfway_params2_filt, aes(x = macprcp_dec, y= site_mean)) + geom_point(size = 3) + theme_bw() + 
#   geom_smooth(method = "lm", se = F) +
#   xlab("mean daily temperature in Jan (C)") + 
#   #xlab("mean precipitation/day in the fall (mm)") + 
#   ylab("date") + scale_color_viridis_c()
# 
# fit <- lm(site_mean ~ macprcp_dec , data = site_halfway_params2_filt)
# summary(fit)
# # fit$residuals
# # str(fit)
# # site_halfway_params2_filt <- site_halfway_params2_filt %>% 
# #   mutate(resid = fit$residuals)
# 
# 
# 
# ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
#   geom_point(aes(x = x, y = y, col = date),# size = pollen),#col = hilo2), pollen / max_p
#              data = site_halfway_params2_filt, alpha = .7, size = 3)  + 
#   scale_color_continuous(low = "blue", high = "red", name = "halfway point (day)", labels=as.Date) + 
#   xlab("") + ylab("") + #theme_few() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   coord_sf(datum=NA) #removes sf induced gridlines
# 
# 
# ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
#   geom_point(aes(x = site_long, y = site_lat, col = resid),# size = pollen),#col = hilo2), pollen / max_p
#              data = site_halfway_params2_filt, alpha = .7, size = 3)  + 
#   scale_color_continuous(low = "blue", high = "red") + 
#   xlab("") + ylab("") + #theme_few() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   coord_sf(datum=NA) #removes sf induced gridlines
# 
# ggplot(weather_at_stations3, aes(x = date, y = met_ppt)) + geom_point() + facet_wrap(~site) + theme_bw() +
#   geom_point(data = site_halfway_params2_filt, aes(x = date, y = -1, color = dif_mean), size = 5) +
#   scale_color_viridis_c() +
#   scale_x_date(limits = c(mdy("12-15-2020"), mdy("1-20-2021")))
# 
# mean(site_halfway_params2_filt$Mean)
# 
# 
# 
# 
# ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
#   geom_point(aes(x = x, y = y, col = mactmean_dec),# size = pollen),#col = hilo2), pollen / max_p
#              data = site_halfway_params2_filt, alpha = .7, size = 3)  + 
#   scale_color_continuous(low = "blue", high = "red") + 
#   xlab("") + ylab("") + #theme_few() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   coord_sf(datum=NA) #removes sf induced gridlines
# 
# 
# 
# 
# 
# sms %>% 
#   filter(sms_date > ymd("20/03/20")) %>% 
#   filter(sms_date < ymd("20/06/01")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   ggplot(aes(x = sms_date, y = sms, group = site, color = d2)) + geom_line(lwd = 1.5, alpha =0.5) + theme_bw()+
#   scale_color_viridis_c(labels=as.Date, name = "peak date") +
#   xlab("date") + ylab("surface soil moisture (mm)")
# summary(sms$sms)
# 
# formula <- y ~ x 
# #formula <- y ~ x + I(x^2)
# library(ggpmisc)
# sms %>% 
#   filter(sms_date > ymd("20/04/1")) %>% 
#   filter(sms_date < ymd("20/05/1")) %>% #filter(site == "29.8 -99.9" | site == "30 -99.4") %>% 
#   group_by(site, d, d2) %>% 
#   summarize(sms_mean = mean(sms)) %>% #%T>% => test 
#   ggplot(aes(x = sms_mean, y = d2)) + geom_point() + #geom_smooth(method = "lm", se = FALSE) +
#   geom_smooth(method = "lm", formula = formula, se = FALSE) +
#   stat_poly_eq(aes(label =  paste(stat(rr.label), stat(p.value.label), sep = "*\", \"*")),
#                formula = formula, parse = TRUE, label.x = .9) +
#   theme_bw() + xlab("surface soil moisture (mm)") + ylab("50% pollen released (day)")
# 
# 
# 