# This script is for analyzing Pollen Trackers data 
#prepare workspace
library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(raster)
library(lubridate)
# devtools::install_github("usa-npn/rnpn")
library(rnpn)

#rm(list=ls())


### manual download of NPN data via their data portal #########################################################################

npn_direct_raw <- npn_download_status_data( #a saved version from october 2021 is available
  request_source = 'Daniel Katz, Cornell',
  species_ids = 43, #Juniperus ashei
  years = c(as.character(2008:2022)), #years to include
  phenophase_ids = c(495, 503) #angiosperms: 501 == "Open flowers", 502 == "Pollen release (flowers)" #conifers: 495 ==  503 ==
)

npn_direct <- npn_direct_raw %>%
  mutate(observation_date = ymd(observation_date),
         doy = yday(observation_date),
         day_experiment = case_when(doy > 180 ~ doy - yday(mdy("12-10-2020")) ,
                                    doy < 180 ~ doy + (yday(mdy("12-31-2020")) - yday(mdy("12-10-2020")))),
         year_experiment = case_when(doy > 180 ~ paste0(year(observation_date), " - ", (year(observation_date) + 1)),
                                     doy < 180 ~ paste0(year(observation_date)  - 1 , " - ", year(observation_date))),
         date_figs = day_experiment + mdy("12-10-2020"),
         pollen_cones_open = case_when(
              phenophase_status == 0 & phenophase_id == 495 ~ 0,
              intensity_value == "Less than 5%" & phenophase_id == 495 ~ 0.025,
              intensity_value ==  "5-24%" & phenophase_id == 495 ~ (0.05+0.24)/2,
              intensity_value == "25-49%" & phenophase_id == 495 ~ (0.25+0.49)/2,
              intensity_value == "50-74%" & phenophase_id == 495 ~ (0.5+0.74)/2,
              intensity_value == "75-94%" & phenophase_id == 495 ~ (0.75+0.94)/2,
              intensity_value == "95% or more" & phenophase_id == 495 ~ 0.97,
              phenophase_status == 1 & phenophase_id == 495 ~ -99),
         pollen_release = case_when(
              phenophase_status == 0 & phenophase_id == 503 ~ "none",
              intensity_value == "Little" & phenophase_id == 503 ~ "little",
              intensity_value == "Some" & phenophase_id == 503 ~ "some",
              intensity_value == "Lots" & phenophase_id == 503 ~ "lots",
              phenophase_status == 1 & intensity_value == -9999 & phenophase_id == 503 ~ "some")
        ) %>%
  mutate(pollen_release = forcats::fct_relevel(pollen_release, "none", "little", "some", "lots")) %>%
  filter(longitude < -95) %>%
  filter(year_experiment == "2019 - 2020" | year_experiment == "2020 - 2021" | year_experiment == "2021 - 2022") %>%
  filter(doy > 330 | doy < 70) %>%
  filter(pollen_cones_open != -99 | is.na(pollen_cones_open))  #removes 70 entries

npn_direct_site_n <- npn_direct %>%
  filter(phenophase_description == "Open pollen cones (conifers)") %>%
  group_by(site_id, individual_id) %>%
  summarize(n_obs_tree = n())

npn_direct <- left_join(npn_direct, npn_direct_site_n)

#save data
#write_csv(npn_direct, "C:/Users/dsk856/Box/texas/pheno/npn_cit_sci/npn_data_downloads/JA_npn_data_211027.csv")
#write_csv(npn_direct, "C:/Users/danka/Box/texas/pheno/npn_cit_sci/npn_data_downloads/JA_npn_data_220214.csv")
#npn_direct <- read_csv("C:/Users/dsk856/Box/texas/pheno/npn_cit_sci/npn_data_downloads/JA_npn_data_211027.csv")


length(unique(npn_direct$individual_id))

# visualize pollen cones opening data
npn_direct %>% 
#  filter(year_experiment == "2018 - 2019") %>%
  filter(phenophase_description == "Open pollen cones (conifers)") %>% #hist(test$pollen_cones_open)
  ggplot(aes(x = day_experiment, y = pollen_cones_open, color = year_experiment)) + geom_point() + theme_bw()

npn_direct %>% 
  filter(phenophase_description == "Open pollen cones (conifers)") %>% #hist(test$pollen_cones_open)
  ggplot(aes(x = day_experiment, y = pollen_cones_open, color = year_experiment, group = individual_id)) + geom_line() + theme_bw() + facet_wrap(~site_id)

#visualize pollen release data 
#one site for an example
npn_direct %>%  
  filter(site_id == "36353") %>% #some sites to check: #40638, 40808, 
  filter(!is.na(pollen_release)) %>%
  filter(year_experiment == "2020 - 2021") %>%
  filter(phenophase_description == "Pollen release (conifers)")  %>% #hist(test$pollen_cones_open)
  ggplot(aes(x = date_figs, y = pollen_release, color = year_experiment, group = individual_id)) + 
  geom_jitter(width = 0, height = 0.01, alpha = 1) +
  geom_line(alpha = 0.8) + ggthemes::theme_few() + ylab("pollen release category") + xlab("date") + scale_color_discrete(name = "year") + 
  scale_x_date(date_labels = "%b %d") + facet_wrap(~site_id) 

#pollen release at all site
npn_direct %>% 
  filter(!is.na(pollen_release)) %>%
  filter(year_experiment == "2020 - 2021") %>%
  filter(phenophase_description == "Pollen release (conifers)")  %>% #hist(test$pollen_cones_open)
  ggplot(aes(x = date_figs, y = pollen_release, color = year_experiment, group = individual_id)) + 
  geom_jitter(width = 0, height = 0.05, alpha = .3) +
  geom_line(alpha = 0.1) + ggthemes::theme_few() + ylab("pollen release category") + xlab("date") + scale_color_discrete(name = "year") + 
  scale_x_date(date_labels = "%b %d") + facet_wrap(~site_id) 


#map of Texas with data I collected via Nature's Notebook app
tx_boundary <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = longitude, y = latitude, color = year_experiment), #size = Observation_Date,#col = hilo2), pollen / max_p
             data = npn_direct, alpha = .05, size = 2)  + #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  scale_color_discrete(name = "year") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  facet_wrap(~year_experiment) +
  coord_sf(datum=NA) #removes sf induced gridlines

#date of maximum release on map
npn_direct_max <- npn_direct %>% 
  filter(phenophase_description == "Pollen release (conifers)") %>%
  #filter(phenophase_description == "Open pollen cones (conifers)") %>%
  filter(site_id != 42205) %>% 
  group_by(site_id, individual_id, latitude, longitude) %>% 
  filter(pollen_release == "lots") %>% 
  summarize(date_max_release = mean(observation_date),
            day_experiment_max_release = mean(day_experiment))
tx_boundary <- read_sf("C:/Users/danka/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = longitude, y = latitude, color = date_max_release), #size = Observation_Date,#col = hilo2), pollen / max_p
             data = npn_direct_max, alpha = .95, size = 2)  + #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  scale_color_viridis_c(name = "date of maximum release", trans = "date") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # facet_wrap(~year_experiment) +
  coord_sf(datum=NA) #removes sf induced gridlines



#number of open pollen cone observations
npn_direct %>%
  filter(phenophase_description == "Pollen release (conifers)") %>%
  #filter(phenophase_description == "Open pollen cones (conifers)") %>%
  group_by(year_experiment) %>%
  summarize(n = n())


#number of trees observed at each site
npn_direct %>% 
  group_by(site_id) %>% 
  select(individual_id) %>% 
  distinct() %>% 
  count() %>% 
  print(n = 100) %>% 
  ggplot(aes(x = n)) + geom_histogram() + theme_bw() + xlab("number of trees per site")
# ungroup() %>% 
# summarize(n_total = sum(n))




### getting the peak day of observed pollen release for each tree 
#untrustworthy_sites <- c(36313)
#maybe_sites(40638, 40808, )
trustworthy_sites <- c(36154, 36197, 36212, 36243, 36263, 36292, 36306, 36353, 36361, 36405, 36413, 36415, 36487, 40808,
                       40863, 41599, 41689, 41690, 41703, 41704, 41726, 41803, 41808, 42078, 42109, 42110, 42128)
peak_release <- npn_direct %>%
  filter(phenophase_description == "Pollen release (conifers)")  %>%
  mutate(pollen_release_n = case_when(pollen_release == "none" ~ 0,
                                      pollen_release == "little" ~ 0.1,
                                      pollen_release == "some" ~ 0.25,
                                      pollen_release == "lots" ~ 1)) %>%
  arrange(individual_id, year_experiment, day_experiment) %>%
  group_by(individual_id, year_experiment, latitude, longitude, site_id, n_obs_tree) %>% #-> test 
  slice_max(order_by = pollen_release_n, n = 1)  %>%
  summarize(day_experiment = mean(day_experiment),
            pollen_release_n = mean(pollen_release_n)) %>%
  ungroup() %>% #-> test
  filter(day_experiment > 0 & day_experiment < 65)  %>%
  filter(pollen_release_n != 0) %>% #-> test
  filter(pollen_release_n != 0.1) %>% #-> test
  #filter(n_obs_tree > 1) %>% 
  filter(site_id %in% trustworthy_sites) 
  
#hist(peak_release$day_experiment)

peak_release_19_20 <- filter(peak_release, year_experiment == "2019 - 2020")
peak_release_19_20_sf <- st_as_sf(peak_release_19_20, coords = c("longitude", "latitude"))
                                  
peak_release_20_21 <- filter(peak_release, year_experiment == "2020 - 2021") 
  # mutate(latitude = round(latitude, 1),
  #        longitude = round(longitude, 1))
peak_release_20_21_sf <- st_as_sf(peak_release_20_21, coords = c("longitude", "latitude")) #plot(peak_release_20_21_sf)

# peak_release %>%
#   ggplot(aes(x = day_experiment, y = pollen_release_n, group = individual_id)) + geom_point() + geom_line() + facet_wrap(~year_experiment)


#map of Texas with data I collected via Nature's Notebook app
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = longitude, y = latitude, color = day_experiment + mdy("12-10-20")), #size = Observation_Date,#col = hilo2), pollen / max_p
             data = peak_release_20_21, alpha = .85, size = 3)  + #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  scale_color_viridis_c(name = "day of year", trans = "date") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  facet_wrap(~year_experiment) +
  coord_sf(datum=NA) #removes sf induced gridlines

#extract soil moisture in April for 2019-2020
ssm_2019_april <- raster("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_tx_downloads/SMAP_ssm_TX_2019_april.tif")
test2 <- raster::extract(ssm_2019_april, peak_release_19_20_sf)
peak_release_19_20 <- peak_release_19_20 %>% mutate(sm_ap = test2)
ggplot(peak_release_19_20, aes(x = sm_ap, y = day_experiment)) + geom_point() + theme_bw() +
  geom_smooth(method = "lm")


#extract soil moisture in April for 2020-2021
ssm_2020_april <- raster("C:/Users/dsk856/Box/texas/pheno/met_data/GEE_pheno_tx_downloads/SMAP_ssm_TX_2020_april.tif")
test2 <- raster::extract(ssm_2020_april, peak_release_20_21_sf)
peak_release_20_21 <- peak_release_20_21 %>% mutate(sm_ap = test2)
peak_release_20_21_sf <- peak_release_20_21_sf %>% mutate(sm_ap = test2)
st_write(peak_release_20_21, "C:/Users/dsk856/Box/texas/pheno/npn_cit_sci/fs20_21_peak_pollen_sites.shp")
ggplot(peak_release_20_21, aes(x = sm_ap, y = day_experiment + mdy("12-10-2020"))) + geom_point() + theme_bw() +
  geom_smooth(method = "lm") + xlab("soil moisture (mm/mm)") + ylab ("peak pollen release (day)")
fit <- lm(day_experiment ~ sm_ap, data = peak_release_20_21)
summary(fit)


### mid-season download for 2021-2022 #########################################################
npn_direct_raw <- npn_download_status_data( #a saved version from october 2021 is available
  request_source = 'Daniel Katz, UT Austin',
  species_ids = 43, #Juniperus ashei
  years = c(as.character(2021:2022)), #years to include
  phenophase_ids = c(495, 503) #angiosperms: 501 == "Open flowers", 502 == "Pollen release (flowers)" #conifers: 495 ==  503 ==
)

npn_direct <- npn_direct_raw %>%
  mutate(observation_date = ymd(observation_date),
         doy = yday(observation_date),
         day_experiment = case_when(doy > 180 ~ doy - yday(mdy("12-10-2020")) ,
                                    doy < 180 ~ doy + (yday(mdy("12-31-2020")) - yday(mdy("12-10-2020")))),
         year_experiment = case_when(doy > 180 ~ paste0(year(observation_date), " - ", (year(observation_date) + 1)),
                                     doy < 180 ~ paste0(year(observation_date)  - 1 , " - ", year(observation_date))),
         date_figs = day_experiment + mdy("12-10-2020"),
         pollen_cones_open = case_when(
           phenophase_status == 0 & phenophase_id == 495 ~ 0,
           intensity_value == "Less than 5%" & phenophase_id == 495 ~ 0.025,
           intensity_value ==  "5-24%" & phenophase_id == 495 ~ (0.05+0.24)/2,
           intensity_value == "25-49%" & phenophase_id == 495 ~ (0.25+0.49)/2,
           intensity_value == "50-74%" & phenophase_id == 495 ~ (0.5+0.74)/2,
           intensity_value == "75-94%" & phenophase_id == 495 ~ (0.75+0.94)/2,
           intensity_value == "95% or more" & phenophase_id == 495 ~ 0.97,
           phenophase_status == 1 & phenophase_id == 495 ~ -99),
         pollen_release = case_when(
           phenophase_status == 0 & phenophase_id == 503 ~ "none",
           intensity_value == "Little" & phenophase_id == 503 ~ "little",
           intensity_value == "Some" & phenophase_id == 503 ~ "some",
           intensity_value == "Lots" & phenophase_id == 503 ~ "lots",
           phenophase_status == 1 & intensity_value == -9999 & phenophase_id == 503 ~ "some")
  ) %>%
  mutate(pollen_release = forcats::fct_relevel(pollen_release, "none", "little", "some", "lots")) %>%
  filter(longitude < -95) %>%
  filter(year_experiment == "2021 - 2022" ) %>% #| year_experiment == "2020 - 2021"
  filter(doy > 330 | doy < 70) %>%
  filter(pollen_cones_open != -99 | is.na(pollen_cones_open))  #removes 70 entries

npn_direct_site_n <- npn_direct %>%
  filter(phenophase_description == "Open pollen cones (conifers)") %>%
  group_by(site_id, individual_id) %>%
  summarize(n_obs_tree = n())

npn_direct <- left_join(npn_direct, npn_direct_site_n)

length(unique(npn_direct$individual_id))

# visualize pollen cones opening data
npn_direct %>% 
  #  filter(year_experiment == "2018 - 2019") %>%
  filter(phenophase_description == "Open pollen cones (conifers)") %>% #hist(test$pollen_cones_open)
  ggplot(aes(x = observation_date, y = pollen_cones_open, color = year_experiment)) + geom_point() + theme_bw()

npn_direct %>% 
  filter(phenophase_description == "Open pollen cones (conifers)") %>% #hist(test$pollen_cones_open)
  ggplot(aes(x = observation_date, y = pollen_cones_open, color = year_experiment, group = individual_id)) + geom_line() + geom_point()+
  theme_bw() + facet_wrap(~site_id)

#visualize pollen release data 
#one site for an example
# npn_direct %>%  
# #  filter(site_id == "36353") %>% #some sites to check: #40638, 40808, 
#   filter(!is.na(pollen_release)) %>%
#   filter(year_experiment == "2021 - 2022") %>%
#   filter(phenophase_description == "Pollen release (conifers)")  %>% #hist(test$pollen_cones_open)
#   ggplot(aes(x = date_figs, y = pollen_release, color = year_experiment, group = individual_id)) + 
#   geom_jitter(width = 0, height = 0.01, alpha = 1) +
#   geom_line(alpha = 0.8) + ggthemes::theme_few() + ylab("pollen release category") + xlab("date") + scale_color_discrete(name = "year") + 
#   scale_x_date(date_labels = "%b %d") + facet_wrap(~site_id) 

#pollen release at all site
npn_direct %>% 
  filter(!is.na(pollen_release)) %>%
  filter(year_experiment == "2021 - 2022") %>%
  filter(phenophase_description == "Pollen release (conifers)")  %>% #hist(test$pollen_cones_open)
  ggplot(aes(x = date_figs, y = pollen_release, color = year_experiment, group = individual_id)) + 
  geom_jitter(width = 0, height = 0.05, alpha = .3) +
  geom_line(alpha = 0.1) + ggthemes::theme_few() + ylab("pollen release category") + xlab("date") + scale_color_discrete(name = "year") + 
  scale_x_date(date_labels = "%b %d") + facet_wrap(~site_id) 


#map of Texas with data I collected via Nature's Notebook app
tx_boundary <- read_sf("C:/Users/danka/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = longitude, y = latitude, color = year_experiment), #size = Observation_Date,#col = hilo2), pollen / max_p
             data = npn_direct, alpha = .05, size = 2)  + #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  scale_color_discrete(name = "year") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  facet_wrap(~year_experiment) +
  coord_sf(datum=NA) #removes sf induced gridlines


#date of maximum release on map
npn_direct_max <- npn_direct %>% 
  filter(phenophase_description == "Pollen release (conifers)") %>%
  #filter(phenophase_description == "Open pollen cones (conifers)") %>%
  filter(site_id != 42205) %>% 
  group_by(site_id, individual_id, latitude, longitude) %>% 
  filter(pollen_release == "lots") %>% 
  summarize(date_max_release = mean(observation_date),
            day_experiment_max_release = mean(day_experiment)) %>% 
  ungroup()
tx_boundary <- read_sf("C:/Users/danka/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
ggplot(tx_boundary) +   #geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_text(aes(x = longitude, y = latitude, color = date_max_release, label = site_id), #size = Observation_Date,#col = hilo2), pollen / max_p
             data = npn_direct_max, alpha = .95, size = 2)  + #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  scale_color_viridis_c(name = "date of maximum release", trans = "date") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # facet_wrap(~year_experiment) +
  coord_sf(datum=NA) #removes sf induced gridlines

  #compare date of maximum release with raw data
npn_direct %>% 
  filter(!is.na(pollen_release)) %>%
  filter(year_experiment == "2021 - 2022") %>%
  filter(phenophase_description == "Pollen release (conifers)")  %>% #hist(test$pollen_cones_open)
  ggplot(aes(x = observation_date, y = pollen_release, color = year_experiment, group = individual_id)) + 
  geom_jitter(width = 0, height = 0.05, alpha = .3) +
  geom_line(alpha = 0.1) + ggthemes::theme_few() + ylab("pollen release category") + xlab("date") + scale_color_discrete(name = "year") + 
  scale_x_date(date_labels = "%b %d") + facet_wrap(~site_id) +
  geom_point(data = npn_direct_max, aes(x = date_max_release, y = "lots"), color = "black")


#number of open pollen cone observations
npn_direct %>%
  filter(phenophase_description == "Pollen release (conifers)") %>%
  #filter(phenophase_description == "Open pollen cones (conifers)") %>%
  group_by(year_experiment) %>%
  summarize(n = n())


#number of trees observed at each site
npn_direct %>% 
  group_by(site_id) %>% 
  dplyr::select(individual_id) %>% 
  distinct() %>% 
  count() %>% 
  print(n = 100) %>% 
  ggplot(aes(x = n)) + geom_histogram() + theme_bw() + xlab("number of trees per site")
# ungroup() %>% 
# summarize(n_total = sum(n))


#compare to SSM
npn_direct_max_sf <- st_as_sf(npn_direct_max, coords = c("longitude", "latitude"))
ssm_2021_april <- raster("C:/Users/danka/Box/texas/pheno/met_data/GEE_pheno_tx_downloads/SMAP_ssm_TX_2020_april.tif")
test2 <- raster::extract(ssm_2021_april, npn_direct_max_sf)
npn_direct_max <- npn_direct_max %>% mutate(sm_ap = test2)

peak_release_21_22_sf <- npn_direct_max_sf %>% mutate(sm_ap = test2)
#st_write(npn_direct_max, "C:/Users/dsk856/Box/texas/pheno/npn_cit_sci/fs21_22_peak_pollen_sites.shp")
ggplot(npn_direct_max, aes(x = sm_ap, y = day_experiment_max_release + mdy("12-10-2020"), label = site_id)) + 
  geom_text() + theme_bw() +
  geom_smooth(method = "lm") + xlab("soil moisture (mm/mm)") + ylab ("peak pollen release (day)")
fit <- lm(day_experiment ~ sm_ap, data = npn_direct_max)
summary(fit)

