# This script is for extracting the observations I recorded via the Nature's Notebook NPN app

#prepare workspace
library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)
# library('devtools')
# devtools::install_github("usa-npn/rnpn")
library(rnpn)



### manual download of NPN data via their data portal #########################################################################
# this can easily include fields like the observer ID (mine is: 49712)
# https://www.usanpn.org/data/observational

npn_data <- read_csv("C:/Users/dsk856/Box/texas/pheno/npn_cit_sci/npn_data_downloads/datasheet_210630/status_intensity_observation_data.csv")
str(npn_data)
npn_data_dk <- filter(npn_data, ObservedBy_Person_ID == 49712) %>% 
               filter(Phenophase_Name == "Open pollen cones")

### visualize data
#when did I collect data via Nature's Notebook
npn_data_dk %>% 
  ggplot(aes(x = Observation_Date, y = Intensity_Value, color = Site_Name)) + geom_point() + theme_bw()


#map of Texas with data I collected via Nature's Notebook app
tx_boundary <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = Longitude, y = Latitude), #size = Observation_Date,#col = hilo2), pollen / max_p
             data = npn_data_dk, alpha = .7, size = 2, col = "red")  + #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines

#save data
write_csv(npn_data_dk, "C:/Users/dsk856/Box/texas/pheno/npn_cit_sci/npn_data_downloads/datasheet_210630/status_intensity_observation_data_dk.csv")



### direct download via the rnpn package ###########################################################################
#(although I couldn't see how to get the observer ID there)
npn_direct <- npn_download_status_data(
  request_source = 'Daniel Katz, UT Austin',
  species_ids = 43,
  years = c('2019','2020', '2021'),
  phenophase_ids = c(495, 503)
)

# npn_direct
# str(npn_direct)
# ?npn_download_individual_phenometrics

obs <- npn_direct %>% 
  mutate(obs_date = ymd(observation_date)) %>% 
  filter(obs_date > ymd("2020-12-01")) #2020 - 2021 field season


obs%>% 
  ggplot(aes(x= longitude, y = latitude)) + geom_point()

ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = longitude, y = latitude),# size = pollen),#col = hilo2), pollen / max_p
             data = obs, alpha = .7, size = 2, col = "red")  + #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines

unique(obs$Phenophase_Description)
active_plants <-
  obs %>% 
  #filter(day_of_year > 300) %>% 
  #filter(phenophase_description == "Open pollen cones (conifers)") 
  filter(phenophase_description == "Pollen release (conifers)") 
#filter(intensity_value != "-9999")


ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = longitude, y = latitude, color = intensity_value),# size = pollen),#col = hilo2), pollen / max_p
             data = active_plants, alpha = .7, size = 0.5)  + 
  #scale_color_continuous(low = "blue", high = "red", name = "relative rank") + 
  xlab("") + ylab("") + #theme_few() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) + #removes sf induced gridlines
  facet_wrap(~day_of_year)
# active_plants$day_of_year


#number of trees observed at each site
obs %>% 
  group_by(site_id) %>% 
  select(individual_id) %>% 
  distinct() %>% 
  count() %>% 
  print(n = 100) #%>% 
# ungroup() %>% 
# summarize(n_total = sum(n))

#graph of open cones for each tree by site
obs %>% 
  filter(phenophase_description == "Open pollen cones (conifers)") %>% 
  select(site_id, individual_id, obs_date, intensity_value) %>% 
  mutate(intensity = forcats::fct_relevel(intensity_value, "-9999", "Less than 5%", "5-24%", "25-49%", "50-74%",
                                          "75-94%", "95% or more" )) %>% 
  ggplot(aes(x = obs_date, y = intensity, color = individual_id)) + geom_point() + geom_line() + theme_bw() +
  facet_wrap(~site_id)

unique(obs$intensity_value)

#graph of pollen release for each tree by site
obs %>% 
  filter(phenophase_description == "Pollen release (conifers)") %>% 
  #select(site_id, individual_id, obs_date, intensity_value) %>% 
  mutate(intensity = forcats::fct_relevel(intensity_value, "-9999", "Little", "Some", "Lots" )) %>% 
  ggplot(aes(x = obs_date, y = intensity, group = individual_id, color = latitude )) + 
  geom_point() + geom_line() + theme_bw() +
  facet_wrap(~site_id)




