# This script is to explore whether PlanetScope can pick up differences in tree phenology or pollen cone density
# PlanetScope data was downloaded by Yiluan and emailed to DK on Nov. 16, 2023
# Phenology data comes from measurements by DK in Dec 2019 - Feb 2020 & Dec 2020 - Jan 2021 and associated models
# Pollen cone abundance comes from coarse visual assessments by DK in first field season
# additional comparisons to female trees are available for the Wade site (made by DK in Jan 2023)
# there are also photos of all trees measured during field seasons, so those could work too

#set up work environment
#rm(list=ls())
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(rjags)

### loading data

#load trees from wade jan 23
wade23 <- readr::read_csv("C:/Users/dsk273/Box/texas/pheno/Jan 2023 fieldwork/Wade_pheno_sex_obs_jan23_male_vs_female.csv")

#load planetscope data from Yiluan
ps <- readr::read_csv("C:/Users/dsk273/Box/texas/pheno/juniper_planet_indices_from_yiluan231129.csv")

# ### exporting each tree coordinate and date for Yiluan
# 
# p1920 <- readr::read_csv("C:/Users/dsk273/Box/texas/pheno/manual_obs/pheno_clean_fs19_20_210910.csv") 
# trees_2019_2020 <- p %>% dplyr::select(sample_date = dates, x, y)
# 
# p2021 <- readr::read_csv("C:/Users/dsk273/Box/texas/pheno/manual_obs/pheno_fs20_21_database_210402.csv") 
# trees_2020_2021 <- p %>% dplyr::select(sample_date, x, y)
# 
# trees_dates_coords <- bind_rows(trees_2019_2020, trees_2020_2021) %>% distinct()
# write_csv(trees_dates_coords, here("texas", "pheno",  "tree_dates_coords_220930.csv"))
# 

### compare male and female trees at wade #######################
wade_x_min <- min(wade23$x)
wade_x_max <- max(wade23$x)
wade_y_min <- min(wade23$y)
wade_y_max <- max(wade23$y)

ps_wade <- ps %>% 
  filter(lon > wade_x_min & lon < wade_x_max) %>% 
  filter(lat > wade_y_min & lat < wade_y_max) %>% 
  mutate(x_join = round(lon, 5),
         y_join = round(lat, 5))

wade23_join <- wade23 %>% 
  mutate(x_join = round(x, 5),
         y_join = round(y, 5))

ps_wade23 <- left_join(ps_wade, wade23_join) %>% 
  mutate(xy = paste(x_join, y_join)) %>% 
  filter(!is.na(cone_density))

unique(ps_wade23$xy)

ggplot(wade23_join, aes(x = x_join, y = y_join, color = sex)) + geom_point() + theme_bw()

ps_wade %>% 
  dplyr::select(x_join, y_join) %>% 
  distinct() %>% 
  ggplot(aes(x = x_join, y = y_join)) + geom_point(color = "black", size = 3, shape =3) + theme_bw() +
  geom_point(data = wade23_join, aes(x = x_join, y = y_join, color = sex))

### select trees at Wade ########################################

#get male trees from Wade

#get female trees from wade

#match up with PlanetScope data

#add qualitative cone density for individual trees

#visualize reflectivity over time using Hannah's index as a function of cone density

#visualize reflectivity over time using yellowness as a function of cone density

#visualize reflectivity as a function of cone density

