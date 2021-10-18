#J. ashei ripening vs airborne juniper pollen in Texas


library(readr)
library(dplyr)
library(tidyr)
library(imputeTS)
library(ggpmisc)
library(lubridate)

### NAB data in Texas ############################################################
# load in NAB data 
nab <- readr::read_csv("C:/Users/dsk856/Box/texas/NAB/NAB_pollen_modeled_linear_interp_201002.csv") %>%
  filter(NAB_station != "Houston") %>%
  filter(NAB_station != "College Station") %>%
  filter(NAB_station != "Dallas") %>%
  filter(NAB_station != "Flower Mound") %>%
  filter(mo < 4 | mo > 11) %>%
  mutate(day_exp = case_when(mo < 4 ~ yday(date) + 21,
                             mo > 10 ~ yday(date) -  yday(dmy(paste(10, 12, year, sep = "/")))),
         yr_exp = case_when(mo < 4 ~ paste0("yr_", (year(date) -1 ), "_", year(date)),
                            mo > 10 ~ paste0("yr_", year(date), "_", (year(date) + 1))))
           
    
#detect low data years 
n_na <- nab %>%
  group_by(yr_exp, NAB_station) %>%
  filter(day_exp > 10 & day_exp < 60) %>%
  filter(!is.na(Cupressaceae))%>%
  summarize(n_non_NA = n(),
            n_NA = 49 - n_non_NA)

nab <- left_join(nab, n_na)

nab %>% 
  filter(n_NA < 20) %>%
  ggplot(aes(x = day_exp, y = Cupressaceae + 1)) + geom_point() + theme_bw() + facet_grid(yr_exp~NAB_station) +
  scale_y_log10() + 
  geom_line(aes(x = day_exp, y = Cupressaceae_m + 1), color = "red", lwd = 0.5)


#filter out low data years
nab_low_na <- nab %>% filter(n_NA < 20)


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

ggplot(nab_low_na, aes(x = day_exp, y = Cupressaceae_m + 1, color = cup_m_cumu_dist)) + geom_line() + theme_bw() + facet_grid(yr_exp~NAB_station) +
  scale_y_log10() + scale_color_viridis_c()

ggplot(nab_low_na, aes(x = day_exp, y = cup_m_cumu, color = cup_m_cumu_dist)) + geom_line() + theme_bw() + facet_grid(yr_exp~NAB_station) +
  scale_y_log10() + scale_color_viridis_c()


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
  scale_y_log10() + scale_color_viridis_c() +
  geom_point(data = nearest_25, aes(x= day_exp, y = Cupressaceae_m + 1 ), col = "red") +
  geom_point(data = nearest_50, aes(x= day_exp, y = Cupressaceae_m + 1 ), col = "red") +
  geom_point(data = nearest_75, aes(x= day_exp, y = Cupressaceae_m + 1 ), col = "red") 
