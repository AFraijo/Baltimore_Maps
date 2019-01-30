##Inital Baltimore Map
##Author: Andrew Fraijo

library(dplyr)
library(sf)
library(ggplot2)
library(tmap)
library(leaflet)

##City_line <- st_read(dsn = "Baltcity_20Line", layer = "baltcity_line", quit = TRUE) %>% 
##  st_transform(2154)


City_line <- read_sf(dsn = "Baltcity_20Line", layer = "baltcity_line") 
plot(City_line$geometry)


NBHDs <- read_sf(dsn = "Neighborhood", layer = "nhood_2010")
plot(NBHDs) ##This is weird, it gives 6 different maps with colors
plot(NBHDs$geometry) ##This gives one map, not color

##Try subsetting the data
NBHD_Labels <- NBHDs %>% select(LABEL, geometry)
plot(NBHD_Labels) ##Better except for weird title

##Fancy GGPLOT time!
NBHD_Acres <- NBHDs %>% select(LABEL, ACRES, geometry)
NBHD_Acres %>% ggplot(aes(fill = ACRES)) + geom_sf() + scale_fill_viridis_c() + ##This scale fill is better than the standard
  theme_bw() +
  labs(title = "Neighborhood Size")

#########
##Let's bring this together with Census data
Balt_Census <- read_sf(dsn = "Census", layer = "2010_Census_Profile_by_Neighborhood_Statistical_Areas")

##Look at Raw Population
NBHD_pop <- as.data.frame(Balt_Census) %>% select(-geometry) ##geometries are sticky, you have to change to data.frame
NBHD_pop <- left_join(NBHD_Acres, NBHD_pop, by = c("LABEL" = "Name"))
NBHD_pop %>% ggplot(aes(fill = Population)) + geom_sf() + scale_fill_viridis_c() + 
  theme_bw() + 
  labs(title = "Neighborhood Population")
##Try Density
NBHD_pop <- NBHD_pop %>% mutate(Density = Population/ACRES)
NBHD_pop %>% ggplot(aes(fill = Density)) + geom_sf() + scale_fill_viridis_c() + 
  theme_bw() + 
  labs(title = "Neighborhood Density",
       subtitle = "People Per Acre")

##Let's see if we can identify the "White L" from the data
NBHD_pop <- NBHD_pop %>% mutate(Perc_White = round((White/Population)*100, digits = 2))
NBHD_pop %>% ggplot(aes(fill = Perc_White)) + geom_sf() + scale_fill_viridis_c() + 
  theme_bw() + 
  labs(title = "Neighborhood Majority",
       subtitle = "Percentage of White People")

NBHD_pop <- NBHD_pop %>% 
  mutate(Majority = if_else((White/Population)>=.5, "White",
                 if_else((Blk_AfAm/Population) >= .5, "Black", 
                         if_else(Population == 0.0, "No Population", "Other"),"Other")))


NBHD_pop %>% ggplot(aes(fill = Majority)) + 
  geom_sf() + scale_fill_viridis_d() + 
  theme_bw() + 
  labs(title = "Neighborhood Majority")
