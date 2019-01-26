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
NBHD_Acres <- NBHDs %>% select(ACRES,geometry)
NBHD_Acres %>% ggplot(aes(fill = ACRES)) + geom_sf() + scale_fill_viridis_c() + ##This scale fill is better than the standard
  theme_bw() 

#########
##Let's bring this together with Census data
Balt_Census <- read_sf(dsn = "Census", layer = "2010_Census_Profile_by_Neighborhood_Statistical_Areas")

##Look at Raw Population
NBHD_pop <- Balt_Census %>% select(Population, geometry)
NBHD_pop %>% ggplot(aes(fill = Population)) + geom_sf() + scale_fill_viridis_c() + 
  theme_bw() + 
  labs(title = "Neighborhood Population")

