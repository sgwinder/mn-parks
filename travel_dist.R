### 
### TPL Travel Distance plots (MN Parks)
###

library(tidyverse)
library(sf)

setwd("~/Documents/TPL/MinnesotaParks/")

# for a very first pass, just using the lat/long pairs for flickr. I assume I can get these for twitter too, 
# but the Cuebiq data at least will require have centroids from the census blocks/tracts.
# That would perhaps be the better method to implement across the board if I can manage it

# copying code in tc_modeling/socmed_euclidean_TC_modeling.R

flickr_homes <- read_csv("pud/flickr-homelocations_bypid_20190827.csv")
key_fi <- read.dbf("GIS/DowntownParksWGS84_pid.dbf", as.is = TRUE)
key_short <- key_fi %>% select(Park_Name, pid)

parks <- read_sf("GIS/DowntownParksWGS84_redo.shp")

# create parks centroid with pids
parks_centroids <- st_centroid(parks) %>% left_join(key_short, by = c("PARK_NAME" = "Park_Name"))
parks_centroids

homes <- flickr_homes %>% arrange(uid) %>% select(lon, lat, pid, uid)
homes_sf <- st_as_sf(homes, 
                     coords = c("lon", "lat"), 
                     crs = "+proj=longlat +datum=WGS84")

# break up by park to save processing time
homes_to_0 <- homes_sf %>% 
  filter(pid == 0) %>%
  mutate(dist = unclass(st_distance(homes_sf %>% filter(pid == 0),
                                    parks_centroids %>% filter(pid == 0))[,1]))

homes_to_1 <- homes_sf %>% 
  filter(pid == 1) %>%
  mutate(dist = unclass(st_distance(homes_sf %>% filter(pid == 1),
                                    parks_centroids %>% filter(pid == 1))[,1]))

homes_to_2 <- homes_sf %>% 
  filter(pid == 2) %>%
  mutate(dist = unclass(st_distance(homes_sf %>% filter(pid == 2),
                                    parks_centroids %>% filter(pid == 2))[,1]))

homes_to_3 <- homes_sf %>% 
  filter(pid == 3) %>%
  mutate(dist = unclass(st_distance(homes_sf %>% filter(pid == 3),
                                    parks_centroids %>% filter(pid == 3))[,1]))

homes_to_4 <- homes_sf %>% 
  filter(pid == 4) %>%
  mutate(dist = unclass(st_distance(homes_sf %>% filter(pid == 4),
                                    parks_centroids %>% filter(pid == 4))[,1]))

homes_to_5 <- homes_sf %>% 
  filter(pid == 5) %>%
  mutate(dist = unclass(st_distance(homes_sf %>% filter(pid == 5),
                                    parks_centroids %>% filter(pid == 5))[,1]))

# put em back together
homes_to_parks <- rbind(homes_to_0, homes_to_1, homes_to_2, homes_to_3, homes_to_4, homes_to_5)
homes_to_parks

# remove spatial component to plot
homes_to_parks_names <- homes_to_parks %>% 
  st_drop_geometry() %>%
  left_join(key_short)

# density plot all together
ggplot(homes_to_parks_names) +
  geom_density(aes(x = dist))

# by park
ggplot(homes_to_parks_names) +
  geom_density(aes(x = dist)) +
  facet_wrap(~Park_Name)

# cumulative?
ggplot(homes_to_parks_names) +
  stat_ecdf(aes(dist)) +
  facet_wrap(~Park_Name) +
  labs(title = "Distance traveled to each park, according to Flickr home locations")

