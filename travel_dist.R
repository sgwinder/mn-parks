### 
### TPL Travel Distance plots (MN Parks)
###

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


setwd("~/Documents/TPL/MinnesotaParks/")

# for a very first pass, just using the lat/long pairs for flickr. I assume I can get these for twitter too, 
# but the Cuebiq data at least will require have centroids from the census blocks/tracts.
# That would perhaps be the better method to implement across the board if I can manage it

# copying code in tc_modeling/socmed_euclidean_TC_modeling.R

flickr_homes <- read_csv("pud/flickr-homelocations_bypid_20190827.csv")
key_fi <- read.dbf("GIS/DowntownParksWGS84_pid.dbf", as.is = TRUE)
key_short <- key_fi %>% select(Park_Name, pid)

parks <- read_sf("GIS/DowntownParksWGS84_redo.shp")

# let's reproject to something in meters
# matt uses conus albers equal epsg:5070. he wouldn't use it for homes in alaska
parks_5070 <- st_transform(parks, crs = 5070)


# create parks centroid with pids
parks_centroids <- st_centroid(parks_5070) %>% left_join(key_short, by = c("PARK_NAME" = "Park_Name"))
parks_centroids

homes <- flickr_homes %>% arrange(uid) %>% select(lon, lat, pid, uid)
homes_sf <- st_as_sf(homes, 
                     coords = c("lon", "lat"), 
                     crs = "+proj=longlat +datum=WGS84")
homes_5070 <- st_transform(homes_sf, crs = 5070)


# cut out anything outside the continental US
usa <- ne_states(country = "United States of America", returnclass = "sf")
continental <- usa %>% filter(!postal %in% c("HI", "AK")) %>% st_transform(crs = 5070)

ggplot() +
  geom_sf(data = continental) +
  geom_sf(data = homes_us)

homes_us <- st_crop(homes_5070, continental)

# break up by park to save processing time
homes_to_0 <- homes_us %>% 
  filter(pid == 0) %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(pid == 0),
                                    parks_centroids %>% filter(pid == 0))[,1]))

homes_to_1 <- homes_us %>% 
  filter(pid == 1) %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(pid == 1),
                                    parks_centroids %>% filter(pid == 1))[,1]))

homes_to_2 <- homes_us %>% 
  filter(pid == 2) %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(pid == 2),
                                    parks_centroids %>% filter(pid == 2))[,1]))

homes_to_3 <- homes_us %>% 
  filter(pid == 3) %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(pid == 3),
                                    parks_centroids %>% filter(pid == 3))[,1]))

homes_to_4 <- homes_us %>% 
  filter(pid == 4) %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(pid == 4),
                                    parks_centroids %>% filter(pid == 4))[,1]))

homes_to_5 <- homes_us %>% 
  filter(pid == 5) %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(pid == 5),
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
  xlab("Distance (m)") +
  labs(title = "Distance traveled to each park, according to Flickr home locations")

flickr_dists <- homes_to_parks_names %>%
  select(-pid, -uid) %>%
  mutate(source = "Flickr")

###########################################
# Ok. adding in the Cuebiq data
cuebiq_demo <- read_excel("CellData/TravelDistanceAndDemographics.xlsx", sheet = "ForAnalysis", skip = 1)
cuebiq_demo

# create a joinkey of park names
parks <- tibble(Park_Name = key_short$Park_Name,
                Park_short = str_extract(Park_Name, ".+(?=\\s)"))

# to match the flickr data, i just need one row per park per user
# start by cutting down the df, then use the tidy "uncount"
cuebiq_demo %>% group_by(`State of BG`) %>% summarise()

cuebiq_to_parks <- cuebiq_demo %>% 
  filter(!`State of BG` %in% c("Hawaii", "Alaska")) %>%
  select(Park, visitors = `Number of unique devices from this block group seen on this day in this park`,
         dist = `Distance to Parks (Meters)`) %>%
  uncount(visitors) %>%
  right_join(parks, by = c("Park" = "Park_short")) %>%
  select(-Park) %>%
  mutate(source = "CUEBIQ")
cuebiq_to_parks

# bind to flickr
dists_c_f <- cuebiq_to_parks %>%
  bind_rows(flickr_dists)

ggplot(dists_c_f) +
  stat_ecdf(aes(dist, col = source)) +
  facet_wrap(~Park_Name) +
  ylab("Cumulative Probability") +
  scale_x_continuous(breaks = seq(0, 4e+06, length.out = 5),
                     labels = seq(0, 4000, length.out = 5), 
                     name = "Distance (km)") +
  labs(title = "Distance traveled to each park (from Continental US)") +
  #coord_cartesian(xlim = c(0, 50000)) +
  theme_bw()

# write it out
#ggsave("mn-parks/figs/distance_by_dataset.png", width = 8, heigh = 6, units = "in")

### TODO: Add Twitter (need to calculate distances from census centroids, or go back and find raw home locs)
## Also, Carrie's paper used a Mann-Whitney U-Test to test for sig differences between the distributions
##  by park. Would be fun to try
