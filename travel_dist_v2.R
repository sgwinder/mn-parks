## 
## Making a new travel_dist script, using census tract centroids for cell data
## 10/8/20
##

library(tidyverse)
library(sf)
library(foreign)

setwd("~/Documents/TPL/MinnesotaParks/")

twitter_homes <- read_csv("MN_parks_twitter/homes_by_site_census_20191106.csv")
twitter_homes

flickr_homes <- read_csv("MN_parks_flickr/homes_by_site_census_20191011.csv")

# creating pid key
key_fi <- read.dbf("GIS/DowntownParksWGS84_pid.dbf", as.is = TRUE)
key_t <- read.dbf("GIS/DowntownParksWGS84_redo_pid.dbf", as.is = TRUE)
# we have two different pid keys. I think that the `redo` file was used for Twitter only, but am awaiting confirmation from SAW

# for mapping, let's read in the cleaner shapefile, without a confusing pid
parks <- read_sf("GIS/DowntownParksWGS84_redo.shp")

# creating a single key
key <- key_fi %>% 
  select(Park_Name, SOPARCID, pid_fi = pid) %>%
  left_join(key_t %>% 
              select(PARK_NAME, SOPARCID, pid_t = pid),
            by = c("SOPARCID", "Park_Name" = "PARK_NAME")) %>%
  select(-SOPARCID)
key

# renaming social media to fit
flickr_homes_r <- flickr_homes %>%
  rename(pid_fi = pid) %>%
  mutate(source = "flickr")

twitter_homes_r <- twitter_homes %>%
  rename(pid_t = pid) %>%
  mutate(source = "twitter")

flickr_homes_parks <- flickr_homes_r  %>% left_join(key) %>% select(-pid_fi, -pid_t)
twitter_homes_parks <- twitter_homes_r  %>% left_join(key) %>% select(-pid_fi, -pid_t)

socmed_homes <- bind_rows(flickr_homes_parks, twitter_homes_parks)

### Ok. Make them spatial (for tracts)
tract_centroids <- read_sf("~/Documents/GIS/Census/Created/cb_2019_us_tract_500k_centroids.shp")
tract_centroids

# make a joinkey
tracts_tj <- tract_centroids %>%
  mutate(tract = paste0(STATEFP, COUNTYFP, TRACTCE))

# add geometry to twitter homes, and drop homes w/o tracts, as well as AK, HI, PR
socmed_homes_sf <- tracts_tj %>%
  dplyr::select(tract, geometry) %>%
  st_transform(crs = 5070) %>%
  right_join(socmed_homes) %>%
  filter(!is.na(tract), !state %in% c("Alaska", "Hawaii", "Puerto Rico"))

socmed_homes_sf

###### 
# let's reproject to something in meters
# matt uses conus albers equal epsg:5070. he wouldn't use it for homes in alaska
parks_5070 <- st_transform(parks, crs = 5070)
parks_5070

# create parks centroid with pids
parks_centroids <- st_centroid(parks_5070) %>% rename(Park_Name = PARK_NAME)
parks_centroids

### Now, start copying over from line 51 of 'travel_dist.r' 
homes_us <- socmed_homes_sf
homes_us %>% group_by(Park_Name) %>% summarise()

# break up by park to save processing time
homes_to_0 <- homes_us %>% 
  filter(Park_Name == "Culture Park") %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(Park_Name == "Culture Park"),
                                    parks_centroids %>% filter(Park_Name == "Culture Park"))[,1]))

homes_to_1 <- homes_us %>% 
  filter(Park_Name == "Ecolab Plaza") %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(Park_Name == "Ecolab Plaza"),
                                    parks_centroids %>% filter(Park_Name == "Ecolab Plaza"))[,1]))

homes_to_2 <- homes_us %>% 
  filter(Park_Name == "Kellogg Mall") %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(Park_Name == "Kellogg Mall"),
                                    parks_centroids %>% filter(Park_Name == "Kellogg Mall"))[,1]))

homes_to_3 <- homes_us %>% 
  filter(Park_Name == "Landmark Plaza") %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(Park_Name == "Landmark Plaza"),
                                    parks_centroids %>% filter(Park_Name == "Landmark Plaza"))[,1]))

homes_to_4 <- homes_us %>% 
  filter(Park_Name == "Mears Park") %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(Park_Name == "Mears Park"),
                                    parks_centroids %>% filter(Park_Name == "Mears Park"))[,1]))

homes_to_5 <- homes_us %>% 
  filter(Park_Name == "Rice Park") %>%
  mutate(dist = unclass(st_distance(homes_us %>% filter(Park_Name == "Rice Park"),
                                    parks_centroids %>% filter(Park_Name == "Rice Park"))[,1]))

# put em back together
homes_to_parks <- rbind(homes_to_0, homes_to_1, homes_to_2, homes_to_3, homes_to_4, homes_to_5)
homes_to_parks

# remove spatial component to plot
homes_to_parks_names <- homes_to_parks %>% 
  st_drop_geometry() 

# to plot using ecdf, "uncount" by visit_days to get one row per visit day
homes_to_parks_names_visit_days <- homes_to_parks_names %>%
  uncount(visit_days)

# and see if unique visitors is very diff
homes_to_parks_names_visitors <- homes_to_parks_names %>%
  uncount(visitors_unq)

# cumulative?
ggplot(homes_to_parks_names_visit_days) +
  stat_ecdf(aes(dist, col = source)) +
  facet_wrap(~Park_Name) +
  xlab("Distance (m)") +
  labs(title = "Distance traveled to each park, by visit days")

ggplot(homes_to_parks_names_visitors) +
  stat_ecdf(aes(dist, col = source)) +
  facet_wrap(~Park_Name) +
  xlab("Distance (m)") +
  labs(title = "Distance traveled to each park, by unique visitors")

### TODO: add back in soparc, and decide between the two plots above
