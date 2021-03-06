## 
## Making a new travel_dist script, using census tract centroids for cell data
## 10/8/20
##

library(tidyverse)
library(sf)
library(foreign)
library(readxl)

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

# Is jewell county, KS causing that big jump?
ggplot(homes_to_parks_names_visit_days %>% filter(county != "Jewell")) +
  stat_ecdf(aes(dist, col = source)) +
  facet_wrap(~Park_Name) +
  xlab("Distance (m)") +
  labs(title = "Distance traveled to each park, by visit days")

# no, actually. 
homes_to_parks_names_visit_days %>%
  group_by(source, tract, county, state, dist, Park_Name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# let's look at only MN visitors
ggplot(homes_to_parks_names_visit_days %>% filter(state == "Minnesota")) +
  stat_ecdf(aes(dist, col = source)) +
  facet_wrap(~Park_Name) +
  xlab("Distance (m)") +
  labs(title = "Distance traveled to each park, by visit days")

# the big jump at ~167 km is Crow Wing County, MN. On wikipedia, I dont' see a population center
## here, but it does seem to be roughly in the center of MN. I'm worried this jump reflects
## people saying that they're from "Minnesota", not from a specific city. 

## Bleh. Well, let's go ahead and add on CUEBIQ and ask spencer for his thoughts later

############# Cuebiq #########
# Ok. adding in the Cuebiq data
cuebiq_demo <- read_excel("CellData/TravelDistanceAndDemographics.xlsx", sheet = "ForAnalysis", skip = 1)
cuebiq_demo

# create a joinkey of park names
parks <- tibble(Park_Name = st_drop_geometry(parks_centroids)$Park_Name,
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

# join to social media. We should use the visit days version, since it's clsoer to cuebiq
homes_to_parks_names_visit_days

combined <- bind_rows(cuebiq_to_parks, homes_to_parks_names_visit_days)

## do some data summaries
combined %>%
  group_by(source) %>%
  summarise(n())

# how many from within 50 km? roughly city boundaries
combined %>%
  group_by(city = dist < 50000, source) %>%
  summarise(n = n()) %>%
  spread(key = city, value = n) %>%
  mutate(proportion_city = `TRUE` / (`FALSE` + `TRUE`))


# how many unique visitors?
homes_to_parks_names_visitors %>%
  group_by(source) %>%
  summarise(n())

ggplot(combined) +
  stat_ecdf(aes(dist, col = source)) +
  facet_wrap(~Park_Name) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(0, 4e+06, length.out = 5),
                     labels = seq(0, 4000, length.out = 5), 
                     name = "Distance (km)") +
  ylab("Cumulative proportion of visitors") +
  labs(title = "Distance traveled to each park (from Continental US)") +
  theme_bw()

# dists for jewell county and crow wing county?
combined %>% filter(county %in% c("Jewell", "Crow Wing"))
# 168 km and 724 km

#ggsave("mn-parks/figs/distance_by_dataset.png", width = 8, heigh = 6, units = "in")

# what about if we wrap by source and color by park? Clsoer to the vis figs
ggplot(combined) +
  stat_ecdf(aes(dist, col = Park_Name)) +
  facet_wrap(~source) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(0, 4e+06, length.out = 5),
                     labels = seq(0, 4000, length.out = 5), 
                     name = "Distance (km)") +
  ylab("Cumulative proportion of visitors") +
  labs(title = "Distance traveled to each park (from Continental US)") +
  theme_bw()
# interesting

### Let's try to come up with an "average" ecdf to report for each park: TODO
# find max distance in dataset
max_dist <- max(combined$dist)

indiv_ecdf_function <- function(source_abb){
  subset_dist <- combined %>% filter(Park_Name == "Ecolab Plaza", source == source_abb)
  ecdf_fun <- ecdf(subset_dist$dist)
  ecdf_seq <- seq(0, max_dist, length.out = 1000)
  ecdf_vec <- ecdf_fun(ecdf_seq)
  output <- tibble(cum_prob = ecdf_vec, x = ecdf_seq, source = source_abb)
  return(output)
}

indiv_ecdf_function("CUEBIQ")

ecolab <- map_df(unique(combined$source), ~indiv_ecdf_function(.))

ggplot(ecolab) +
  geom_line(aes(x = x, y = cum_prob, col = source))

### Great. So that works, looping over source, but with fixed park
## Let's see if I can generalize it
park_source <- list("Rice Park", "CUEBIQ")

gen_ecdf_function <- function(park_source){
  park_abb <- park_source[[1]]
  source_abb <- park_source[[2]]
  subset_dist <- combined %>% filter(Park_Name == park_abb, source == source_abb)
  ecdf_fun <- ecdf(subset_dist$dist)
  ecdf_seq <- seq(0, max_dist, length.out = 1000)
  ecdf_vec <- ecdf_fun(ecdf_seq)
  output <- tibble(cum_prob = ecdf_vec, dist = ecdf_seq, source = source_abb, Park_Name = park_abb)
  return(output)
}

gen_ecdf_function(list("Culture Park", "flickr"))

# do it for everything
all_ecdfs <- cross2(unique(combined$Park_Name), unique(combined$source)) %>%
  map_df(~gen_ecdf_function(.))

ggplot(all_ecdfs) +
  geom_line(aes(x = dist, y = cum_prob, col = source)) +
  facet_wrap(~Park_Name) 

# amazing.
# Ok, calculate the average ecdf for each park
avg_ecdf <- all_ecdfs %>%
  group_by(Park_Name, dist) %>%
  summarise(cum_prob = mean(cum_prob)) %>%
  mutate(source = "Average")

ggplot(avg_ecdf) +
  geom_line(aes(x = dist, y = cum_prob, col = Park_Name))

# how about if I add it to the other plot instead
ecdfs_together <- all_ecdfs %>%
  bind_rows(avg_ecdf)

ggplot(ecdfs_together) +
  geom_line(aes(x = dist, y = cum_prob, col = source, lty = source)) +
  scale_color_manual(breaks = c("Average", "CUEBIQ", "flickr", "twitter"),
                     values = c("Black", '#1b9e77','#d95f02','#7570b3'),
                     labels = c("Average", "CUEBIQ", "Flickr", "Twitter"),
                     name = NULL) +
  scale_linetype_manual(breaks = c("Average", "CUEBIQ", "flickr", "twitter"),
                        values = c(1, 2, 2, 2),
                        labels = c("Average", "CUEBIQ", "Flickr", "Twitter"),
                        name = NULL) +
  facet_wrap(~Park_Name) +
  scale_x_continuous(breaks = seq(0, 4e+06, length.out = 5),
                     labels = seq(0, 4000, length.out = 5), 
                     name = "Distance (km)") +
  ylab("Cumulative proportion of visitors") +
  labs(title = "Distance traveled to each park (from Continental US)") +
  theme_bw()

## write it out
#ggsave("mn-parks/figs/distance_by_dataset_avg.png", width = 8, height = 6, units = "in")




########## Playing with a bar chart grouped by different distances (per kristen's request) ####
# “For now, I think it would be nice to break up the distance traveled into categories - 
# within a half mile, then half mile to three miles (roughly St. Paul), then up to twenty miles 
# (roughly the metro area), then above 100 miles.”
combined

# First, let's add columns for distance in miles, and then for bins
combined_mi <- combined %>%
  mutate(dist_mi = dist / 1609.344,
         dist_cat = cut(dist_mi, 
                        breaks = c(0, .5, 3, 20, 100, max(dist_mi)), 
                        labels = c("< 0.5", "0.5 - 3", "3 - 20", "20 - 100", "> 100"),
                        include.lowest = TRUE))

ggplot(combined_mi) +
  geom_bar(aes(x = dist_cat, fill = source), position = "dodge") +
  facet_wrap(~Park_Name)

# let's add those up individually, since I want "0"s for the park*source*distances that aren't represented

dist_perc <- combined_mi %>%
  group_by(Park_Name, source, dist_cat) %>%
  summarise(visitors = n()) %>%
  complete(Park_Name, source, dist_cat, fill = list(visitors = 0)) %>%
  group_by(Park_Name, source) %>%
  mutate(perc_vis = visitors / sum(visitors) * 100)

ggplot(dist_perc) +
  geom_col(aes(x = dist_cat, y = perc_vis, fill = fct_rev(source)), position = "dodge") +
  scale_x_discrete(name = "Distance Traveled (miles)", limits = rev(levels(dist_perc$dist_cat))) +
  ylab("Percent of Visitors") +
  scale_fill_brewer(palette = "Set2",
                    name = "Dataset",
                    breaks = c("CUEBIQ", "flickr", "twitter"),
                    labels = c("CUEBIQ", "Flickr", "Twitter")) +
  coord_flip() +
  facet_wrap(~Park_Name) +
  theme_bw()

# is it better with vertical bars?
ggplot(dist_perc) +
  geom_col(aes(x = dist_cat, y = perc_vis, fill = (source)), position = "dodge") +
  scale_x_discrete(name = "Distance Traveled (miles)", 
                   limits = (levels(dist_perc$dist_cat))) +
  ylab("Percent of Visitors") +
  scale_fill_brewer(palette = "Set2",
                    name = "Dataset",
                    breaks = c("CUEBIQ", "flickr", "twitter"),
                    labels = c("CUEBIQ", "Flickr", "Twitter")) +
  #coord_flip() +
  facet_wrap(~Park_Name) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# write it out
#ggsave("mn-parks/figs/distance_by_dataset_barchart.png", width = 8, height = 5.5, units = "in")
