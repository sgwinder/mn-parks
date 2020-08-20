####
#### MN Parks Figures - TPL
#### Total Visitation Across Parks. Differences in relative vis by data source
#### 8/19/20
####

library(tidyverse)
library(sf)
library(foreign)
library(readxl)

setwd("~/Documents/TPL/MinnesotaParks/")

# read in data
# just using avg ann UD for now
flickr <- read_csv("pud/userdays_avg_annual_bypid.csv") # It looks like we didn't share avg_annual_ud in MN_parks_flickr
twitter <- read_csv("MN_parks_twitter/userdays_avg_annual_bypid.csv")
instagram <- read_csv("MN_parks_instagram/userdays_avg_annual_bypid.csv")

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
            by = c("SOPARCID", "Park_Name" = "PARK_NAME"))
key

# renaming social media to fit
flickr_r <- flickr %>%
  rename(pid_fi = pid, flickr = avg_ann_ud)

instag_r <- instagram %>%
  rename(pid_fi = pid, instag = avg_ann_ud)

twitter_r <- twitter %>%
  rename(pid_t = pid, twitter = avg_ann_ud)

avg_ann_ud_wide <- key %>%
  left_join(flickr_r, by = "pid_fi") %>%
  left_join(twitter_r, by = "pid_t") %>%
  left_join(instag_r, by = "pid_fi") %>%
  select(-pid_fi, -pid_t)

avg_ann_ud_wide

avg_ann_ud_tall <- avg_ann_ud_wide %>% 
  gather(key = "socmed", value = "avg_ann_ud", flickr, twitter, instag)

ggplot(avg_ann_ud_tall) +
  geom_col(aes(x = Park_Name, y = avg_ann_ud, fill = socmed), position = "dodge") +
  coord_flip()

# Cool. Something like this. Now need to add on SOPARC data, and eventually also Cuebiq
## One alternative would be to make these maps + lists

# trying maps
parks_simple <- parks %>%
  select(Park_Name = PARK_NAME, SOPARCID) 

parks_ud <- parks_simple %>%
  left_join(avg_ann_ud_tall)

ggplot(parks_ud) +
  geom_sf(aes(fill = avg_ann_ud)) +
  facet_wrap(~socmed)
# hmm. I really want them each to have their own scale

ggplot(parks_ud %>% filter(socmed == "flickr")) +
  geom_sf(aes(fill = avg_ann_ud)) 

ggplot(parks_ud %>% filter(socmed == "twitter")) +
  geom_sf(aes(fill = avg_ann_ud)) 

ggplot(parks_ud %>% filter(socmed == "instag")) +
  geom_sf(aes(fill = avg_ann_ud)) 

# I could also accomplish this with a facet by turning each into "proportion of total UD"
parks_ud_prop <- parks_ud %>%
  group_by(socmed) %>%
  mutate(prop_ud = avg_ann_ud / sum(avg_ann_ud))

ggplot(parks_ud_prop) +
  geom_sf(aes(fill = prop_ud)) +
  facet_wrap(~socmed)
# there it is. Informative? Maybe

############################################################
### SOPARC

soparc_all <- read_excel("OutputFromR_weekdaySOPARC_ForSpencer.xlsx", sheet = "AllSOPARCCombined", skip = 1)
soparc_vis <- read_excel("OutputFromR_weekdaySOPARC_ForSpencer.xlsx", sheet = "OverallVisitation")

# using the cleaner soparc_vis for now
soparc_vis_c <- soparc_vis %>%
  rename(source = `data source`, park_day_month = `park, day, month`, Total_Visitors = `Total Visitors`) %>%
  mutate(Park_Name = str_sub(park_day_month, end = -6))


# starting by just adding up the total number of estimated visitors to each park on the sampled days
soparc_totvis <- soparc_vis_c %>%
  group_by(Park_Name, source) %>%
  summarise(visitors = sum(Total_Visitors)) %>%
  mutate(vis_metric = "sum of Total Visitors across all survey days")

# join it on and make it comparable to the socmed (have to do this without geometries)
sm_sp_tall <- parks_ud %>%
  st_set_geometry(NULL) %>%
  rename(source = socmed, visitors = avg_ann_ud) %>%
  mutate(vis_metric = "avg_ann_ud") %>%
  bind_rows(soparc_totvis)

ggplot(sm_sp_tall) +
  geom_col(aes(x = Park_Name, y = visitors, fill = source))


# stacked bar chart by data source
ggplot(sm_sp_tall) +
  geom_col(aes(x = source, y = visitors, fill = Park_Name), position = "fill") +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab("Proportion of Visitors") +
  xlab("Data Source") +
  coord_flip() +
  labs(title = "Proportion of Visitors Visiting Each Park According to Different Data Sources") +
  theme_classic()

# proportion of all observed by source?
sm_sp_props <- sm_sp_tall %>%
  group_by(source) %>%
  mutate(prop_vis = visitors / sum(visitors))

ggplot(sm_sp_props) +
  geom_col(aes(x = Park_Name, y = prop_vis, fill = source), position = "dodge") +
  scale_fill_viridis_d() +
  ylab("Proportion of Visitors Visiting Each Park") +
  labs(title = "Proportion of all observed visitors visiting each park, by data source")

ggplot(sm_sp_props) +
  geom_col(aes(x = source, y = prop_vis, fill = Park_Name), position = "dodge") +
  scale_fill_viridis_d()

# and, on a map?
sm_sp_props_sf <- parks_simple %>%
  left_join(sm_sp_props %>% ungroup() %>% select(-SOPARCID))
sm_sp_props_sf

ggplot(sm_sp_props_sf) +
  geom_sf(aes(fill = prop_vis)) +
  facet_wrap(~source) +
  scale_fill_distiller(palette = "YlOrRd")
