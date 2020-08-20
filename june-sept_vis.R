###
### TPL MN Parks
### June - Sept Total vistitation by data source
###
### Roughly based on `summarising_vis.R` on 8/20/20
###

library(tidyverse)
library(foreign)
library(readxl)
library(lubridate)

setwd("~/Documents/TPL/MinnesotaParks/")

# read in data
# let's use the total monthly data now
flickr <- read_csv("pud/userdays_total_monthly_bypid.csv") # It looks like we didn't share avg_annual_ud in MN_parks_flickr
twitter <- read_csv("MN_parks_twitter/userdays_total_monthly_bypid.csv")
instagram <- read_csv("MN_parks_instagram/userdays_total_monthly_bypid.csv")

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

tail(flickr)
# ok, looks like we only have through 2018. So can't compare the same exact time period without grabbing more data
tail(twitter)
tail(instagram)

# renaming social media to fit
flickr_r <- flickr %>%
  rename(pid_fi = pid, flickr = userdays)

instag_r <- instagram %>%
  rename(pid_fi = pid, instag = userdays)

twitter_r <- twitter %>%
  rename(pid_t = pid, twitter = userdays)

monthly_wide <- key %>%
  left_join(flickr_r, by = c("pid_fi")) %>%
  left_join(twitter_r, by = c("pid_t", "month", "year")) %>%
  left_join(instag_r, by = c("pid_fi", "month", "year")) %>%
  select(-pid_fi, -pid_t)

monthly_tall <- monthly_wide %>%
  gather(key = "source", value = "userdays", flickr, twitter, instag) %>%
  mutate(d2p = ymd(paste0(year, "-", month, "-15")))

ggplot(monthly_tall) +
  geom_line(aes(x = d2p, y = userdays, col = source)) +
  facet_wrap(~Park_Name)

######################
## Soparc

soparc_vis <- read_excel("OutputFromR_weekdaySOPARC_ForSpencer.xlsx", sheet = "OverallVisitation")

# using the cleaner soparc_vis for now
soparc_vis_c <- soparc_vis %>%
  rename(source = `data source`, park_day_month = `park, day, month`, Total_Visitors = `Total Visitors`) %>%
  mutate(Park_Name = str_sub(park_day_month, end = -6))
soparc_vis_c

# starting by just adding up the total number of estimated visitors to each park on the sampled days
soparc_totvis <- soparc_vis_c %>%
  group_by(Park_Name, source) %>%
  summarise(visitors = sum(Total_Visitors)) %>%
  mutate(vis_metric = "sum of Total Visitors across all survey days")
soparc_totvis
# soparc surveying happened for one week in June, and one week in September, 2019, for each park

# since I don't have 2019 social media data, let's compare the soparc data to June-Sept 2018 for social media
# (remember that i have the "total" average annual UD comparisons in `summarising_vis.R`)

# filter to only June-sept 2018
ud_junesept18 <- monthly_tall %>%
  filter(between(d2p, ymd("2018-06-01"), ymd("2018-10-01"))) %>%
  group_by(Park_Name, source) %>%
  summarise(visitors = sum(userdays)) %>%
  mutate(vis_metric = "sum of userdays between June and Sept of 2018")

sm_sp_js_tall <- soparc_totvis %>%
  bind_rows(ud_junesept18)

sm_sp_js_tall %>% group_by(source) %>%
  summarise(sum(visitors))

# stacked bar chart by data source
ggplot(sm_sp_js_tall) +
  geom_col(aes(x = source, y = visitors, fill = Park_Name), position = "fill") +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab("Proportion of Visitors") +
  xlab("Data Source") +
  coord_flip() +
  labs(title = "June - Sept Proportion of Visitors Visiting Each Park According to Different Data Sources",
       subtitle = "Social media UD from June-Sept 2018 ONLY",
       caption = "Totals: TUD = 92, SOPARC = 8382, IUD = 571, PUD = 1") +
  theme_classic()
# Ok. Really not eough socmed to make that work...

# one final test is the average UD in June - Sept (across years)

ud_junesept_avg <- monthly_tall %>%
  filter(month %in% 6:9) %>%
  group_by(Park_Name, source, year) %>%
  summarise(js_ud = sum(userdays)) %>%
  group_by(Park_Name, source) %>%
  summarise(visitors = mean(js_ud, na.rm = TRUE)) %>%
  mutate(vis_metric = "Average UD/year in June-Sept")

sm_sp_js_avg_tall <- soparc_totvis %>%
  bind_rows(ud_junesept_avg)

sm_sp_js_avg_tall %>% group_by(source) %>%
  summarise(sum(visitors))

ggplot(sm_sp_js_avg_tall) +
  geom_col(aes(x = source, y = visitors, fill = Park_Name), position = "fill") +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab("Proportion of Visitors") +
  xlab("Data Source") +
  coord_flip() +
  labs(title = "June - Sept Proportion of Visitors Visiting Each Park According to Different Data Sources",
       subtitle = "Social media is the AVERAGE UDs posted from June-Sept (all years)",
       caption = "Totals: TUD = 167, SOPARC = 8382, IUD = 394, PUD = 18.8") +
  theme_classic()

# how about as a side by side bar chart?
# need to caluclate prop_vis first
sm_sp_js_avg_prop <- sm_sp_js_avg_tall %>%
  group_by(source) %>%
  mutate(prop_vis = visitors / sum(visitors))

ggplot(sm_sp_js_avg_prop) +
  geom_col(aes(x = Park_Name, y = prop_vis, fill = source), position = "dodge") +
  scale_fill_viridis_d() +
  ylab("Proportion of Visitors") +
  xlab("Data Source") +
  labs(title = "June - Sept Proportion of Visitors Visiting Each Park According to Different Data Sources",
       subtitle = "Social media is the AVERAGE UDs posted from June-Sept (all years)",
       caption = "Totals: TUD = 167, SOPARC = 8382, IUD = 394, PUD = 18.8") +
  theme_classic()
