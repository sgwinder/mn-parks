###
### Demographics plots for TPL St Paul Parks
### 10/2/20
###

library(tidyverse)
library(readxl)

setwd("~/Documents/TPL/MinnesotaParks/")

# read in data
# why don't we start with the relatively easy "gender"

soparc_gender <- read_excel("OutputFromR_weekdaySOPARC_ForSpencer.xlsx", sheet = 'gender')
soparc_gender

# also want to get the total visitors for the day to calculate percents
soparc_all <- read_excel("OutputFromR_weekdaySOPARC_ForSpencer.xlsx", sheet = "AllSOPARCCombined", skip = 1)

# clean it up
soparc_tots <- soparc_all %>%
  select(park_day_month = rn, starts_with("ThatDay")) %>%
  rename_at(.vars = vars(starts_with("ThatDay")), .fun = function(x) {str_remove(x, "ThatDay")}) %>%
  mutate(Park_Name = str_sub(park_day_month, end = -6))
soparc_tots



# make it a bit friendlier
# using the cleaner soparc_vis for now
soparc_gen_c <- soparc_gender %>%
  rename(source = `data source`, park_day_month = `park, day, month`) %>%
  mutate(Park_Name = str_sub(park_day_month, end = -6))
soparc_gen_c

# ok, let's calculate gender breakdown by park, then overall
soparc_gen_by_park <- soparc_gen_c %>%
  group_by(Park_Name) %>%
  summarise(female = sum(Females), male = sum(Males))

# make it tall
soparc_gen_by_park_tall <- soparc_gen_by_park %>% gather(key = "gender", value = "visitors", female, male)

# plot it
ggplot(soparc_gen_by_park_tall) +
  geom_col(aes(x = Park_Name, y = visitors, fill = gender), position = "fill")

# lets make overall totals
soparc_gen_totals <- soparc_gen_by_park_tall %>%
  group_by(gender) %>%
  summarise(visitors = sum(visitors)) %>%
  mutate(source = "SOPARC")

# let's make percents (for other demo variables, start this at the beginning. But for 
# gender let's assume that everyone is gendered in the dataset)
soparc_gen_perc <- soparc_gen_totals %>%
  mutate(percent = visitors / sum(visitors))

###################
### Add in cuebiq data

cuebiq_demo <- read_excel("CellData/TravelDistanceAndDemographics.xlsx", sheet = "ForAnalysis", skip = 1)
cuebiq_demo

# Ok. Need to figure out the weigthed average % male vs female.
# Weighting on the number of pings from that block group

# first manipulate tibble to be cleaner, and calculate % male and female
cuebiq_gen <- cuebiq_demo %>%
  select(Park, blockgroup = `Block Group ID`, 
         visitors = `Number of unique devices from this block group seen on this day in this park`,
         totpop = `Total Population`, Males, Females) %>%
  mutate(perc_male = Males/totpop,
         perc_female = Females/totpop)

# calculate weighted % male and female by park
cuebiq_gen_by_park <- cuebiq_gen %>%
  filter(!is.na(Park)) %>%
  group_by(Park) %>%
  summarise(perc_male_weighted = sum(visitors*perc_male) / sum(visitors),
            perc_female_weighted = sum(visitors*perc_female) / sum(visitors))

# make it tall and match the soparc data
cuebiq_gen_by_park_tall <- cuebiq_gen_by_park %>%
  rename(male = perc_male_weighted, female = perc_female_weighted) %>%
  gather(key = "gender", value = "percent", male, female) %>%
  mutate(source = "CUEBIQ")

# And, calculate overall. Need to go back a step for this (since %s)
cuebiq_gen_totals <- cuebiq_gen %>%
  filter(!is.na(Park)) %>%
  summarise(male = sum(visitors*perc_male) / sum(visitors),
            female = sum(visitors*perc_female) / sum(visitors)) %>%
  gather(key = "gender", value = "percent") %>%
  mutate(source = "CUEBIQ")

##### Combine and plot
gender <- soparc_gen_perc %>%
  bind_rows(cuebiq_gen_totals)

ggplot(gender) +
  geom_col(aes(x = reorder(source, desc(source)), y = percent, fill = gender), position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  geom_hline(aes(yintercept = .5)) +
  coord_flip() + 
  labs(title = "Estimated gender breakdown by dataset") +
  xlab(NULL) +
  ylab("Percent of Visitors") +
  theme_bw()

# write it out
#ggsave("mn-parks/figs/gender_by_dataset.png", width = 6, heigh = 4, units = "in")

### What about by park?

# create a joinkey of park names
# create a joinkey of park names
parks <- soparc_tots %>%
  select(Park_Name) %>%
  distinct() %>%
  mutate(Park_short = str_extract(Park_Name, ".+(?=\\s)"))
parks

soparc_gen_by_park_perc <- soparc_gen_by_park_tall %>%
  group_by(Park_Name) %>%
  mutate(percent = visitors / sum(visitors),
         source = "SOPARC")

gender_parks <- cuebiq_gen_by_park_tall %>%
  left_join(parks, by = c(Park = "Park_short")) %>%
  select(-Park) %>%
  bind_rows(soparc_gen_by_park_perc)

ggplot(gender_parks) +
  geom_col(aes(x = Park_Name, y = percent*100, fill = gender), position = "dodge") +
  scale_fill_brewer(name = "Gender", labels = c("Female", "Male"), palette = "Set2") +
  ylab("Percent of Visitors") +
  xlab(NULL) +
  facet_grid(rows = vars(source)) +
  theme_bw()

# write it out
#ggsave("mn-parks/figs/gender_by_park_by_dataset.png", width = 8, height = 6, units = "in")
