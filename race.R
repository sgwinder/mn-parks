###
### TPL demographics - race
### 10/2/20
###
### Note: I am not comfortable with this comparison
###

# Somewhat forked/copied from `gender.R`

library(tidyverse)
library(readxl)

setwd("~/Documents/TPL/MinnesotaParks/")

# read in data

soparc_all <- read_excel("OutputFromR_weekdaySOPARC_ForSpencer.xlsx", sheet = "AllSOPARCCombined", skip = 1)

# clean it up
soparc_tots <- soparc_all %>%
  select(park_day_month = rn, starts_with("ThatDay")) %>%
  rename_at(.vars = vars(starts_with("ThatDay")), .fun = function(x) {str_remove(x, "ThatDay")}) %>%
  mutate(Park_Name = str_sub(park_day_month, end = -6))
soparc_tots

# create a joinkey of park names
parks <- soparc_tots %>%
  select(Park_Name) %>%
  distinct() %>%
  mutate(Park_short = str_extract(Park_Name, ".+(?=\\s)"))


# Ok. Calculating % in each racial category, by park
# Adding a column "unclassified" to add up to 100% of visitors
soparc_race_by_park <- soparc_tots %>%
  rowwise() %>%
  mutate(Unclassified = Visitors - sum(Black, White, Other)) %>%
  group_by(Park_Name) %>%
  summarise_at(vars(Black, White, Other, Unclassified, Visitors), .funs = sum)

soparc_race_by_park_tall <- soparc_race_by_park %>%
  rename(TotVisitors = Visitors) %>%
  gather(key = "race", value = "visitors", Black, White, Other, Unclassified) %>%
  mutate(percent = visitors/TotVisitors*100,
         source = "SOPARC")

# ordering for plotting
soparc_race_by_park_tall$race <- factor(soparc_race_by_park_tall$race, levels = c("White", "Black", "Other", "Unclassified"))

ggplot(soparc_race_by_park_tall) +
  geom_col(aes(x = Park_Name, y = percent, fill = race))
# egh. perceived race is weird

# calculate totals
soparc_race_perc <- soparc_race_by_park %>%
  summarise_at(vars(Black, White, Other, Unclassified, Visitors), .funs = sum) %>%
  rename(TotVisitors = Visitors) %>%
  gather(key = "race", value = "visitors", Black, White, Other, Unclassified) %>%
  mutate(percent = visitors/TotVisitors * 100, 
         source = "SOPARC") 
soparc_race_perc

#########
## Cuebiq


cuebiq_demo <- read_excel("CellData/TravelDistanceAndDemographics.xlsx", sheet = "ForAnalysis", skip = 1)
cuebiq_demo

# Ok. Need to figure out the weigthed average % male vs female.
# Weighting on the number of pings from that block group

# first manipulate tibble to be cleaner, and calculate % each race
cuebiq_race <- cuebiq_demo %>%
  select(Park, blockgroup = `Block Group ID`, 
         visitors = `Number of unique devices from this block group seen on this day in this park`,
         totpop = `Total Population`, 
         White, Black, NativeAmerican, Asian, PacificIslander, OtherRace, TwoOrMoreRaces) %>%
  rowwise() %>%
  mutate(perc_white = White/totpop,
         perc_black = Black/totpop,
         perc_other = sum(NativeAmerican, Asian, PacificIslander, OtherRace, TwoOrMoreRaces) / totpop)

# calculate weighted % white, black, other by park
cuebiq_race_by_park <- cuebiq_race %>%
  filter(!is.na(Park)) %>%
  group_by(Park) %>%
  summarise(perc_white_weighted = sum(visitors*perc_white) / sum(visitors) * 100,
            perc_black_weighted = sum(visitors*perc_black) / sum(visitors) * 100,
            perc_other_weighted = sum(visitors*perc_other) / sum(visitors) * 100) %>%
  left_join(parks, by = c(Park = "Park_short"))
cuebiq_race_by_park

# make it tall and match the soparc data
cuebiq_race_by_park_tall <- cuebiq_race_by_park %>%
  rename(Black = perc_black_weighted, White = perc_white_weighted, Other = perc_other_weighted) %>%
  gather(key = "race", value = "percent", Black, White, Other) %>%
  mutate(source = "CUEBIQ")
cuebiq_race_by_park_tall

# ordering for plotting
cuebiq_race_by_park_tall$race <- factor(cuebiq_race_by_park_tall$race, levels = c("White", "Black", "Other", "Unclassified"))


# And, calculate overall. Need to go back a step for this (since %s)
cuebiq_race_totals <- cuebiq_race %>%
  ungroup() %>%
  filter(!is.na(Park)) %>%
  summarise(White = sum(visitors*perc_white) / sum(visitors) * 100,
            Black = sum(visitors*perc_black) / sum(visitors) * 100,
            Other = sum(visitors*perc_other) / sum(visitors) * 100) %>%
  gather(key = "race", value = "percent") %>%
  mutate(source = "CUEBIQ")


##### Combine and plot
race <- soparc_race_perc %>%
  bind_rows(cuebiq_race_totals)
race
### Need to work here to get them to match better

ggplot(soparc_race_by_park_tall) +
  geom_col(aes(x = Park_Name, y = visitors, fill = race), position = "dodge")

ggplot(race) +
  geom_col(aes(x = source, y = percent, fill = race), position = "dodge") +
  #scale_fill_brewer(palette = "Dark2") +
  #coord_flip() +
  labs(title = "Estimated racial breakdown by dataset")

# write it out
#ggsave("mn-parks/figs/race_by_dataset.png", width = 6, heigh = 4, units = "in")


### Let's try by park

race_park <- soparc_race_by_park_tall %>%
  bind_rows(cuebiq_race_by_park_tall)

race_park

ggplot(race_park) +
  geom_col(aes(x = Park_Name, y = percent, fill = race), position = "dodge") +
  #scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#999999")) +
  #scale_fill_manual(values = wesanderson::wes_palette("FantasticFox2")) +
  scale_fill_brewer(palette = "Set2") +
  #ggsci::scale_fill_uchicago() +
  ylab("Percent of Visitors") +
  xlab(NULL) +
  facet_grid(rows = vars(source)) +
  theme_bw()

# write it out
#ggsave("mn-parks/figs/race_by_park_by_dataset.png", width = 8, height = 6, units = "in")
