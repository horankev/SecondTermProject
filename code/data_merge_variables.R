library(tidyverse)
library(sf)
library(parlitools)
library(GGally)
library(patchwork)
library(here)

rm(list=ls())
here()

#################################

# GET DATASETS
elect2019 <- bes_2019 %>% 
  filter(country != "Northern Ireland")  # census dataset does not include NI
saveRDS(elect2019, here("data", "elect2019.rds"))

elect2017 <- bes_2017 %>% 
  filter(country != "Northern Ireland")
saveRDS(elect2017, here("data", "elect2017.rds"))

elect2015 <- bes_2015 %>%
  filter(country != "Northern Ireland")
saveRDS(elect2015, here("data", "elect2015.rds"))

census <- census_11 
saveRDS(census, here("data", "census.rds"))

hex <- west_hex_map %>% 
  filter(region_name != "Northern Ireland")
saveRDS(hex, here("data", "hex.rds"))

party_colour <- party_colour
saveRDS(party_colour, here("data", "party_colour.rds"))

#################################

# MERGE 'elect2019' with results from 17,15,10, AND 'census', THEN 'hex'

# first make temporary reduced data-frames to make merging smoother...
temp_census <- census  %>% 
  select(-c("ons_const_id", "constituency_name", "country", "region", "constituency_type")) 
# because different letter cases will disrupt the merging with elect2019
temp_hex <- hex  %>% 
  select(-"constituency_name")
# because different letter cases will disrupt the merging with elect2019

all_elections <- left_join(elect2019, temp_census, by = "pano") %>%
  left_join(elect2017 %>% select(ons_const_id, ends_with("_17")), by = "ons_const_id") %>% 
  left_join(elect2015 %>% select(ons_const_id, ends_with("_15")), by = "ons_const_id") %>% 
  left_join(elect2015 %>% select(ons_const_id, ends_with("_10")), by = "ons_const_id") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
#  left_join(party_colour, by = c("winner_19" = "party_name")) %>% 
  select(pano, everything()) %>% # put pano as column 1 to match 2017 and 2015 structures
  rename_at(
    vars(ends_with(".x")),
    ~str_replace(., "\\..$","")
  ) %>% 
  select_at(
        vars(-ends_with(".y"))
  ) %>% 
st_as_sf()
saveRDS(all_elections, here("data", "all_elections.rds"))

#################################

# SET UP COLOUR AND FILL SCALES

# first extract the colours only for the parties who won seats each time
colvals19 <- elect2019 %>% 
  select(winner_19) %>% 
  left_join(party_colour, by = c("winner_19" = "party_name")) %>% 
  select(winner_19, party_colour) %>% 
  unique()

colvals17 <- elect2017 %>% 
  select(winner_17) %>% 
  left_join(party_colour, by = c("winner_17" = "party_name")) %>% 
  select(winner_17, party_colour) %>% 
  unique()

colvals15 <- elect2015 %>% 
  select(winner_15) %>% 
  left_join(party_colour, by = c("winner_15" = "party_name")) %>% 
  select(winner_15, party_colour) %>% 
  unique()

colvals10 <- elect2015 %>%  
  select(winner_10) %>% 
  left_join(party_colour, by = c("winner_10" = "party_name")) %>% 
  select(winner_10, party_colour) %>% 
  unique()

mycolours_party2019 <- colvals19$party_colour
names(mycolours_party2019) <- colvals19$winner_19
mycolours_party2017 <- colvals17$party_colour
names(mycolours_party2017) <- colvals17$winner_17
mycolours_party2015 <- colvals15$party_colour
names(mycolours_party2015) <- colvals15$winner_15
mycolours_party2010 <- colvals10$party_colour
names(mycolours_party2010) <- colvals10$winner_10

saveRDS(mycolours_party2019, here("data", "mycolours_party2019.rds"))
saveRDS(mycolours_party2017, here("data", "mycolours_party2017.rds"))
saveRDS(mycolours_party2015, here("data", "mycolours_party2015.rds"))
saveRDS(mycolours_party2010, here("data", "mycolours_party2010.rds"))

# then make palettes for colouring by party
colscale_party2019 <- scale_colour_manual(names(mycolours_party2019), values=mycolours_party2019, name="Party")
colscale_party2017 <- scale_colour_manual(names(mycolours_party2017), values=mycolours_party2017, name="Party") 
colscale_party2015 <- scale_colour_manual(names(mycolours_party2015), values=mycolours_party2015, name="Party")
colscale_party2010 <- scale_colour_manual(names(mycolours_party2010), values=mycolours_party2010, name="Party")

# then make palettes for filling by party
fillscale_party2019 <- scale_fill_manual(names(mycolours_party2019), values=mycolours_party2019, name="Party")
fillscale_party2017 <- scale_fill_manual(names(mycolours_party2017), values=mycolours_party2017, name="Party") 
fillscale_party2015 <- scale_fill_manual(names(mycolours_party2015), values=mycolours_party2015, name="Party")
fillscale_party2010 <- scale_fill_manual(names(mycolours_party2010), values=mycolours_party2010, name="Party")

# then make palettes for colouring and filling by party and country
mycolours_country <- c("red", "blue4", "#008142")
names(mycolours_country) <- c("England", "Scotland", "Wales")
colscale_country <- scale_colour_manual(names(mycolours_country), values=mycolours_country, name="Country")
fillscale_country <- scale_fill_manual(names(mycolours_country), values=mycolours_country, name="Country")
#################################

# show results as hex maps
p2019 <- ggplot() + 
  geom_sf(data=all_elections, aes(fill=winner_19)) +
  fillscale_party2019 +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2019", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

p2017 <- ggplot() + 
  geom_sf(data=all_elections, aes(fill=winner_17)) +
  fillscale_party2017 +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2017", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

p2015 <- ggplot() + 
  geom_sf(data=all_elections, aes(fill=winner_15)) +
  fillscale_party2015 +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2015", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

p2010 <- ggplot() + 
  geom_sf(data=all_elections, aes(fill=winner_10)) +
  fillscale_party2010 +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2010", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

combined <- p2019 + p2017 + p2015 + p2010

#+ plot_layout(guides = "collect") & theme(legend.position = "bottom")
combined

#################################


# choose variables

# extract the variables or combination variables which have been chosen...

# produce a census with only the variables desired (and pano for joining)
census_reduced <- all_elections %>%
  mutate(age_18_44 = age_18_to_19 + age_20_to_24 + age_25_to_29 + age_30_to_44,
         age_45_64 = age_45_to_59 + age_60_to_64, 
         health_bad_both = (health_very_bad + health_bad),
         born_elsewhere = born_ireland + born_other_eu + born_other_pre_2004_eu + born_post_2004_eu + born_other) %>% 
  select(age_18_44, age_45_64, cars_none, qual_none, health_bad_both, deprived_none, house_owned, household_one_person, 
         ethnicity_white, born_uk, christian, no_religion, unemployed, retired, born_elsewhere, pano) %>% 
  st_drop_geometry()
saveRDS(census_reduced, here("data", "census_reduced.rds"))

# pairs plot without pano (only needed for merging)
ggpairs(census_reduced %>% select(-pano))

# join to election data for each election (19,17,15,10), hex, as before
all_elections_reduced <- left_join(elect2019, census_reduced, by = "pano") %>%
  left_join(elect2017 %>% select(ons_const_id, ends_with("_17")), by = "ons_const_id") %>% 
  left_join(elect2015 %>% select(ons_const_id, ends_with("_15")), by = "ons_const_id") %>% 
  left_join(elect2015 %>% select(ons_const_id, ends_with("_10")), by = "ons_const_id") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
#  left_join(party_colour, by = c("winner_19" = "party_name")) %>% 
  select(pano, everything()) %>% # put pano as column 1 to match 2017 and 2015 structures
  rename_at(
    vars(ends_with(".x")),
    ~str_replace(., "\\..$","")
  ) %>% 
  select_at(
    vars(-ends_with(".y"))
  ) %>% 
st_as_sf()
saveRDS(all_elections_reduced, here("data", "all_elections_reduced.rds"))




