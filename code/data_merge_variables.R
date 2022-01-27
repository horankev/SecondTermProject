library(tidyverse)
library(sf)
library(parlitools)
library(GGally)
library(patchwork)
library(lme4)
library(mgcv)
library(ggfortify)
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

# MERGE 'elect2019' AND 'census', THEN 'hex', THEN 'party_colour'

# first make temporary reduced data-frames to make merging smoother...
temp_census <- census  %>% 
  select(-c("ons_const_id", "constituency_name", "country", "region", "constituency_type")) 
# because different letter cases will disrupt the merging with elect2019
temp_hex <- hex  %>% 
  select(-"constituency_name")
# because different letter cases will disrupt the merging with elect2019

comb2019 <- left_join(elect2019, temp_census, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
  left_join(party_colour, by = c("winner_19" = "party_name")) %>% 
  select(pano, everything()) %>% # put pano as column 1 to match 2017 and 2015 structures
  st_as_sf()
saveRDS(comb2019, here("data", "comb2019.rds"))

comb2017 <- left_join(elect2017, temp_census, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
  left_join(party_colour, by = c("winner_17" = "party_name")) %>% 
  st_as_sf()
saveRDS(comb2017, here("data", "comb2017.rds"))

comb2015 <- left_join(elect2015, temp_census, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>% 
  left_join(party_colour, by = c("winner_15" = "party_name")) %>% 
  st_as_sf()
saveRDS(comb2015, here("data", "comb2015.rds"))

#################################

# make colour and fill scales for future use

# COLOUR and FILL scales for parites
# only show parties which won seats in the legend by using unique
mycolours_party <- party_colour$party_colour
names(mycolours_party) <- party_colour$party_name

colscale_party2019 <- scale_colour_manual(names(mycolours_party), values=mycolours_party, breaks=unique(comb2019$winner_19))
colscale_party2017 <- scale_colour_manual(names(mycolours_party), values=mycolours_party, breaks=unique(comb2017$winner_17)) 
colscale_party2015 <- scale_colour_manual(names(mycolours_party), values=mycolours_party, breaks=unique(comb2015$winner_15))
fillscale_party2019 <- scale_fill_manual(names(mycolours_party), values=mycolours_party, breaks=unique(comb2019$winner_19))
fillscale_party2017 <- scale_fill_manual(names(mycolours_party), values=mycolours_party, breaks=unique(comb2017$winner_17)) 
fillscale_party2015 <- scale_fill_manual(names(mycolours_party), values=mycolours_party, breaks=unique(comb2015$winner_15))

# COLOUR and FILL scales for countries
mycolours_country <- c("red", "blue4", "#008142")
names(mycolours_country) <- c("England", "Scotland", "Wales")

colscale_country <- scale_colour_manual(names(mycolours_country), values=mycolours_country)
fillscale_country <- scale_fill_manual(names(mycolours_country), values=mycolours_country)

#################################

# show results as hex maps
p2019 <- ggplot() + 
  geom_sf(data=comb2019, aes(fill=winner_19)) +
  fillscale_party2019 +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2019", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

p2017 <- ggplot() + 
  geom_sf(data=comb2017, aes(fill=winner_17)) +
  fillscale_party2017 +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2017", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

p2015 <- ggplot() + 
  geom_sf(data=comb2015, aes(fill=winner_15)) +
  fillscale_party2015 +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2015", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

combined <- p2019 + p2017 + p2015
#+ plot_layout(guides = "collect") & theme(legend.position = "bottom")
combined

#################################


# choose variables

# extract the variables or combination variables which have been chosen...

# produce a census with only the variables desired (and pano for joining)
census_reduced <- comb2019 %>%
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

# join to election data, hex, and part-colour datasets, as before
comb2019_reduced <- left_join(elect2019, census_reduced, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
  left_join(party_colour, by = c("winner_19" = "party_name")) %>% 
  select(pano, everything()) %>% # put pano as column 1 to match 2017 and 2015 structures
  st_as_sf()
saveRDS(comb2019_reduced, here("data", "comb2019_reduced.rds"))

comb2017_reduced <- left_join(elect2017, census_reduced, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
  left_join(party_colour, by = c("winner_17" = "party_name")) %>% 
  select(pano, everything()) %>% # put pano as column 1 to match 2017 and 2015 structures
  st_as_sf()
saveRDS(comb2019_reduced, here("data", "comb2017_reduced.rds"))

comb2015_reduced <- left_join(elect2015, census_reduced, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
  left_join(party_colour, by = c("winner_15" = "party_name")) %>% 
  select(pano, everything()) %>% # put pano as column 1 to match 2017 and 2015 structures
  st_as_sf()
saveRDS(comb2019_reduced, here("data", "comb2015_reduced.rds"))



