library(tidyverse)
library(sf)
library(parlitools)
library(GGally)
library(patchwork)

rm(list=ls())

# GET DATASETS
elect2019 <- bes_2019 %>% 
  filter(country != "Northern Ireland") # census dataset does not include NI

elect2017 <- bes_2017 %>% 
  filter(country != "Northern Ireland")

elect2015 <- bes_2015 %>%
  filter(country != "Northern Ireland")

census <- census_11

hex <- west_hex_map %>%
  filter(region_name != "Northern Ireland")

party_colour <- party_colour

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

comb2017 <- left_join(elect2017, temp_census, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
  left_join(party_colour, by = c("winner_17" = "party_name")) %>% 
  st_as_sf()

comb2015 <- left_join(elect2015, temp_census, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>% 
  left_join(party_colour, by = c("winner_15" = "party_name")) %>% 
  st_as_sf()

#################################

# show results as hex maps

# make a colour scale
# only show parties which won seats in the legend by using unique
mycolours <- party_colour$party_colour
names(mycolours) <- party_colour$party_name

colscale <- scale_fill_manual(names(mycolours), values=mycolours, breaks=unique(comb2019$winner_19))
p2019 <- ggplot() + 
  geom_sf(data=comb2019, aes(fill=winner_19)) +
  colscale +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2019", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

colscale <- scale_fill_manual(names(mycolours), values=mycolours, breaks=unique(comb2017$winner_17)) 
p2017 <- ggplot() + 
  geom_sf(data=comb2017, aes(fill=winner_17)) +
  colscale +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2017", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

colscale <- scale_fill_manual(names(mycolours), values=mycolours, breaks=unique(comb2015$winner_15))
p2015 <- ggplot() + 
  geom_sf(data=comb2015, aes(fill=winner_15)) +
  colscale +
  coord_sf(datum = NA) +
  theme_light() +
  labs(title = "2015", fill = "Parties") +
  theme(legend.position = c(.8,.88), legend.key.size = unit(0.3, 'cm'))

combined <- p2019 + p2017 + p2015
#+ plot_layout(guides = "collect") & theme(legend.position = "bottom")
combined

#################################

# some test regressions...
xxx <- lm(con_19~unemployed+degree+retired+christian+household_all_students+house_private_landlord, data=comb2019)
summary(xxx)

#################################

# choose variables

# extract the variables or combination variables which have been chosen...
# these cols for three types of qualification have NA when should be zero (they add to 100% so zero seems correct)
comb2019$degree <- comb2019$degree %>% 
  replace(is.na(.), 0)
comb2019$qual_other <- comb2019$qual_other %>% 
  replace(is.na(.), 0)
comb2019$qual_apprentice <- comb2019$qual_apprentice %>% 
  replace(is.na(.), 0)

# produce a census with only the variables desired (and pano for joining)
census_reduced <- comb2019 %>%
  mutate(age_18_29 = age_18_to_19 + age_20_to_24 + age_25_to_29,
         age_30_59 = age_30_to_44 + age_45_to_59,
         age_60_over = age_60_to_64 + age_65_to_74 + age_75_to_84 + age_85_to_89 + age_90_plus,
         cars_mean = (cars_one + 2*cars_two + 3*cars_three + 4*cars_four)/100,
         qual_mean = (qual_level_1 + 2*qual_level_2 + 3*qual_level_3 + 4*qual_level_4 + 5*degree)/100,
         health_mean = (health_bad + 2*health_fair + 3*health_good + 4*health_very_good)/100,
         deprived_mean = (deprived_1 + 2*deprived_2 + 3*deprived_3 + 4*deprived_4)/100) %>% 
  select(age_18_29, age_30_59, age_60_over, cars_mean, qual_mean, health_mean, deprived_mean, house_owned, household_one_person, 
         ethnicity_white, born_uk, christian, no_religion, unemployed, retired, pano) %>% 
  st_drop_geometry()

# join to election data, hex, and part-colour datasets, as before
sel_vars_2019 <- left_join(elect2019, census_reduced, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
  left_join(party_colour, by = c("winner_19" = "party_name")) %>% 
  select(pano, everything()) %>% # put pano as column 1 to match 2017 and 2015 structures
  st_as_sf()

# pairs plot without pano (only needed for merging)
ggpairs(census_reduced[,1:15])

#################################


#################################
ggplot(comb2019) +
  geom_sf(aes(fill=ld_19)) +
  coord_sf(datum=NA) + 
  scale_fill_gradient(low="white",high="red") + 
  theme_minimal() + facet_wrap(~country)

ggplot(comb2019) + 
  geom_point(aes(y=lab_19,x=retired,colour=country)) + 
  geom_smooth(aes(y=lab_19,x=retired)) + 
  theme_minimal() + facet_wrap(~winner_19)


