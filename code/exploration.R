library(tidyverse)
library(sf)
library(parlitools)
library(GGally)
library(patchwork)
library(lme4)
library(mgcv)
library(ggfortify)

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


# choose variables

# extract the variables or combination variables which have been chosen...

# produce a census with only the variables desired (and pano for joining)
census_reduced <- comb2019 %>%
  mutate(age_18_44 = age_18_to_19 + age_20_to_24 + age_25_to_29 + age_30_to_44,
         age_45_64 = age_45_to_59 + age_60_to_64,health_bad_both = (health_very_bad + health_bad),
         born_elsewhere = born_ireland + born_other_eu + born_other_pre_2004_eu + born_post_2004_eu + born_other) %>% 
  select(age_18_44, age_45_64, cars_none, qual_none, health_bad_both, deprived_none, house_owned, household_one_person, 
         ethnicity_white, born_uk, christian, no_religion, unemployed, retired, born_elsewhere, pano) %>% 
  st_drop_geometry()

# pairs plot without pano (only needed for merging)
ggpairs(census_reduced[,1:ncol(census_reduced)-1])

# join to election data, hex, and part-colour datasets, as before
sel_vars_2019 <- left_join(elect2019, census_reduced, by = "pano") %>% 
  left_join(temp_hex, by = c("ons_const_id"="gss_code")) %>%
  left_join(party_colour, by = c("winner_19" = "party_name")) %>% 
  select(pano, everything()) %>% # put pano as column 1 to match 2017 and 2015 structures
  st_as_sf()

#################################

# make dataset with binary Scotland column
sel_vars_2019_sep_scot <- sel_vars_2019 %>% 
  mutate(scotland = ifelse(country == "Scotland", 1, 0))

# OLS for conservative %, labour %, libdem %
reg_cons <- lm(con_19 ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
                 qual_none + health_bad_both + deprived_none + house_owned +
                 household_one_person + ethnicity_white + born_uk + christian +
                 unemployed + retired + scotland + born_elsewhere, data=sel_vars_2019_sep_scot)
summary(reg_cons)

reg_lab <- lm(lab_19 ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
                qual_none + health_bad_both + deprived_none + house_owned +
                household_one_person + ethnicity_white + born_uk + christian +
                unemployed + retired + scotland + born_elsewhere, data=sel_vars_2019_sep_scot)
summary(reg_lab)

reg_ld <- lm(ld_19 ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
               qual_none + health_bad_both + deprived_none + house_owned +
               household_one_person + ethnicity_white + born_uk + christian +
               unemployed + retired + scotland + born_elsewhere, data=sel_vars_2019_sep_scot)
summary(reg_ld)


#################################

# PCA

# calculate principal components
PCAresults <- prcomp(census_reduced[,1:ncol(census_reduced)-1], scale. = TRUE)
# 
# # reverse the signs
# PCAresults$rotation <- -1*PCAresults$rotation
# 
# # display principal components
# PCAresults$rotation
# 
# # reverse the signs of the scores
# PCAresults$x <- -1*PCAresults$x
# 
# # display the first six scores
# head(PCAresults$x)
# 
# # biplot
# biplot(PCAresults, scale = 0)
# 
# # calculate total variance explained by each principal component
# PCAresults$sdev^2 / sum(PCAresults$sdev^2)
# 
# # makescree plot
# # calculate total variance explained by each principal component
# var_explained = PCAresults$sdev^2 / sum(PCAresults$sdev^2)
# 
# # create scree plot
# qplot(c(1:15), var_explained) + 
#   geom_line() + 
#   xlab("Principal Component") + 
#   ylab("Variance Explained") +
#   ggtitle("Scree Plot") +
#   ylim(0, 1)

# biplot coloured by country
autoplot(PCAresults, data = comb2019, geom = 'point',colour = 'country', size=2, loadings = TRUE, 
         loadings.colour = 'blue', 
         loadings.label = TRUE, loadings.label.size = 3) + 
  theme(plot.background=element_blank(),
        panel.background=element_rect(fill='transparent',color='black',size=1),
        legend.text=element_text(hjust=1),
        legend.key=element_blank())

# biplot coloured by country, faceted by country
autoplot(PCAresults, data = comb2019, geom = 'point',colour = 'country', size=2, loadings = TRUE, 
         loadings.colour = 'blue', 
         loadings.label = TRUE, loadings.label.size = 3) + 
  theme(plot.background=element_blank(),
        panel.background=element_rect(fill='transparent',color='black',size=1),
        legend.text=element_text(hjust=1),
        legend.key=element_blank()) + 
  facet_wrap(~country)

# biplot coloured by winner
autoplot(PCAresults, data = comb2019, geom = 'point',colour = 'winner_19', size=1, loadings = TRUE,
         loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) + 
  theme(plot.background=element_blank(),
        panel.background=element_rect(fill='transparent',color='black',size=2),
        legend.text=element_text(hjust=1),
        legend.key=element_blank())

# biplot coloured by winner, faceted by country
autoplot(PCAresults, data = comb2019, geom = 'point',colour = 'winner_19', size=1, loadings = TRUE,
         loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) + 
  theme(plot.background=element_blank(),
        panel.background=element_rect(fill='transparent',color='black',size=2),
        legend.text=element_text(hjust=1),
        legend.key=element_blank()) + 
  facet_wrap(~country)

# biplot coloured by winner, faceted by region
autoplot(PCAresults, data = comb2019, geom = 'point',colour = 'winner_19', size=2, loadings = TRUE,
         loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3) + 
  theme(plot.background=element_blank(),
        panel.background=element_rect(fill='transparent',color='black',size=2),
        legend.text=element_text(hjust=1),
        legend.key=element_blank()) + 
  facet_wrap(~region)



