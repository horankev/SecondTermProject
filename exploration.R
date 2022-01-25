library(tidyverse)
library(sf)
library(parlitools)
library(GGally)
library(patchwork)
library(lme4)

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
  mutate(age_18_44 = age_18_to_19 + age_20_to_24 + age_25_to_29 + age_30_to_44,
         age_45_64 = age_45_to_59 + age_60_to_64,
         cars_mean = (cars_one + 2*cars_two + 3*cars_three + 4*cars_four)/100,
         qual_mean = (qual_level_1 + 2*qual_level_2 + 3*qual_level_3 + 4*qual_level_4 + 5*degree)/100,
         health_bad_both = (health_very_bad + health_bad),
         health_good_both = (health_good + health_very_good),
         deprived_mean = (deprived_1 + 2*deprived_2 + 3*deprived_3 + 4*deprived_4)/100,
         born_elsewhere = born_ireland + born_other_eu + born_other_pre_2004_eu + born_post_2004_eu + born_other) %>% 
  select(age_18_44, age_45_64, cars_mean, qual_mean, health_bad_both, health_good_both, deprived_mean, house_owned, household_one_person, 
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
reg_cons <- lm(con_19 ~ leave_hanretty + age_18_44 + age_45_64 + cars_mean + qual_mean + health_bad_both + health_good_both + deprived_mean + house_owned + household_one_person + 
          ethnicity_white + born_uk + christian + unemployed + retired + scotland + born_elsewhere, data=sel_vars_2019_sep_scot)
summary(reg_cons)

reg_lab <- lm(lab_19 ~ leave_hanretty + age_18_44 + age_45_64 + cars_mean + qual_mean + health_bad_both + health_good_both + deprived_mean + house_owned + household_one_person + 
                 ethnicity_white + born_uk + christian + unemployed + retired + scotland + born_elsewhere, data=sel_vars_2019_sep_scot)
summary(reg_lab)

reg_ld <- lm(ld_19 ~ leave_hanretty + age_18_44 + age_45_64 + cars_mean + qual_mean + health_bad_both + health_good_both + deprived_mean + house_owned + household_one_person + 
                ethnicity_white + born_uk + christian + unemployed + retired + scotland + born_elsewhere, data=sel_vars_2019_sep_scot)
summary(reg_ld)



#################################

# trying multilevel model with lme4 from lmer...
# for different regions on conservative % vote
# first, using cars_mean, then deprived_mean, then health_bad_both, then age_18_44, then qual_mean, then retired


######
# from: https://benwhalley.github.io/just-enough-r/extending-traditional-rm-anova.html
slope.model <- lmer(con_19 ~ cars_mean + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ cars_mean + (cars_mean | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_cars_mean <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=cars_mean, color=region)) +
  # add axis label
  xlab("cars_mean") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(02,1.8), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ deprived_mean + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ deprived_mean + (deprived_mean | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_deprived_mean <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=deprived_mean, color=region)) +
  # add axis label
  xlab("deprived_mean") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(0.5,1.4), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ health_bad_both + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ health_bad_both + (health_bad_both | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_health_bad_both <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=health_bad_both, color=region)) +
  # add axis label
  xlab("health_bad_both") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(2.4,12), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ age_18_44 + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ age_18_44 + (age_18_44 | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_age_18_44 <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=age_18_44, color=region)) +
  # add axis label
  xlab("age_18_44") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(2.4,12), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ qual_mean + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ qual_mean + (qual_mean | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_qual_mean <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=qual_mean, color=region)) +
  # add axis label
  xlab("qual_mean") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(1.2,5.4), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ retired + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ retired + (retired | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_retired <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=retired, color=region)) +
  # add axis label
  xlab("retired") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(4.2,26)) +
  scale_y_continuous(limits=c(-100, 100))

##
p_cars_mean + p_deprived_mean + p_health_bad_both + p_age_18_44 + p_qual_mean + p_retired + plot_layout(ncol=3)

