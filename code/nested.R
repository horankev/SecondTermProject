library(tidyverse)
library(sf)
library(here)
library(broom)

rm(list=ls())
here()

all_elections_reduced <- readRDS(here("data", "all_elections_reduced.rds"))


# NEST THE DATA BY REGION AND YEAR

# remove the year tag on the variable names,
# separately for each election year dataset
# and add a column indicating the year
df2019 <- all_elections_reduced %>% 
  select(pano, ends_with("_19")) %>% 
  mutate(year = 2019)
names(df2019) <- gsub("_19", "", names(df2019), fixed = TRUE)

df2017 <- all_elections_reduced %>% 
  select(pano, ends_with("_17"), -majority_vote_17) %>% 
  mutate(year = 2017)
names(df2017) <- gsub("_17", "", names(df2017), fixed = TRUE)

df2015 <- all_elections_reduced %>% 
  select(pano, ends_with("_15")) %>% 
  mutate(year = 2015)
names(df2015) <- gsub("_15", "", names(df2015), fixed = TRUE)

df2010 <- all_elections_reduced %>% 
  select(pano, ends_with("_10")) %>% 
  mutate(year = 2010)
names(df2010) <- gsub("_10", "", names(df2010), fixed = TRUE)

# extract a dataset without the year-specific data
dfbase <- all_elections_reduced %>% 
  select(-ends_with("_19"), 
         -ends_with("_17"), 
         -ends_with("_15"), 
         -ends_with("_10")) %>% 
  st_drop_geometry()

# join each year separately to this dataset
temp19 <- dfbase %>% 
  left_join(df2019)
temp17 <- dfbase %>% 
  left_join(df2017)
temp15 <- dfbase %>% 
  left_join(df2015)
temp10 <- dfbase %>% 
  left_join(df2010)

# then join these four together
longdata <- temp19 %>% 
  full_join(temp17) %>% 
  full_join(temp15) %>% 
  full_join(temp10) %>% 
  select(-ends_with("1719")) # these can be calculated from other cols

# perform the nesting
nested <- longdata %>% 
  group_by(region,year) %>% 
  nest()


