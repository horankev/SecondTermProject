library(tidyverse)
library(sf)
library(parlitools)
library(here)

rm(list=ls())
here()

#################################

# read in required data

all_elections <- readRDS(here("data", "all_elections.rds"))

#################################

# pick variables for PCA, including results from previous (17) election for Lab and Cons
vars_selection <- all_elections %>% 
  filter(!is.na(con_17) & !is.na(lab_17)) %>% 
  select(country,con_17,lab_17,population_density, age_30_to_44, cars_none, qual_none, health_bad, health_very_bad, deprived_none, house_owned, household_one_person, 
         ethnicity_white, unemployed, retired, pano) %>% 
  filter(country != "Scotland") %>% 
  select(-country) %>% 
  st_drop_geometry()

# PCA

# calculate principal components
PCAresults <- prcomp(vars_selection %>% select(-pano), scale. = TRUE)


# identify conservative flips, conservative hold, Labour hold, others
all_elections_flip <- all_elections %>% 
  filter(country != "Scotland") %>% 
  filter(!is.na(con_19) & !is.na(lab_19)) %>% 
  mutate(consflip = factor(
    ifelse(winner_19 == "Conservative" & winner_17 != "Conservative", "Conservative Flip", 
                           ifelse(winner_19 == "Conservative" & winner_17 == "Conservative","Conservative",
                                  ifelse(winner_19 == "Labour","Labour","Other"))))
  )
# biplot for England and Wale
autoplot(PCAresults, data = all_elections_flip, geom = 'point', colour = "consflip", size=1.5, loadings = TRUE,
         loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 4, loadings.label.colour = "black", loadings.label.hjust = -0.15)  + 
  scale_color_manual(values = c("lightblue","darkgreen","pink","white")) + 
  theme_minimal()
  
# biplot by region
autoplot(PCAresults, data = all_elections_flip, geom = 'point', colour = "consflip", size=1.5, loadings = TRUE,
         loadings.colour = 'grey',
         loadings.label = TRUE, loadings.label.size = 4, loadings.label.colour = "black", loadings.label.hjust = -0.15,alpha=1)  + 
  scale_color_manual(values = c("lightblue","darkgreen","pink","white")) + 
  theme_minimal() + 
  facet_wrap(~region)

