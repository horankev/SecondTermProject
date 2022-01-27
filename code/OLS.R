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

# read in required data
all_elections_reduced <- readRDS(here("data", "all_elections_reduced.rds"))

#################################
# OLS for conservative %, labour %, libdem %
reg_cons <- lm(con_19 ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
                 qual_none + health_bad_both + deprived_none + house_owned +
                 household_one_person + ethnicity_white + born_uk + christian +
                 unemployed + retired + born_elsewhere, data=all_elections_reduced)
summary(reg_cons)

reg_lab <- lm(lab_19 ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
                qual_none + health_bad_both + deprived_none + house_owned +
                household_one_person + ethnicity_white + born_uk + christian +
                unemployed + retired + born_elsewhere, data=all_elections_reduced)
summary(reg_lab)

reg_ld <- lm(ld_19 ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
               qual_none + health_bad_both + deprived_none + house_owned +
               household_one_person + ethnicity_white + born_uk + christian +
               unemployed + retired + born_elsewhere, data=all_elections_reduced)
summary(reg_ld)

