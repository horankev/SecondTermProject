library(tidyverse)
library(sf)
library(here)
library(broom)
library(patchwork)

rm(list=ls())
here()

#################################

# read in required data
all_elections_reduced <- readRDS(here("data", "all_elections_reduced.rds"))
longdata <- readRDS(here("data", "longdata.rds"))


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

#################################

# remove some correlated variables and model across 4 elections
all_con <- lm(con ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
                 qual_none + health_bad_both + house_owned +
                 household_one_person + ethnicity_white + 
                 unemployed + born_elsewhere, data=longdata)
summary(all_con)

all_lab <- lm(lab ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
                qual_none + health_bad_both + house_owned +
                household_one_person + ethnicity_white + 
                unemployed + born_elsewhere, data=longdata)
summary(all_lab)

all_ld <- lm(ld ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
                qual_none + health_bad_both + house_owned +
                household_one_person + ethnicity_white + 
                unemployed + born_elsewhere, data=longdata)
summary(all_ld)

#################################

# 4 x elections
r1 <- lm(con ~ unemployed, data = longdata %>% filter(!is.na(con)))
summary(r1)
a <- augment(r1)


ppp <- longdata %>% 
  filter(!is.na(con)) %>%
  left_join(a) %>% 
  select(PK,pano,year,constituency_name,region,country,winner,geometry,68) %>% 
  st_as_sf()

p_all <- ggplot() + geom_sf(data = ppp, aes(fill = .std.resid)) +
  scale_fill_gradient2(low="red",mid = "white", high="black") +
  labs(title="All Four Elections - Standardised Residuals",
       subtitle = "OLS Regression On Conservative Vote By Unemployment")

# 2019 election

r2 <- lm(con ~ unemployed, data = longdata %>% filter(year=="2019"))
summary(r2)
b <- augment(r2)

ppp1 <- longdata %>% 
  filter(!is.na(con), year=="2019") %>%
  left_join(b) %>% 
  select(PK,pano,year,constituency_name,region,country,winner,geometry,69) %>% 
  st_as_sf()

p_2019 <- ggplot() + geom_sf(data = ppp1, aes(fill = .std.resid)) +
  scale_fill_gradient2(low="red",mid = "white", high="black") +
  labs(title="2019 - Standardised Residuals",
       subtitle = "OLS Regression On Conservative Vote By Unemployment")

p_all + p_2019


#################################

# do regression on conservative vote four each year on one variable
# draw map showing the standardised residuals


dfc19 <- all_elections_reduced %>%
  select(con_19,unemployed) %>% 
  filter(!is.na(con_19)) %>% 
  st_drop_geometry()
reg19 <- lm(con_19 ~ .,data=dfc19)
aug19 <- augment(reg19)
dfc19 <- all_elections_reduced %>%
  select(con_19,unemployed) %>% 
  filter(!is.na(con_19)) %>%  
  mutate(stdres = aug19$.std.resid) %>% 
  st_as_sf()

dfc17 <- all_elections_reduced %>% 
  select(con_17,unemployed) %>% 
  filter(!is.na(con_17)) %>% 
  st_drop_geometry()
reg17 <- lm(con_17 ~ .,data=dfc17)
aug17 <- augment(reg17)
dfc17 <- all_elections_reduced %>%
  select(con_17,unemployed) %>% 
  filter(!is.na(con_17)) %>% 
  mutate(stdres = aug17$.std.resid) %>% 
  st_as_sf()

dfc15 <- all_elections_reduced %>% 
  select(con_15,unemployed,geometry) %>% 
  filter(!is.na(con_15)) %>% 
  st_drop_geometry()
reg15 <- lm(con_15 ~ .,data=dfc15)
aug15 <- augment(reg15)
dfc15 <- all_elections_reduced %>%
  select(con_15,unemployed) %>% 
  filter(!is.na(con_15)) %>% 
  mutate(stdres = aug15$.std.resid) %>% 
  st_as_sf()

dfc10 <- all_elections_reduced %>%
  select(con_10,unemployed,geometry) %>% 
  filter(!is.na(con_10)) %>% 
  st_drop_geometry()
reg10 <- lm(con_10 ~ .,data=dfc10)
aug10 <- augment(reg10)
dfc10 <- all_elections_reduced %>%
  select(con_10,unemployed) %>% 
  filter(!is.na(con_10)) %>% 
  mutate(stdres = aug10$.std.resid) %>% 
  st_as_sf()

p_2019 <- ggplot() + geom_sf(data = dfc19, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
  labs(title="2019 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Conservative Vote By "))
p_2017 <- ggplot() + geom_sf(data = dfc17, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
  labs(title="2017 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Conservative Vote By "))
p_2017 <- ggplot() + geom_sf(data = dfc15, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
  labs(title="2015 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Conservative Vote By "))
p_2010 <- ggplot() + geom_sf(data = dfc10, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
  labs(title="2010 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Conservative Vote By "))

(p_2019 + p_2017 + p_2017 + p_2010 + plot_layout(guides = "collect") & theme(legend.position = 'right'))


##################

# make the above into a function

draw_map <- function(x){
  dfc19 <- all_elections_reduced %>%
    select(con_19,{{x}}) %>% 
    filter(!is.na(con_19)) %>% 
    st_drop_geometry()
  reg19 <- lm(con_19 ~ .,data=dfc19)
  aug19 <- augment(reg19)
  dfc19 <- all_elections_reduced %>%
    select(con_19,{{x}}) %>% 
    filter(!is.na(con_19)) %>%  
    mutate(stdres = aug19$.std.resid) %>% 
    st_as_sf()
  
  dfc17 <- all_elections_reduced %>% 
    select(con_17,{{x}}) %>% 
    filter(!is.na(con_17)) %>% 
    st_drop_geometry()
  reg17 <- lm(con_17 ~ .,data=dfc17)
  aug17 <- augment(reg17)
  dfc17 <- all_elections_reduced %>%
    select(con_17,{{x}}) %>% 
    filter(!is.na(con_17)) %>% 
    mutate(stdres = aug17$.std.resid) %>% 
    st_as_sf()
  
  dfc15 <- all_elections_reduced %>% 
    select(con_15,{{x}},geometry) %>% 
    filter(!is.na(con_15)) %>% 
    st_drop_geometry()
  reg15 <- lm(con_15 ~ .,data=dfc15)
  aug15 <- augment(reg15)
  dfc15 <- all_elections_reduced %>%
    select(con_15,{{x}}) %>% 
    filter(!is.na(con_15)) %>% 
    mutate(stdres = aug15$.std.resid) %>% 
    st_as_sf()
  
  dfc10 <- all_elections_reduced %>%
    select(con_10,{{x}},geometry) %>% 
    filter(!is.na(con_10)) %>% 
    st_drop_geometry()
  reg10 <- lm(con_10 ~ .,data=dfc10)
  aug10 <- augment(reg10)
  dfc10 <- all_elections_reduced %>%
    select(con_10,{{x}}) %>% 
    filter(!is.na(con_10)) %>% 
    mutate(stdres = aug10$.std.resid) %>% 
    st_as_sf()

 
  p_2019 <- ggplot() + geom_sf(data = dfc19, aes(fill = stdres)) +
    scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
    labs(title="2019 - Standardised Residuals",
         subtitle = str_c("OLS Regression On Conservative Vote By "))
  p_2017 <- ggplot() + geom_sf(data = dfc17, aes(fill = stdres)) +
    scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
    labs(title="2017 - Standardised Residuals",
         subtitle = str_c("OLS Regression On Conservative Vote By "))
  p_2017 <- ggplot() + geom_sf(data = dfc15, aes(fill = stdres)) +
    scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
    labs(title="2015 - Standardised Residuals",
         subtitle = str_c("OLS Regression On Conservative Vote By "))
  p_2010 <- ggplot() + geom_sf(data = dfc10, aes(fill = stdres)) +
    scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
    labs(title="2010 - Standardised Residuals",
         subtitle = str_c("OLS Regression On Conservative Vote By "))
  
  (p_2019 + p_2017 + p_2017 + p_2010 + plot_layout(guides = "collect") & theme(legend.position = 'right'))
  
}
draw_map(unemployed)
draw_map(retired)
draw_map(leave_hanretty)
draw_map(qual_none)

################################

# look at the change in conservative vote from 2017-2019
# regression with this as y, and unemployed as explanatory variable
dfc19ch <- all_elections_reduced %>% 
  mutate(con_19ch = con_19 - con_17) %>% # new col for change
  select(con_19ch,unemployed) %>% 
  filter(!is.na(con_19ch)) %>% 
  st_drop_geometry()
reg19ch <- lm(con_19ch ~ .,data=dfc19ch)
aug19ch <- augment(reg19ch)
dfc19ch <- all_elections_reduced %>%
  mutate(con_19ch = con_19 - con_17) %>% 
  select(con_19ch,unemployed) %>% 
  filter(!is.na(con_19ch)) %>%  
  mutate(stdres = aug19ch$.std.resid) %>% 
  st_as_sf()

p_2019ch <- ggplot() + geom_sf(data = dfc19ch, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
  labs(title="2019 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2017-2019 By Unemployment"))
p_2019ch


# and the same for change from 2015-2017
dfc17ch <- all_elections_reduced %>% 
  mutate(con_17ch = con_17 - con_15) %>% # new col for change
  select(con_17ch,unemployed) %>% 
  filter(!is.na(con_17ch)) %>% 
  st_drop_geometry()
reg17ch <- lm(con_17ch ~ .,data=dfc17ch)
aug17ch <- augment(reg17ch)
dfc17ch <- all_elections_reduced %>%
  mutate(con_17ch = con_17 - con_15) %>% 
  select(con_17ch,unemployed) %>% 
  filter(!is.na(con_17ch)) %>%  
  mutate(stdres = aug17ch$.std.resid) %>% 
  st_as_sf()

p_2017ch <- ggplot() + geom_sf(data = dfc17ch, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
  labs(title="2019 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2015-2017 By Unemployment"))
p_2017ch

p_2019ch + p_2017ch