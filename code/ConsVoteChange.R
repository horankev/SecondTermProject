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




# look at the change in conservative vote from 2017-2019
# regression with this as y, and unemployed as explanatory variable
# dfc19ch = df cons 2019 change
dfc19ch <- all_elections_reduced %>% 
  mutate(con_19ch = (con_19 - con_17)/con_17) %>% # new col for change
  filter(!is.na(con_19ch)) %>% 
  select(con_19ch,unemployed) %>% 
  st_drop_geometry()
reg19ch <- lm(con_19ch ~ .,data=dfc19ch)
aug19ch <- augment(reg19ch)
dfc19ch <- all_elections_reduced %>% 
  mutate(con_19ch = (con_19 - con_17)/con_17) %>%  
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
  mutate(con_17ch = (con_17 - con_15)/con_15) %>% # new col for change
  select(con_17ch,unemployed) %>% 
  filter(!is.na(con_17ch)) %>% 
  st_drop_geometry()
reg17ch <- lm(con_17ch ~ .,data=dfc17ch)
aug17ch <- augment(reg17ch)
dfc17ch <- all_elections_reduced %>%
  mutate(con_17ch = (con_17 - con_15)/con_15) %>% 
  select(con_17ch,unemployed) %>% 
  filter(!is.na(con_17ch)) %>%  
  mutate(stdres = aug17ch$.std.resid) %>% 
  st_as_sf()

p_2017ch <- ggplot() + geom_sf(data = dfc17ch, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
  labs(title="2017 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2015-2017 By Unemployment"))
p_2017ch

# and the same for change from 2010-2015
dfc15ch <- all_elections_reduced %>% 
  mutate(con_15ch = (con_15 - con_10)/con_10) %>% # new col for change
  select(con_15ch,unemployed) %>% 
  filter(!is.na(con_15ch)) %>% 
  st_drop_geometry()
reg15ch <- lm(con_15ch ~ .,data=dfc15ch)
aug15ch <- augment(reg15ch)
dfc15ch <- all_elections_reduced %>%
  mutate(con_15ch = (con_15 - con_10)/con_10) %>% 
  select(con_15ch,unemployed) %>% 
  filter(!is.na(con_15ch)) %>%  
  mutate(stdres = aug15ch$.std.resid) %>% 
  st_as_sf()

p_2015ch <- ggplot() + geom_sf(data = dfc15ch, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="black",limits=c(-2,2)) +
  labs(title="2015 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2010-2015 By Unemployment"))
p_2015ch

p_2019ch + p_2017ch + p_2015ch
