library(tidyverse)
library(sf)
library(here)
library(broom)
library(patchwork)
library(GGally)

rm(list=ls())
here()

#################################

# read in required data
all_elections_reduced <- readRDS(here("data", "all_elections_reduced.rds"))

####################

# OLS for conservative vote change

df_con_19change <- all_elections_reduced %>% 
  filter(!is.na(con_19), !is.na(con_17)) %>% 
  mutate(con_19change = (con_19 - con_17)) 
# remove cases when there was no cons candidate
# and create new column call con_19change
# the %2019 minus %2017

#####################################

# Visualise change in Conservative Vote

# Hex Map
ggplot() + 
  geom_sf(data=df_con_19change, aes(fill=con_19change)) + 
  scale_fill_gradient2(low = "darkred", high = "darkblue") + 
  coord_sf(datum = NA) + 
  theme_light() +
  labs(title = "2019-17 Conservative Vote Change", fill = "% Change")

# Histograms
ggplot(data=df_con_19change, aes(x=con_19change)) + 
  geom_histogram(fill = "lightblue", colour = "darkblue") +
  geom_vline(xintercept = 0, colour="red") + 
  labs(title = "Change In Conservative Vote 2019-17",
       subtitle = "Histograms By Region",
       x="% Change") + 
  facet_wrap(~region)

# Boxplots
ggplot(data=df_con_19change, aes(x=region,y=con_19change)) + 
  geom_boxplot(fill= "lightblue") + 
#  geom_jitter(width = 0.1) +
  geom_hline(yintercept=0, colour="red") + 
  labs(title = "Change In Conservative Vote 2019-17",
       subtitle = "Boxplots By Region",
       x="% Change") + 
  coord_flip()


#####################################

ggpairs(
  df_con_19change %>% 
    st_drop_geometry() %>% 
    select(
  con_19change, leave_hanretty, age_18_44, age_45_64, cars_none,
    qual_none, health_bad_both, deprived_none, house_owned,
    household_one_person, ethnicity_white, born_uk, christian,
    unemployed, retired, born_elsewhere)
)

ggcorr(
  df_con_19change %>% 
    st_drop_geometry() %>% 
    select(
      con_19change, leave_hanretty, age_18_44, age_45_64, cars_none,
      qual_none, health_bad_both, deprived_none, house_owned,
      household_one_person, ethnicity_white, born_uk, christian,
      unemployed, retired, born_elsewhere), 
  label = TRUE, label_round = 2)

# remove a few variables
ggpairs(
  df_con_19change %>% 
    st_drop_geometry() %>% 
    select(
      con_19change, con_19change, leave_hanretty, 
      health_bad_both, 
      household_one_person, 
      unemployed, retired), 
  lower=list(continuous="smooth", aes(colour="blue"))
)

ggcorr(
  df_con_19change %>% 
    st_drop_geometry() %>% 
    select(
      con_19change, leave_hanretty, 
      health_bad_both, 
      household_one_person, 
      unemployed, retired), 
  label = TRUE, label_round = 2)


# make function for nice ggpairs plot
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
g1 = ggpairs(df_con_19change %>% 
               st_drop_geometry() %>% 
               select(
                 con_19change, leave_hanretty, age_18_44, age_45_64, cars_none,
                 qual_none, health_bad_both, deprived_none, house_owned,
                 household_one_person, ethnicity_white, born_uk, christian,
                 unemployed, retired, born_elsewhere), 
             lower = list(continuous = my_fn))
g1

g2 = ggpairs(df_con_19change %>% 
               st_drop_geometry() %>% 
               select(
                 con_19change, leave_hanretty, 
                 health_bad_both, 
                 household_one_person, 
                 unemployed, retired), 
             lower = list(continuous = my_fn))
g2

#########################################


# perform OLS with various combinations of explanatory variables
model_con_19_change1 <- lm(con_19change ~ leave_hanretty + age_18_44 + age_45_64 + cars_none +
                qual_none + health_bad_both + deprived_none + house_owned +
                household_one_person + ethnicity_white + born_uk + christian +
                unemployed + retired + born_elsewhere, data = df_con_19change)
summary(model_con_19_change1)

model_con_19_change2 <- lm(con_19change ~ age_18_44 + age_45_64 + cars_none +
                qual_none + health_bad_both + deprived_none + house_owned +
                household_one_person + ethnicity_white + born_uk + christian +
                unemployed + retired + born_elsewhere, data = df_con_19change)
summary(model_con_19_change2)

model_con_19_change3 <- lm(con_19change ~ age_18_44 + age_45_64 + cars_none +
                 deprived_none + household_one_person + christian +
                 retired, data = df_con_19change)
summary(model_con_19_change3)

model_con_19_change4 <- lm(con_19change ~ leave_hanretty + age_18_44 + 
                qual_none + health_bad_both + deprived_none + 
                household_one_person + retired + born_elsewhere, data = df_con_19change)
summary(model_con_19_change4)

model_con_19_change5 <- lm(con_19change ~ leave_hanretty +
                           health_bad_both + 
                           household_one_person + 
                           unemployed + retired, data = df_con_19change)
summary(model_con_19_change5)

#################################

# LASSO

library(glmnet)

y <- df_con_19change$con_19change
x <- as.matrix(
  df_con_19change %>% 
  select(leave_hanretty, age_18_44, age_45_64, cars_none,
         qual_none, health_bad_both, deprived_none, house_owned,
         household_one_person, ethnicity_white, born_uk, christian,
         unemployed, retired) %>% 
  st_drop_geometry()
)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


#################################

# FOCUS ON 3 CHANGES IN CONSERVATIVE VOTE BETWEEN 4 ELECTIONS


# look at the change in conservative vote from 2017-2019
# regression with this as y, and age_18_44 as explanatory variable
# df_con_19change = df cons 2019 change

df_con_19change <- all_elections_reduced %>% 
  filter(!is.na(con_19), !is.na(con_17)) %>% 
  mutate(con_19change = (con_19 - con_17)) %>% # new col for change
  select(con_19change, age_18_44) %>% 
  st_drop_geometry()
reg19ch <- lm(con_19change ~ ., data = df_con_19change) # do the regression
aug19ch <- augment(reg19ch) # use augment to make a dataset with some results
df_con_19change <- all_elections_reduced %>% 
  filter(!is.na(con_19), !is.na(con_17)) %>% 
  mutate(con_19change = (con_19 - con_17),
         consflip = factor(ifelse(winner_19 == "Conservative" & winner_17 != "Conservative", 1, 0))) %>%  
           select(con_19change, age_18_44, consflip) %>% 
           mutate(stdres = aug19ch$.std.resid) %>% # add one column for std.resid from the augmented dataset
           st_as_sf()

# show these standardised residuals on a hex map
p_2019ch <- ggplot() + 
  geom_sf(data = df_con_19change, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2019 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2017-2019"))

# show which constituencies flipped to Conservative on a hex map
p_2019chflip <- ggplot() + 
  geom_sf(data = df_con_19change, aes(fill = consflip)) +
  scale_fill_manual(values=c("Grey","DarkBlue")) +
  labs(title="2019-17 - Conservative Flips")

# scatterplot of vote change vs exlanatory variable, coloured by flip or not
scat_2019ch <- ggplot(df_con_19change) + 
  geom_point(aes(x=age_18_44, y=con_19change, colour=consflip)) +
  scale_color_manual(values=c("Grey","darkblue")) + 
  geom_smooth(aes(x=age_18_44, y=con_19change), se=FALSE) + 
  geom_hline(yintercept = 0) # since scales are different, emphasise where 0 is


# and the same for change from 2015-2017
df_con_17change <- all_elections_reduced %>% 
  filter(!is.na(con_17), !is.na(con_15)) %>% 
  mutate(con_17change = (con_17 - con_15)) %>% # new col for change
  select(con_17change, age_18_44) %>% 
  st_drop_geometry()
reg17ch <- lm(con_17change ~ ., data = df_con_17change)
aug17ch <- augment(reg17ch)
df_con_17change <- all_elections_reduced %>%
  filter(!is.na(con_17), !is.na(con_15)) %>% 
  mutate(con_17change = (con_17 - con_15),
         consflip = factor(ifelse(winner_17 == "Conservative" & winner_15 != "Conservative", 1, 0))) %>%  
  select(con_17change, age_18_44, consflip) %>%
  mutate(stdres = aug17ch$.std.resid) %>% 
  st_as_sf()

p_2017ch <- ggplot() + geom_sf(data = df_con_17change, aes(fill = stdres)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2017 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2015-2017"))

p_2017chflip <- ggplot() + geom_sf(data = df_con_17change, aes(fill = consflip)) +
  scale_fill_manual(values=c("Grey","DarkBlue")) +
  labs(title="2017-15 - Conservative Flips")

scat_2017ch <- ggplot(df_con_17change) + 
  geom_point(aes(x=age_18_44, y=con_17change, colour=consflip)) +
  scale_color_manual(values=c("Grey","darkblue")) + 
  geom_smooth(aes(x=age_18_44, y=con_17change), se=FALSE) +
  geom_hline(yintercept=0)


# and the same for change from 2010-2015
df_con_15change <- all_elections_reduced %>% 
  filter(!is.na(con_15), !is.na(con_10)) %>% 
  mutate(con_15change = (con_15 - con_10)/con_10) %>% # new col for change
  select(con_15change, age_18_44) %>% 
  st_drop_geometry()
reg15ch <- lm(con_15change ~ ., data = df_con_15change)
aug15ch <- augment(reg15ch)
df_con_15change <- all_elections_reduced %>%
  filter(!is.na(con_15), !is.na(con_10)) %>% 
  mutate(con_15change = (con_15 - con_10)/con_10,
         consflip = factor(ifelse(winner_15 == "Conservative" & winner_10 != "Conservative", 1, 0))) %>%  
  select(con_15change, age_18_44, consflip) %>%
  mutate(stdres = aug15ch$.std.resid) %>% 
  st_as_sf()

p_2015ch <- ggplot() + geom_sf(data = df_con_15change, aes(fill = stdres)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2015 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2010-2015"))

p_2015chflip <- ggplot() + geom_sf(data = df_con_15change, aes(fill = consflip)) +
  scale_fill_manual(values=c("Grey","DarkBlue")) +
  labs(title="2015-10 - Conservative Flips")

scat_2015ch <- ggplot(df_con_15change) + 
  geom_point(aes(x=age_18_44, y=con_15change, colour=consflip)) +
  scale_color_manual(values=c("Grey","darkblue")) + 
  geom_smooth(aes(x=age_18_44, y=con_15change), se=FALSE) +
  geom_hline(yintercept=0)

(scat_2019ch + scat_2017ch + scat_2015ch + plot_layout(guides = "collect") & theme(legend.position = 'right'))

(p_2019ch + p_2017ch + p_2015ch + plot_layout(guides = "collect") & theme(legend.position = 'right'))

(p_2019chflip + p_2017chflip + p_2015chflip + plot_layout(guides = "collect") & theme(legend.position = 'right'))





###################################
# REMOVE SCOTLAND AND REPEAT THE ABOVE TO SEE EFFECT...
############

df_con_19change <- all_elections_reduced %>% 
  filter(!is.na(con_19), country != "Scotland") %>% 
  mutate(con_19change = (con_19 - con_17)) %>% # new col for change
  select(con_19change, age_18_44) %>% 
  st_drop_geometry()
reg19ch <- lm(con_19change ~ ., data = df_con_19change)
aug19ch <- augment(reg19ch)
df_con_19change <- all_elections_reduced %>% 
  filter(!is.na(con_19), country != "Scotland") %>% 
  mutate(con_19change = (con_19 - con_17),
         consflip = factor(ifelse(winner_19 == "Conservative" & winner_17 != "Conservative", 1, 0))) %>%  
  select(con_19change, age_18_44, consflip) %>% 
  mutate(stdres = aug19ch$.std.resid) %>% 
  st_as_sf()

p_2019ch <- ggplot() + geom_sf(data = df_con_19change, aes(fill = stdres)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2019 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2017-2019"))

p_2019chflip <- ggplot() + geom_sf(data = df_con_19change, aes(fill = consflip)) +
  scale_fill_manual(values=c("Grey","DarkBlue")) +
  labs(title="2019-17 - Conservative Flips")

scat_2019ch <- ggplot(df_con_19change) + 
  geom_point(aes(x=age_18_44, y=con_19change, colour=consflip)) +
  scale_color_manual(values=c("Grey","darkblue")) +
  geom_smooth(aes(x=age_18_44, y=con_19change), se=FALSE) +
  geom_hline(yintercept=0) +
  ylim(-15,25)
  

# and the same for change from 2015-2017
df_con_17change <- all_elections_reduced %>% 
  filter(!is.na(con_17), !is.na(con_15), country != "Scotland") %>% 
  mutate(con_17change = (con_17 - con_15)) %>% # new col for change
  select(con_17change, age_18_44) %>% 
  st_drop_geometry()
reg17ch <- lm(con_17change ~ ., data = df_con_17change)
aug17ch <- augment(reg17ch)
df_con_17change <- all_elections_reduced %>%
  filter(!is.na(con_17), !is.na(con_15), country != "Scotland") %>% 
  mutate(con_17change = (con_17 - con_15),
         consflip = factor(ifelse(winner_17 == "Conservative" & winner_15 != "Conservative", 1, 0))) %>%  
  select(con_17change, age_18_44, consflip) %>%
  mutate(stdres = aug17ch$.std.resid) %>% 
  st_as_sf()

p_2017ch <- ggplot() + geom_sf(data = df_con_17change, aes(fill = stdres)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2017 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2015-2017"))

p_2017chflip <- ggplot() + geom_sf(data = df_con_17change, aes(fill = consflip)) +
  scale_fill_manual(values=c("Grey","DarkBlue")) +
  labs(title="2017-15 - Conservative Flips")

scat_2017ch <- ggplot(df_con_17change) + 
  geom_point(aes(x=age_18_44, y=con_17change, colour=consflip)) +
  scale_color_manual(values=c("Grey","darkblue")) +
  geom_smooth(aes(x=age_18_44, y=con_17change), se=FALSE) +
  geom_hline(yintercept=0) +
  ylim(-15,25)
  

# and the same for change from 2010-2015
df_con_15change <- all_elections_reduced %>% 
  filter(!is.na(con_15), country != "Scotland") %>% 
  mutate(con_15change = (con_15 - con_10)/con_10) %>% # new col for change
  select(con_15change, age_18_44) %>% 
  st_drop_geometry()
reg15ch <- lm(con_15change ~ ., data = df_con_15change)
aug15ch <- augment(reg15ch)
df_con_15change <- all_elections_reduced %>%
  filter(!is.na(con_15), country != "Scotland") %>% 
  mutate(con_15change = (con_15 - con_10)/con_10,
         consflip = factor(ifelse(winner_15 == "Conservative" & winner_10 != "Conservative", 1, 0))) %>%  
  select(con_15change, age_18_44, consflip) %>%
  mutate(stdres = aug15ch$.std.resid) %>% 
  st_as_sf()

p_2015ch <- ggplot() + geom_sf(data = df_con_15change, aes(fill = stdres)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2015 - Standardised Residuals",
       subtitle = str_c("OLS Regression On Change In Conservative Vote 2010-2015"))

p_2015chflip <- ggplot() + geom_sf(data = df_con_15change, aes(fill = consflip)) +
  scale_fill_manual(values=c("Grey","DarkBlue")) +
  labs(title="2015-10 - Conservative Flips")

scat_2015ch <- ggplot(df_con_15change) + 
  geom_point(aes(x=age_18_44, y=con_15change, colour=consflip)) +
  scale_color_manual(values=c("Grey","darkblue")) +
  geom_smooth(aes(x=age_18_44, y=con_15change), se=FALSE) +
  geom_hline(yintercept=0)+
  ylim(-15,25)
  
(scat_2019ch + scat_2017ch + scat_2015ch + plot_layout(guides = "collect") & theme(legend.position = 'right'))

(p_2019ch + p_2017ch + p_2015ch + plot_layout(guides = "collect") & theme(legend.position = 'right'))

(p_2019chflip + p_2017chflip + p_2015chflip + plot_layout(guides = "collect") & theme(legend.position = 'right'))


###################################

# FOCUS JUST ON CHANGE IN CONSERVATIVE VOTE 2017-19

# look at the change in conservative vote from 2017-2019
# regression with this as y, and age_18_44 as explanatory variable
# df_con_19change = df cons 2019 change

df_con_19change <- all_elections_reduced %>% 
  filter(country != "Scotland") %>% 
  filter(!is.na(con_19)) %>% 
  mutate(con_19change = (con_19 - con_17)) %>% # new col for change
  select(con_19change,age_18_44) %>% 
  st_drop_geometry()
reg19ch <- lm(con_19change ~ ., data = df_con_19change)
aug19ch <- augment(reg19ch)
df_con_19change <- all_elections_reduced %>% 
  filter(country != "Scotland") %>% 
  filter(!is.na(con_19)) %>% 
  mutate(con_19change = (con_19 - con_17),
         consflip = factor(ifelse(winner_19 == "Conservative" & winner_17 != "Conservative", 1, 0))) %>%  
  select(con_19change, consflip, age_18_44) %>% 
  mutate(stdres = aug19ch$.std.resid) %>% 
  st_as_sf()

p_2019ch <- ggplot() + geom_sf(data = df_con_19change, aes(fill = stdres)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2019 - Standardised Residuals",
       subtitle = str_c("England And Wales"))

p_2019ch_isolate <- ggplot() + 
  geom_sf(data = df_con_19change, fill = "white") + 
  geom_sf(data = df_con_19change %>% filter(consflip==1), aes(fill = stdres)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2019 - Standardised Residuals",
       subtitle = str_c("Where Conservative Flips Occurred"))

p_2019chflip <- ggplot() + geom_sf(data = df_con_19change, aes(fill = consflip)) +
  scale_fill_manual(values=c("Grey","DarkBlue")) +
  labs(title="2019-17 - Conservative Flips")

scat_2019ch <- ggplot(df_con_19change) + 
  geom_point(aes(x=age_18_44, y=con_19change, colour=consflip)) +
  scale_color_manual(values=c("Grey","darkblue")) + 
  ylim(min(df_con_19change$con_19change), max(df_con_19change$con_19change)) +
  geom_smooth(aes(x=age_18_44, y=con_19change),se=FALSE) +
  facet_wrap(~consflip)

scat_2019ch + p_2019ch + p_2019ch_isolate + p_2019chflip


# MAKE A FUNCTION TO GENERALISE THIS

funct2019 <- function(x) {
df_con_19change <- all_elections_reduced %>% 
  filter(country != "Scotland") %>% 
  filter(!is.na(con_19)) %>% 
  mutate(con_19change = (con_19 - con_17)) %>% # new col for change
  select(con_19change,x) %>% 
  st_drop_geometry()
reg19ch <- lm(con_19change ~ ., data = df_con_19change)
aug19ch <- augment(reg19ch)
df_con_19change <- all_elections_reduced %>% 
  filter(country != "Scotland") %>% 
  filter(!is.na(con_19)) %>% 
  mutate(con_19change = (con_19 - con_17),
         consflip = factor(ifelse(winner_19 == "Conservative" & winner_17 != "Conservative", 1, 0))) %>%  
  select(con_19change, consflip, x) %>% 
  mutate(stdres = aug19ch$.std.resid) %>% 
  st_as_sf()

p_2019ch <- ggplot() + geom_sf(data = df_con_19change, aes(fill = stdres)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2019 - Standardised Residuals",
       subtitle = str_c("England And Wales"))

p_2019ch_isolate <- ggplot() + 
  geom_sf(data = df_con_19change, fill = "white") + 
  geom_sf(data = df_con_19change %>% filter(consflip==1), aes(fill = stdres)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(df_con_19change$stdres), max(df_con_19change$stdres))) +
  labs(title="2019 - Standardised Residuals",
       subtitle = str_c("Where Conservative Flips Occurred"))

p_2019chflip <- ggplot() + geom_sf(data = df_con_19change, aes(fill = consflip)) +
  scale_fill_manual(values=c("Grey","DarkBlue")) +
  labs(title="2019-17 - Conservative Flips")

p_2019ch + p_2019ch_isolate + p_2019chflip

}

############################
# make vectors of variables to try in the function

vars <- c("leave_hanretty","age_18_44", "age_45_64", "cars_none",
            "qual_none", "health_bad_both", "deprived_none", "house_owned",
            "household_one_person", "ethnicity_white", "born_uk", "christian",
            "unemployed", "retired", "born_elsewhere")
vars1 <- c("unemployed")
funct2019(vars)


################################

