library(tidyverse)
library(sf)
library(parlitools)
library(here)
library(ggfortify)
library(patchwork)
library(stringr)
library(here)
library(GGally)
library(spatialreg)
library(spdep)
library(broom)
library(spgwr)
library(gridExtra)
library(grid)
library(Hmisc)
library(MASS)

rm(list=ls())
here()

#################################

# read in required data

all_elections <- readRDS(here("data", "all_elections.rds"))
party_colour <- party_colour

#################################
# focus on change in votes
# map this for different parties

ggplot() + geom_sf(data=all_elections, aes(fill=con_19-con_17)) + 
  scale_fill_gradient2(low = "black", high = "#0087DC", na.value = "darkred") +
  labs(fill = "Percentage",
       title = "Vote Change 2017-19",
       subtitle = "Conservative")
ggplot() + geom_sf(data=all_elections, aes(fill=lab_19-lab_17)) + 
  scale_fill_gradient2(low = "black", high = "#DC241F", na.value = "darkred") +
  labs(fill = "Percentage",
       title = "Vote Change 2017-19",
       subtitle = "Labour")
ggplot() + geom_sf(data=all_elections, aes(fill=ld_19-ld_17)) + 
  scale_fill_gradient2(low = "black", high = "#FDBB30", na.value = "darkred") +
  labs(fill = "Percentage",
       title = "Vote Change 2017-19",
       subtitle = "Liberal Democrat")
ggplot() + geom_sf(data=all_elections, aes(fill=green_19-green_17)) + 
  scale_fill_gradient2(low = "black", high = "#6AB023", na.value = "darkred") +
  labs(fill = "Percentage",
       title = "Vote Change 2017-19",
       subtitle = "Green Party")
ggplot() + geom_sf(data=all_elections, aes(fill=pc_19-pc_17)) + 
  scale_fill_gradient2(low = "black", high = "#008142", na.value = "darkred") +
  labs(fill = "Percentage",
       title = "Vote Change 2017-19",
       subtitle = "Plaid Cymru")
ggplot() + geom_sf(data=all_elections, aes(fill=snp_19-snp_17)) + 
  scale_fill_gradient2(low = "black", high = "#FFFF00", na.value = "darkred") +
  labs(fill = "Percentage",
       title = "Vote Change 2017-19",
       subtitle = "Scottish National Party")

# since SNP completely dominant in Scotland, but does not run elsewhere
# will treat England & Wales as one dataset "all_elections1"
# Scotland will be "all_elections2"
# examine each separately

# focus on change in conservative vote in England and Wales
# will add variable for change in conservative vote 2017-19: "con_change"
all_elections <- all_elections %>% 
  mutate(con_change = con_19 - con_17) %>% 
  filter(!is.na(con_change)) # where didn't compete, speaker's seat
all_elections1 <- all_elections %>% 
  filter(country != "Scotland")
all_elections2 <- all_elections %>% 
  filter(country == "Scotland")

ggplot() + geom_sf(data=all_elections1, aes(fill = con_19 - con_17)) + 
  scale_fill_gradient2(low = "black", high = "#0087DC", na.value = "darkred") +
  labs(fill = "Conservative %")
#################################

# by eyeballing, general Conservative decrease in London and contiguous areas 
# of South West, West and East Midlands,
# general Conservative increase elsewhere

# Moran's I of "con_change"

# read in polygon spatial data for constituencies
constituency_polygons <- readRDS(here("data", "constituency_polygons.rds"))

all_elections_polygons <- left_join(all_elections1 %>% st_drop_geometry(), 
                            constituency_polygons, by = c("ons_const_id" = "pcon19cd")) %>% 
  st_as_sf()

# # make appropriate matrices for spatial analysis
# neighbours <- poly2nb(all_elections_polygons, queen=T)
# weights <- nb2listw(neighbours, style="W", zero.policy = T)
# 
# # check to see which have no neighbours
# weights[["neighbours"]]
# # 253 and 570
# # remove these two islands

# construct two matrices for spatial analysis
neighbours <- poly2nb(all_elections_polygons[-c(253,570),], queen=T)
weights <- nb2listw(neighbours, style="W", zero.policy = T)
weights[["neighbours"]]

moran.plot(all_elections1$con_change[-c(253,570)], listw=weights, 
           xlab="Change in Vote", ylab="Neighbors Change in Vote",
           main=c("Moran Scatterplot for Change in Conservative Vote 2017-19") )

moran.mc(all_elections1$con_change[-c(253,570)], weights, nsim=999)

#################################

# select variables

# extract the variables or combination variables which have been chosen...

all_elections1 <- all_elections1 %>%
  mutate(children = age_0_to_4 + age_5_to_7 + age_8_to_9 + age_10_to_14 + age_15 + age_16_to_17,
         age_18_44 = age_18_to_19 + age_20_to_24 + age_25_to_29 + age_30_to_44,
         age_45_64 = age_45_to_59 + age_60_to_64,
         born_elsewhere = born_ireland + born_other_eu + born_other_pre_2004_eu + born_post_2004_eu + born_other)
vars <- all_elections1 %>% 
  dplyr::select(con_change, con_17, population_density, children, age_18_44, qual_none, health_very_bad, house_owned, household_one_person, 
                ethnicity_white, unemployed, economically_inactive_student, retired, born_elsewhere, leave_hanretty, geometry) %>% 
  st_drop_geometry()


#################################

# examine data distribution
Hmisc::hist.data.frame(vars)

#################################
# compare correlation

ggcorr(vars, label = TRUE, label_round = 2, hjust = 0.9)
# iteratively removed some where high correltion was a clear additive outcome
# some high correlations remain by seem explanatory or other type of correlation...

sc_p1 <- ggplot(data=vars, aes(x=con_change, y=con_17)) + geom_point() + geom_smooth()
sc_p2 <- ggplot(data=vars, aes(x=con_change, y=population_density)) + geom_point() + geom_smooth()
sc_p3 <- ggplot(data=vars, aes(x=con_change, y=children)) + geom_point() + geom_smooth()
sc_p4 <- ggplot(data=vars, aes(x=con_change, y=age_18_44)) + geom_point() + geom_smooth()
sc_p5 <- ggplot(data=vars, aes(x=con_change, y=qual_none)) + geom_point() + geom_smooth()
sc_p6 <- ggplot(data=vars, aes(x=con_change, y=health_very_bad)) + geom_point() + geom_smooth()
sc_p7 <- ggplot(data=vars, aes(x=con_change, y=house_owned)) + geom_point() + geom_smooth()
sc_p8 <- ggplot(data=vars, aes(x=con_change, y=household_one_person)) + geom_point() + geom_smooth()
sc_p9 <- ggplot(data=vars, aes(x=con_change, y=ethnicity_white)) + geom_point() + geom_smooth()
sc_p10 <- ggplot(data=vars, aes(x=con_change, y=unemployed)) + geom_point() + geom_smooth()
sc_p11 <- ggplot(data=vars, aes(x=con_change, y=economically_inactive_student)) + geom_point() + geom_smooth()
sc_p12 <- ggplot(data=vars, aes(x=con_change, y=retired)) + geom_point() + geom_smooth()
sc_p13 <- ggplot(data=vars, aes(x=con_change, y=born_elsewhere)) + geom_point() + geom_smooth()
sc_p14 <- ggplot(data=vars, aes(x=con_change, y=leave_hanretty)) + geom_point() + geom_smooth()

sc_p1 + sc_p2 + sc_p3 + sc_p4 + sc_p5
sc_p6 + sc_p7 + sc_p8 + sc_p9 + sc_p10
sc_p11 + sc_p12 + sc_p13 + sc_p14

#################################

# LM
lin_mod1 <- lm(con_change ~ ., data = vars)
summary(lin_mod1)

#################################

# ?? find better selection of variables
# Stepwise regression model
lin_mod2 <- stepAIC(lin_mod1, direction = "both", 
                      trace = FALSE)
summary(lin_mod2)

#################################
# diagnostics

autoplot(lin_mod2)

#################################

# map LM residuals

aug_lin_mod <- augment(lin_mod1)

all_elections1 <- all_elections1 %>%
  mutate(stdres = aug_lin_mod$.std.resid) %>% 
  st_as_sf()

ggplot() + 
  geom_sf(data = all_elections1, aes(fill = stdres)) +
  scale_fill_gradient2(low="red",mid = "white", high="darkblue", 
                       limits=c(min(all_elections1$stdres), max(all_elections1$stdres))) +
  labs(title="Change In Conservative Vote 2017-19",
       subtitle = "Linear Model Residuals")

#################################

# Moran's I of LM residuals

moran.plot(lin_mod1$residuals[-c(253,570)], listw=weights, 
           xlab="Change in Vote", ylab="Neighbors Change in Vote",
           main=c("Moran Scatterplot for Change in Conservative Vote 2017-19") )

moran.mc(lin_mod1$residuals[-c(253,570)], weights, nsim=999)

#################################

# perform SAM
fit.lag<-lagsarlm(con_change ~ con_17 + population_density + children + age_18_44 + 
                    qual_none + health_very_bad + house_owned + household_one_person + 
                    ethnicity_white + unemployed + economically_inactive_student + 
                    retired + born_elsewhere + leave_hanretty, data=vars[-c(253,570),], listw = weights) 
summary(fit.lag)

#################################

# Moran's I of SAM residuals

temp <- all_elections1[-c(253,570),]
temp$sp_reg_residuals <- fit.lag$residuals

moran.plot(temp$sp_reg_residuals, listw=weights, 
           xlab="Change in Vote", ylab="Neighbors Change in Vote",
           main=c("Moran Scatterplot for Change in Conservative Vote 2017-19") )

moran.mc(temp$sp_reg_residuals, weights, nsim=999)

#################################

# Map SAM residuals

ggplot() + geom_sf(data=temp,aes(fill=sp_reg_residuals)) +
  scale_fill_gradient2(low="red",mid = "white", high="darkblue", 
                       limits=c(min(all_elections1$stdres), max(all_elections1$stdres))) +
  labs(title="Change In Conservative Vote 2017-19",
       subtitle = "Spatial Autocorrelation Model Residuals")


#################################

# GWR
# to identify different coefficients for diff constituencies

# create a matrix of co-ordinates for this function
coords <- st_centroid(all_elections_polygons$geometry) %>% 
  st_coordinates()

# GWR using variables from lin_mod1
GWRbandwidth <- gwr.sel(con_change ~ con_17 + population_density + children + age_18_44 + 
                          qual_none + health_very_bad + house_owned + household_one_person + 
                          ethnicity_white + unemployed + economically_inactive_student + 
                          retired + born_elsewhere + leave_hanretty, data=all_elections1, coords=coords, adapt=T)

GWRbandwidth

#run the gwr model
gwr.model = gwr(con_change ~ con_17 + population_density + children + age_18_44 + 
                  qual_none + health_very_bad + house_owned + household_one_person + 
                  ethnicity_white + unemployed + economically_inactive_student + 
                  retired + born_elsewhere + leave_hanretty, data=all_elections1, coords=coords, 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#print the results of the model
gwr.model

GWR_results <- as.data.frame(gwr.model$SDF)
head(GWR_results)

all_elections1 <- all_elections1 %>% 
  mutate(
    GWRcoef_Intercept = GWR_results$X.Intercept.,    
    GWRcoef_con_17 = GWR_results$con_17,
    GWRcoef_population_density = GWR_results$population_density,
    GWRcoef_children = GWR_results$children,
    GWRcoef_age_18_44 = GWR_results$age_18_44,
    GWRcoef_qual_none = GWR_results$qual_none,
    GWRcoef_health_very_bad = GWR_results$health_very_bad,
    GWRcoef_house_owned = GWR_results$house_owned,
    GWRcoef_household_one_person = GWR_results$household_one_person,
    GWRcoef_ethnicity_white = GWR_results$ethnicity_white,
    GWRcoef_unemployed = GWR_results$unemployed,
    GWRcoef_economically_inactive_student = GWR_results$economically_inactive_student,
    GWRcoef_retired = GWR_results$retired,
    GWRcoef_born_elsewhere = GWR_results$born_elsewhere, 
    GWRcoef_leave_hanretty = GWR_results$leave_hanretty
  )

# function to plot coefficients
plotfunc <- function(x) {
  p_2019change_sp <- ggplot() + 
    geom_sf(data = all_elections1, aes_string(fill = x)) +
    scale_fill_gradient2(low="red",mid = "white", high="darkblue") +
    labs(title = str_sub(x,start=9)) + 
    theme(legend.title = element_blank())
}

p1 <- plotfunc("GWRcoef_Intercept")
p2 <- plotfunc("GWRcoef_con_17")
p3 <- plotfunc("GWRcoef_population_density")
p4 <- plotfunc("GWRcoef_children")
p5 <- plotfunc("GWRcoef_age_18_44")
p6 <- plotfunc("GWRcoef_qual_none")
p7 <- plotfunc("GWRcoef_health_very_bad")
p8 <- plotfunc("GWRcoef_house_owned")
p9 <- plotfunc("GWRcoef_household_one_person") 
p10 <- plotfunc("GWRcoef_ethnicity_white") 
p11 <- plotfunc("GWRcoef_unemployed") 
p12 <- plotfunc("GWRcoef_economically_inactive_student") 
p13 <- plotfunc("GWRcoef_retired") 
p14 <- plotfunc("GWRcoef_born_elsewhere") 
p15 <- plotfunc("GWRcoef_leave_hanretty") 

# # using grid-arrange
# grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15, 
#              nrow=3, ncol=5, 
#              top=textGrob("Coefficients for GWR", gp=gpar(fontsize=25,font=12)))

# using patchwork
p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+p13+p14+p15 + plot_layout(nrow = 3) + 
  plot_annotation(title = 'Coefficients for GWR',
                  theme = theme(plot.title = element_text(size = 18))) & 
  theme(text = element_text('mono'))

#################################

# plot local r2

all_elections1 <- all_elections1 %>% 
  mutate(
    GWR_localR2 = GWR_results$localR2)

ggplot() + 
  geom_sf(data = all_elections1, aes(fill = GWR_localR2)) +
  scale_fill_gradient2(low="red",mid = "white", high="darkblue") +
  labs(title = "Local R-squared") + 
  theme(legend.title = element_blank())



#################################
#################################
#################################

# GWR2 using variables from lin_mod2, derived from step-wise function

# GWR 2
GWRbandwidth2 <- gwr.sel(con_change ~ con_17 + population_density + age_18_44 + 
                          qual_none + health_very_bad + house_owned + household_one_person + 
                          retired + leave_hanretty, data=all_elections1, coords=coords, adapt=T)

GWRbandwidth2

#run the gwr model
gwr.model2 = gwr(con_change ~ con_17 + population_density + age_18_44 + 
                   qual_none + health_very_bad + house_owned + household_one_person + 
                   retired + leave_hanretty, data=all_elections1, coords=coords, 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#print the results of the model
gwr.model2

GWR_results2 <- as.data.frame(gwr.model2$SDF)
head(GWR_results2)

all_elections1 <- all_elections1 %>% 
  mutate(
    GWR2coef_Intercept = GWR_results2$X.Intercept.,    
    GWR2coef_con_17 = GWR_results2$con_17,
    GWR2coef_population_density = GWR_results2$population_density,
    GWR2coef_age_18_44 = GWR_results2$age_18_44,
    GWR2coef_qual_none = GWR_results2$qual_none,
    GWR2coef_health_very_bad = GWR_results2$health_very_bad,
    GWR2coef_house_owned = GWR_results2$house_owned,
    GWR2coef_household_one_person = GWR_results2$household_one_person,
    GWR2coef_retired = GWR_results2$retired,
    GWR2coef_leave_hanretty = GWR_results2$leave_hanretty
  )

# function to plot coefficients
plotfunc <- function(x) {
  p_2019change_sp <- ggplot() + 
    geom_sf(data = all_elections1, aes_string(fill = x)) +
    scale_fill_gradient2(low="red",mid = "white", high="darkblue") +
    labs(title = str_sub(x,start=9)) + 
    theme(legend.title = element_blank())
}

p2.1 <- plotfunc("GWRcoef_Intercept")
p2.2 <- plotfunc("GWRcoef_con_17")
p2.3 <- plotfunc("GWRcoef_population_density")
p2.4 <- plotfunc("GWRcoef_age_18_44")
p2.5 <- plotfunc("GWRcoef_qual_none")
p2.6 <- plotfunc("GWRcoef_health_very_bad")
p2.7 <- plotfunc("GWRcoef_house_owned")
p2.8 <- plotfunc("GWRcoef_household_one_person") 
p2.9 <- plotfunc("GWRcoef_retired") 
p2.10 <- plotfunc("GWRcoef_leave_hanretty") 

# # using grid-arrange
# grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15, 
#              nrow=3, ncol=5, 
#              top=textGrob("Coefficients for GWR", gp=gpar(fontsize=25,font=12)))

# using patchwork
p2.1+p2.2+p2.3+p2.4+p2.5+p2.6+p2.7+p2.8+p2.9+p2.10 + plot_layout(nrow = 3) + 
  plot_annotation(title = 'Coefficients for GWR2',
                  theme = theme(plot.title = element_text(size = 18))) & 
  theme(text = element_text('mono'))

#################################

# plot local r2

all_elections1 <- all_elections1 %>% 
  mutate(
    GWR2_localR2 = GWR_results2$localR2)

ggplot() + 
  geom_sf(data = all_elections1, aes(fill = GWR2_localR2)) +
  scale_fill_gradient2(low="red",mid = "white", high="darkblue") +
  labs(title = "Local R-squared") + 
  theme(legend.title = element_blank())


#################################
#################################
#################################
#################################

# compare AICs

AIC(lin_mod1)
AIC(lin_mod2)
fit.lag$AIC_lm.model
gwr.model$results$AICc
gwr.model2$results$AICc

#################################

