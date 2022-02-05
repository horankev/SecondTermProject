library(tidyverse)
library(sf)
library(parlitools)
library(here)
library(ggfortify)
library(patchwork)
library(MASS)
library(stringr)
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

#################################

# Variable selection OLS

cons_change_df <- all_elections %>% 
  filter(!is.na(con_19), !is.na(con_17)) %>% 
  mutate(con_19change = (con_19 - con_17),
         children = age_0_to_4 + age_5_to_7 + age_8_to_9 + age_10_to_14 + age_15 + age_16_to_17,
         consflip = factor(ifelse(winner_19 == "Conservative" & winner_17 != "Conservative", "Flip", "No Flip")),
         redwall = ifelse(str_detect(region,"Midlands")|
                            str_detect(region,"North")|
                            str_detect(region,"York")|
                            str_detect(region,"Wales"),"Yes","No")) %>% 
  filter(country != "Scotland",
         !is.na(con_17) & !is.na(lab_17))

#################################

# Histograms

hist_cons_change1 <- ggplot(data=cons_change_df, aes(x=con_19change)) + 
  geom_histogram(aes(fill = winner_17), colour = "darkblue", position = "stack") + 
  scale_fill_manual(values = c("#0087DC","#6AB023","#DC241F","#FDBB30","#008142")) + 
  geom_vline(xintercept = 0, colour="red") + 
  labs(title = "Change In Conservative Vote 2017-19",
       subtitle = "colour by 2017 election affiliation",
       x="% Change") + 
  theme(legend.title = element_blank()) 

hist_cons_change2 <- ggplot(data=cons_change_df, aes(x=con_19change)) + 
  geom_histogram(aes(fill = winner_19), colour = "darkblue", position = "stack") + 
  scale_fill_manual(values = c("#0087DC","#6AB023","#DC241F","#FDBB30","#008142")) + 
  geom_vline(xintercept = 0, colour="red") + 
  labs(title = "Change In Conservative Vote 2017-19",
       subtitle = "colour by 2019 election affiliation",
       x="% Change") + 
  theme(legend.title = element_blank()) 

hist_cons_change3 <- ggplot(data=cons_change_df, aes(x=con_19change)) + 
  geom_histogram(aes(fill = consflip), colour = "darkblue", position = "stack") + 
  scale_fill_manual(values = c("pink","#0087DC")) + 
  geom_vline(xintercept = 0, colour="red") + 
  labs(title = "Change In Conservative Vote 2017-19",
       subtitle = "colour by flipped seat to Conservative 2019",
       x="% Change") + 
  theme(legend.title = element_blank()) 

(hist_cons_change1 + hist_cons_change2  + hist_cons_change3 + 
    plot_layout(guides = "collect") & theme(legend.position = 'bottom'))

hist_cons_change4 <- ggplot(data=cons_change_df, aes(x=con_19change)) + 
  geom_histogram(aes(fill = region), colour = "darkblue", position = "stack") + 
  geom_vline(xintercept = 0, colour="red") + 
  labs(title = "Change In Conservative Vote 2017-19",
       subtitle = "by region",
       x="% Change") + 
  theme(legend.title = element_blank()) 

hist_cons_change4


hist_cons_change5 <- ggplot(data=cons_change_df, aes(x=con_19change)) + 
  geom_histogram(aes(fill = redwall), colour = "darkblue", position = "stack") + 
  geom_vline(xintercept = 0, colour="red") + 
  labs(title = "Change In Conservative Vote 2017-19",
       subtitle = "by region",
       x="% Change") 

hist_cons_change5


hist_cons_change6 <- ggplot(data=cons_change_df %>% filter(redwall=="Yes"), aes(x=con_19change)) + 
  geom_histogram(aes(fill = consflip), colour = "darkblue", position = "stack") + 
  geom_vline(xintercept = 0, colour="red") + 
  labs(title = "Change In Conservative Vote 2017-19",
       subtitle = "by region",
       x="% Change") + 
  theme(legend.title = element_blank()) 

hist_cons_change6


hist_cons_change7 <- ggplot(data=cons_change_df, aes(x=con_19change)) + 
  geom_histogram(aes(fill = consflip), colour = "darkblue", position = "stack") + 
  geom_vline(xintercept = 0, colour="red") + 
  labs(title = "Change In Conservative Vote 2017-19",
       subtitle = "by region",
       x="% Change") + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~region)

hist_cons_change7

# Boxplots

ggplot(data=cons_change_df, aes(x=region,y=con_19change)) + 
  geom_boxplot(fill= "lightblue") + 
  #  geom_jitter(width = 0.1) +
  geom_hline(yintercept=0, colour="red") + 
  labs(title = "Change In Conservative Vote 2019-17",
       subtitle = "Boxplots By Region",
       x="% Change") + 
  coord_flip()


#################################

model1 <- lm(con_19change ~ con_17 + population_density + leave_hanretty + turnout_19 + 
               children + age_20_to_24 + cars_none + unemployed_16_to_24 + unemployed_50_to_74 + 
               qual_none + health_very_bad + health_very_good + deprived_none + deprived_2 +
               deprived_4 + house_owned + economically_inactive_student + 
               household_one_person + ethnicity_white + born_uk + christian +
               unemployed + retired, data=cons_change_df)
summary(model1)




# stepwise to find best model
# Fit the full model 
full.model <- lm(con_19change ~ con_17 + population_density + leave_hanretty + turnout_19 + 
                   children + age_20_to_24 + cars_none + unemployed_16_to_24 + unemployed_50_to_74 + 
                   qual_none + health_very_bad + health_very_good + deprived_none + deprived_2 +
                   deprived_4 + house_owned + economically_inactive_student + 
                   household_one_person + ethnicity_white + born_uk + christian + retired, 
                 data = cons_change_df)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

cons_change_df$residuals.step <- full.model$residuals
cons_change_df$consflip = 
  factor(ifelse(cons_change_df$winner_19 == "Conservative" & cons_change_df$winner_17 != "Conservative", "Flip", "No Flip"))

p_2019change <- ggplot() + 
  geom_sf(data = cons_change_df, aes(fill = residuals.step)) +
  scale_fill_gradient2(low="red",mid = "white", high="darkblue", 
                       limits=c(min(full.model$residuals), max(full.model$residuals))) +
  labs(title="2017-19 Change In Conservative Vote: Residuals",
       subtitle = str_c("using 'stepwise best choice' of OLS model"))

p_2019changeflip <- ggplot() + 
  geom_sf(data = cons_change_df, aes(fill = consflip)) +
  scale_fill_manual(values=c("DarkBlue", "Grey")) +
  labs(title="2019-17 - Conservative flips")

p_2019change_isolate <- ggplot() + 
  geom_sf(data = cons_change_df, fill = "white") + 
  geom_sf(data = cons_change_df %>% filter(consflip=="Flip"), aes(fill = residuals.step)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(cons_change_df$residuals.step), max(cons_change_df$residuals.step))) +
  labs(title="2019 - Residuals",
       subtitle = str_c("where Conservative flips occurred"))


p_2019change + p_2019changeflip + p_2019change_isolate

#################################
# OLS separately for each region
# show value of each coefficient and intercept for each region on plot
# Moran's I show spatial autocorrelation
# implement SAR spatial lag model
# then repeat showing of coefficents etc

# https://crd230.github.io/lab8.html#Exploratory_Spatial_Data_Analysis
# Then I write a function to get what I want from the regression (the coefficients in this case)
regfun <- function(x)  {
  dat <- cons_change_df[cons_change_df$region == x, ]
  m <- glm(con_19change ~ population_density + leave_hanretty + children + 
           health_very_bad + deprived_none + household_one_person + 
           born_uk + christian, data=dat)
  coefficients(m)
}

# And now run this for all counties using sapply:
region_names <- unique(cons_change_df$region)
res <- sapply(region_names, regfun)

# Plot of a single coefficient
dotchart(sort(res['health_very_bad', ]), cex=0.65)

# https://rspatial.org/raster/analysis/7-spregression.html
# Moran's I
library(broom)
library(stargazer)
library(spatialreg)
library(spdep)

constituency_mapdata <- here("data", "westminster-parliamentary-constituencies.geojson") %>%
  st_read()
cons_change_df_withgeojson <- left_join(cons_change_df %>% st_drop_geometry(), 
                                        constituency_mapdata, by = c("ons_const_id" = "pcon19cd")) %>% 
  st_as_sf()


neighbours<-poly2nb(cons_change_df_withgeojson[-c(252,569),], queen=T)
weights<-nb2listw(neighbours, style="W", zero.policy = T)

# check to see which have no neighbours: 252 and 569
weights[["neighbours"]]

moran.plot(full.model$residuals[-c(252,569)], listw=weights, 
           xlab="Change in Vote", ylab="Neighbors Change in Vote",
           main=c("Moran Scatterplot for Change in Conservative Vote 2017-19") )

moran.mc(full.model$residuals[-c(252,569)], weights, nsim=999)

fit.lag<-lagsarlm(con_19change ~ population_density + leave_hanretty + children + 
                    health_very_bad + deprived_none + household_one_person + 
                    born_uk + christian, data=cons_change_df_withgeojson[-c(252,569),], listw = weights) 
summary(fit.lag)

####
# plot residuals from sp reg
cons_change_df2 <- cons_change_df[-c(252,569),]
cons_change_df2$spreg_residuals <- fit.lag$residuals

p_2019change_sp <- ggplot() + 
  geom_sf(data = cons_change_df2, aes(fill = spreg_residuals)) +
  scale_fill_gradient2(low="red",mid = "white", high="darkblue", 
                       limits=c(min(fit.lag$residuals), max(fit.lag$residuals))) +
  labs(title="2017-19 Change In Conservative Vote: Residuals after SAR",
       subtitle = str_c("using 'stepwise best choice' of OLS model"))

p_2019changeflip_sp <- ggplot() + 
  geom_sf(data = cons_change_df2, aes(fill = consflip)) +
  scale_fill_manual(values=c("DarkBlue", "Grey")) +
  labs(title="2019-17 - Conservative flips")

p_2019change_isolate_sp <- ggplot() + 
  geom_sf(data = cons_change_df2, fill = "white") + 
  geom_sf(data = cons_change_df2 %>% filter(consflip=="Flip"), aes(fill = spreg_residuals)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(cons_change_df2$spreg_residuals), max(cons_change_df2$spreg_residuals))) +
  labs(title="2019 - Residuals after SAR",
       subtitle = str_c("where Conservative flips occurred"))


p_2019change_sp + p_2019changeflip_sp + p_2019change_isolate_sp

p_2019change + p_2019changeflip + p_2019change_isolate + p_2019change_sp + p_2019changeflip_sp + p_2019change_isolate_sp

###########################
# GWR to identify different coeffs for diff constits

library(spgwr)
coords <- st_centroid(cons_change_df_withgeojson$geometry) %>% 
  st_coordinates()

# GWR 1
GWRbandwidth <- gwr.sel(con_19change ~ population_density + leave_hanretty + children + 
                          health_very_bad + deprived_none + household_one_person + 
                          born_uk + christian, data=cons_change_df_withgeojson, coords=coords, adapt=T)

#run the gwr model
gwr.model = gwr(con_19change ~ population_density + leave_hanretty + children + 
                  health_very_bad + deprived_none + household_one_person + 
                  born_uk + christian, data=cons_change_df_withgeojson, coords=coords, 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#print the results of the model
gwr.model

results<-as.data.frame(gwr.model$SDF)
head(results)

cons_change_df3 <- cons_change_df %>% 
  mutate(
    coef_Intercept = results$X.Intercept.,    
    coef_population_density = results$population_density,
    coef_leave_hanretty = results$leave_hanretty,
    coef_children = results$children,
    coef_health_very_bad = results$health_very_bad,
    coef_deprived_none = results$deprived_none,
    coef_household_one_person = results$household_one_person,
    coef_born_uk = results$born_uk,
    coef_christian = results$christian,
  )


# using grid.arrange
plotfunc <- function(x) {
  p_2019change_sp <- ggplot() + 
    geom_sf(data = cons_change_df3, aes_string(fill = x)) +
    scale_fill_gradient2(low="red",mid = "white", high="darkblue") +
    labs(title = str_sub(x,start=6),
         subtitle = "All Constituencies")
  p_2019change_isolate_sp <- ggplot() + 
    geom_sf(data = cons_change_df, fill = "palegreen") + 
    geom_sf(data = cons_change_df3 %>% filter(consflip=="Flip"), aes_string(fill = x)) +
    scale_fill_gradient2(low="red", mid = "white", high="darkblue") +
    labs(title = str_sub(x,start=6),
         subtitle = "Conservative Flip Constituencies")
  grid.arrange(p_2019change_sp, p_2019change_isolate_sp, ncol=2)
}
p1 <- plotfunc("coef_Intercept")
p2 <- plotfunc("coef_population_density")
p3 <- plotfunc("coef_leave_hanretty")
p4 <- plotfunc("coef_children")
p5 <- plotfunc("coef_health_very_bad")
p6 <- plotfunc("coef_deprived_none")
p7 <- plotfunc("coef_household_one_person")
p8 <- plotfunc("coef_born_uk")
p9 <- plotfunc("coef_christian") 

grid.arrange(p1,p2,p3,p4,p5,p6, nrow=3, ncol=2)

#########

# GWR 2
# using a selection of other variables
GWRbandwidth2 <- gwr.sel(con_19change ~ leave_hanretty + age_20_to_24 + age_30_to_44 + cars_none +
                           qual_none + health_very_good + deprived_4 + house_owned +
                           household_one_person + ethnicity_white + born_uk + christian +
                           unemployed + retired, data=cons_change_df_withgeojson, coords=coords, adapt=T)

#run the gwr model
gwr.model2 = gwr(con_19change ~ leave_hanretty + age_20_to_24 + age_30_to_44 + cars_none +
                  qual_none + health_very_good + deprived_4 + house_owned +
                  household_one_person + ethnicity_white + born_uk + christian +
                  unemployed + retired, data=cons_change_df_withgeojson, coords=coords, 
                adapt=GWRbandwidth2, hatmatrix=TRUE, se.fit=TRUE) 
#print the results of the model
gwr.model2

results<-as.data.frame(gwr.model2$SDF)
head(results)

cons_change_df4 <- cons_change_df %>% 
  mutate(
    coef_Intercept = results$X.Intercept.,    
    coef_age_20_to_24 = results$age_20_to_24,
    coef_leave_hanretty = results$leave_hanretty,
    coef_age_30_to_44 = results$age_30_to_44,
    coef_cars_none = results$cars_none,
    coef_qual_none = results$qual_none,
    coef_health_very_good = results$health_very_good,
    coef_deprived_4 = results$deprived_4,
    coef_house_owned = results$house_owned,
    coef_household_one_person = results$household_one_person,
    coef_ethnicity_white = results$ethnicity_white,
    coef_born_uk = results$born_uk,
    coef_christian = results$christian,
    coef_unemployed = results$unemployed,
    coef_retired = results$retired
  )


# using grid.arrange
plotfunc <- function(x) {
  p_2019change_sp <- ggplot() + 
    geom_sf(data = cons_change_df4, aes_string(fill = x)) +
    scale_fill_gradient2(low="red",mid = "white", high="darkblue") +
    labs(title = str_sub(x,start=6),
         subtitle = "All Constituencies")
  p_2019change_isolate_sp <- ggplot() + 
    geom_sf(data = cons_change_df, fill = "palegreen") + 
    geom_sf(data = cons_change_df4 %>% filter(consflip=="Flip"), aes_string(fill = x)) +
    scale_fill_gradient2(low="red", mid = "white", high="darkblue") +
    labs(title = str_sub(x,start=6),
         subtitle = "Conservative Flip Constituencies")
  grid.arrange(p_2019change_sp, p_2019change_isolate_sp, ncol=2)
}
p1 <- plotfunc("coef_age_20_to_24")
p2 <- plotfunc("coef_leave_hanretty")
p3 <- plotfunc("coef_age_30_to_44")
p4 <- plotfunc("coef_cars_none")
p5 <- plotfunc("coef_qual_none")
p6 <- plotfunc("coef_health_very_good")
p7 <- plotfunc("coef_deprived_4")
p8 <- plotfunc("coef_house_owned")
p9 <- plotfunc("coef_household_one_person") 
p10 <- plotfunc("coef_ethnicity_white")
p11 <- plotfunc("coef_born_uk")
p12 <- plotfunc("coef_christian")
p13 <- plotfunc("coef_unemployed")
p14 <- plotfunc("coef_retired")

grid.arrange(p1,p2,p3,p4,p5,p6, nrow=3, ncol=2)
grid.arrange(p7,p8,p9,p10,p11,p12, nrow=3, ncol=2)
