library(tidyverse)
library(sf)
library(parlitools)
library(here)
library(ggfortify)
library(patchwork)
library(MASS)
library(stringr)

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

model1 <- lm(con_19change ~ con_19 + population_density + leave_hanretty + turnout_19 + 
               children + age_20_to_24 + cars_none + unemployed_16_to_24 + unemployed_50_to_74 + 
               qual_none + health_very_bad + health_very_good + deprived_none + deprived_2 +
               deprived_4 + house_owned + economically_inactive_student + 
               household_one_person + ethnicity_white + born_uk + christian +
               unemployed + retired, data=cons_change_df)
summary(model1)




# stepwise to find best model
# Fit the full model 
full.model <- lm(con_19change ~ con_19 + population_density + leave_hanretty + turnout_19 + 
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
cons_change_df$consflip = factor(ifelse(cons_change_df$winner_19 == "Conservative" & cons_change_df$winner_17 != "Conservative", 1, 0))

p_2019ch <- ggplot() + 
  geom_sf(data = cons_change_df, aes(fill = residuals.step)) +
  scale_fill_gradient2(low="red",mid = "white", high="darkblue", 
                       limits=c(min(full.model$residuals), max(full.model$residuals))) +
  labs(title="2017-19 Change In Conservative Vote: Residuals",
       subtitle = str_c("using 'stepwise best choice' of OLS model, Y = "))

p_2019chflip <- ggplot() + 
  geom_sf(data = cons_change_df, aes(fill = consflip)) +
  scale_fill_manual(values=c("Grey","DarkBlue")) +
  labs(title="2019-17 - Conservative flips")

p_2019ch_isolate <- ggplot() + 
  geom_sf(data = cons_change_df, fill = "white") + 
  geom_sf(data = cons_change_df %>% filter(consflip==1), aes(fill = residuals.step)) +
  scale_fill_gradient2(low="red", mid = "white", high="darkblue", 
                       limits=c(min(cons_change_df$residuals.step), max(cons_change_df$residuals.step))) +
  labs(title="2019 - Residuals",
       subtitle = str_c("where Conservative flips occurred"))


p_2019ch + p_2019chflip + p_2019ch_isolate
