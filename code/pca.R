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
census_reduced <- readRDS(here("data", "census_reduced.rds"))
comb2019_reduced <- readRDS(here("data", "comb2019_reduced.rds"))
party_colour <- readRDS(here("data", "party_colour.rds"))
comb2019 <- readRDS(here("data", "comb2019.rds"))
comb2017 <- readRDS(here("data", "comb2017.rds"))
comb2015 <- readRDS(here("data", "comb2015.rds"))

# set up colour scales
mycolours_party2019 <- unique(comb2019$party_colour)
names(mycolours_party2019) <- unique(comb2019$winner_19)
mycolours_party2017 <- unique(comb2017$party_colour)
names(mycolours_party2017) <- unique(comb2017$winner_17)
mycolours_party2015 <- unique(comb2015$party_colour)
names(mycolours_party2015) <- unique(comb2015$winner_15)

colscale_party2019 <- scale_colour_manual(names(mycolours_party2019), values=mycolours_party2019, name="Party")
colscale_party2017 <- scale_colour_manual(names(mycolours_party2017), values=mycolours_party2017, name="Party") 
colscale_party2015 <- scale_colour_manual(names(mycolours_party2015), values=mycolours_party2015, name="Party")
fillscale_party2019 <- scale_fill_manual(names(mycolours_party2019), values=mycolours_party2019, name="Party")
fillscale_party2017 <- scale_fill_manual(names(mycolours_party2017), values=mycolours_party2017, name="Party") 
fillscale_party2015 <- scale_fill_manual(names(mycolours_party2015), values=mycolours_party2015, name="Party")
mycolours_country <- c("red", "blue4", "#008142")
names(mycolours_country) <- c("England", "Scotland", "Wales")
colscale_country <- scale_colour_manual(names(mycolours_country), values=mycolours_country, name="Country")
fillscale_country <- scale_fill_manual(names(mycolours_country), values=mycolours_country, name="Country")
#################################

# PCA

# calculate principal components
PCAresults <- prcomp(census_reduced %>% select(-pano), scale. = TRUE)

# function for drawing bi-plot
make_biplot <- function(data, colourby, colourscale) {
  autoplot(PCAresults, data = data, geom = 'point', colour = colourby, size=1.5, loadings = TRUE,
           loadings.colour = 'black',
           loadings.label = TRUE, loadings.label.size = 4, loadings.label.colour = "black", loadings.label.hjust = -0.15) + 
    colourscale + 
    theme(plot.background=element_blank(),
          panel.background=element_rect(fill='transparent', color='black',size=2),
          legend.text=element_text(hjust=1),
          legend.key=element_blank())
}

# bi-plot coloured by country
make_biplot(comb2019,"country",colscale_country)

# bi-plot coloured by country, faceted by country
make_biplot(comb2019,"country",colscale_country) + 
  facet_wrap(~country)

# bi-plot coloured by winner
make_biplot(comb2019,"winner_19",colscale_party2019)

# bi-plot coloured by winner, faceted by country
make_biplot(comb2019,"winner_19",colscale_party2019) + 
  facet_wrap(~country)

# bi-plot coloured by winner, faceted by region
make_biplot(comb2019,"winner_19",colscale_party2019) + 
  facet_wrap(~region)









