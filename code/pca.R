library(tidyverse)
library(sf)
library(parlitools)
library(patchwork)
library(ggfortify)
library(here)

rm(list=ls())
here()

#################################

# read in required data
census_reduced <- readRDS(here("data", "census_reduced.rds"))
all_elections_reduced <- readRDS(here("data", "all_elections_reduced.rds"))
all_elections <- readRDS(here("data", "all_elections.rds"))

# read in colour data
mycolours_party2019 <- readRDS(here("data", "mycolours_party2019.rds"))
mycolours_party2017 <- readRDS(here("data", "mycolours_party2017.rds"))
mycolours_party2015 <- readRDS(here("data", "mycolours_party2015.rds"))
mycolours_party2010 <- readRDS(here("data", "mycolours_party2010.rds"))

# then make palettes for colouring by party
colscale_party2019 <- scale_colour_manual(names(mycolours_party2019), values=mycolours_party2019, name="Party")
colscale_party2017 <- scale_colour_manual(names(mycolours_party2017), values=mycolours_party2017, name="Party") 
colscale_party2015 <- scale_colour_manual(names(mycolours_party2015), values=mycolours_party2015, name="Party")
colscale_party2010 <- scale_colour_manual(names(mycolours_party2010), values=mycolours_party2010, name="Party")

# then make palettes for filling by party
fillscale_party2019 <- scale_fill_manual(names(mycolours_party2019), values=mycolours_party2019, name="Party")
fillscale_party2017 <- scale_fill_manual(names(mycolours_party2017), values=mycolours_party2017, name="Party") 
fillscale_party2015 <- scale_fill_manual(names(mycolours_party2015), values=mycolours_party2015, name="Party")
fillscale_party2010 <- scale_fill_manual(names(mycolours_party2010), values=mycolours_party2010, name="Party")

# then make palettes for colouring and filling by party and country
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
make_biplot(all_elections,"country",colscale_country)

# bi-plot coloured by country, faceted by country
make_biplot(all_elections,"country",colscale_country) + 
  facet_wrap(~country)

# bi-plot coloured by winner
make_biplot(all_elections,"winner_19",colscale_party2019)

# bi-plot coloured by winner, faceted by country
make_biplot(all_elections,"winner_19",colscale_party2019) + 
  facet_wrap(~country)

# bi-plot coloured by winner, faceted by region
make_biplot(all_elections,"winner_19",colscale_party2019) + 
  facet_wrap(~region)

# and the same as above for 2017
make_biplot(all_elections,"winner_17",colscale_party2017) + 
  facet_wrap(~region)

# and the same as above for 2015
make_biplot(all_elections,"winner_15",colscale_party2015) + 
  facet_wrap(~region)

# and the same as above for 2010
make_biplot(all_elections,"winner_10",colscale_party2010) + 
  facet_wrap(~region)




