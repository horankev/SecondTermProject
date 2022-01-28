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

# then make palettes for colouring and filling by country
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

#################################

# performing a SEPARATE PCA CALCULATED ON EACH REGION, 
# and showing party winner for each year, and visualising it...

# vector of regions...
r <- unique(all_elections_reduced$region)

# vector of elections
w <- c("winner_19", "winner_17", "winner_15", "winner_10")

# empty list
plot_list = list()

# initialise list's index
z=1

# loop through each year, sub_looping through each region
# adding each plot into a 44 length list (4 elections * 11 regions)

for (j in 1:length(w)) {
  for (i in 1:length(r)) {
    
    y <- all_elections_reduced %>% 
      filter(region==r[i]) %>% 
      st_drop_geometry()
    
    PCAresults_loop <- prcomp(y[,116:130], scale. = TRUE)
    plot_list[[z]] <- autoplot(PCAresults_loop, data = y, geom = 'point', 
                               colour = w[j], size=1.5, loadings = TRUE,
                               loadings.colour = 'black', 
                               loadings.label = TRUE, loadings.label.size = 4, 
                               loadings.label.colour = "black", loadings.label.hjust = -0.15) + 
      colscale_party2019 + 
      theme(plot.background=element_blank(),
          panel.background=element_rect(fill='transparent', color='black',size=2),
          legend.text=element_text(hjust=1),
          legend.key=element_blank()) + 
      labs(title = paste0(r[i], " 20", str_sub(w[j], start= -2)))
    z=z+1
  }
}

# 2019
(plot_list[[1]] + plot_list[[2]] + plot_list[[3]] + plot_list[[4]] + plot_list[[5]] + plot_list[[6]] +
    plot_list[[7]] + plot_list[[8]]+ plot_list[[9]] + plot_list[[10]] + plot_list[[11]]) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# South East
(plot_list[[3]]+ plot_list[[14]] + plot_list[[25]] + plot_list[[36]]) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# North West
(plot_list[[5]]+ plot_list[[16]] + plot_list[[27]] + plot_list[[38]]) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# North East
(plot_list[[11]]+ plot_list[[22]] + plot_list[[33]] + plot_list[[44]]) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# Yorkshire and The Humber
(plot_list[[8]]+ plot_list[[19]] + plot_list[[30]] + plot_list[[41]]) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

