library(tidyverse)
library(sf)
library(parlitools)
votes2019 <- bes_2019
votes2017 <- bes_2017
votes2015 <- bes_2015
census <- census_11
carts <- west_hex_map
carts2019 <- left_join(votes2019,carts,
                      by = c("ons_const_id"="gss_code")) %>% 
  st_as_sf()
ggplot(carts2019) + geom_sf(aes(fill=green_19))
