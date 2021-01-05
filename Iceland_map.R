#Alessandra Cianfanelli 
#5/01/2020 
#Making a map of iceland and my study site 

library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot

#things to include: general area, finnafjord

#importing world map and checking everything is alright 
world <- map_data("world")
head(world)
unique(world$region)
str(world$region)

world$region <- as.factor(world$region) #making region a factor 

ice_map <- world %>% 
  filter(region == "Iceland") %>% 
  dplyr::select(-subregion)

#Plotting Iceland
(iceland <- ggplot() +
    geom_polygon(data = ice_map, aes(x = long, y = lat, group = group) 
                 , color="black", alpha = 0.7, size = 0.3) + #plot the data points on the map
    theme_minimal() + #choosing what type of background we want to display 
    ylim(62,68) +
    xlim(-27, -10) +
    coord_map())

#trying to plot things on the map 
