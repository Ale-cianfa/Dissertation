#Alessandra Cianfanelli 
#5/01/2020 
#Making a map of iceland and my study site 

library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot
library(ggrepel)

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
    #ylim(65,68) + 
    #xlim(-28, -9) + 
    coord_map())

#trying to plot things on the map 64.144555, -21.941021

location <- c("Finnafjörður", "Reykjavík")
lat <- c(66.116667, 64.144555)
long <- c(-15.133333, -21.941021)
group <- as.factor(c("a", "b"))

imp_locations <- data.frame(location, lat, long, group)
head(imp_locations)
str(imp_locations)

#Plotting Iceland
(iceland <- ggplot() +
    geom_polygon(data = ice_map, aes(x = long, y = lat, group = group) 
                 , color = "black", fill = "#698B69", alpha = 0.6, size = 0.3) + #plot the data points on the map
    geom_point(data = imp_locations, aes(x = long, y = lat, colour = group), 
               colour = c("#0E4749", "#CC978E"), size = 3) +
    theme_minimal() + 
    geom_label_repel(data = imp_locations, aes(x = long, y = lat,
                                                     label = location),
                     box.padding = 5, size = 5, alpha = 0.9, nudge_x = 1,
                     min.segment.length = 0, inherit.aes = FALSE) + 
    ylim(62,68) +
    xlim(-27, -10) +
    coord_map())



