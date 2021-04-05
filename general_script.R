# R SCRIPT FOR DATA ANALYSIS OF ENVIRONMENTAL VARIABLES

## PACKAGES----

library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot
library(ggrepel)
library(ggthemes) #for extra map themes

### Iceland map background----

#importing world map and checking everything is alright 
world <- map_data("world")
head(world)
unique(world$region)
str(world$region)

world$region <- as.factor(world$region) #making region a factor 

ice_map <- world %>% 
  filter(region == "Iceland") %>% 
  dplyr::select(-subregion)

### Making a map with the new csv----

n_ice_survey <- read.csv("modified NASS/north_nass_survey_prova.csv") #this is the new csv file with the broke down pods

n_ice_survey$year <- as.factor(n_ice_survey$year)
str(n_ice_survey)

## Making map + lables

(iceland1 <- ggplot() +
   geom_polygon(data = ice_map, aes(x = long, y = lat, group = group) 
                , color = "black", fill = "#698B69", alpha = 0.6, size = 0.3) + #plot the data points on the map
   geom_point(data = imp_locations, aes(x = long, y = lat, colour = group), 
              colour = c("#0E4749", "#CC978E"), size = 3) +
   geom_point(data = n_ice_survey, aes(x = lo2, y = la2, colour = year)) + #adding pod sixe ad a point size, not sure is useful, just an idea
   #scale_color_manual(values = c("#70161E", "#F6BE13", "#5C80BC")) +
   scale_color_manual(values = c("#A06B9A", "#8D9EC6", "#082241")) +
   #geom_rect(data = ice_map, aes(xmin = 65, xmax = 68, ymin = -9, ymax = -20),
   #fill = "transparent", color = "red", size = 0.5) + 
   theme_minimal() + 
   theme(legend.position = "right",
         legend.title = element_text(size = 13, face ="bold"),
         legend.text = element_text(size = 12)) +
   geom_label_repel(data = imp_locations, aes(x = long, y = lat,
                                              label = location),
                    box.padding = 5, size = 5, alpha = 0.9, nudge_y = -0.5,
                    min.segment.length = 0, inherit.aes = FALSE) + 
   ylim(62,68) +
   xlim(-27, -10) +
   labs(title = "", colour = "Survey Year") +
   coord_map())

### Total sightings per year----

sightings_x_years <- n_ice_survey %>% group_by(year) %>% 
  summarise(total_count = n())

sightings_x_years$total_count <- as.factor(sightings_x_years$total_count)

(sightings_x_years_plot <- ggplot(sightings_x_years, aes(x = year, y = total_count, fill = total_count)) + #specifying what to put on the axis
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = c("#8D9EC6","#082241", "#A06B9A")) +    theme_minimal() + 
    theme(legend.position = "none",
          legend.title = element_text(size = 13, face ="bold"),
          legend.text = element_text(size = 12)) +
    labs(fill = "Total Sightings", 
         x = "Years", 
         y ="Total Sightings"))
#ggsave(sightings_x_years_plot, file = "img/sightings_x_years.png", height = 5, width = 9)









