# CROPPING THE DATA FOR MY DISSERTATION 


## PACKAGES----

library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot
library(ggrepel)
library(ggthemes) #for extra map themes

getwd()
## LOADING FILES----

### Iceland shapefile (that I do not have)----
#iceland <- st_read("data/iceland-isn93/is50v_strandlina_flakar_24122017.shp")

### NASS survey----
survey <- read.csv("original NASS/ale_nass-sightings_tot.csv")
head(survey)
str(survey)
survey$spec <- as.factor(survey$spec)
#we are going to use la2 and lo2 as the coordinates for the whales, because those were the locations
  #of observers when they saw the whale and is as good as its gonna get 

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

### File with survey efferts----

effort <- st_read()

## TYDING UP NASS CSV----
#I want a spatial extent that includes only the NE (-19, -9 in long and 65, 68 in lat)
  #Also, i am going to remove all species that are not humpback whales 

new_survey <- survey %>% 
  select(year, month, spec, la2, lo2, pods) %>% 
  filter(spec == "mn") #%>% #keeping only the humpback whales 
  #filter(lo2 > -20) #keeping only the neat area -19.99999 to -9

#write_csv(new_survey, file = "new_survey.csv")
#NOTA: I need to find a way to make pods into individual observations 

new_survey$year <- as.factor(new_survey$year)
str(new_survey)

# PLOTTING ICELAND----

##Making the box----
lon <- c(-20, -9) # Longitudinal range
lat <- c(65, 68) # Latitudinal range
pts <- rbind(c(lon[1], lat[1]), c(lon[2], lat[1]), c(lon[2], lat[2]), c(lon[1], lat[2]), c(lon[1], lat[1]))
box <- st_sfc(st_polygon(list(pts)), crs = 4326) # creating bounding box
ggplot(data = box) + geom_sf() # plotting to check

## Basic Iceland plot----
(iceland <- ggplot() +
    geom_polygon(data = ice_map, aes(x = long, y = lat, group = group) 
                 , color="black", alpha = 0.7, size = 0.3) + #plot the data points on the map
    theme_minimal() + #choosing what type of background we want to display 
    ylim(62,68) +
    xlim(-27, -10) +
    #ylim(65,68) + 
    #xlim(-28, -9) + 
    coord_map())

## Adding locations of Reykjavik and Port----
location <- c("Finnafjörður", "Reykjavík")
lat <- c(66.116667, 64.144555)
long <- c(-15.133333, -21.941021)
group <- as.factor(c("a", "b"))

imp_locations <- data.frame(location, lat, long, group)
head(imp_locations)
str(imp_locations)

## Making map + lables----
(iceland <- ggplot() +
    geom_polygon(data = ice_map, aes(x = long, y = lat, group = group) 
                 , color = "black", fill = "#698B69", alpha = 0.6, size = 0.3) + #plot the data points on the map
    geom_point(data = imp_locations, aes(x = long, y = lat, colour = group), 
               colour = c("#0E4749", "#CC978E"), size = 3) +
    geom_point(data = new_survey, aes(x = lo2, y = la2, colour = year, size = pods)) + #adding pod sixe ad a point size, not sure is useful, just an idea
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

#ggsave(iceland, file = "img/survey2.png", height = 5, width = 8)

## Adding effort to the map----


effort <- st_read("original NASS/ale_nass-effort.gpkg") %>% 
  # crop to bounding box
  st_crop(box)

#TRYING MY NEW CSV----
n_ice_survey <- read.csv("modified NASS/north_nass_survey_prova.csv") #this is the new csv file with the broke down pods

n_ice_survey$year <- as.factor(n_ice_survey$year)
str(n_ice_survey)

## Making map + lables pt2----
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

#Total survey per year----

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

#this could go into an important discussion on the implicit bias that, maybe 2007 might have been a great year, we just do't know becuse there aren't nearly as many sightings





