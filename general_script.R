# R SCRIPT FOR DATA ANALYSIS OF ENVIRONMENTAL VARIABLES

## Packages----

library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot
library(ggrepel)
library(ggthemes) #for extra map themes
library(raster)
library(rgdal)
library(rasterVis)
library(sp)


## NASS survey----
survey <- read.csv("Survey/original NASS/ale_nass-sightings_tot.csv")
head(survey)
str(survey)
survey$spec <- as.factor(survey$spec)

### NASS ships
vessels <- read.csv("Survey/original NASS/nass_vessels.csv")

unique(survey$vID) #TOTAL NUMBER OF VESSELS

## Iceland map----

### Importing world map and checking everything is alright 
world <- map_data("world")
head(world)
unique(world$region)
str(world$region)

world$region <- as.factor(world$region) #making region a factor 

ice_map <- world %>% 
  filter(region == "Iceland") %>% 
  dplyr::select(-subregion)

### New CSV file (that might not be useful after all)

n_ice_survey <- read.csv("Survey/modified NASS/north_nass_survey_prova.csv") #this is the new csv file with the broke down pods

n_ice_survey$year <- as.factor(n_ice_survey$year)
str(n_ice_survey)

### Adding the effort geopackage 

effort <- st_read("Survey/original NASS/ale_nass-effort.gpkg")

### Adding locations of Reykjavik and Port
location <- c("Finnafjörður", "Reykjavík")
lat <- c(66.116667, 64.144555)
long <- c(-15.133333, -21.941021)
group <- as.factor(c("a", "b"))

imp_locations <- data.frame(location, lat, long, group)
head(imp_locations)
str(imp_locations)

### Making map + lables

(site_map <- ggplot() +
   geom_polygon(data = ice_map, aes(x = long, y = lat, group = group) 
                , color = "black", fill = "#698B69", alpha = 0.6, size = 0.3) + #plot the data points on the map
   geom_point(data = imp_locations, aes(x = long, y = lat, colour = group), 
              colour = c("#0E4749", "#CC978E"), size = 3) +
   geom_sf(data = effort, color = "black", alpha = 0.6) + # effort
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
   coord_sf())

#ggsave(site_map, file = "img/survey_final.png", height = 5, width = 8)

### Total sightings per year

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

## Visualizing raster data----

### Bathymetry:

bathymetry <- raster("Parameters/bathymetry/bat_1.tif")

bat_TID <- raster("Parameters/bathymetry/bat_2_TID.tif")

bathymetry #to get the properties

plot(bathymetry)

#png("img/bat.png", width = 6, height = 4, units = "in", res = 300)

#image(bathymetry, col = viridis_pal(option = "D", direction = -1)(10), main = "Bathymetry of Iceland")

#dev.off()

#OPTION = A character string indicating the colormap option to use. 
#Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), 
#"viridis" (or "D", the default option) and "cividis" (or "E").

## plotting bathymetry with ggplot 

bat_df <- as.data.frame(bathymetry, xy = TRUE, na.rm = TRUE)

bat_df <- bat_df %>% filter(bat_1 < 0)

(bat_plot <- ggplot() +
      geom_raster(data = bat_df, aes(x = x, y = y, fill = bat_1)) +
      scale_fill_viridis_c() +
      coord_quickmap() +
      ggtitle("North of Iceland Bathymetric profile") +
      ylim(64.5,68) +
      xlim(-27, -10) +
      theme_classic() +
      theme(legend.position = "right",
            legend.title = element_text(size = 13, face ="bold"),
            legend.text = element_text(size = 12)) + # removes defalut grey background
      theme(plot.title = element_text(size = 15, face ="bold", hjust = 0.5),             # centres plot title
            text = element_text(size=20),		       	    # font size
            axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(fill = "Depth (m)", 
           x = "longitude", 
           y ="latitude")) # rotates x axis text

#ggsave(bat_plot, file = "img/bat_plot.png", height = 5, width = 9)

### Chlorophyll:

chlor_0103 <- raster("Parameters/chlor_a/chlor_a_200103.tif")
chlor_0104 <- raster("Parameters/chlor_a/chlor_a_200104.tif")
chlor_0105 <- raster("Parameters/chlor_a/chlor_a_200105.tif")
chlor_0106 <- raster("Parameters/chlor_a/chlor_a_200106.tif")
chlor_0107 <- raster("Parameters/chlor_a/chlor_a_200107.tif")
chlor_0108 <- raster("Parameters/chlor_a/chlor_a_200108.tif")

## Creating a data frame for each year to show the progress in a facet plot

chlor_0103_df <- as.data.frame(chlor_0103, xy = TRUE, na.rm = TRUE)
chlor_0104_df <- as.data.frame(chlor_0104, xy = TRUE, na.rm = TRUE)
chlor_0105_df <- as.data.frame(chlor_0105, xy = TRUE, na.rm = TRUE)
chlor_0106_df <- as.data.frame(chlor_0106, xy = TRUE, na.rm = TRUE)
chlor_0107_df <- as.data.frame(chlor_0107, xy = TRUE, na.rm = TRUE)
chlor_0108_df <- as.data.frame(chlor_0108, xy = TRUE, na.rm = TRUE)


## Making the facet plot 

(chlor_facet_01_03 <- ggplot() +
   geom_raster(data = chlor_0103_df, aes(x = x, y = y, fill = chlor_a_200103)) +
   scale_fill_viridis_c() +
   coord_quickmap() +
   ggtitle("Chlorophyll") +
   
   xlab("Longitude") +
   ylab("Latitude") +
   ylim(62,68) +
   xlim(-27, -10) +
   theme_classic() +   					    # removes defalut grey background
   theme(plot.title = element_text(hjust = 0.5),             # centres plot title
         text = element_text(size=20),		       	    # font size
         axis.text.x = element_text(angle = 90, hjust = 1)))  # rotates x axis text

(chlor_facet_01_04 <- ggplot() +
      geom_raster(data = chlor_0104_df, aes(x = x, y = y, fill = chlor_a_200104)) +
      scale_fill_viridis_c() +
      coord_quickmap() +
      ggtitle("Chlorophyll") +
      xlab("Longitude") +
      ylab("Latitude") +
      ylim(62,68) +
      xlim(-27, -10) +
      theme_classic() +   					    # removes defalut grey background
      theme(plot.title = element_text(hjust = 0.5),             # centres plot title
            text = element_text(size=20),		       	    # font size
            axis.text.x = element_text(angle = 90, hjust = 1)))  # rotates x axis text

(chlor_facet_01_05 <- ggplot() +
      geom_raster(data = chlor_0105_df, aes(x = x, y = y, fill = chlor_a_200105)) +
      scale_fill_viridis_c() +
      coord_quickmap() +
      ggtitle("Chlorophyll") +
      xlab("Longitude") +
      ylab("Latitude") +
      ylim(62,68) +
      xlim(-27, -10) +
      theme_classic() +   					    # removes defalut grey background
      theme(plot.title = element_text(hjust = 0.5),             # centres plot title
            text = element_text(size=20),		       	    # font size
            axis.text.x = element_text(angle = 90, hjust = 1)))  # rotates x axis text

(chlor_facet_01_06 <- ggplot() +
      geom_raster(data = chlor_0106_df, aes(x = x, y = y, fill = chlor_a_200106)) +
      scale_fill_viridis_c() +
      coord_quickmap() +
      ggtitle("Chlorophyll") +
      xlab("Longitude") +
      ylab("Latitude") +
      ylim(62,68) +
      xlim(-27, -10) +
      theme_classic() +   					    # removes defalut grey background
      theme(plot.title = element_text(hjust = 0.5),             # centres plot title
            text = element_text(size=20),		       	    # font size
            axis.text.x = element_text(angle = 90, hjust = 1)))  # rotates x axis text


(chlor_facet_01_07 <- ggplot() +
      geom_raster(data = chlor_0107_df, aes(x = x, y = y, fill = chlor_a_200107)) +
      scale_fill_viridis_c() +
      coord_quickmap() +
      ggtitle("Chlorophyll") +
      xlab("Longitude") +
      ylab("Latitude") +
      ylim(62,68) +
      xlim(-27, -10) +
      theme_classic() +   					    # removes defalut grey background
      theme(plot.title = element_text(hjust = 0.5),             # centres plot title
            text = element_text(size=20),		       	    # font size
            axis.text.x = element_text(angle = 90, hjust = 1)))  # rotates x axis text

(chlor_facet_01_08 <- ggplot() +
      geom_raster(data = chlor_0108_df, aes(x = x, y = y, fill = chlor_a_200108)) +
      scale_fill_viridis_c(direction = 1) +
      coord_quickmap() +
      ggtitle("Chlorophyll") +
      xlab("Longitude") +
      ylab("Latitude") +
      ylim(62,68) +
      xlim(-27, -10) +
      theme_classic() +   					    # removes defalut grey background
      theme(plot.title = element_text(hjust = 0.5),             # centres plot title
            text = element_text(size=20),		       	    # font size
            axis.text.x = element_text(angle = 90, hjust = 1)))  # rotates x axis text

## Trying to make a facet plot 
#attach(mtcars)

#par(mfrow = c(3, 3))

## Survey per unit effort----

# do i actually need to do this? 


## Splitting dataset by year to process it in QGIS----
   #I am doing this so i can work independetly on each year and then merge them together in R after saving them from qgis

### 2001
survey_01 <- survey %>% 
   dplyr::select(year, month, spec, la2, lo2, pods) %>% 
   filter(spec == "mn") %>% 
   filter (year == "2001")

write_csv(survey_01, file = "Survey/modified NASS/survey_01.csv")

### 2007
survey_07 <- survey %>% 
   dplyr::select(year, month, spec, la2, lo2, pods) %>% 
   filter(spec == "mn") %>% 
   filter (year == "2007")

write_csv(survey_07, file = "Survey/modified NASS/survey_07.csv")

### 2015
survey_15 <- survey %>% 
   dplyr::select(year, month, spec, la2, lo2, pods) %>% 
   filter(spec == "mn") %>% 
   filter (year == "2015")

write_csv(survey_15, file = "Survey/modified NASS/survey_15.csv")

## Putting together csvs from the different years----
complete_01 <- read_csv("Survey/qgis_survey/2001_complete.csv")
complete_07 <- read_csv("Survey/qgis_survey/2007_complete.csv")
complete_15 <- read_csv("Survey/qgis_survey/2015_complete.csv")

#Working on dataset 2001
complete_01

#prova_final <- rbind(complete_01, complete_07, complete_15) #you have to make sure to save them in the right order otherwise the name do not match












