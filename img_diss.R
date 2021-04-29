## Alessandra Cianfanelli: Script for dissertation figures

# PACKAGES----
library(tidyverse)
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
library(mgcv)
library(RColorBrewer)
library(ggsn)

# GRAPHIC THEME----

graphic_theme <- function(){
  theme_minimal() + 
    theme(legend.position = "right",
          legend.title = element_text(size = 13, face ="bold"),
          legend.text = element_text(size = 12),
          text = element_text(size = 15),
          plot.title = element_text(size = 16, face ="bold", hjust = 0.5),
          axis.text.x = element_text(hjust = 1))
}

## NASS SURVEY----
survey <- read.csv("NASS/original/orignal_ale_nass-sightings_tot.csv")
head(survey)
str(survey)
survey$spec <- as.factor(survey$spec)

## ICELAND MAP----
### Importing world map and checking everything is alright 
world <- map_data("world")
head(world)
unique(world$region)
str(world$region)

world$region <- as.factor(world$region) #making region a factor 

ice_map <- world %>% 
  filter(region == "Iceland") %>% 
  dplyr::select(-subregion)

### Original Survey----

mn.df <- survey %>% 
  dplyr::select(year, month, spec, la2, lo2, pods) %>% 
  filter(spec == "mn") 

mn.df$year <- as.factor(mn.df$year)
str(mn.df)

### Effort----
effort <- st_read("NASS/ale_nass-effort.gpkg")

### Adding locations of Reykjavik and Port----
location <- c("Finnafjörður", "Reykjavík")
lat <- c(66.116667, 64.144555)
long <- c(-15.133333, -21.941021)
group <- as.factor(c("a", "b"))

### Dataframe with locations
imp_locations <- data.frame(location, lat, long, group)
head(imp_locations)
str(imp_locations)

### Making map + lables----
(site_map <- ggplot() +
   geom_polygon(data = ice_map, aes(x = long, y = lat, group = group) 
                , color = "black", fill = "#698B69", alpha = 0.6, size = 0.3) + #plot the data points on the map
   geom_point(data = imp_locations, aes(x = long, y = lat, colour = group), 
              colour = c("#0E4749", "#CC978E"), size = 3) +
   geom_sf(data = effort, color = "black", alpha = 0.6) + # effort
   geom_point(data = mn.df, aes(x = lo2, y = la2, colour = year), fill = "black") + 
   scale_color_manual(values = c("#A06B9A", "#8D9EC6", "#082241")) +
   graphic_theme() +
   geom_label_repel(data = imp_locations, aes(x = long, y = lat,
                                              label = location),
                    box.padding = 5, size = 5, alpha = 0.9, nudge_y = -0.5,
                    min.segment.length = 0, inherit.aes = FALSE) +
   ylim(63,68.03) +
   xlim(-27, -10) +
   labs(title = "", colour = "Survey Year", 
        x = "Longitude", y = "Latitude") + 
   coord_sf())


(site_map2 <- site_map +
  ggsn::scalebar(data = ice_map,
           transform = TRUE, dist = 100, dist_unit = "km", model='WGS84',
           height = 0.03, location = "bottomright", anchor = c(x = -12.2, y = 63.3), st.dist = 0.06))

#ggsave(site_map2, file = "img/survey_final4.png", height = 5, width = 8)

## SIGHTING X YEAR----

sightings_x_years <- mn.df %>% group_by(year) %>% 
  summarise(total_count = n()) %>% ungroup()

sightings_x_years$total_count <- as.factor(sightings_x_years$total_count)

(sightings_x_years_plot <- ggplot(sightings_x_years, aes(x = year, y = total_count, fill = total_count)) + #specifying what to put on the axis
    geom_bar(color = "black", size = 0.3, stat = "identity", width = 0.7) + 
    scale_fill_manual(values = c("#8D9EC6","#082241", "#A06B9A")) +   
    theme_classic() + 
    theme(legend.position = "none", text = element_text(size = 15)) +
    labs(fill = "Total Sightings", 
         x = "\nYear", 
         y ="Total Sightings"))

#ggsave(sightings_x_years_plot, file = "img/sightings_x_years.png", height = 5, width = 6)

## VIS RASTER DATA----

### Bathymetry-----

bathymetry <- raster("Parameters/bathymetry/bat_reclass.tif")

bathymetry #to get the properties

plot(bathymetry)

bat_df <- as.data.frame(bathymetry, xy = TRUE, na.rm = TRUE)

bat_df <- bat_df %>% filter(bat_reclass < 0)

(bat_plot <- ggplot() +
geom_raster(data = bat_df, aes(x = x, y = y, fill = bat_reclass)) +
    geom_point(data = mn.df, aes(x = lo2, y = la2, colour = year), fill = "black") +
    scale_color_manual(values = c("#A06B9A", "#8D9EC6", "#082241")) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("Bathymetric profile of the North of Iceland") +
  ylim(64.5,68.5) +
  xlim(-27, -10) +
  graphic_theme() +
  theme_classic() +
  theme (plot.title = element_text(size = 15, 
                                   face ="bold", hjust = 0.5)) +
  labs(fill = "Depth (m)", 
       x = "Longitude", 
       y ="Latitude")) # rotates x axis text

#ggsave(bat_plot, file = "img/bat_plot4.png", height = 5, width = 9)

## Bathymetry distribution hinstogram----

(bat_dist <- ggplot()+
   geom_histogram(data = bat_df, fill = "#006d77",
                  color = "black", size = 0.2, 
                  alpha = 0.7, bins = 35, aes(x = bat_reclass)) +
   scale_x_reverse() +
   theme_minimal() +
   theme(text = element_text(size = 15),		       	    # font size
         axis.text.x = element_text(hjust = 1)) +
   labs(fill = "Bathymetry profile", 
        x = "Depth (m)", 
        y ="Count")) 

#ggsave(bat_dist, file = "img/bat_prof.png", height = 5, width = 9)

### Chlorophyll----

chlor_0107 <- raster("Parameters/chlor_a/chlor_0107.tif")

## Creating a data frame for each year to show the progress in a facet plot

chlor_0107_df <- as.data.frame(chlor_0107, xy = TRUE, na.rm = TRUE)

## Making the plot 

(chlor_01_07 <- ggplot() +
    geom_raster(data = chlor_0107_df, aes(x = x, y = y, fill = chlor_0107)) +
    scale_fill_viridis_c()+
    geom_point(data = mn.df, aes(x = lo2, y = la2, colour = year), fill = "black") +
    coord_quickmap() +
    ggtitle("Chlorophyll-a map of Northern Iceland, July 2007") +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 13, face ="bold"),
          legend.text = element_text(size = 12)) + # removes defalut grey background
    theme(plot.title = element_text(size = 15, face ="bold", hjust = 0.5), # centres plot title
          text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Chlorophyll-a \n(mg m-3)", 
         x = "Longitude", 
         y = "Latitude"))

#set this as theme!! 

#ggsave(chlor_01_07, file = "img/chlor_ex.png", height = 5, width = 9)

## Sea Surface Temperature----

sst_0107 <- raster("Parameters/analysed_sst/sst_0107.tif")

sst_0107_df <- as.data.frame(sst_0107, xy = TRUE, na.rm = TRUE)

(sst_01_07 <- ggplot() +
    geom_raster(data = sst_0107_df, aes(x = x, y = y, fill = sst_0107)) +
    scale_fill_viridis_c(option = "plasma", direction = 1)+
    geom_point(data = mn.df, aes(x = lo2, y = la2, colour = year), fill = "black") +
    coord_quickmap() +
    ggtitle("SST map of Northern Iceland, July 2001") +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 13, face ="bold"),
          legend.text = element_text(size = 12)) + # removes defalut grey background
    theme(plot.title = element_text(size = 15, face ="bold", hjust = 0.5), # centres plot title
          text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Temperature (K)", 
         x = "Longitude", 
         y = "Latitude"))

#ggsave(sst_01_07, file = "img/sst_ex.png", height = 5, width = 9)

## MLD----

mld_0107 <- raster("Parameters/mlotst/mlotst_0107.tif")

mld_0107_df <- as.data.frame(mld_0107, xy = TRUE, na.rm = TRUE)

(mld_01_07 <- ggplot() +
    geom_raster(data = mld_0107_df, aes(x = x, y = y, fill = mlotst_0107)) +
    scale_fill_viridis_c(direction = -1)+
    geom_point(data = mn.df, aes(x = lo2, y = la2, colour = year), fill = "black") +
    coord_quickmap() +
    ggtitle("MLD in Northern Iceland, July 2001") +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 13, face ="bold"),
          legend.text = element_text(size = 12)) + # removes defalut grey background
    theme(plot.title = element_text(size = 15, face ="bold", hjust = 0.5), # centres plot title
          text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Depth (m)", 
         x = "Longitude", 
         y = "Latitude"))

#ggsave(mld_01_07, file = "img/mld_ex.png", height = 5, width = 9)


## MAPS WITH EACH YEAR AND SIGHTINGS----

### 2001:
survey_01 <- survey %>% 
  filter (year == "2001") 

#write_csv(survey_01, file = "NASS/intermediate surveys/by_year/survey_01.csv")

### 2007:
survey_07 <- survey %>% 
  filter (year == "2007")

#write_csv(survey_07, file = "NASS/intermediate surveys/by_year/survey_07.csv")

### 2015:
survey_15 <- survey %>% 
  filter (year == "2015")


### Chlorophyll-a 2001----

chlor_0105 <- raster("Parameters/chlor_a/chlor_0105.tif")

## Creating a data frame for each year to show the progress in a facet plot

chlor_0105_df <- as.data.frame(chlor_0105, xy = TRUE, na.rm = TRUE)

chlor_0105_df <- chlor_0105_df %>% 
      filter(chlor_0105 < 20)
  
(chlor_01_05 <- ggplot() +
   geom_raster(data = chlor_0105_df, aes(x = x, y = y, fill = chlor_0105)) +
   scale_fill_viridis_c(option = mako)+
   geom_point(data = survey_01, aes(x = lo2, y = la2), colour = "black", size = 0.5) +
   coord_quickmap() +
   ylim(64.5,68.5) +
   xlim(-27, -10) +
   theme_classic() + # removes defalut grey background
   theme(legend.position = "right",
         legend.title = element_text(size = 12, face ="bold"),
         legend.text = element_text(size = 11)) + # removes defalut grey background
   theme(text = element_text(size=12),		# font size
         axis.text.x = element_text(angle = 90, hjust = 1)) +      
   labs(fill = "Chlorophyll-a \n(mg m-3)", 
        x = "Longitude", 
        y = "Latitude"))

### Chlorophyll-a 2007----

chlor_0705 <- raster("Parameters/chlor_a/chlor_0705.tif")

## Creating a data frame for each year to show the progress in a facet plot

chlor_0705_df <- as.data.frame(chlor_0705, xy = TRUE, na.rm = TRUE)

(chlor_07_05 <- ggplot() +
    geom_raster(data = chlor_0705_df, aes(x = x, y = y, fill = chlor_0705)) +
    scale_fill_viridis_c()+
    geom_point(data = survey_07, aes(x = lo2, y = la2), colour = "black", size = 0.5) +
    coord_quickmap() +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 12, face ="bold"),
          legend.text = element_text(size = 11)) + # removes defalut grey background
    theme(text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Chlorophyll-a \n(mg m-3)", 
         x = "Longitude", 
         y = "Latitude"))

chlor_0705_df <- chlor_0705_df %>% 
  filter(chlor_0705 < 37)


### Chlorophyll-a 2015----

chlor_1505 <- raster("Parameters/chlor_a/chlor_1505.tif")

## Creating a data frame for each year to show the progress in a facet plot

chlor_1505_df <- as.data.frame(chlor_1505, xy = TRUE, na.rm = TRUE)

(chlor_15_05 <- ggplot() +
    geom_raster(data = chlor_1505_df, aes(x = x, y = y, fill = chlor_1505)) +
    scale_fill_viridis_c() +
    geom_point(data = survey_15, aes(x = lo2, y = la2), colour = "black", size = 0.5) +
    coord_quickmap() +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 12, face ="bold"),
          legend.text = element_text(size = 11)) + # removes defalut grey background
    theme(text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Chlorophyll-a \n(mg m-3)", 
         x = "Longitude", 
         y = "Latitude"))

### SST 2001----

sst_0105 <- raster("Parameters/analysed_sst/sst_0105.tif")

sst_0105_df <- as.data.frame(sst_0105, xy = TRUE, na.rm = TRUE)

(sst_01_05 <- ggplot() +
    geom_raster(data = sst_0105_df, aes(x = x, y = y, fill = sst_0105)) +
    scale_fill_viridis_c(option = "plasma", direction = 1)+
    geom_point(data = survey_01, aes(x = lo2, y = la2), fill = "black", size = 0.5) +
    coord_quickmap() +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 12, face ="bold"),
          legend.text = element_text(size = 11)) + # removes defalut grey background
    theme(text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Temperature (K)", 
         x = "Longitude", 
         y = "Latitude"))


### SST 2007----

sst_0705 <- raster("Parameters/analysed_sst/sst_0705.tif")

sst_0705_df <- as.data.frame(sst_0705, xy = TRUE, na.rm = TRUE)

(sst_07_05 <- ggplot() +
    geom_raster(data = sst_0705_df, aes(x = x, y = y, fill = sst_0705)) +
    scale_fill_viridis_c(option = "plasma", direction = 1)+
    geom_point(data = survey_07, aes(x = lo2, y = la2), fill = "black", size = 0.5) +
    coord_quickmap() +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 12, face ="bold"),
          legend.text = element_text(size = 11)) + # removes defalut grey background
    theme(text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Temperature (K)", 
         x = "Longitude", 
         y = "Latitude"))

### SST 2015----

sst_1505 <- raster("Parameters/analysed_sst/sst_1505.tif")

sst_1505_df <- as.data.frame(sst_1505, xy = TRUE, na.rm = TRUE)

(sst_15_05 <- ggplot() +
    geom_raster(data = sst_1505_df, aes(x = x, y = y, fill = sst_1505)) +
    scale_fill_viridis_c(option = "plasma", direction = 1)+
    geom_point(data = survey_15, aes(x = lo2, y = la2), fill = "black", size = 0.5) +
    coord_quickmap() +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 12, face ="bold"),
          legend.text = element_text(size = 11)) + # removes defalut grey background
    theme(text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Temperature (K)", 
         x = "Longitude", 
         y = "Latitude"))


### MLD 2001----

mld_0105 <- raster("Parameters/mlotst/mlotst_0105.tif")

mld_0105_df <- as.data.frame(mld_0105, xy = TRUE, na.rm = TRUE)

(mld_01_05 <- ggplot() +
    geom_raster(data = mld_0105_df, aes(x = x, y = y, fill = mlotst_0105)) +
    scale_fill_viridis_c(direction = -1)+
    geom_point(data = survey_01, aes(x = lo2, y = la2), fill = "black", size = 0.5) +
    coord_quickmap() +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 12, face ="bold"),
          legend.text = element_text(size = 11)) + # removes defalut grey background
    theme(text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Depth (m)", 
         x = "Longitude", 
         y = "Latitude"))

### MLD 2007----

mld_0705 <- raster("Parameters/mlotst/mlotst_0705.tif")

mld_0705_df <- as.data.frame(mld_0705, xy = TRUE, na.rm = TRUE)

(mld_07_05 <- ggplot() +
    geom_raster(data = mld_0705_df, aes(x = x, y = y, fill = mlotst_0705)) +
    scale_fill_viridis_c(direction = -1)+
    geom_point(data = survey_07, aes(x = lo2, y = la2), fill = "black", size = 0.5) +
    coord_quickmap() +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 12, face ="bold"),
          legend.text = element_text(size = 11)) + # removes defalut grey background
    theme(text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Depth (m)", 
         x = "Longitude", 
         y = "Latitude"))

### MLD 2015----

mld_1505 <- raster("Parameters/mlotst/mlotst_1505.tif")

mld_1505_df <- as.data.frame(mld_1505, xy = TRUE, na.rm = TRUE)

(mld_15_05 <- ggplot() +
    geom_polygon(data = ice_map, aes(x = long, y = lat, group = group), fill = NA) +
    geom_raster(data = mld_1505_df, aes(x = x, y = y, fill = mlotst_1505)) +
    scale_fill_viridis_c(direction = -1)+
    geom_point(data = survey_15, aes(x = lo2, y = la2), fill = "black", size = 0.5) +
    coord_quickmap() +
    ylim(64.5,68.5) +
    xlim(-27, -10) +
    theme_classic() + # removes defalut grey background
    theme(legend.position = "right",
          legend.title = element_text(size = 12, face ="bold"),
          legend.text = element_text(size = 11)) + # removes defalut grey background
    theme(text = element_text(size=12),		# font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +      
    labs(fill = "Depth (m)", 
         x = "Longitude", 
         y = "Latitude"))

(sst_07_052 <- sst_07_05 +
    ggsn::scalebar(data = ice_map,
                   transform = TRUE, dist = 100, dist_unit = "km", model='WGS84',
                   height = 0.03, location = "topleft", anchor = c(x = -15.8, y = 68.2), 
                   st.size = 3, st.dist = 0.08, st.color = "white"))


### facet for dynamic variables----
library(ggpubr)

(variables <- ggarrange(sst_01_05, sst_07_052, sst_15_05,
                        chlor_01_05, chlor_07_05, chlor_15_05,
                        mld_01_05, mld_07_05, mld_15_05,
            labels = c("sst 2001", "sst 2007", "sst 2015",
                       "Chlorphyll 2001", "Chlorphyll 2007",
                       "Chlorphyll 2015", "MLD 2001", 
                       "MLD 2007", "MLD 2015"),
            ncol = 2, nrow = 5))

#ggsave(variables, file = "img/variables_panel.png", height = 16, width = 12)

library(wesanderson)

pal <- wes_palette("GrandBudapest2", 100, type = "continuous")


(bat_plot_tot <- ggplot() +
   geom_polygon(data = ice_map, aes(x = long, y = lat, group = group), fill = NA) +
   geom_raster(data = bat_df, aes(x = x, y = y, fill = bat_reclass)) +
   geom_point(data = mn.df, aes(x = lo2, y = la2), fill = "black") +
   scale_color_manual(values = c("#A06B9A", "#8D9EC6", "#082241")) +
   scale_fill_viridis_c() +
   #scale_fill_gradientn(colours = pal) +
   coord_quickmap() +
   ylim(64.5,68.5) +
   xlim(-27, -10) +
   graphic_theme() +
   theme_classic() +
   theme (plot.title = element_text(size = 15, 
                                    face ="bold", hjust = 0.5)) +
   labs(fill = "Depth (m)", 
        x = "Longitude", 
        y ="Latitude")) # rotates x axis text

(bat_plot_tot2 <- bat_plot_tot +
    ggsn::scalebar(data = ice_map,
                   transform = TRUE, dist = 70, dist_unit = "km", model='WGS84',
                   height = 0.03, location = "bottomright", anchor = c(x = -23.3, y = 64.6), 
                   st.size = 3, st.dist = -0.08, st.color = "black"))


#ggsave(bat_plot_tot2, file = "img/bathymethry.png", height = 5, width = 9)

