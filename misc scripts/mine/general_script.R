# R SCRIPT FOR DATA ANALYSIS OF ENVIRONMENTAL VARIABLES

## Packages----

library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(lme4) #for mixed models
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
library(gam)

getwd()

## Graphic theme----

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

### NASS ships
vessels <- read.csv("NASS/original/nass_vessels.csv")

unique(survey$vID) #TOTAL NUMBER OF VESSELS

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

### New CSV file (that might not be useful after all)

n_ice_survey <- read.csv("NASS/intermediate surveys/long_north_nass.csv") #this is the new csv file with the broke down pods

n_ice_survey$year <- as.factor(n_ice_survey$year)
str(n_ice_survey)

## Original dataset----

mn.df <- survey %>% 
   dplyr::select(year, month, spec, la2, lo2, pods) %>% 
   filter(spec == "mn") 

mn.df$year <- as.factor(mn.df$year)
str(mn.df)

### Adding the effort geopackage 

effort <- st_read("NASS/ale_nass-effort.gpkg")

### Adding locations of Reykjavik and Port
location <- c("Finnafjörður", "Reykjavík")
lat <- c(66.116667, 64.144555)
long <- c(-15.133333, -21.941021)
group <- as.factor(c("a", "b"))

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
   geom_point(data = mn.df, aes(x = lo2, y = la2, colour = year, size = pods), alpha = 0.7, fill = "black") + #adding pod sixe ad a point size, not sure is useful, just an idea
   #scale_color_manual(values = c("#70161E", "#F6BE13", "#5C80BC")) +
   scale_color_manual(values = c("#A06B9A", "#8D9EC6", "#082241")) +
   #geom_rect(data = ice_map, aes(xmin = 65, xmax = 68, ymin = -9, ymax = -20),
   #fill = "transparent", color = "red", size = 0.5) + 
   graphic_theme() +
   geom_label_repel(data = imp_locations, aes(x = long, y = lat,
                                              label = location),
                    box.padding = 5, size = 5, alpha = 0.9, nudge_y = -0.5,
                    min.segment.length = 0, inherit.aes = FALSE) + 
   ylim(62,68) +
   xlim(-27, -10) +
   labs(title = "", colour = "Survey Year", 
        x = "Longitude", y = "Latitude") + 
   coord_sf())

#ggsave(site_map, file = "img/survey_final2.png", height = 5, width = 8)

### Total sightings per year

sightings_x_years <- n_ice_survey %>% group_by(year) %>% 
  summarise(total_count = n()) %>% ungroup()

sightings_x_years$total_count <- as.factor(sightings_x_years$total_count)

(sightings_x_years_plot <- ggplot(sightings_x_years, aes(x = year, y = total_count, fill = total_count)) + #specifying what to put on the axis
    geom_bar(color = "black", size = 0.2, stat = "identity", width = 0.6) + 
    scale_fill_manual(values = c("#8D9EC6","#082241", "#A06B9A")) +    theme_minimal() + 
    graphic_theme() +
    theme(legend.position = "none") +
    labs(fill = "Total Sightings", 
         x = "Years", 
         y ="Total Sightings"))

#ggsave(sightings_x_years_plot, file = "img/sightings_x_years.png", height = 5, width = 4.5)

## Visualizing raster data----

### Bathymetry-----

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

display.brewer.all()

bat_df <- as.data.frame(bathymetry, xy = TRUE, na.rm = TRUE)

bat_df <- bat_df %>% filter(bat_1 < 0)

#(bat_plot <- ggplot() +
      geom_raster(data = bat_df, aes(x = x, y = y, fill = bat_1)) +
      geom_contour(aes(z = Chla), binwidth = 2, colour = "red", alpha = 0.2) +
      geom_contour(aes(z = Chla), breaks = 0.1, colour = "darkgrey") +
      geom_contour(aes(z = Chla), breaks = 0.2, colour = "darkgrey") +
      geom_contour(aes(z = Chla), breaks = 0.3, colour = "black") +
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
                   alpha = 0.7, bins = 35, aes(x = bat_1)) +
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
      coord_quickmap() +
      ggtitle("Chlorophyll map of Northern Iceland, July 2001") +
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
      scale_fill_viridis_c(option = "plasma", direction = -1)+
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
      scale_fill_viridis_c(direction = 1)+
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

#write_csv(survey_01, file = "NASS/intermediate surveys/by_year/survey_01.csv")

### 2007
survey_07 <- survey %>% 
   dplyr::select(year, month, spec, la2, lo2, pods) %>% 
   filter(spec == "mn") %>% 
   filter (year == "2007")

#write_csv(survey_07, file = "NASS/intermediate surveys/by_year/survey_07.csv")

### 2015
survey_15 <- survey %>% 
   dplyr::select(year, month, spec, la2, lo2, pods) %>% 
   filter(spec == "mn") %>% 
   filter (year == "2015")

#write_csv(survey_15, file = "NASS/intermediate surveys/by_year/survey_15.csv")

## Putting together csvs from the different years----

complete_01 <- read_csv("NASS/qgis_survey/2001_complete.csv")
complete_07 <- read_csv("NASS/qgis_survey/2007_complete.csv")
complete_15 <- read_csv("NASS/qgis_survey/2015_complete.csv")

str(complete_01)

## Working on dataset 2001 (renaming the columns)

complete_01 <- complete_01 %>% 
   rename(chlorMarch = chlor0103, chlorApril = chlor0104, 
          chlorMay = chlor0105, chlorJune = chlor0106, 
          chlorJuly = chlor0107, chlorAug = chlor0108,
          mldMarch = mlotst0103, mldApril = mlotst0104,
          mldMay = mlotst0105, mldJune = mlotst0106, 
          mldJuly = mlotst0107, mldAug = mlotst0108, 
          sstMarch = sst0103, sstApril = sst0104,
          sstMay = sst0105, sstJune = sst0106, 
          sstJuly = sst0107, sstAug = sst0108)
         
## Working on dataset 2007 (renaming the columns)

complete_07 <- complete_07 %>% 
   rename(chlorMarch = chlor0703, chlorApril = chlor0704, 
          chlorMay = chlor0705, chlorJune = chlor0706, 
          chlorJuly = chlor0707, chlorAug = chlor0708,
          mldMarch = mlotst0703, mldApril = mlotst0704,
          mldMay = mlotst0705, mldJune = mlotst0706, 
          mldJuly = mlotst0707, mldAug = mlotst0708, 
          sstMarch = sst0703, sstApril = sst0704,
          sstMay = sst0705, sstJune = sst0706, 
          sstJuly = sst0707, sstAug = sst0708)

## Working on dataset 2015 (renaming the columns)

complete_15 <- complete_15 %>% 
   rename(chlorMarch = chlor1503, chlorApril = chlor1504, 
          chlorMay = chlor1505, chlorJune = chlor1506, 
          chlorJuly = chlor1507, chlorAug = chlor1508,
          mldMarch = mlotst1503, mldApril = mlotst1504,
          mldMay = mlotst1505, mldJune = mlotst1506, 
          mldJuly = mlotst1507, mldAug = mlotst1508, 
          sstMarch = sst1503, sstApril = sst1504,
          sstMay = sst1505, sstJune = sst1506, 
          sstJuly = sst1507, sstAug = sst1508)

## Binding the data sets per year----

prova_df <- rbind(complete_01, complete_07, complete_15) #you have to make sure to save them in the right order otherwise the name do not match

prova_df <- prova_df %>% mutate(presence = "1")

(pods_dist <- ggplot(prova_df, aes(x = pods)) + 
      geom_histogram(colour = "aquamarine4", fill = "aquamarine3")) #nice colors!


## Rasterizing Shapefile----

# Cell size: 0.08333 degrees

# Albers projection 

albers.crs <- "+proj=aea +x_0=0 +y_0=0 +lon_0=-30 +lat_0=30 +lat_1=43 +lat_2=62 +units=m +ellps=WGS84 +datum=WGS84 +no_defs"
e.min <- -1250000
e.max <- 2375000
n.min <- 2070000
n.max <- 5295000
albers.cell <- 25000 # 25 km

# WGS84
wgs.crs <- "+init=epsg:4326" # all numbers below should match this CRS
lon.min <- -27
lon.max <- -10
lat.min <- 64.5
lat.max <- 68.5
wgs.cell <- 0.0833 # 0.25 degrees

# Create empty rasters 

# ALBERS
albers.ref <- raster() 
projection(albers.ref) <- crs(albers.crs)
extent(albers.ref) <- c(e.min, e.max, n.min, n.max)
res(albers.ref) <- albers.cell
albers.ref # check the metadata make sense

# WGS84
wgs.ref <- raster() 
projection(wgs.ref) <- crs(wgs.crs)
extent(wgs.ref) <- c(lon.min, lon.max, lat.min, lat.max)
res(wgs.ref) <- wgs.cell
wgs.ref # check the metadata make sense

### STEP 2: rasterise sightings
mn.shape <- mn.df %>%
   mutate_at(vars(lo2, la2), as.numeric) %>%
   st_as_sf(coords = c("lo2", "la2"), crs = wgs.crs)

mn.shape <- st_transform(mn.shape, crs = albers.crs) # change crs from wgs84 to albers

plot(mn.shape) 


# Create function to calculate number of points in each raster cell. From https://gis.stackexchange.com/questions/309407/computing-number-of-points-in-a-raster-grid-cell-in-r
pointcount = function(r, pts){
   # make a raster of zeroes like the input
   r2 = r
   r2[] = 0
   # get the cell index for each point and make a table:
   counts = table(cellFromXY(r,pts))
   # fill in the raster with the counts from the cell index:
   r2[as.numeric(names(counts))] = counts
   return(r2)
}

## Making a binary map
sight_sf <- shapefile("NASS/intermediate surveys/by_year/2001_sf/2001_sf.shp")
ext <- extent(-27, -10, 64.5, 68.5)
ras <- raster(ext, res = 1/12)

rr <- rasterize(sight_sf, ras, value=1, background = 0)







