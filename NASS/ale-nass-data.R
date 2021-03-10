############################################
### NASS data for Alessandra Cianfanelli ###
############################################
# 05/01/2021
# Tom Grove
# tomgrove20@yahoo.co.uk

# Cropping NASS data for the undergraduate dissertation of Alessandra Cianfanelli (University of Edinburgh), supervised by Lea-Anne Henry

################
### PACKAGES ###
################

packages <- c("tidyverse", "sf", "ggplot2", "viridis")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, require, character.only = TRUE)

##################
### PLOT THEME ###
##################

theme_set(theme_bw())
map.theme <- theme(axis.text.x = element_text(size = 13, margin=margin(7,0,0,0)),
                   axis.text.y = element_text(size = 13, margin=margin(0,7,0,0)),
                   axis.title = element_blank(),
                   strip.text = element_text(size = 13),
                   panel.background = element_rect(fill = "#f2f2f2"),
                   panel.grid.major = element_line(colour = "black"),
                   legend.title = element_text(size = 13, margin = margin(0,0,6,0)),
                   legend.text = element_text(size = 13))

# also going to upload shapefile of Iceland
iceland <- st_read("data/iceland-isn93/is50v_strandlina_flakar_24122017.shp")

######################
### SPECIFICATIONS ###
######################

# box denoting spatial extent
lon <- c(-28, -9) # Longitudinal range
lat <- c(65, 68) # Latitudinal range
pts <- rbind(c(lon[1], lat[1]), c(lon[2], lat[1]), c(lon[2], lat[2]), c(lon[1], lat[2]), c(lon[1], lat[1]))
box <- st_sfc(st_polygon(list(pts)), crs = 4326) # creating bounding box
ggplot(data = box) + geom_sf() # plotting to check

# temporal extent
times <- c(2001, 2007, 2015) # three most recent surveys

#######################
### DATA PROCESSING ###
#######################


## SIGHTINGS (all species and no sightings)
sight <- read.csv("final-products/nass_total_cleaned.csv") %>%
  filter(spec != 0) %>%
  # convert data frame to sf (simple spatial feature) to allow cropping
  st_as_sf(coords = c("lo2", "la2"), remove = FALSE, crs = 4326) %>%
  # crop to bounding box
  st_crop(box) %>%
  # filter by years
  filter(year %in% times)

# which species?
unique(sight$spec)

## EFFORT
effort <- st_read("intermediate-products/nass_effort-lines.gpkg") %>%
  # crop to bounding box
  st_crop(box) %>%
  # filter by years
  filter(year %in% times)

# plot to check
ggplot(data = box) + # bounding box
  geom_sf(col = "red", fill = "transparent", size = 1.5) +
  geom_sf(data = effort, color = "black") + # effort
  geom_sf(data = filter(sight, spec == "mn"), aes(color = as.factor(year))) + # sightings
  scale_color_brewer(palette = "Dark2") +
  geom_sf(data = iceland, col = "black", fill = "black") +
  labs(color = "Year") +
  map.theme
ggsave("final-products/ale/ale_nass-mn_map.png")

# finally saving sightings and effort
write.csv(sight, "final-products/ale/ale_nass-sightings_tot.csv")
st_write(effort, "final-products/ale/ale_nass-effort.gpkg")

