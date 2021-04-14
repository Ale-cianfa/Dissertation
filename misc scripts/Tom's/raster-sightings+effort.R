### Calculating whale density over raster
### Tom Grove
### 01/06/2020

# AIM: Calculate whale density over a raster covering the study area
# Study area: 50-75 N, -46-3 E (Icelandic waters and beyond)
# Study species: humpback whale (data from NASS, Iceland)
# Cell size: 0.25 degrees

# Work flow (for each survey year separately): 
# - Upload effort and rasterise with empty raster
# - Upload sightings and rasterise with empty raster 
# - Calculate SPUE in raster
# - Calculate g0 and p (detection) 
# - Estimate density per square

# Packages
packages <- c("tidyverse", "sf", "ggplot2", "raster", "lubridate", "sp", "pbapply", "rgeos")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, require, character.only = TRUE)

# set wd if remote desktop
setwd("/home/s1857169/scratch/r-density-whale/") # for xrdp linux

### STEP 1: define the study raster
 
# Set parameters, CHANGE!

# ALBERS Projection
albers.crs <- "+proj=aea +x_0=0 +y_0=0 +lon_0=-30 +lat_0=30 +lat_1=43 +lat_2=62 +units=m +ellps=WGS84 +datum=WGS84 +no_defs"
e.min <- -1250000
e.max <- 2375000
n.min <- 2070000
n.max <- 5295000
albers.cell <- 25000 # 25 km

# WGS84
wgs.crs <- "+init=epsg:4326" # all numbers below should match this CRS
lon.min <- -46
lon.max <- 3
lat.min <- 50
lat.max <- 75
wgs.cell <- 0.25 # 0.25 degrees

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

# Uploading sightings
mn.csv <- read.csv("final-products/nass_mn_cleaned.csv") %>%
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d")) %>%
  mutate(month = month(date)) %>%
  filter(spec == "MN")
head(mn.csv)

# Creating separate data frames for each year and month
mn.shape <- mn.csv %>%
  mutate_at(vars(lo2, la2), as.numeric) %>%
  st_as_sf(coords = c("lo2", "la2"), crs = wgs.crs)

mn.shape <- st_transform(mn.shape, crs = albers.crs) # change crs from wgs84 to albers
  
plot(mn.shape) # check the metadata make sense

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


# Now we can calculate number of points per cell
for (i in unique(mn.shape$year)) {
  
  mn.year <- mn.shape %>%
    filter(year == i)
  
  for (j in unique(mn.year$month)) {
    
    mn.month <- mn.year %>%
      filter(month == j)
    
    mn.coords <- do.call(rbind, st_geometry(mn.month)) %>% 
      as_tibble() %>% 
      setNames(c("x","y")) %>%
      as.data.frame
    
    mn.raster.month <- rasterize(mn.coords, albers.ref, fun=function(x,...)length(x)) # calculating number of points per cell
    
    writeRaster(mn.raster.month, paste0("intermediate-products/raster_mn_",i,"_",j,".tiff"), overwrite = FALSE)
  }
}

plot(mn.raster.month) # check last plot to see it worked!


### STEP 3: rasterise effort

# Uploading effort (wgs84)
effort.line <- read_sf("intermediate-products/nass_effort_lines.gpkg")  %>%
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d")) %>%
  mutate(month = month(date))# note: do not plot to check, takes too long 

st_crs(effort.line) <- wgs.crs # set crs
effort.line <- st_transform(effort.line, crs = albers.crs) # now change to planar 


# Now calculate length for each month of each year (will take a long time)

for (i in unique(effort.line$year)) {
  
  effort.year <- effort.line %>%
    filter(year == i)
  
  for (j in unique(effort.year$month)) {
    
    effort.month <- effort.year %>%
      filter(month == j)
    effort.month <- as_Spatial(effort.month) # must convert from sf to SpatialLines
   
    lengths <- pbsapply(1:ncell(albers.ref), function(i) {
      tmp_ras <- albers.ref
      tmp_ras[i] <- 1
      tmp_shp <- rasterToPolygons(tmp_ras)
      
      if (gIntersects(effort.month, tmp_shp)) {
        tryCatch( # use tryCatch to allow for and record errors
          {effort.crp <- crop(effort.month, tmp_shp)
          effort.crp.length <- gLength(effort.crp)
          return(effort.crp.length)},
          error = function(err) {
            message(err)
            print(paste0("for cell ",i))
            return(0)
          })
      } else {
        return(0)
      }
    }) 
    
    # now rasterise
    effort.r <- albers.ref
    effort.r[] <- lengths
    
    # now save raster 
    writeRaster(effort.r, filename = paste0("intermediate-products/effort_",i,"_",j,".tiff"), overwrite = FALSE)
  }
}

## STEP 4: calculate ESW and g(0)
# esw (effective strip width) is needed to convert km effort to km2 effort
# g(0) is needed for probability of detection


# GOOD UP TO HERE

