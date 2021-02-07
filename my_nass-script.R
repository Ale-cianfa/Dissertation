# CROPPING THE DATA FOR MY DISSERTATION 

## PACKAGES----

library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)

getwd()
## LOADING FILES----

### Iceland shapefile (that I do not have)----
iceland <- st_read("data/iceland-isn93/is50v_strandlina_flakar_24122017.shp")

### NASS survey----
survey <- read.csv("NASS/ale_nass-sightings_tot.csv")
head(survey)
str(survey)
survey$spec <- as.factor(survey$spec)
#we are going to use la2 and lo2 as the coordinates for the whales, because those were the locations
  #of observers when they saw the whale and is as good as its gonna get 

## TYDING UP NASS CSV----
#I want a spatial extent that includes only the NE (-19, -9 in long and 65, 68 in lat)
  #Also, i am going to remove all species that are not humpback whales 

new_survey <- survey %>% 
  select(year, month, spec, la2, lo2, pods) %>% 
  filter(spec == "mn") %>% #keeping only the humpback whales 
  
  
  
















