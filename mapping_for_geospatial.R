## Alessandra Cianfanelli: Script for analysis

## LOADING PACKAGES---- 

library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(lme4) #for mixed models
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot
library(ggrepel)
library(ggthemes) #for extra map themes
library(raster) #for plotting env variables
library(rgdal)
library(rasterVis) 
library(sp) 
library(mgcv) #for gams
library(car) #can't remember for the life of me

getwd() #checking that this is right

## GENERAL SURVEY:----
# Here I am going to load the survey Tom sent me and I will do some high level filtering

### Reading the main survey:
survey <- read.csv("NASS/original/orignal_ale_nass-sightings_tot.csv") #reading survey

head(survey) #making sure is imported right 
str(survey) #checking what each thing is 

survey$spec <- as.factor(survey$spec) #species needs to be a factor for filtering

### Selecting only the important bits of the dataset: 
survey <- survey %>% 
  dplyr::select(year, month, spec, la2, lo2, bfss,pods)

















