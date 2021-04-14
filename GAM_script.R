# GAM STARTER SCRIPT FOR DISS

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

## Loading survey----
survey <- read.csv("NASS/original/orignal_ale_nass-sightings_tot.csv")
head(survey)
str(survey)
survey$spec <- as.factor(survey$spec)

## FAKE GAM----

# fake data
n <- 50
sig <- 2
dat <- gamSim(1,n=n,scale=sig)

# P-spline smoothers (with lambda=0.6) used for x1 and x2; x3 is parametric.
b1 <- mgcv::gam(y ~ s(x1, bs='ps', sp=0.6) + s(x2, bs='ps', sp=0.6) + x3, data = dat)
summary(b1)
plot(b1)

#basically what i need is a y, which could be, in theory, just a column called sightings with a 1 next to it
#maybe i could use pod size (similar to what they do in the books)
#basically is the presence of something and how that realtes to specific environmental variables 

# plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
p <- predict(b1, type="lpmatrix")
beta <- coef(b1)[grepl("x1", names(coef(b1)))]
s <- p[,grepl("x1", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, dat$x1), aes(x=dat$x1, y=s)) + geom_line()

# predict
newdf <- gamSim(1,n=n,scale=sig)
f <- predict(b1, newdata=newdf)

# select smoothing parameters with REML, using P-splines
b2 <- mgcv::gam(y ~ s(x1, bs='ps') + s(x2, bs='ps') + x3, data = dat, method="REML")

# select variables and smoothing parameters
b3 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3) , data = dat, method="REML", select=TRUE)

# loess smoothers with the gam package (restart R before loading gam)
b4 <- gam::gam(y ~ lo(x1, span=0.6) + lo(x2, span=0.6) + x3, data = dat)
summary(b4)

## Trying a real gam----

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

#write.csv(prova_df,file = "Parameters/prova_complete_df.csv" )

str(prova_df$presence)

prova_df$presence <- as.numeric(prova_df$presence)

p1 <- mgcv::gam(pods ~ s(bat1, bs='ps', sp=0.5),data = prova_df)

b3 <- mgcv::gam(pods ~ s(chlorJuly) + s(sstJuly), data = prova_df, method="REML", select=TRUE)

## do i need to have presence/absence? no because I don't have that 
## can i just use pods? can i just add it again as a random effect

summary(b3)

plot(b3)

AIC(b3)

## Effective Strip Width (ESW) for sightings----








