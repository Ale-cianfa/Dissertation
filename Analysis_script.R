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
library(raster)
library(rgdal)
library(rasterVis)
library(sp)
library(mgcv) #for gam 1
library(gam) #for gam 2
library(car)

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
  dplyr::select(year, month, spec, la2, lo2, bfss,pods) %>% 
  filter(spec == "mn")

## SURVEY BY YEAR:----
  # I am doing this so that I can work on a QGIS project for each year independently 

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

#write_csv(survey_15, file = "NASS/intermediate surveys/by_year/survey_15.csv")

#Now that I have save all of these, these can be loaded into QGIS. 
  #Before doing that though, i will need to also separate the survey effort by year- 
  #-so that I can work on them individually as well.

## READING BIG TABLES AND JOINING:----

### 2001:
comp_2001 <- read.csv("final_df/by_year/2001_final_ds.csv")

str(comp_2001)

comp_2001 <- comp_2001 %>% 
  rename(chlorMarch = chlor_0103, chlorApril = chlor_0104, 
         chlorMay = chlor_0105, chlorJune = chlor_0106, 
         chlorJuly = chlor_0107, chlorAug = chlor_0108,
         mldMarch = mlotst_103, mldApril = mlotst_104,
         mldMay = mlotst_105, mldJune = mlotst_106, 
         mldJuly = mlotst_107, mldAug = mlotst_108, 
         sstMarch = sst_0103, sstApril = sst_0104,
         sstMay = sst_0105, sstJune = sst_0106, 
         sstJuly = sst_0107, sstAug = sst_0108)

#comp_2001$year <- 2001
  

### 2007:
comp_2007 <- read.csv("final_df/by_year/2007_final_ds.csv")

str(comp_2007)

comp_2007 <- comp_2007 %>% 
  rename(chlorMarch = chlor_0703, chlorApril = chlor_0704, 
         chlorMay = chlor_0705, chlorJune = chlor_0706, 
         chlorJuly = chlor_0707, chlorAug = chlor_0708,
         mldMarch = mlotst_703, mldApril = mlotst_704,
         mldMay = mlotst_705, mldJune = mlotst_706, 
         mldJuly = mlotst_707, mldAug = mlotst_708, 
         sstMarch = sst_0703, sstApril = sst_0704,
         sstMay = sst_0705, sstJune = sst_0706, 
         sstJuly = sst_0707, sstAug = sst_0708)

#comp_2007$year <- 2007

### 2015:
comp_2015 <- read.csv("final_df/by_year/2015_final_df2.csv") #MISSING LAT LONG!!

str(comp_2015)

comp_2015 <- comp_2015 %>% 
  rename(chlorMarch = chlor_1503, chlorApril = chlor_1504, 
         chlorMay = chlor_1505, chlorJune = chlor_1506, 
         chlorJuly = chlor_1507, chlorAug = chlor_1508,
         mldMarch = mlotst_153, mldApril = mlotst_154,
         mldMay = mlotst_155, mldJune = mlotst_156, 
         mldJuly = mlotst_157, mldAug = mlotst_158, 
         sstMarch = sst_1503, sstApril = sst_1504,
         sstMay = sst_1505, sstJune = sst_1506, 
         sstJuly = sst_1507, sstAug = sst_1508) 

#comp_2015$year <- 2015


## CHANGING ORDER OF COLUMNS:----

### 2001: 

colnames(comp_2001) #printing out the column names

col_order <- c("fid", "PA", "Lat", "Long", "bat", "mldAug", "mldJuly", "mldJune", "mldMay", "mldApril", "mldMarch",
"sstAug", "sstJuly", "sstJune", "sstMay", "sstApril", "sstMarch", "chlorAug", "chlorJuly", "chlorJune",
"chlorMay", "chlorApril", "chlorMarch")   

comp_2001 <- comp_2001[, col_order]
comp_2001

### 2007: 

colnames(comp_2007)

comp_2007 <- comp_2007[, col_order] #since the order is the same i should be able to reuse it
comp_2007

### 2015: 

colnames(comp_2015) 

comp_2015 <- comp_2015[, col_order]
comp_2015

## CREATING ONE BIG DATA FRAME:----

comp_df <- rbind(comp_2001, comp_2007, comp_2015) #you have to make sure to save them in the right order otherwise the name do not match

comp_df <- comp_df %>% 
  drop_na() #remov ed all NAs from the dataframe (around 200 rows!)

#comp_df$year <- as.factor(comp_df$year)
#write.csv(comp_df, file = "final_df/complete_data.csv")

#IT WORKED!! 

# MODELING---- 

## CORRELATION ANALYSIS:----

## how should I test this, considering I have different moths? 
  ## think i can probably divide it by month because I am not going to be mixing them 

### Trying to use package from Jacob's Tutorial: 
library(DataExplorer)

plot_bar(comp_df) #Plotting the distribution of the categorical variables

plot_histogram(comp_df) #Plotting the distribution of all other variables 

plot_qq(comp_df)

plot_correlation(comp_df) # I think i need to check for colinearity by month? 

# In the box there is the 
  #rho is the Spearman’s correlation coefficient or
  #cor Pearson's correlation (same thang) 


### March: 
March <-comp_df[, c("Lat", "Long", "bat", "mldMarch", "sstMarch", "chlorMarch")]
March.plot <- plot_correlation(March)

#ggsave(March.plot, file = "img/correlation/march.png", height = 4, width = 7)

### April: 
April <-comp_df[, c("Lat", "Long", "bat", "mldApril", "sstApril", "chlorApril")]
April.plot <- plot_correlation(April)

#ggsave(April.plot, file = "img/correlation/april.png", height = 4, width = 7)

### May: 
May <- comp_df[, c("Lat", "Long", "bat", "mldMay", "sstMay", "chlorMay")]
May.plot <- plot_correlation(May)

#ggsave(May.plot, file = "img/correlation/may.png", height = 4, width = 7)

### June: 
June <- comp_df[, c("Lat", "Long", "bat", "mldJune", "sstJune", "chlorJune")]
June.plot <- plot_correlation(June)

#ggsave(June.plot, file = "img/correlation/june.png", height = 4, width = 7)

### July: 
July <- comp_df[, c("Lat", "Long", "bat", "mldJuly", "sstJuly", "chlorJuly")]
July.plot <- plot_correlation(July)

#ggsave(July.plot, file = "img/correlation/july.png", height = 4, width = 7)

### August: 
August <- comp_df[, c("Lat", "Long", "bat", "mldAug", "sstAug", "chlorAug")]
Aug.plot <- plot_correlation(August)

#ggsave(Aug.plot, file = "img/correlation/august.png", height = 4, width = 7)

## Trying Pearson's correlation: 


## GAMS:----

## June:----

### june 2001: 
gam_june_2001 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                          s(chlorJune, k = 5) + 
                          s(mldJune, k = 5) + 
                          s(sstJune, k = 5) +
                          s(Lat, k = 5) +
                          s(Long, k = 5),
                        family = binomial,
                        data = comp_2001)
summary(gam_june_2001)

gam.check(gam_june_2001) #k index is close to 1 for almost all, lat and long are weird 

plot(gam_june_2001, pages = 1, shade = TRUE, shade.col = "#A36D9EAC")

AIC(gam_june_2001) #288.4568


### june 2007: 
gam_june_2007 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                             s(chlorJune, k = 5) + 
                             s(mldJune, k = 5) + 
                             s(sstJune, k = 5) +
                             s(Lat, k = 5) +
                             s(Long, k = 5),
                           family = binomial,
                           data = comp_2007)
summary(gam_june_2007)

gam.check(gam_june_2007) #k index is close to 1 for almost all, lat and long are weird 

plot(gam_june_2007, pages = 1, shade = TRUE, shade.col = "#092342BB")

AIC(gam_june_2007) #79.76937

### june 2015

gam_june_2015 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                             s(chlorJune, k = 5) + 
                             s(mldJune, k = 5) + 
                             s(sstJune, k = 5) +
                             s(Lat, k = 5) +
                             s(Long, k = 5),
                           family = binomial,
                           data = comp_2015)
summary(gam_june_2015)

gam.check(gam_june_2015) #k index is close to 1 for almost all, lat and long are weird 

plot(gam_june_2015, pages = 1, shade = TRUE, shade.col = "#2B496BC2")

AIC(gam_june_2015) #161.8301

#K is the numnber of basis functions that we want to set for our data, 
  #and that also determines the smoothness

### June 1: #just trying cause the smoothing paramenter, the more is small the more is overfitted
june_1 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                          s(chlorJune, k = 5) + 
                          s(mldJune, k = 5) + 
                          s(sstJune, k = 5),
                        family = "binomial",
                        method = "REML",
                        data = comp_df)
summary(june_1)

gam.check(june_1) #none of the p values are significant and k are almost at 1

plot(june_1, pages = 1, residuals = FALSE, shade = TRUE, shade.col = "#B0A6C9")

AIC(june_1)

### June 2: 
june_2 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                      #s(chlorJune, k = 5) + as this wasn't significant in the model before
                      s(mldJune, k = 5) + 
                      s(sstJune, k = 5),
                    family = "binomial",
                    data = comp_df)
summary(june_2)

gam.check(june_2) #none of the p values are significant and k are almost at 1

plot(june_2, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")

AIC(june_2)

### June 3: 
june_3 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                      s(chlorJune, k = 5) +
                      #s(mldJune, k = 5) + #as this is always having issues in the gam check
                      s(sstJune, k = 5),
                    family = "binomial",
                    data = comp_df)
summary(june_3)

gam.check(june_3) #none of the p values are significant and k are almost at 1

plot(june_3, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")

AIC(june_3)

### June 4: 
june_4 <- mgcv::gam(PA ~ s(bat, k = 4) + 
                      s(chlorJune, k = 4) +
                      #s(mldJune, k = 5) +
                      s(sstJune, k = 4) +
                      s(Lat, k = 5) +
                      s(Long, k = 4),
                    family = "binomial",
                    data = comp_df)
summary(june_4)

gam.check(june_4) #none of the p values are significant and k are almost at 1

plot(june_4, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")

AIC(june_4)

### June 5: 
june_5 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                      s(chlorJune, k = 5) +
                      s(mldJune, k = 5) +
                      s(sstJune, k = 5) +
                      s(Lat, k = 5) +
                      s(Long, k = 5),
                    family = "binomial",
                    data = comp_df)
summary(june_5)

gam.check(june_5) #none of the p values are significant and k are almost at 1

plot(june_5, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")

AIC(june_5)

### June 6: 
june_6 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                      s(chlorJune, k = 5) +
                      #s(mldJune, k = 5) +
                      s(sstJune, k = 5) +
                      s(Lat, k = 5) +
                      s(Long, k = 5),
                    family = "binomial",
                    method = "REML",
                    data = comp_df)
summary(june_6)

gam.check(june_6) #none of the p values are significant and k are almost at 1

plot(june_6, pages = 1, residuals = FALSE, shade = TRUE, shade.col = "#B0A6C9")

# light green #B3C6B3
# nice purple #9B9AB8 and #B8B7CC (ligher)
#even nicer purple #9E90BD
AIC(june_6)

## JULY:---- 

### july_1
jul_1 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                         s(chlorJuly, k = 5) + 
                         s(mldJuly, k = 5) + 
                         s(sstJuly, k = 5) +
                         s(Lat, k = 5) +
                         s(Long, k = 5),
                       family = "binomial",
                       data = comp_df)
summary(jul_1)
gam.check(jul_1)

plot(jul_1, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")
AIC(jul_1)

### july 2
jul_2 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                     s(chlorJuly, k = 5) + 
                     #s(mldJuly, k = 5) + 
                     s(sstJuly, k = 5) +
                     #s(Lat, k = 5),
                     #s(Long, k = 5),
                   family = "binomial",
                   data = comp_df)
summary(jul_2)
gam.check(jul_2)

plot(jul_2, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")
AIC(jul_2)


### july 3
jul_3 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                     s(chlorJuly, k = 5) + 
                     #s(mldJuly, k = 5) + 
                     s(sstJuly, k = 5) +
                   s(Lat, k = 5),
                   #s(Long, k = 5),
                   family = "binomial",
                   data = comp_df)
summary(jul_3)
gam.check(jul_3)

plot(jul_3, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")
AIC(jul_3)

### july 4
jul_4 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                     s(chlorJuly, k = 5) + 
                     s(mldJuly, k = 5) + 
                     s(sstJuly, k = 5),
                     #s(Lat, k = 5) +
                     #s(Long, k = 5),
                   family = "binomial",
                   data = comp_df)
summary(jul_4)
gam.check(jul_4)

plot(jul_4, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")
AIC(jul_4)


may_1 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                     s(chlorMay, k = 5) + 
                     s(mldMay, k = 5) + 
                     s(sstMay, k = 5) +
                     s(Lat, k = 5) +
                     s(Long, k = 5),
                   family = "binomial",
                   data = comp_df)
summary(may_1)
gam.check(may_1)

plot(may_1, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")
AIC(may_1)



## CHANGES IN ENV VARIABLES OVER THE YEARS----
(sst_change <- ggplot() +
  geom_boxplot(data = comp_df, aes(x = year, y = sstJune, group = year)) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(legend.position="none",
    plot.title = element_text(size=11)) +
  ggtitle("sst_change") +
  xlab(""))

(mld_change <- ggplot() +
    geom_boxplot(data = comp_df, aes(x = year, y = mldJune, group = year)) +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_ipsum() +
    theme(legend.position="none",
          plot.title = element_text(size=11)) +
    ggtitle("mld_change") +
    xlab(""))

(chlor_change <- ggplot() +
    geom_boxplot(data = comp_df, aes(x = year, y = chlorJune, group = year)) +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_ipsum() +
    theme(legend.position="none",
          plot.title = element_text(size=11)) +
    ggtitle("chlor_change") +
    xlab(""))





dataset <- comp_df %>% 
  dplyr::select(-c(mldMarch, sstMarch, chlorMarch, 
            mldApril, sstApril, chlorApril,  
            mldAug, sstAug, chlorAug, mldMay, sstMay, chlorMay))
write_csv(dataset, file = "final_df/datset_result_appendix.csv")









