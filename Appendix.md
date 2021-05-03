Appendix 1: Code for Site Map
```
## Importing World map 
world <- map_data("world")
head(world)
str(world$region)

world$region <- as.factor(world$region) #making region a factor 

### Selecting Icelandic region
ice_map <- world %>% 
  filter(region == "Iceland") %>% 
  dplyr::select(-subregion)
```
```
## Downloading NASS survey
mn.df <- survey %>% 
  dplyr::select(year, month, spec, la2, lo2, pods) %>% 
  filter(spec == "mn") 

mn.df$year <- as.factor(mn.df$year) #making year a factor
str(mn.df)

## Downloading Survey Effort
effort <- st_read("NASS/ale_nass-effort.gpkg")

## Adding locations of Reykjavik and Port
location <- c("Finnafjörður", "Reykjavík")
lat <- c(66.116667, 64.144555)
long <- c(-15.133333, -21.941021)
group <- as.factor(c("a", "b"))
```

```
## Creating dataframe with locations
imp_locations <- data.frame(location, lat, long, group)
head(imp_locations)
str(imp_locations)
```

```
## Making Site Map with important locations
(site_map <- ggplot() +
   geom_polygon(data = ice_map, aes(x = long, y = lat, group = group) 
                , color = "black", fill = "#698B69", alpha = 0.6, size = 0.3) + 
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
```
```
## Adding the Scale bar
(site_map2 <- site_map +
  ggsn::scalebar(data = ice_map,
           transform = TRUE, dist = 100, dist_unit = "km", model='WGS84',
           height = 0.03, location = "bottomright", 
           anchor = c(x = -12.2, y = 63.3), st.dist = 0.06))
```

Appendix 2: Code used to filter and prepare the data for the QGIS analysis

```
survey <- read.csv("NASS/original/orignal_ale_nass-sightings_tot.csv") #reading survey

head(survey) #making sure is imported right 
str(survey) #checking what each thing is 

survey$spec <- as.factor(survey$spec) #species needs to be a factor for filtering

### Cleaning the dataset: 
survey <- survey %>% 
  dplyr::select(year, month, spec, la2, lo2, pods) %>% 
  filter(spec == "mn")
```
```
## SURVEY BY YEAR:

### 2001:
survey_01 <- survey %>% 
  filter (year == "2001") 

write_csv(survey_01, file = "NASS/intermediate surveys/by_year/survey_01.csv")

### 2007:
survey_07 <- survey %>% 
  filter (year == "2007")

write_csv(survey_07, file = "NASS/intermediate surveys/by_year/survey_07.csv")

### 2015:
survey_15 <- survey %>% 
  filter (year == "2015")

write_csv(survey_15, file = "NASS/intermediate surveys/by_year/survey_15.csv")
```
Appendix 3: Joining the different Presence/Absence dataframes in R
```
## 2001:
comp_2001 <- read.csv("final_df/by_year/2001_final_ds.csv")

str(comp_2001)

comp_2001 <- comp_2001 %>% 
  rename(chlorApril = chlor_0104, 
         chlorMay = chlor_0105, chlorJune = chlor_0106, 
         chlorJuly = chlor_0107,mldApril = mlotst_104,
         mldMay = mlotst_105, mldJune = mlotst_106, 
         mldJuly = mlotst_107, sstApril = sst_0104,
         sstMay = sst_0105, sstJune = sst_0106, 
         sstJuly = sst_0107)
```
```
## 2007:
comp_2007 <- read.csv("final_df/by_year/2007_final_ds.csv")

str(comp_2007)

comp_2007 <- comp_2007 %>% 
  rename(chlorApril = chlor_0704, 
         chlorMay = chlor_0705, chlorJune = chlor_0706, 
         chlorJuly = chlor_0707, mldApril = mlotst_704,
         mldMay = mlotst_705, mldJune = mlotst_706, 
         mldJuly = mlotst_707, sstApril = sst_0704,
         sstMay = sst_0705, sstJune = sst_0706, 
         sstJuly = sst_0707)
```
```
## 2015:
comp_2015 <- read.csv("final_df/by_year/2015_final_df2.csv")

str(comp_2015)

comp_2015 <- comp_2015 %>% 
  rename(chlorApril = chlor_1504, 
         chlorMay = chlor_1505, chlorJune = chlor_1506, 
         chlorJuly = chlor_1507, mldApril = mlotst_154,
         mldMay = mlotst_155, mldJune = mlotst_156, 
         mldJuly = mlotst_157, sstApril = sst_1504,
         sstMay = sst_1505, sstJune = sst_1506, 
         sstJuly = sst_1507) 
```
```
# Changing column order

## 2001: 

colnames(comp_2001) #printing out the column names

col_order <- c("fid", "PA", "Lat", "Long", "bat", 
"mldJuly", "mldJune", "mldMay", "mldApril",
"sstJuly", "sstJune", "sstMay", "sstApril", 
"chlorJuly", "chlorJune",
"chlorMay", "chlorApril")   

comp_2001 <- comp_2001[, col_order]
comp_2001

## 2007: 

colnames(comp_2007)

comp_2007 <- comp_2007[, col_order] 
comp_2007

## 2015: 

colnames(comp_2015) 

comp_2015 <- comp_2015[, col_order]
comp_2015

## Creating a final dataframe: 

comp_df <- rbind(comp_2001, comp_2007, comp_2015)

comp_df <- comp_df %>% 
  drop_na() 
```
Appendix 4: Correlation Analysis between variables
```
library(DataExplorer)

### April: 
April <-comp_df[, c("Lat", "Long", "bat", "mldApril", "sstApril", "chlorApril")]
April.plot <- plot_correlation(April, type = "continuous")

#ggsave(April.plot, file = "img/correlation/april.png", height = 4, width = 7)

### May: 
May <- comp_df[, c("Lat", "Long", "bat", "mldMay", "sstMay", "chlorMay")]
May.plot <- plot_correlation(May, type = "continuous")

#ggsave(May.plot, file = "img/correlation/may.png", height = 4, width = 7)

### June: 
June <- comp_df[, c("Lat", "Long", "bat", "mldJune", "sstJune", "chlorJune")]
June.plot <- plot_correlation(June, type = "continuous")

#ggsave(June.plot, file = "img/correlation/june.png", height = 4, width = 7)

### July: 
July <- comp_df[, c("Lat", "Long", "bat", "mldJuly", "sstJuly", "chlorJuly")]
July.plot <- plot_correlation(July, type = "continuous")

#ggsave(July.plot, file = "img/correlation/july.png", height = 4, width = 7)
```

```
gam_june_9 <- mgcv::gam(PA ~ s(bat, k = 5) + 
                          s(chlorJune, k = 5) + 
                          s(mldJune, k = 5) + 
                          s(sstJune, k = 5),
                        family = "binomial",
                        data = comp_df)
```                   

Appendix 5: Example of GAM code

``` 
gam <- mgcv::gam(PA ~ s(bat, k = 5) + 
                      s(chlorophyll, k = 5) +
                      s(mld, k = 5) +
                      s(sst, k = 5) +
                      s(Latitude, k = 5) +
                      s(Longitude, k = 5),
                    family = "binomial",
                    data = comp_df)
summary(gam)

plot(gam, pages = 1, shade = TRUE, shade.col = "#B0A6C9")

AIC(gam)
``` 
           




















