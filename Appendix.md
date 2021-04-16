Appendix methods pt1:

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

Appendix methods pt2:
```
library(DataExplorer)

### March: 
March <-comp_df[, c("Lat", "Long", "bat", "mldMarch", "sstMarch", "chlorMarch")]
March.plot <- plot_correlation(March)

ggsave(March.plot, file = "img/correlation/march.png", height = 4, width = 7)
```





















