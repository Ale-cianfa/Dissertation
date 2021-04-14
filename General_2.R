# New script for GAM 

library(lme4)
prova_01 <- read.csv("final_csv/prova_table_2001.csv")

prova_01 <- prova_01 %>% 
    filter(bat_reclas < 0)

prova_01 <- prova_01 %>% 
  left_join(survey_01, by = c("la2" = "Lat"))


## GAM
b1 <- mgcv::gam(PA_rast ~ s(bat_reclas, bs='ps', sp=0.6), s(mlotst_07, bs='ps', sp=0.6), data = prova_01)
summary(b1)
plot(b1)

AIC(b1)

