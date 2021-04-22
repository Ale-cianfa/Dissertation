## discarded gams! 
### June 1: 
gam_june_1 <- mgcv::gam(PA ~ s(bat, bs = "ps", sp = 1) + 
                          s(chlorJune, bs='ps', sp= 1) + 
                          s(mldJune, bs='ps', sp= 1) + 
                          s(sstJune, bs='ps', sp= 1) +
                          s(Lat, bs = 're') +
                          s(Long, bs = 're'),
                        family = "binomial", 
                        data = comp_df)
summary(gam_june_1)

gam.check(gam_june_1) #k index is close to 1 for almost all, lat and long are weird 

plot(gam_june_1, pages = 1)

AIC(gam_june_1) #484.0315

### June 2:
gam_june_2 <- mgcv::gam(PA ~ s(bat, k = 70) + 
                          s(chlorJune, k = 70) + 
                          s(mldJune, k = 70) + 
                          s(sstJune, k = 70) +
                          s(Lat, bs = 're') +
                          s(Long, bs = 're'),
                        family = binomial("logit"), 
                        data = comp_df)

summary(gam_june_2)

gam.check(gam_june_2)

plot(gam_june_2, pages = 1) #this almost looks linear, kind of weir, not like

AIC(gam_june_2, gam_june_1)

### June 3:
gam_june_3 <- mgcv::gam(PA ~ s(bat, k = 60) + 
                          s(chlorJune, k = 60) + 
                          s(mldJune, k = 60) + 
                          s(sstJune, k = 60),
                        family = "binomial", #("logit")
                        data = comp_df)#, method = "REML")

summary(gam_june_3)

gam.check(gam_june_3) #none of the p values are significant and k are almost at 1

plot(gam_june_3, pages = 1, shade = TRUE, shade.col = "lightblue")

AIC(gam_june_3)

### June 5: 
gam_june_5 <- mgcv::gam(PA ~ s(bat, k = 80) + 
                          s(chlorJune, k = 80) + 
                          s(mldJune, k = 80) + 
                          s(sstJune, k = 80) +
                          s(Lat, bs = "re") +
                          s(Long, bs = "re"),
                        family = binomial("logit"),
                        data = comp_df)

summary(gam_june_5)

gam.check(gam_june_5) #none of the p values are significant and k are almost at 1

plot(gam_june_5, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")

AIC(gam_june_5)


### June 6:
gam_june_6 <- mgcv::gam(PA ~ s(bat, k = 10) + 
                          s(chlorJune, k = 10) + 
                          s(mldJune, k = 10) + 
                          s(sstJune, k = 10) +
                          s(Lat, bs = "re") +
                          s(Long, bs = "re"),
                        family = binomial("logit"),
                        data = comp_df)

summary(gam_june_6)

gam.check(gam_june_6) #none of the p values are significant and k are almost at 1

plot(gam_june_6, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")

AIC(gam_june_6)

### June 7:
gam_june_7 <- mgcv::gam(PA ~ s(bat, k = 20) + 
                          s(chlorJune, k = 20) + 
                          s(mldJune, k = 20) + 
                          s(sstJune, k = 20) +
                          s(Lat, bs = "re") +
                          s(Long, bs = "re"),
                        family = binomial("logit"),
                        data = comp_df)
summary(gam_june_7)

gam.check(gam_june_7) #none of the p values are significant and k are almost at 1

plot(gam_june_7, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")

AIC(gam_june_7)

### June 8:
gam_june_8 <- mgcv::gam(PA ~ s(bat, fx = F, k = 22) + 
                          s(chlorJune,fx = F, k = 22) + 
                          s(mldJune, fx = F, k = 22) + 
                          s(sstJune, fx = F, k = 22) +
                          s(Lat, fx = F, k = 22) +
                          s(Long, fx = F, k = 22),
                        family = binomial,
                        data = comp_df)
summary(gam_june_8)

gam.check(gam_june_8) #none of the p values are significant and k are almost at 1

plot(gam_june_8, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")

AIC(gam_june_8)

### June 10: #just trying cause the smoothing paramenter, the more is small the more is overfitted
gam_june_10 <- mgcv::gam(PA ~ s(bat, bs="tp") + 
                           s(chlorJune, bs="tp") + 
                           s(mldJune, bs="tp") + 
                           s(sstJune, bs="tp") +
                           s(Lat, bs="tp") +
                           s(Long, bs="tp"),
                         family = binomial,
                         data = comp_df)
summary(gam_june_10)

gam.check(gam_june_10) #none of the p values are significant and k are almost at 1

plot(gam_june_10, pages = 1, residuals = TRUE, shade = TRUE, shade.col = "lightblue")

AIC(gam_june_10, gam_june_8)

### June 4:
gam_june_4 <- mgcv::gam(PA ~ s(chlorJune) + 
                          s(mldJune) + 
                          s(sstJune) +
                          s(bat),
                        family = "binomial", 
                        method = "REML",
                        data = comp_df)
summary(gam_june_4)

gam.check(gam_june_4)

plot(gam_june_4, pages = 1)

AIC(gam_june_4)









