### Calculating effective strip width for humpback NASS sightings (iATLANTIC)
### Adapted by Tom Grove
### 07/06/2020

# AIM: effective strip width can be used to calculate the area effectively searched during transect surveys. Area = length * 2ESW (either side)
# Using this guide: https://cran.r-project.org/web/packages/Rdistance/vignettes/Rdistance_BeginnerLineTransectCovar.pdf

packages <- c("tidyverse", "Rdistance", "MuMIn")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, require, character.only = TRUE)

# Require 2 data sets:
# 1) Sightings
# 2) Transects

## DATA 

effort <- read.csv("final-products/nass_mn_cleaned.csv") %>%
  mutate(bfss = round(bfss)) # rounding up bfss

sight.mn <- effort %>%
  filter(spec == "MN") # should be 774 obs

head(sight.mn)

# We will exclude bfss > 5. Let's check the the proportion of effort and sightings that are lost

# effort 
count(filter(effort, bfss > 5))
count(effort)
(count(filter(effort, bfss > 5))/count(effort)) * 100
sum(filter(effort, bfss > 5)$nm)
sum(effort$nm)
(sum(filter(effort, bfss > 5)$nm)/sum(effort$nm))*100
# Removing 1286 out of 27503 effort tracks lost (or 4.7%)
# Removing 3138 out of 46663 nm effort (6.7%)

# sightings
count(filter(sight.mn, bfss > 5))
count(sight.mn)
(count(filter(sight.mn, bfss > 5))/count(sight.mn))*100
# Removing 5 out of 774 MN sightings (0.6%)

# now exclude bfss > 5
effort <- filter(effort, bfss <= 5)
sight.mn <- filter(sight.mn, bfss <= 5)

# Now we will calculate perpendicular distance from rdist and ang values

sight.mn <- sight.mn %>%
  mutate(dist = abs(perpDists(sightDist="rdist",
                        sightAngle="ang",
                        data = sight.mn))) %>%
  select(-c(rdist, ang)) # no longer need these columns

 # Explore distribution of distances

hist(sight.mn$dist, col="grey", main="",xlab="Distance (m)")
rug(sight.mn$dist,quiet = TRUE)

summary(sight.mn$dist)

# We can see it pretty much ends at 6000 m. We will truncate at 6000 m


## BASIC DETECTION FUNCTION

(func.basic<- dfuncEstim(formula = dist~1,
                        detectionData = sight.mn,
                        likelihood = "hazrate", w.hi = 6000))

png('intermediate-products/esw.png', height = 1500, width = 2000, unit = "px", res = 300)
plot(func.basic)
dev.off()



# AICc = 12560. ESW = 2250

# NOTE: we are not estimating g(0) from here. We may be taking this value from the published literature (humpbacks, T-NASS). 

## PREP COVARIATES FOR DETECTION FUNCTION
# Beaufort sea state, 

# bfss
(f.bfss <- dfuncEstim(formula = dist~bfss,
                       detectionData = sight.mn,
                       likelihood = "halfnorm", w.hi = 6000))
# AICc = 12560, ESW = 2261. No improvement

# bfss.type. Alternative to bfss, where calm = 0-2, mod = 3-5
sight.mn <- sight.mn %>%
  mutate(bfss.type = ifelse((sight.mn$bfss >= 0 & sight.mn$bfss <= 2), "calm", "mod"))

(f.bfss.type <- dfuncEstim(formula = dist~bfss.type,
                    detectionData = sight.mn,
                    likelihood = "halfnorm", w.hi = 6000))
# AICc = 12560, ESW = 2261. No improvement

# vID (vessel ID) and platform height
unique(sight.mn$vID) # need some info for each one

# Need to upload vessel information. We will only focus on height of primary (lower) platform for now
(vess <- read.csv("data/nass_vessels.csv") %>%
  rename(vID = ID, l.height = Lower.platform) %>%
  select(vID, l.height) %>%
  group_by(vID) %>%
  mutate(l.height = mean(l.height)) %>% # for a few vessels, different heights are reported for different transects. They're not very different, so we are averaging their heights here for simplicity
  distinct(vID, l.height))

# so looking at this, we can group vessels into 3 height classes. 9 m, 10 m and 15 m.

sight.mn <- sight.mn %>%
  left_join(vess) %>% # warning is fine
  mutate(h.plat = ifelse(round(l.height) == 9, "9m",
                         ifelse(round(l.height) == 10, "10m",
                                ifelse(round(l.height) == 15, "15m", "")))) # giving heights one of 3 categories: 9m, 10m, 15m 

(f.h.plat <- dfuncEstim(formula = dist~h.plat,
                    detectionData = sight.mn,
                    likelihood = "halfnorm", w.hi = 6000))
# AICc = 12541, ESW = 2251. Slight improvement

# group size 
unique(sight.mn$pods) # looks good to use
(f.pods <- dfuncEstim(formula = dist~pods,
                      detectionData = sight.mn,
                      likelihood = "halfnorm", w.hi = 6000))
# AICc = 12560, ESW = 2262. No improvement.

# So from this, including h.plat (platform height category) provided the best fit. 

# Let's check out some plots of 9m, 10m and 15m platforms to see

#9m
hist(filter(sight.mn, h.plat == "9m")$dist, col="grey", main="",xlab="Distance (m)")
rug(filter(sight.mn, h.plat == "9m")$dist,quiet = TRUE)

#10m
hist(filter(sight.mn, h.plat == "10m")$dist, col="grey", main="",xlab="Distance (m)")
rug(filter(sight.mn, h.plat == "10m")$dist,quiet = TRUE)

#15m
hist(filter(sight.mn, h.plat == "15m")$dist, col="grey", main="",xlab="Distance (m)")
rug(filter(sight.mn, h.plat == "15m")$dist,quiet = TRUE)

# Now let's compare different likelihood functions: half-hormal, hazard, Gamma, negative exponential

(hn.plat <- dfuncEstim(formula = dist~h.plat,
                        detectionData = sight.mn,
                        likelihood = "halfnorm", w.hi = 6000))
# AICc = 12541, ESW = 2251 

(haz.plat <- dfuncEstim(formula = dist~h.plat,
                        detectionData = sight.mn,
                        likelihood = "hazrate", w.hi = 6000))
# AICc = 12481, ESW = 1707

(gamma.plat <- dfuncEstim(formula = dist~h.plat,
                        detectionData = sight.mn,
                        likelihood = "Gamma", w.hi = 6000))
# Failed

(negexp.plat <- dfuncEstim(formula = dist~h.plat,
                          detectionData = sight.mn,
                          likelihood = "negexp", w.hi = 6000))
# AICc = 215999, ESW = 15.1

# From this, we will choose hazard function, with vessel category as a significant covariate, deltaAICc > 2 when added




png('intermediate-products/esw_haz_plat-height.png', height = 1500, width = 2000, unit = "px", res = 300)
plot(haz.plat, newdata=data.frame(h.plat = unique(sight.mn$h.plat)), legend = TRUE)
legend("topright", title="Platform height (m)", legend = c("9","10","15"))
dev.off()

########################
### FINAL VALUES #######
########################
### Link: hazard #######
### Covariate: h.plat ##
### ESW: 1707 ##########
### AICc: 12481 ########
########################