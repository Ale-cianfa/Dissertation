# GAM STARTER SCRIPT FOR DISS

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