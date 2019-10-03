# Entering the data
dat <- as.data.frame(cbind(
  y=c(2,3,2,7,6,8,10,7,8,12,11,14),
  x1=c(0,2,2,2,4,4,4,6,6,6,8,8),
  x2=c(2,6,7,5,9,8,7,10,11,9,15,13)
))
dat
# Saving and recovering the data file
save(dat, file="ej72.RData")
rm(dat)
dat
load(file="ej72.RData")
dat
# Fitting models
mod1 <- lm(y~x1, data=dat)
mod2 <- lm(y~x2, data=dat)
mod3 <- lm(y~x1+x2, data=dat)
mod1
mod2
mod3