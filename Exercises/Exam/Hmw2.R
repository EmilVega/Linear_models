# Reading the data
dat <- read.table("table101.txt", header=T, sep="")
library(car)
attach(dat)
n <- nrow(dat)
n

# Scatter plot and regression line
require(ggplot2)
ggplot(data, aes(x = x1, y = y)) +
  geom_point(colour="blue", shape=21, size=2) +
  geom_smooth(method='lm', colour="red", se=FALSE) +
  ylab("Lymphocyte count (y)") +
  xlab("Hemoglobin concentration (x1)")

ggplot(data, aes(x = x2, y = y)) +
  geom_point(colour="blue", shape=21, size=2) +
  geom_smooth(method='lm', colour="red", se=FALSE) +
  ylab("Lymphocyte count (y)") +
  xlab("Packed-cell volume (x2)")

ggplot(data, aes(x = x3, y = y)) +
  geom_point(colour="blue", shape=21, size=2) +
  geom_smooth(method='lm', colour="red", se=FALSE) +
  ylab("Lymphocyte count (y)") +
  xlab("White blood cell count (x.01) (x3)")

ggplot(data, aes(x = x4, y = y)) +
  geom_point(colour="blue", shape=21, size=2) +
  geom_smooth(method='lm', colour="red", se=FALSE) +
  ylab("Lymphocyte count (y)") +
  xlab("Neutrophil count (x4)")

ggplot(data, aes(x = x5, y = y)) +
  geom_point(colour="blue", shape=21, size=2) +
  geom_smooth(method='lm', colour="red", se=FALSE) +
  ylab("Lymphocyte count (y)") +
  xlab("Serum lead concentration (x5)")

# BLUE estimator of B
mod <- lm(y~x1+x2+x3+x4+x5, data=dat, x=TRUE)
mod
X <- mod$x
X
Xt <- t(X)
Xt
B <- solve(Xt%*%X)%*%Xt%*%y
B

# Value estimated for (sigma)^2
k <- ncol(X)-1
H <- X %*% solve(t(X) %*% X) %*% t(X)
H
I <- diag(1,n)
I

SSE <- t(y) %*% (I-H) %*% y
SSE

s2 <- as.double(SSE/(n-k-1))
s2

# What about (R)^2
j <- as.vector(c(rep(1,n)))
j
J <- j %*% t(j)
J
SSR <- t(y) %*% (H-(1/n)*J) %*% y
SSR
SST <- t(y) %*% (I-(1/n)*J) %*% y
SST
R2 <- SSR/SST
R2

# Test H0: B2=0 where B2=[B3 B4]'
qf(0.05, df1= 2,df2=n-k-1, lower.tail=FALSE)
C <- rbind(c(0,0,0,1,0,0),c(0,0,0,0,1,0))
C
linearHypothesis(mod,C)

# Test H0: B1=B3=B4
qf(0.05, df1= 2,df2=n-k-1, lower.tail=FALSE)

C <- rbind(c(0,1,0,-1,0,0),c(0,0,0,1,-1,0))
C
linearHypothesis(mod,C)



