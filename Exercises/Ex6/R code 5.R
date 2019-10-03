load(file="ej72.RData")
n <- nrow(dat)
attach(dat)
j <- as.vector(c(rep(1,n)))
X <- cbind(j, x1, x2)
Xt <- t(X)
XtX <- Xt %*% X
XtXinv <- solve(XtX)
Xty <- Xt %*% y
B <- XtXinv %*% Xt %*% y
XtX
Xty
XtXinv
B

# Fitting model to compare
mod3 <- lm(y~x1+x2, data=dat)
mod3