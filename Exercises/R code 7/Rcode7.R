# Reading the data
data <- read.table("data01.txt",
                   header=T, sep=";")
data
# Suppose we know sigma^2
s2 <- 2.5
# Building the Matrices
n <- nrow(data); n
j <- as.vector(rep(1,n)); j
X <- as.matrix(cbind(x0=j, x1=data$x)); X
Y <- data$y; Y

# Fitting original model
Xt <- t(X)
XtX <- Xt %*% X
XtXinv <- solve(XtX)
B1 <- XtXinv %*% Xt %*% Y
B1
VB1 <- s2 * XtXinv
VB1

# Now suppose that V <> I
V <- diag(ifelse(data$x==0,1,data$x))
V
Vinv <- solve(V)
Vinv
# Fitting GLS model
XVX <- solve(Xt %*% Vinv %*% X)
XVX
B2 <- XVX %*% Xt %*% Vinv %*% Y
B2
VB2 <- s2 * XVX
VB2

# The incorrect variance
VB2_inc <- s2 * XtXinv %*% Xt %*%
  V %*% X %*% XtXinv
VB2_inc
# The Generalized Variance |Sigma|
det(VB2_inc)
det(VB2)
# Cholesky decomposition
V_Ch <- chol(V)
V_Ch
V_Ch %*% t(V_Ch)
V
