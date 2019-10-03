# Calculating s2 and s
data <- read.table("data01.txt", header=T, sep=";")
attach(data)
n <- nrow(data)
xy <- x*y
x2 <- x^2
sxy <- sum(xy)
sx2 <- sum(x2)
mx <- mean(x)
my <- mean(y)
b1 <- (n*mx*my-sxy)/(n*mx^2-sx2)
b0 <- my - mx*b1
s2 <- 0
for (i in 1:n) {
  s2 <- s2 + (y[i]-b0-b1*x[i])^2
}
s2 <- s2/(n-2)
s <- sqrt(s2)
cat('s2=',s2,'s=',s,'\n')


# Calculating t from the data (tc)
xt <- 0
i <- 1
while (i<=n){
  xt <- xt + (x[i]-mx)^2
  i <- i+1
}
tc <- b1 / (s/sqrt(xt))
cat('tc=',tc,'\n')

# Calculating theoretical t
alpha <- 0.05
n <- 18
t <- qt(p=alpha/2, df=n-2, ncp=0, lower.tail = FALSE, log.p = FALSE)
cat('t=',t,'\n')

# The conclusion
if(tc>t){
  con <- "Reject Ho"
} else {
  con <- "We do not have enough"
  con <- paste(con, "evidence to reject Ho", sep=" ")
}
cat('t-calc=', tc, ', t-theo=', t, ' . Con: ', con, '\n')

# Calculating a CI
cat('Betha1 in (', b1-t*s/sqrt(xt), ';', b1+t*s/sqrt(xt), ')\n')

