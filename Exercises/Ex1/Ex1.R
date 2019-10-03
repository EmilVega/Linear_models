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
cat('Bethao=',b0,'Betha1=',b1,'\n')

