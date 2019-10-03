
library(ggplot2)
library(car)

data <- read.table("uscrime.txt", header = T, sep = "\t")

p1 <- ggplot(data, aes(x = Prob, y = Crime)) + geom_point(colour="blue", shape=21, size=2) + geom_smooth(method='lm', colour="red", se=FALSE) + ylab("Crimes rate: number of offenses per 100,000 population in 1960") +  xlab("Probability of imprisonment: ratio of number of commitments to number of offenses") 

ggsave("p1.eps")

p2 <- ggplot(data, aes(x = Ineq, y = Crime)) + geom_point(colour="blue", shape=21, size=2) + geom_smooth(method='lm', colour="red", se=FALSE) + ylab("Crimes rate: number of offenses per 100,000 population in 1960") +  xlab("Income inequality: percentage of families earning below half the median income") 

ggsave("p2.eps")

p3 <- ggplot(data, aes(x = Ed, y = Crime)) + geom_point(colour="blue", shape=21, size=2) + geom_smooth(method='lm', colour="red", se=FALSE) + ylab("Crimes rate: number of offenses per 100,000 population in 1960") +  xlab("Mean years of schooling of the population aged 25 years or over") 

ggsave("p3.eps")

# Initial linear model
mod <- lm(Crime~., data=data, x = TRUE)
summary(mod)

attach(data)
n <- nrow(data)
j <- as.vector(c(rep(1,n)))
X <- cbind(j, M, So, Ed, Po1, Po2, LF, M.F, Pop, NW, U1, U2, Wealth, Ineq, Prob, Time)
k <- ncol(X)-1

# Overall Regression (See the impact of all the variables)
C <- cbind(rep(0,k), diag(1,k))
linearHypothesis(mod,C)

# What if Education = 0?
linearHypothesis(mod,c("Ed=0"))

# What if the effect of Education and Inequality both equal to 0?
linearHypothesis(mod,c("Ed=0","Ineq=0"))

# And what if also consider Probability = 0?
linearHypothesis(mod,c("Ed=0","Ineq=0","Prob=0"))

# What is the effect of the sum of the three equal to zero?
linearHypothesis(mod,c("Ed+Ineq+Prob=0"))

# Is the effect of wealth equivalent to inequality?
linearHypothesis(mod,c("Wealth=Ineq"))

# And this hipothesis?
linearHypothesis(mod,c("M=0","Ed=Ineq","U2+Prob=0","Po1=0"))



# Second linear model
mod2 <- lm(Crime~M+Ed+Po1+U2+Ineq+Prob, data=data, x = TRUE)
summary(mod2)

# Overall Regression (See the impact of all the variables)
C <- cbind(rep(0,6), diag(1,6))
linearHypothesis(mod2,C)

# What if Education = 0?
linearHypothesis(mod2,c("Ed=0"))

# What if the effect of Education and Inequality both equal to 0?
linearHypothesis(mod2,c("Ed=0","Ineq=0"))

# And what if also consider Probability = 0?
linearHypothesis(mod2,c("Ed=0","Ineq=0","Prob=0"))

# What is the effect of the sum of the three equal to zero?
linearHypothesis(mod2,c("Ed+Ineq+Prob=0"))

# And this hipothesis?
linearHypothesis(mod,c("M=0","Ed=Ineq","U2+Prob=0","Po1=0"))



# Third linear model
mod3 <- lm(log(Crime)~., data=data, x = TRUE)
summary(mod3)

p4 <- ggplot(data, aes(x = Prob, y = log(Crime))) + geom_point(colour="blue", shape=21, size=2) + geom_smooth(method='lm', colour="red", se=FALSE) + ylab("Crimes rate: number of offenses per 100,000 population in 1960 (Log Scale)") +  xlab("Probability of imprisonment: ratio of number of commitments to number of offenses")

ggsave("p4.eps")

# Overall Regression (See the impact of all the variables)
C <- cbind(rep(0,k), diag(1,k))
linearHypothesis(mod3,C)

# What if Education = 0?
linearHypothesis(mod3,c("Ed=0"))

# What if the effect of Education and Inequality both equal to 0?
linearHypothesis(mod3,c("Ed=0","Ineq=0"))

# And what if also consider Probability = 0?
linearHypothesis(mod3,c("Ed=0","Ineq=0","Prob=0"))

# What is the effect of the sum of the three equal to zero?
linearHypothesis(mod3,c("Ed+Ineq+Prob=0"))

# Is the effect of wealth equivalent to inequality?
linearHypothesis(mod3,c("Wealth=Ineq"))

# And this hipothesis?
linearHypothesis(mod,c("M=0","Ed=Ineq","U2+Prob=0","Po1=0"))


