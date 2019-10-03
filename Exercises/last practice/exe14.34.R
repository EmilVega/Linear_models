library(ggplot2)
data<-read.table("tab14.7.txt",header=T,sep=",")
data
ggplot(data,aes(R,y)) + geom_point()
ggplot(data,aes(M,y)) + geom_point()
ggplot(data, aes(R,y)) + geom_boxplot()
ggplot(data, aes(M,y)) + geom_boxplot()
mod <- lm(y~R*M, data=data, x=T)
mod
mod$x
summary(mod)
anova(mod)

