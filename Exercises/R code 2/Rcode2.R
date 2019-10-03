# Scatter plot and regression line
require(ggplot2)
data <- read.table("data01.txt", header=T, sep=";")
ggplot(data, aes(x=x, y=y)) + geom_point(colour="blue", shape=21, size=2) + 
  geom_smooth(method='lm', colour="red", se=FALSE) +
  ylab("Midterm exam (y)") +
  xlab("Homework (x)")
