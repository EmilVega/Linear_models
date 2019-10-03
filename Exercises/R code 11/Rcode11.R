require(ggplot2)
# t-Student
ggplot(NULL, aes(xmin = -10, xmax = 20)) +
  stat_function(fun = dt,
                args = list(df = 3, ncp = 0),
                lwd = 2, col = 'blue') +
  stat_function(fun = dt,
                args = list(df = 3, ncp = 5),
                lwd = 2, col = 'red') +
  xlab("x") +
  scale_x_continuous() +
  geom_text(x=2.5, y=0.3,
            aes(label=paste("lambda==0"),
                group=NULL), size = 4,
            color = "blue", parse = T) +
  geom_text(x=5, y=0.2,
            aes(label=paste("lambda==5"),
                group=NULL), size = 4,
            color = "red", parse = T) +
  ggtitle(bquote(paste('Distribution: ',
                        t['(3)'])))