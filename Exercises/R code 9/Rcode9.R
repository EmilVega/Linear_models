require(ggplot2)
# Chi-Square
ggplot(NULL, aes(xmin = 0, xmax = 25)) +
  stat_function(fun = dchisq,
                args = list(df = 4, ncp = 0),
                lwd = 2, col = 'blue') +
  stat_function(fun = dchisq,
                args = list(df = 4, ncp = 8),
                lwd = 2, col = 'red') +
  xlab("x") +
  scale_x_continuous() +
  geom_text(x=5, y=0.15,
            aes(label=paste("lambda==0"),
                group=NULL), size = 4,
            color = "blue", parse = T) +
  geom_text(x=15, y=0.07,
            aes(label=paste("lambda==8"),
                group=NULL), size = 4,
            color = "red", parse = T) +
  ggtitle(bquote(paste('Distribution: ',
                        chi^2, '(4)')))
