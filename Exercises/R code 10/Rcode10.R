require(ggplot2)
# F-Snedecor
ggplot(NULL, aes(xmin = 0, xmax = 10)) +
  stat_function(fun = df,
                args = list(df1=20, df2=15, ncp = 0),
                lwd = 2, col = 'blue') +
  stat_function(fun = df,
                args = list(df1=20, df2=15, ncp = 25),
                lwd = 2, col = 'red') +
  xlab("x") +
  scale_x_continuous() +
  geom_text(x=2, y=0.7,
            aes(label=paste("lambda==0"),
                group=NULL), size = 4,
            color = "blue", parse = T) +
  geom_text(x=5, y=0.2,
            aes(label=paste("lambda==25"),
                group=NULL), size = 4,
            color = "red", parse = T) +
  ggtitle(bquote(paste('Distribution: ',
                        F['(20, 15)'])))