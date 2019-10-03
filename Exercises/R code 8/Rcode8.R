require(ggplot2)
# Normal
ggplot(NULL, aes(xmin = -4, xmax = 4)) +
  stat_function(fun = dnorm,
                args = list(mean = 0, sd = 1),
                lwd = 2, col = 'blue') +
  xlab("x") +
  scale_x_continuous(bquote(mu),
                     labels=NULL) +
  ggtitle(bquote(paste('Normal Dist. (', mu, ', ', sigma^2, '=1)')))