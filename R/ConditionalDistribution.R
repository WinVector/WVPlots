

ConditionalDistributionPlot <- function(frame, xvar, truthVar,breaks=40,title='conditionianl distribution plot') {
  xlims = c(min(frame[[xvar]]),max(frame[[xvar]]))
  p1 <- DoubleDensityPlot(frame, xvar, truthVar,  title='') +
    xlim(xlims)  # assumes no xlim set in DoubleDensityPlot
  p2 <- DoubleHistogramPlot(frame, xvar, truthVar,breaks=breaks,title='') +
    xlim(xlims) # assumes no xlim set in DoubleHistogramPlot
  grid.arrange(p1,p2,
               ncol = 1, nrow = 2,
               main=title)
}
