

ConditionalDistributionPlot <- function(frame, xvar, truthVar,breaks=40,title='conditionianl distribution plot') {
  p1 <- DoubleDensityPlot(frame, xvar, truthVar,  title='')
  p2 <- DoubleHistogramPlot(frame, xvar, truthVar,breaks=breaks,title='')
  grid.arrange(p1,p2,
               ncol = 1, nrow = 2,
               main=title)
}
