

#' @export
ConditionalDistributionPlot <- function(frame, xvar, truthVar,breaks=40,title='conditionianl distribution plot') {
  checkArgs(frame,xvar,truthVar)
  xlims = c(min(frame[[xvar]]),max(frame[[xvar]]))
  p1 <- DoubleDensityPlot(frame, xvar, truthVar,  title='')
  p2 <- DoubleHistogramPlot(frame, xvar, truthVar,breaks=breaks,title='')
  # assumes no scale_y_continuous in DoubleDensityPlot
  # assumes no scale_y_continuous in DoubleHistogramPlot
  newPlots <- alignPlotYlabels(p1 +
                                 xlim(xlims), # assumes no xlim set in DoubleDensityPlot
                               p2 +
                                 xlim(xlims)) # assumes no xlim set in DoubleHistogramPlot
  grid.arrange(newPlots$p1,
               newPlots$p2,
               ncol = 1, nrow = 2,
               main=title)
}
