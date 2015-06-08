

#' @export
ConditionalDistributionPlot <- function(frame, xvar, truthVar,breaks=40,title='conditionianl distribution plot') {
  checkArgs(frame,xvar,truthVar)
  xlims = c(min(frame[[xvar]]),max(frame[[xvar]]))
  p1 <- DoubleDensityPlot(frame, xvar, truthVar,  title='')  +
    xlim(xlims) # assumes no xlim set in DoubleDensityPlot
  p2 <- DoubleHistogramPlot(frame, xvar, truthVar,breaks=breaks,title='') +
    xlim(xlims) # assumes no xlim set in DoubleHistogramPlot
  # assumes no scale_y_continuous in DoubleDensityPlot
  # assumes no scale_y_continuous in DoubleHistogramPlot
  yPadFn <- designYLabelPadFunction(p1,p2)
  grid.arrange(p1 + scale_y_continuous(label=yPadFn),
               p2 + scale_y_continuous(label=yPadFn),
               ncol = 1, nrow = 2,
               main=title)
}
