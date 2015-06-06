

alignPlotYlabels <- function(p1,p2) {
  info1 = ggplot_build(p1)
  origlabs1 <- info1$panel$ranges[[1]]$y.labels
  info2 = ggplot_build(p2)
  origlabs2 <- info2$panel$ranges[[1]]$y.labels
  lengthTarget <- max(nchar(c(origlabs1,origlabs2)))
  list(p1=p1+scale_y_continuous(breaks=as.numeric(origlabs1),
                                labels=str_pad(origlabs1,lengthTarget)),
       p2=p2+scale_y_continuous(breaks=as.numeric(origlabs2),
                                labels=str_pad(origlabs2,lengthTarget)))
}


ConditionalDistributionPlot <- function(frame, xvar, truthVar,breaks=40,title='conditionianl distribution plot') {
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
