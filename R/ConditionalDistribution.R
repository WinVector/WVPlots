

#' Plot conditional density and conditional histogram.
#'
#' @param frame data frame to get values from
#' @param xvar name of the indepement (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unamed argument, added to force named binding of later arguments.
#' @param breaks number of breaks in histogram
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,y=y,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' frm$absY <- abs(frm$y)
#' frm$posY = frm$y > 0
#' frm$costX = 1
#' WVPlots::ConditionalDistributionPlot(frm, "x", "yC", title="Example conditional distribution plot")
#'
#' @export
ConditionalDistributionPlot <- function(frame, xvar, truthVar, title, ...,
                                        breaks=40) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  xlims = c(min(frame[[xvar]]),max(frame[[xvar]]))
  p1 <- DoubleDensityPlot(frame, xvar, truthVar,  title='')  +
    ggplot2::xlim(xlims) # assumes no xlim set in DoubleDensityPlot
  p2 <- DoubleHistogramPlot(frame, xvar, truthVar,breaks=breaks,title='') +
    ggplot2::xlim(xlims) # assumes no xlim set in DoubleHistogramPlot
  # assumes no scale_y_continuous in DoubleDensityPlot
  # assumes no scale_y_continuous in DoubleHistogramPlot
  yPadFn <- designYLabelPadFunction(p1,p2)

  gridExtra::grid.arrange(p1 + ggplot2::scale_y_continuous(label=yPadFn),
               p2 + ggplot2::scale_y_continuous(label=yPadFn),
               top=grid::textGrob(title),
               ncol = 1, nrow = 2)
}
