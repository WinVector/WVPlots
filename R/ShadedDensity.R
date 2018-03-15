
#' Plot the distribution of a variable with a tail shaded
#'
#' @param frame data frame to get values from
#' @param xvar name of the variable to be density plotted
#' @param threshold boundary value for the tail
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param tail which tail to shade, 'left' (default) or 'right'
#' @examples
#'
#' set.seed(52523)
#' d = data.frame(meas=rnorm(100))
#' threshold = -1.5
#' WVPlots::ShadedDensity(d, "meas", threshold,
#'                        title="Example shaded density plot, left tail")
#' WVPlots::ShadedDensity(d, "meas", -threshold, tail="right",
#'                        title="Example shaded density plot, right tail")
#'
#' @export
ShadedDensity <- function(frame, xvar, threshold,title,..., tail="left") {
  checkArgs(frame=frame,xvar=xvar,yvar=xvar,title=title,...)
  x <- NULL # used as a symbol, declare not an unbound variable

  # calculate the distribution by hand
  dens = density(frame[[xvar]], adjust=0.5)
  densityframe = data.frame(x=dens$x, density=dens$y)

  # ecdf returns a FUNCTION that computes the empirical cdf
  densityfun = ecdf(frame[[xvar]])

  densityframe$tail = numeric(nrow(densityframe))
  sign = ifelse(tail=="left", 1, -1)
  densityframe$tail = ifelse(sign*densityframe$x < sign*threshold, densityframe$density, 0)

  area = switch(tail,
                left = densityfun(threshold),
                right= 1- densityfun(threshold)
  )
  text = paste("tail area = ", format(area, digits=2))
  texty = 0.1*max(dens$y)
  xrange = max(dens$x)-min(dens$x)
  textx = threshold + sign*0.01*xrange

  ggplot2::ggplot() +
    ggplot2::geom_line(data=densityframe, ggplot2::aes(x=x, y=density), color="darkgray") +
    ggplot2::geom_ribbon(data=densityframe, ggplot2::aes(x=x, ymin=0, ymax=tail), fill="darkblue", alpha=0.5) +
    ggplot2::geom_vline(xintercept=threshold, color="darkblue",  linetype=2) +
    ggplot2::annotate("text", x=textx, y=texty, label=text, size=5, hjust="inward", vjust="bottom") +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xvar)
}



