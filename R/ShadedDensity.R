
#' Plot the distribution of a variable with a tail shaded
#'
#' Plot the distribution of a variable with a tail shaded. Annotate with the area of the shaded region.
#'
#' @param frame data frame to get values from
#' @param xvar name of the variable to be density plotted
#' @param threshold boundary value for the tail
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param tail which tail to shade, 'left' (default) or 'right'
#' @param linecolor color of density curve
#' @param shading color of shaded region and boundaries
#' @param annotate_area if TRUE (default), report the area of the shaded region
#'
#' @seealso \code{\link{ShadedDensityCenter}}
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' set.seed(52523)
#' d = data.frame(meas=rnorm(100))
#' threshold = -1.5
#' WVPlots::ShadedDensity(d, "meas", threshold,
#'                        title="Example shaded density plot, left tail")
#' if (FALSE) {
#' WVPlots::ShadedDensity(d, "meas", -threshold, tail="right",
#'                        title="Example shaded density plot, right tail")
#' }
#'
#' @export
ShadedDensity <- function(frame, xvar, threshold, title,
                          ...,
                          tail="left",
                          linecolor = "darkgray",
                          shading = "darkblue",
                          annotate_area = TRUE) {
  frame <- as.data.frame(frame)
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::ShadedDensity")
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

  plt = ggplot2::ggplot() +
    ggplot2::geom_line(data=densityframe, ggplot2::aes(x=x, y=density), color=linecolor) +
    ggplot2::geom_ribbon(data=densityframe, ggplot2::aes(x=x, ymin=0, ymax=tail), fill=shading, alpha=0.5) +
    ggplot2::geom_vline(xintercept=threshold, color=shading,  linetype=3) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xvar)

  if(annotate_area) {
    plt = plt + ggplot2::annotate("text", x=textx, y=texty, label=text, size=5, hjust="inward", vjust="bottom")
  }

  plt
}

#' Plot the distribution of a variable with a center region shaded
#'
#' Plot the distribution of a variable with a center region shaded. Annotate with the area of the shaded region.
#'
#' @param frame data frame to get values from
#' @param xvar name of the variable to be density plotted
#' @param boundaries vector of the min and max boundaries of the shaded region
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param linecolor color of density curve
#' @param shading color of shaded region and boundaries
#' @param annotate_area if TRUE (default), report the area of the shaded region
#'
#' @seealso \code{\link{ShadedDensity}}
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' set.seed(52523)
#' d = data.frame(meas=rnorm(100))
#' boundaries = c(-1.5, 1.5)
#' WVPlots::ShadedDensityCenter(d, "meas", boundaries,
#'                        title="Example center-shaded density plot")
#'
#' @export
ShadedDensityCenter <- function(frame, xvar, boundaries, title,
                                ...,
                                linecolor = "darkgray",
                                shading = "darkblue",
                                annotate_area = TRUE) {
  frame <- as.data.frame(frame)
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::ShadedDensity")
  if(length(boundaries) != 2) {
    stop("The argument boundaries must be exactly length 2: c(min, max).")
  }
  if(boundaries[1] >= boundaries[2] ) {
    stop("The maximum boundary must be strictly larger than the minimum boundary.")
  }

  x <- NULL # used as a symbol, declare not an unbound variable

  # calculate the distribution by hand
  dens = density(frame[[xvar]], adjust=0.5)
  densityframe = data.frame(x=dens$x, density=dens$y)

  # ecdf returns a FUNCTION that computes the empirical cdf
  densityfun = ecdf(frame[[xvar]])

  densityframe$shaded = numeric(nrow(densityframe))
  densityframe$shaded = ifelse(boundaries[1] < densityframe$x & densityframe$x < boundaries[2],
                               densityframe$density, 0)

  area = densityfun(boundaries[2]) - densityfun(boundaries[1])
  text = paste("shaded area = ", format(area, digits=2))
  texty = 0.1*max(dens$y)
  xrange = max(dens$x)-min(dens$x)
  textx = boundaries[1] + 0.01*xrange

  bframe = data.frame(boundaries = boundaries)

  shaded <- NULL # don't look like an unbound reference

  plt =  ggplot2::ggplot() +
    ggplot2::geom_line(data=densityframe, ggplot2::aes(x=x, y=density), color=linecolor) +
    ggplot2::geom_ribbon(data=densityframe, ggplot2::aes(x=x, ymin=0, ymax=shaded), fill=shading, alpha=0.5) +
    ggplot2::geom_vline(data=bframe, aes(xintercept=boundaries), color=shading,  linetype=3) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xvar)

  if(annotate_area) {
    plt = plt + ggplot2::annotate("text", x=textx, y=texty, label=text, size=5, hjust="inward", vjust="bottom")
  }

 plt
}


