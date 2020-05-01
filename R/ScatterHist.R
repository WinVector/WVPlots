
#' @importFrom sigr wrapFTest render
#' @importFrom stats complete.cases
#' @importFrom grid unit
#' @importFrom gridExtra grid.arrange
#' @importFrom mgcv gam
NULL

#' Plot a scatter plot with marginals.
#'
#' Plot a scatter plot with optional smoothing curves or contour lines, and marginal histogram/density plots.
#' Based on \url{http://www.win-vector.com/blog/2015/06/wanted-a-perfect-scatterplot-with-marginals/}.
#' See also \code{ggExtra::ggMarginal}.
#'
#' If \code{smoothmethod} is:
#' \itemize{
#' \item 'auto', 'loess' or 'gam':  the appropriate smoothing curve is added to the scatterplot.
#' \item 'lm' (the default): the best fit line is added to the scatterplot.
#' \item 'identity':  the line x = y is added to the scatterplot. This is useful for comparing model predictions to true outcome.
#' \item 'none': no smoothing line is added to the scatterplot.
#' }
#'
#' If \code{estimate_sig} is TRUE and \code{smoothmethod} is:
#' \itemize{
#' \item 'lm': the R-squared of the linear fit is reported.
#' \item  'identity': the R-squared of the exact relation between \code{xvar} and \code{yvar} is reported.
#' }
#'
#' Note that the identity R-squared is NOT the square of the correlation between \code{xvar} and \code{yvar}
#' (which includes an implicit shift and scale). It is the coefficient of determination between \code{xvar} and
#' \code{yvar}, and can be negative. See \url{https://en.wikipedia.org/wiki/Coefficient_of_determination} for more details.
#' If \code{xvar} is the output of a model to predict \code{yvar}, then the identity R-squared, not the lm R-squared,
#' is the correct measure.
#'
#' If \code{smoothmethod} is neither 'lm' or 'identity' then \code{estimate_sig} is ignored.
#'
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param yvar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param smoothmethod (optional) one of 'auto', 'loess', 'gam', 'lm', 'identity', or 'none'.
#' @param estimate_sig logical if TRUE and smoothmethod is 'identity' or 'lm', report goodness of fit and significance of relation.
#' @param minimal_labels logical drop some annotations
#' @param binwidth_x  numeric binwidth for x histogram
#' @param binwidth_y  numeric binwidth for y histogram
#' @param adjust_x  numeric adjust x density plot
#' @param adjust_y  numeric adjust y density plot
#' @param point_alpha numeric opaqueness of the plot points
#' @param contour logical if TRUE add a 2d contour plot
#' @param point_color color for scatter plots
#' @param hist_color fill color for marginal histograms
#' @param smoothing_color color for smoothing line
#' @param density_color color for marginal density plots
#' @param contour_color color for contour plots
#' @return plot grid
#'
#' @seealso \code{\link{ScatterHistC}}
#'
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,y=y)
#' WVPlots::ScatterHist(frm, "x", "y",
#'   title= "Example Fit",
#'   smoothmethod = "gam",
#'   contour = TRUE)
#'
#' # Same plot with custom colors
#' WVPlots::ScatterHist(frm, "x", "y",
#'   title= "Example Fit",
#'   smoothmethod = "gam",
#'   contour = TRUE,
#'   point_color = "#006d2c", # dark green
#'   hist_color = "#6baed6", # medium blue
#'   smoothing_color = "#54278f", # dark purple
#'   density_color = "#08519c", # darker blue
#'   contour_color = "#9e9ac8") # lighter purple
#' @export
#'
ScatterHist = function(frame, xvar, yvar, title, ...,
                       smoothmethod="lm", # only works for 'auto', 'loess', 'gam', 'lm', 'none' and 'identity'
                       estimate_sig=FALSE,
                       minimal_labels = TRUE,
                       binwidth_x = NULL,
                       binwidth_y = NULL,
                       adjust_x = 1,
                       adjust_y = 1,
                       point_alpha = 0.5,
                       contour = FALSE,
                       point_color = "black",
                       hist_color = "gray",
                       smoothing_color = "blue",
                       density_color = "blue",
                       contour_color = "blue") {
  if((!requireNamespace("grid", quietly = TRUE)) ||
     (!requireNamespace("gridExtra", quietly = TRUE))) {
    return("WVPlots::ScatterHist requires the grid and gridExtra packages be installed")
  }
  frame <- as.data.frame(frame)
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar, yvar = yvar),
                        title = title,
                        funname = "WVPlots::ScatterHist")
  if(!(smoothmethod %in% c('auto','loess','gam','lm', 'none', 'identity'))) {
    stop("smoothed method must be one of 'auto','lm','none', or 'identity'")
  }
  frame <- frame[, c(xvar,yvar), drop=FALSE]
  frame <- frame[complete.cases(frame), , drop=FALSE]
  ..density.. <- NULL # used as a symbol, declare not an unbound variable

  if(estimate_sig && (smoothmethod %in% c("identity", "lm"))) {
    if(smoothmethod=='identity') {
      sig <- sigr::wrapFTest(frame, xvar, yvar)
      title <- paste0(title, "\nidentity relation: ", render(sig, format = "ascii"))
    }
    if(smoothmethod=='lm') {
      f <- paste(xvar, "~", yvar)
      lmm <- lm(as.formula(f), data = frame)
      sig <- sigr::wrapFTest(lmm)
      title <- paste0(title, "\nlinear relation: ", render(sig, format = "ascii"))
    }
  }

  # placeholder plot - prints nothing at all
  empty =  ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(c(0,1), c(0,1)), colour = "white") +
    ggplot2::theme(plot.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(1, 1, 0, 0), "lines"))

  # if we are showing a linear fit, print the fit's parameters
  gSmooth = NULL
  if(smoothmethod != 'none') {
    if(smoothmethod=='identity') {
      gSmooth = ggplot2::geom_abline(slope=1,linetype=2,color=smoothing_color)
    } else {
      gSmooth = ggplot2::geom_smooth(method=smoothmethod, color=smoothing_color, se=FALSE, formula = y ~ x)
    }
  }

  # scatterplot of x and y
  plot_center = ggplot2::ggplot(frame, ggplot2::aes_string(x=xvar,y=yvar)) +
    ggplot2::geom_point(color=point_color, alpha=point_alpha) +
    ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "lines"))
  if(!is.null(gSmooth)) {
    plot_center = plot_center + gSmooth
  }
  if(contour) {
    plot_center = plot_center + ggplot2::geom_density2d(color=contour_color)
  }

  # get the data range, to help align plots
  x = frame[[xvar]]
  y = frame[[yvar]]
  xlims =  c(min(x), max(x))
  ylims =  c(min(y), max(y))

  #  print(xlims)
  # print(ggplot_build(plot_center)$panel$ranges[[1]]$x.range)

  plot_center = plot_center +
    ggplot2::coord_cartesian(xlim=xlims, ylim=ylims) +
    ggplot2::scale_x_continuous(expand = c(0,0))

  # print(ggplot_build(plot_center)$panel$ranges[[1]]$x.range)

  # marginal density of x - plot on top
  #
  # 0,0,0,0 -- title squooshed down
  # 1,0,0,0 -- title has space
  # 0,1,0,0 -- right side is shorter
  # 0,0,1,0 -- bottom gap bigger
  # 0,0,0,1 -- left side is shorter
  #
  plot_top <- ggplot2::ggplot(frame, ggplot2::aes_string(x=xvar)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), fill=hist_color,
                            color="white", binwidth=binwidth_x, bins=30) +
    ggplot2::geom_line(stat='density',color=density_color, adjust=adjust_x) +
    ggplot2::coord_cartesian(xlim=xlims) +
    ggplot2::scale_x_continuous(expand = c(0,0))
  if(minimal_labels) {
    plot_top = plot_top +
      ggplot2::theme(legend.position = "none",
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     plot.margin = grid::unit(c(1, 0, 0, 0), "lines"))
  } else {
    plot_top = plot_top +
      ggplot2::theme(plot.margin = grid::unit(c(1, 0, 0, 0), "lines"))
  }



  # estimate size
  yPadFn <- designYLabelPadFunction(plot_center +
                                      ggplot2::scale_y_continuous(limits=ylims, expand = c(0,0)),
                                    plot_top)
  # adjust using estimate
  plot_center <- plot_center +
    ggplot2::scale_y_continuous(limits=ylims, label=yPadFn, expand = c(0,0))
  plot_top <- plot_top + ggplot2::scale_y_continuous(label=yPadFn)
  # # From: http://stackoverflow.com/questions/27374409/get-tick-break-positions-in-ggplot
  # cRanges <-  ggplot_build(plot_center)$layout$panel_ranges[[1]]
  # yBreaks <- cRanges$y.major_source
  # xBreaks <- cRanges$x.major_source

  # marginal density of y - plot on the right
  plot_right <- ggplot2::ggplot(frame, ggplot2::aes_string(x=yvar)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), fill=hist_color,
                            color="white", binwidth=binwidth_y, bins=30) +
    ggplot2::geom_line(stat='density',color=density_color, adjust=adjust_y) +
    # ggplot2::coord_cartesian(xlim=ylims) + # causes a warning with ggplot2 2.2.1.9000
    ggplot2::scale_x_continuous(expand = c(0,0)) + # , breaks= yBreaks) +
    ggplot2::coord_flip(xlim=ylims, expand = 0) # see: https://github.com/tidyverse/ggplot2/issues/2013
  if(minimal_labels) {
    plot_right = plot_right +
      ggplot2::theme(legend.position = "none",
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     plot.margin = grid::unit(c(0, 1, 0, 0), "lines"))
  } else {
    plot_right = plot_right +
      ggplot2::theme(plot.margin = grid::unit(c(0, 1, 0, 0), "lines"))
  }

  # arrange the plots together, with appropriate height and width
  # for each row and column

  gridExtra::grid.arrange(plot_top, empty, plot_center, plot_right,
                          top=grid::textGrob(title),
                          ncol = 2, nrow = 2, widths = c(4,1), heights = c(1, 4))
}
