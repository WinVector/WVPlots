

#' Plot a scatter box plot.
#'
#' Plot a boxplot with the data points superimposed.
#'
#' \code{xvar} is a discrete variable and \code{yvar} is a continuous variable.
#' If \code{frm[[xvar]]} is an integer column, it will be converted to a factor.
#' This means that additional layers that rely on continuous x scales (like geom_smooth) won't work.
#'
#' @param frm data frame to get values from
#' @param xvar name of the independent column in frame; assumed discrete
#' @param yvar name of the continuous column in frame
#' @param title plot title
#' @param ... (doesn't take additional arguments, used to force later arguments by name)
#' @param pt_alpha transparency of points in scatter plot
#'
#' @seealso \code{\link{ScatterBoxPlotH}}
#'
#' @examples
#'
#' classes = c("a", "b", "c")
#' means = c(2, 4, 3)
#' names(means) = classes
#' label = sample(classes, size=1000, replace=TRUE)
#' meas = means[label] + rnorm(1000)
#' frm2 = data.frame(label=label,
#'                   meas = meas)
#' WVPlots::ScatterBoxPlot(frm2, "label", "meas", pt_alpha=0.2, title="Example Scatter/Box plot")
#'
#' @export
ScatterBoxPlot = function(frm, xvar, yvar, title, ...,
                          pt_alpha=0.3) {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
                        frame = frm,
                        name_var_list = list(xvar = xvar, yvar = yvar),
                        title = title,
                        funname = "WVPlots::ScatterBoxPlot")
  if(!isDiscrete(frm[[xvar]])) {
    stop(paste(xvar, "should be discrete (factor, character, integer, or logical)"))
  }
  if(is.integer(frm[[xvar]])) {
    frm[[xvar]] = as.factor(frm[[xvar]])
  }

  ggplot2::ggplot(frm, ggplot2::aes_string(x=xvar, y=yvar)) +
    ggplot2::geom_boxplot(outlier.size=0, fill="lightgray") +
    ggplot2::geom_point(alpha=pt_alpha, position=ggplot2::position_jitter(width=0.1,height=0)) +
    ggplot2::ggtitle(title)
}


#' Plot a scatter box plot in horizontal mode.
#'
#' Plot a boxplot with the data points superimposed. Box plots are aligned horizontally.
#'
#' \code{xvar} is a continuous variable and \code{yvar} is a discrete variable.
#' If frm[[yvar]] is an integer column, it will be converted to a factor.
#' This means that additional layers that rely on continuous y scales won't work.
#'
#'
#' @param frm data frame to get values from
#' @param xvar name of the continuous column in frame
#' @param yvar name of the independent column in frame; assumed discrete
#' @param title plot title
#' @param ... (doesn't take additional arguments, used to force later arguments by name)
#' @param pt_alpha transparency of points in scatter plot
#'
#' @seealso \code{\link{ScatterBoxPlot}}
#'
#' @examples
#'
#' classes = c("a", "b", "c")
#' means = c(2, 4, 3)
#' names(means) = classes
#' label = sample(classes, size=1000, replace=TRUE)
#' meas = means[label] + rnorm(1000)
#' frm2 = data.frame(label=label,
#'                   meas = meas)
#' WVPlots::ScatterBoxPlotH(frm2, "meas", "label",  pt_alpha=0.2, title="Example Scatter/Box plot")
#'
#'
#' @export
ScatterBoxPlotH = function(frm, xvar, yvar, title,
                           ...,
                           pt_alpha=0.3) {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
                        frame = frm,
                        name_var_list = list(xvar = xvar, yvar = yvar),
                        title = title,
                        funname = "WVPlots::ScatterBoxPlotH")
  ScatterBoxPlot(frm, yvar, xvar, title=title, ...,
                 pt_alpha=pt_alpha) + ggplot2::coord_flip()
}
