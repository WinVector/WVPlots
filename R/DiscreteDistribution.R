
is_integral = function(x) {
  if(!is.numeric(x)) return(FALSE)
  if(is.integer(x)) return(TRUE)

  xint = as.integer(x)
  return(sum(xint != x)==0)
}

#' Plot distribution of a single discrete numerical variable.
#'
#' Similar to calling \code{ClevelandDotPlot} with \code{sort = 0} on a numerical x variable that
#' takes on a discrete set of values.
#'
#' @param frm data frame to get values from
#' @param xvar numeric: name of the variable whose distribution is to be plotted
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param stem if TRUE add whisker/stems to plot
#' @param color color of points and stems
#' @examples
#'
#' frmx = data.frame(x = rbinom(1000, 20, 0.5))
#' WVPlots::DiscreteDistribution(frmx, "x","Discrete example")
#'
#' @export
DiscreteDistribution = function(frm, xvar, title, ...,
                                stem=TRUE, color='black') {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
                        frame = frm,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::DiscreteDistribution")
  if(!is_integral(frm[[xvar]])) {
    stop(paste("Column", xvar, "must have integer values"))
  }

  frm$unit=1

  if(stem) {
    geom="pointrange"
  } else {
    geom="point"
  }
  ggplot2::ggplot(data = frm, mapping = ggplot2::aes(!!!simulate_aes_string(x = xvar, y = "unit"))) +
    ggplot2::stat_summary(fun=sum, fun.max=sum, fun.min=function(x){0}, geom=geom, color=color) +
    ggplot2::ggtitle(title)
}
