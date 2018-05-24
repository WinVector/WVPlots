
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
#' @examples
#'
#' frmx = data.frame(x = rbinom(1000, 20, 0.5))
#' WVPlots::DiscreteDistribution(frmx, "x","Discrete example")
#'
#' @export
DiscreteDistribution = function(frm, xvar, title, ...,
                                stem=TRUE) {
  frm <- check_frame_args_list(...,
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
  ggplot2::ggplot(frm, ggplot2::aes_string(xvar, "unit")) +
    ggplot2::stat_summary(fun.y=sum, fun.ymax=sum, fun.ymin=function(x){0}, geom=geom) +
    ggplot2::ggtitle(title)
}
