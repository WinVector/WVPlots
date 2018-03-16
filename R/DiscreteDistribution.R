
is_integral = function(x) {
  if(!is.numeric(x)) return(FALSE)
  if(is.integer(x)) return(TRUE)

  xint = as.integer(x)
  return(sum(xint != x)==0)
}

#' Plot distribution of a single continuous variable.
#'
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
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
  checkArgs(frame=frm,xvar=xvar,yvar=xvar,title=title,...)
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
