isDiscrete = function(x) {
  return (is.factor(x) || is.character(x) || is.integer(x))
}


#' Plot a scatter plot.  xvar is the independent variable (input or model) and yvar is the dependent variable
#' @param frame data frame to get values from
#' @param xvar name of the independent column in frame; assumed discrete.
#' if frm[[xvar]] is an interger column, it will be converted to a factor. This means that
#' additional layers that rely on continuous x scales (like geom_smooth) won't work
#' @param yvar name of the dependent column in frame
#' @param pt_alpha transparency of points in scatter plot
#' @param title plot title
#'
#' @export
ScatterBoxPlot = function(frm, xvar, yvar, pt_alpha=0.3, title='') {
  checkArgs(frm,xvar,yvar)
  if(!isDiscrete(frm[[xvar]])) {
    stop(paste(xvar, "should be discrete (factor, character, or integer)"))
  }
  if(is.integer(frm[[xvar]])) {
    frm[[xvar]] = as.factor(frm[[xvar]])
  }

  ggplot(frm, aes_string(x=xvar, y=yvar)) +
    geom_boxplot(outlier.size=0, fill="lightgray") +
    geom_point(alpha=pt_alpha, position=position_jitter(width=0.1)) +
    ggtitle(title)

}
