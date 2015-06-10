
#' Plot a scatter plot of a binary variable.  xvar is the continuous independent variable and yvar is the dependent binary variable
#' @param frame data frame to get values from
#' @param xvar name of the independent column in frame
#' @param yvar name of the dependent (output or result to be modeled) column in frame
#'
#' @export
BinaryYScatterPlot = function(frame, xvar, yvar, title='') {
  checkArgs(frame,xvar,yvar)

  frame[[yvar]] = as.numeric(frame[[yvar]])
  if(length(unique(frame[[yvar]])) != 2) stop(paste("outcome column", yvar, "not a binary variable"))

  ggplot(frame, aes_string(x=xvar, y=yvar)) +
    geom_point(position=position_jitter(height=0.01), alpha=0.5) + geom_smooth() + ggtitle(title)


}
