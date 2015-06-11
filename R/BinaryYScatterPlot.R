
#' Plot a scatter plot of a binary variable.  xvar is the continuous independent variable and yvar is the dependent binary variable
#' @param frame data frame to get values from
#' @param xvar name of the independent column in frame
#' @param yvar name of the dependent (output or result to be modeled) column in frame
#' @param se if TRUE, add error bars (defaults to FALSE). Ignored if useGLM is TRUE
#' @param use_glm if TRUE, "smooths" with a one-variable logistic regression (defaults to FALSE)
#'
#' @export
BinaryYScatterPlot = function(frame, xvar, yvar, se=FALSE, use_glm=FALSE, title='') {
  checkArgs(frame,xvar,yvar)

  frame[[yvar]] = as.numeric(frame[[yvar]])
  if(length(unique(frame[[yvar]])) != 2) stop(paste("outcome column", yvar, "not a binary variable"))

  if(use_glm) {
    model = glm(frame[[yvar]] == max(frame[[yvar]]) ~ frame[[xvar]], family=binomial(link="logit"))
    frame$smooth = predict(model, type="response")
    ggplot(frame, aes_string(x=xvar)) +
      geom_point(aes_string(y=yvar), position=position_jitter(height=0.01), alpha=0.5) +
      geom_line(aes(y=smooth), color='blue') +
      ggtitle(title)
  } else {
    ggplot(frame, aes_string(x=xvar, y=yvar)) +
      geom_point(position=position_jitter(height=0.01), alpha=0.5) +
      geom_smooth(method="gam", formula=y~s(x), se=se) + ggtitle(title)
  }
}
