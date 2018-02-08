

#' Plot a trend on log-log paper.
#'
#' This plot is intended for ploting functions that are observed costs
#' or durations as a function of problem size.  In this case we expect
#' the ideal or expected cost function to be non-decreasing.
#' Any negative trends are assumed to arise from the noise model.
#' The graph is specialized to compare non-decreasing linear and
#' non-decreasing quadratic growth.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param yvar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(5326)
#' frm = data.frame(x = 1:20)
#' frm$y <- 5 + frm$x + 0.2 * frm$x * frm$x + 0.1*abs(rnorm(nrow(frm)))
#' WVPlots::LogLogPlot(frm, "x", "y", title="Example Trend")
#'
#' @export
LogLogPlot <- function(frame, xvar, yvar, title, ...) {
  XVAR <- NULL # don't look like an unbound variable
  YVAR <- NULL # don't look like an unbound variable
  linear_trend <- NULL # don't look like an unbound variable
  quadratic_trend <- NULL # don't look like an unbound variable
  wrapr::let(
    c(XVAR = xvar, YVAR = yvar),
    {
      mfull <- lm(YVAR ~ XVAR + I(XVAR*XVAR), data = frame)
      sfull <- summary(mfull)
      cq <- sfull$coefficients[3, 'Estimate']
      pq <- sfull$coefficients[3, 'Pr(>|t|)']
      if(cq<=0) {
        pq = NA
      }
      ps <- sigr::render(sigr::wrapSignificance(pq),
                         format = "ascii")
      mlinear <- lm(YVAR ~ 0 + XVAR, data = frame)
      frame$linear_trend <- predict(mlinear, newdata = frame)
      mquad <- lm(YVAR ~ 0 + I(XVAR*XVAR), data = frame)
      frame$quadratic_trend <- predict(mquad, newdata = frame)

      qs <- as.numeric(quantile(frame$YVAR, probs=c(0.25,0.75)))
      mult <- sqrt(qs[[2]]/qs[[1]])
      if(is.na(mult)||is.nan(mult)||is.infinite(mult)||mult<=0) {
        mult = 2.0
      }

      plt <- ggplot2::ggplot(frame,
                      ggplot2::aes(x = XVAR, y = YVAR)) +
        ggplot2::geom_smooth(se = FALSE) +
        ggplot2::geom_point() +
        ggplot2::geom_line(ggplot2::aes(y = linear_trend),
                  linetype = 2, color = "green", alpha=0.5) +
        ggplot2::geom_line(ggplot2::aes(y = (1/mult)*linear_trend),
                  linetype = 2, color = "green", alpha=0.5) +
        ggplot2::geom_line(ggplot2::aes(y = mult*linear_trend),
                  linetype = 2, color = "green", alpha=0.5) +
        ggplot2::geom_line(ggplot2::aes(y = quadratic_trend),
                  linetype = 2, color = "red", alpha=0.5) +
        ggplot2::geom_line(ggplot2::aes(y = (1/mult)*quadratic_trend),
                  linetype = 2, color = "red", alpha=0.5) +
        ggplot2::geom_line(ggplot2::aes(y = mult*quadratic_trend),
                  linetype = 2, color = "red", alpha=0.5) +
        ggplot2::scale_x_log10() +
        ggplot2::scale_y_log10() +
        ggplot2::ggtitle(title,
                         subtitle = paste0(
                           "linear and quadtratic growth rates shown as dashed lines",
                           "\nsignificance of positive quadratic trend component: ",
                                         ps))
      plt
    })
}
