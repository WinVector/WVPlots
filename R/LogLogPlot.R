

#' Log-log plot
#'
#' Plot a trend on log-log paper.
#'
#' This plot is intended for plotting functions that are observed costs
#' or durations as a function of problem size.  In this case we expect
#' the ideal or expected cost function to be non-decreasing.
#' Any negative trends are assumed to arise from the noise model.
#' The graph is specialized to compare non-decreasing linear and
#' non-decreasing quadratic growth.
#'
#' Some care must be taken in drawing conclusions from log-log plots,
#' as the transform is fairly violent.  Please see:
#' "(Mar's Law) Everything is linear if plotted log-log with a fat magic marker"
#' (from Akin's Laws of Spacecraft Design \url{http://spacecraft.ssl.umd.edu/akins_laws.html}),
#' and "So You Think You Have a Power Law" \url{http://bactra.org/weblog/491.html}.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param yvar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param use_coord_trans logical if TRUE, use coord_trans instead of \code{coord_trans(x = "log10", y = "log10")} instead of \code{scale_x_log10() + scale_y_log10()} (useful when there is not enough range to show ticks).
#' @param point_color the color of the data points
#' @param linear_color the color of the linear growth lines
#' @param quadratic_color the color of the quadratic growth lines
#' @param smoothing_color the color of the smoothing line through the data
#' @examples
#'
#' set.seed(5326)
#' frm = data.frame(x = 1:20)
#' frm$y <- 5 + frm$x + 0.2 * frm$x * frm$x + 0.1*abs(rnorm(nrow(frm)))
#' WVPlots::LogLogPlot(frm, "x", "y", title="Example Trend")
#'
#' @export
LogLogPlot <- function(frame, xvar, yvar, title,
                       ...,
                       use_coord_trans = FALSE,
                       point_color = 'black',
                       linear_color = '#018571',
                       quadratic_color = '#a6611a',
                       smoothing_color = 'blue') {
  frame <- as.data.frame(frame)
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar, yvar = yvar),
                        title = title,
                        funname = "WVPlots::LogLogPlot")
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
      qs <- as.numeric(quantile(frame$YVAR, probs=c(0.25,0.75)))
      mult <- sqrt(qs[[2]]/qs[[1]])
      if(is.na(mult)||is.nan(mult)||is.infinite(mult)||mult<=0) {
        mult = 2.0
      }
      mlinear <- lm(YVAR ~ 0 + XVAR, data = frame)
      mquad <- lm(YVAR ~ 0 + I(XVAR*XVAR), data = frame)
      # more eval points to work around visible kinks in coord_trans()
      # lines
      mn <- min(frame$XVAR, na.rm = TRUE)
      mx <- max(frame$XVAR, na.rm = TRUE)
      tframe <- data.frame(XVAR = sort(unique(c(
        frame$XVAR,
        seq(mn, mx, length.out = 201),
        exp(seq(log(mn), log(mx), length.out = 201))))))
      tframe$linear_trend <- predict(mlinear, newdata = tframe)
      tframe$quadratic_trend <- predict(mquad, newdata = tframe)

      plt <- ggplot2::ggplot(data = frame,
                             ggplot2::aes(x = XVAR, y = YVAR)) +
        ggplot2::geom_smooth(color=smoothing_color, se = FALSE) +
        ggplot2::geom_point(color=point_color) +
        ggplot2::geom_line(data = tframe,
                           ggplot2::aes(y = linear_trend),
                           linetype = 2, color = linear_color, alpha=0.5) +
        ggplot2::geom_line(data = tframe,
                           ggplot2::aes(y = (1/mult)*linear_trend),
                           linetype = 2, color = linear_color, alpha=0.5) +
        ggplot2::geom_line(data = tframe,
                           ggplot2::aes(y = mult*linear_trend),
                           linetype = 2, color = linear_color, alpha=0.5) +
        ggplot2::geom_line(data = tframe,
                           ggplot2::aes(y = quadratic_trend),
                           linetype = 2, color = quadratic_color, alpha=0.5) +
        ggplot2::geom_line(data = tframe,
                           ggplot2::aes(y = (1/mult)*quadratic_trend),
                           linetype = 2, color = quadratic_color, alpha=0.5) +
        ggplot2::geom_line(data = tframe,
                           ggplot2::aes(y = mult*quadratic_trend),
                           linetype = 2, color = quadratic_color, alpha=0.5) +
        ggplot2::ggtitle(title,
                         subtitle = paste0(
                           "linear and quadtratic growth rates shown as dashed lines",
                           "\nsignificance of positive quadratic trend component: ",
                           ps))
      if(use_coord_trans) {
        plt <- plt +
          ggplot2::coord_trans(x = "log10", y = "log10")
      } else {
        plt <- plt +
          ggplot2::scale_x_log10() +
          ggplot2::scale_y_log10()
      }
      plt
    })
}
