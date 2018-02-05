

#' Plot a trend on log-log paper.
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
  wrapr::let(
    c(XVAR = xvar, YVAR = yvar),
    {
      mfull <- lm(YVAR ~ XVAR + I(XVAR*XVAR), data = frame)
      sfull <- summary(mfull)
      cq <- sfull$coefficients[1, 'Pr(>|t|)']
      pq <- sfull$coefficients[3, 'Pr(>|t|)']
      if(cq<=0) {
        pq = NA
      }
      ps <- sigr::render(sigr::wrapSignificance(pq))
      mlinear <- lm(YVAR ~ 0 + XVAR, data = frame)
      frame$linear_trend <- predict(mlinear, newdata = frame)
      mquad <- lm(YVAR ~ 0 + I(XVAR*XVAR), data = frame)
      frame$quadratic_trend <- predict(mquad, newdata = frame)

      qs <- as.numeric(quantile(frame$YVAR, probs=c(0.25,0.75)))
      mult <- sqrt(qs[[2]]/qs[[1]])
      if(is.na(mult)||is.nan(mult)||is.infinite(mult)||mult<=0) {
        mult = 2.0
      }

      # mini plots to get default ticks
      # working around: https://github.com/tidyverse/ggplot2/issues/2439
      linplt <- ggplot2::ggplot(frame,
                                ggplot2::aes(x = XVAR, y = YVAR)) +
        ggplot2::geom_point()
      linybrks <- as.numeric(getYLabs(linplt))
      linxbrks <- as.numeric(getXLabs(linplt))
      logplt <- linplt +
        ggplot2::scale_y_log10() +
        ggplot2::scale_x_log10()
      logybrks <- as.numeric(getYLabs(logplt))
      logxbrks <- as.numeric(getXLabs(logplt))
      if(length(logybrks)<2) {
        logybrks <- linybrks
      }
      if(length(logxbrks)<2) {
        logxbrks <- linxbrks
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
        ggplot2::scale_x_log10(breaks = logxbrks) +
        ggplot2::scale_y_log10(breaks = logybrks) +
        ggplot2::ggtitle(title,
                         subtitle = paste0("significance of positive quadratic trend component: ",
                                         ps))
      plt
    })
}
