
#' Plot Precision-Recall plot
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,y=y,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' frm$absY <- abs(frm$y)
#' frm$posY = frm$y > 0
#' frm$costX = 1
#' WVPlots::PRPlot(frm, "x", "yC", title="Example Precision-Recall plot")
#'
#' @export
PRPlot <- function(frame, xvar, truthVar,title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  outcol <- frame[[truthVar]]
  if(length(unique(outcol))!=2) {
    return(NULL)
  }
  predcol <- frame[[xvar]]
  pred <- ROCR::prediction(predcol,outcol)
  perf <-  ROCR::performance(pred,'prec','rec')

  pf <- data.frame(
    Recall=perf@x.values[[1]],
    Precision=perf@y.values[[1]])

  palletName = "Dark2"
  plot= ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=pf,
                         ggplot2::aes(x=Recall,ymax=Precision,ymin=0),
                         alpha=0.3) +
    ggplot2::geom_point(data=pf,
                        ggplot2::aes(x=Recall,y=Precision)) +
    ggplot2::geom_line(data=pf,
                       ggplot2::aes(x=Recall,y=Precision)) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(paste(title,'\n',
                           truthVar, '~', xvar))
  plot
}
