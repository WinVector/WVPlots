
#' Plot reciever operating characteristic plot.
#'
#' @param frame data frame to get values from
#' @param xvar name of the indepement (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unamed argument, added to force named binding of later arguments.
#'
#' @export
ROCPlot <- function(frame, xvar, truthVar,title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  outcol <- frame[[truthVar]]
  predcol <- frame[[xvar]]
  pred <- ROCR::prediction(predcol,outcol)
  perf <-  ROCR::performance(pred,'tpr','fpr')
  auc <- as.numeric(ROCR::performance(pred,'auc')@y.values)
  pf <- data.frame(
    FalsePositiveRate=perf@x.values[[1]],
    TruePositiveRate=perf@y.values[[1]])
  palletName = "Dark2"
  plot= ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=pf,
                         ggplot2::aes(x=FalsePositiveRate,ymax=TruePositiveRate,ymin=0),
                alpha=0.3) +
    ggplot2::geom_point(data=pf,
                        ggplot2::aes(x=FalsePositiveRate,y=TruePositiveRate)) +
    ggplot2::geom_line(data=pf,
                       ggplot2::aes(x=FalsePositiveRate,y=TruePositiveRate)) +
    ggplot2::geom_line(ggplot2::aes(x=c(0,1),y=c(0,1))) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(paste(title,'\n',
                  truthVar, '~', xvar, '\n',
                  'AUC:',format(auc,digits=2)))
  plot
}
