

#' @export
ROCPlot <- function(frame, xvar, truthVar,title='ROC plot') {
  checkArgs(frame,xvar,truthVar)
  outcol <- frame[[truthVar]]
  predcol <- frame[[xvar]]
  pred <- prediction(predcol,outcol)
  perf <- performance(pred,'tpr','fpr')
  auc <- as.numeric(performance(pred,'auc')@y.values)
  pf <- data.frame(
    FalsePositiveRate=perf@x.values[[1]],
    TruePositiveRate=perf@y.values[[1]])
  palletName = "Dark2"
  plot=ggplot() +
    geom_ribbon(data=pf,aes(x=FalsePositiveRate,ymax=TruePositiveRate,ymin=0),
                alpha=0.3) +
    geom_point(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
    geom_line(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
    geom_line(aes(x=c(0,1),y=c(0,1))) + coord_fixed() +
    scale_fill_brewer(palette=palletName) + scale_color_brewer(palette=palletName) +
    ggtitle(paste(title,'\n',
                  truthVar, '~', xvar, '\n',
                  'AUC:',format(auc,digits=2)))
  plot
}
