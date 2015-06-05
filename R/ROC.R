
deviance <- function(truth,pred,epsilon=1.e-6) {
  pred = pmax(pred, epsilon)
  pred = pmin(pred, 1-epsilon)
  S = 0.0 # assumed log-likelihood of saturated model
  -2*(sum(ifelse(truth,log(pred),log(1-pred)))-S)
}

plotROC <- function(frame, xvar, truthVar,title='ROC plot') {
  outcol <- frame[[truthVar]]
  predcol <- frame[[xvar]]
  pred <- prediction(predcol,outcol)
  perf <- performance(pred,'tpr','fpr')
  auc <- as.numeric(performance(pred,'auc')@y.values)
  pf <- data.frame(
    FalsePositiveRate=perf@x.values[[1]],
    TruePositiveRate=perf@y.values[[1]])
  plot=ggplot() +
    geom_ribbon(data=pf,aes(x=FalsePositiveRate,ymax=TruePositiveRate,ymin=0),
                fill='blue',alpha=0.3) +
    geom_point(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
    geom_line(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
    geom_line(aes(x=c(0,1),y=c(0,1))) + coord_fixed() +
    ggtitle(paste(title,'\nAUC:',format(auc,digits=2)))
  plot
}
