
#' @importFrom sigr formatAUC
NULL

#' calculate AUC.
#'
#' Based on:
#'  http://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
#'
#'  See also https://github.com/WinVector/sigr
#'
#' @param modelPredictions numeric predictions (not empty)
#' @param yValues logical truth (not empty, same lenght as model predictions)
#' @return line graph, point graph, and area under curve
#'
calcAUC <- function(modelPredictions,yValues) {
  ord <- order(modelPredictions, decreasing=TRUE)
  yValues <- yValues[ord]
  modelPredictions <- modelPredictions[ord]
  # FPR is the x-axis, TPR the y.
  x <- cumsum(!yValues)/max(1,sum(!yValues)) # FPR = x-axis
  y <- cumsum(yValues)/max(1,sum(yValues))   # TPR = y-axis
  pointGraph <- data.frame(FalsePositiveRate=x,TruePositiveRate=y,
                           stringsAsFactors = FALSE)
  # each point should be fully after a bunch of points or fully before a
  # decision level. remove dups to achieve this.
  dup <- c(modelPredictions[-1]>=modelPredictions[-length(modelPredictions)],
           FALSE)
  # And add in ideal endpoints just in case (redundancy here is not a problem).
  x <- c(0,x[!dup],1)
  y <- c(0,y[!dup],1)
  lineGraph <- data.frame(FalsePositiveRate=x,TruePositiveRate=y,
                          stringsAsFactors = FALSE)
  # sum areas of segments (triangle topped vertical rectangles)
  n <- length(y)
  area <- sum( ((y[-1]+y[-n])/2) * (x[-1]-x[-n]) )
  list(lineGraph=lineGraph,pointGraph=pointGraph,area=area)
}

#' Plot receiver operating characteristic plot.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' WVPlots::ROCPlot(frm, "x", "yC", TRUE, title="Example ROC plot")
#'
#' @export
ROCPlot <- function(frame, xvar, truthVar, truthTarget, title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  outcol <- frame[[truthVar]]==truthTarget
  if(length(unique(outcol))!=2) {
    return(NULL)
  }
  predcol <- frame[[xvar]]
  rocList <- calcAUC(predcol,outcol)
  aucsig <- sigr::formatAUC(data.frame(pred=predcol,outcome=outcol,
                                       stringsAsFactors =FALSE),
                            'pred','outcome',TRUE,pLargeCutoff=1,
                            nrep=100,format = 'ascii')
  auc <- rocList$area
  palletName = "Dark2"
  plot= ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=rocList$lineGraph,
                         ggplot2::aes(x=FalsePositiveRate,ymax=TruePositiveRate,ymin=0),
                         alpha=0.2,color=NA) +
    ggplot2::geom_point(data=rocList$pointGraph,
                        ggplot2::aes(x=FalsePositiveRate,y=TruePositiveRate),
                        color='darkblue',alpha=0.5) +
    ggplot2::geom_line(data=rocList$lineGraph,
                       ggplot2::aes(x=FalsePositiveRate,y=TruePositiveRate),
                       color='darkblue') +
    ggplot2::geom_abline(slope=1,intercept=0) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(paste0(title,'\n',
                            truthVar, '==', truthTarget, ' ~ ', xvar, '\n',
                            'AUC=',aucsig$scoreString,' ',aucsig$pString)) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  plot
}



#' Plot two receiver operating characteristic plots.
#'
#' @param frame data frame to get values from
#' @param xvar1 name of the first independent (input or model) column in frame
#' @param xvar2 name of the second independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' x1 = rnorm(50)
#' x2 = rnorm(length(x1))
#' y = 0.2*x2^2 + 0.5*x2 + x1 + rnorm(length(x1))
#' frm = data.frame(x1=x1,x2=x2,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' # WVPlots::ROCPlot(frm, "x1", "yC", TRUE, title="Example ROC plot")
#' # WVPlots::ROCPlot(frm, "x2", "yC", TRUE, title="Example ROC plot")
#' WVPlots::ROCPlotPair(frm, "x1", "x2", "yC", TRUE, title="Example ROC pair plot")
#'
#' @export
ROCPlotPair <- function(frame, xvar1, xvar2, truthVar, truthTarget, title,...) {
  checkArgs(frame=frame,xvar=xvar1,yvar=truthVar,title=title,...)
  checkArgs(frame=frame,xvar=xvar2,yvar=truthVar,title=title,...)
  outcol <- frame[[truthVar]]==truthTarget
  if(length(unique(outcol))!=2) {
    return(NULL)
  }
  rocList1 <- calcAUC(frame[[xvar1]],outcol)
  rocList2 <- calcAUC(frame[[xvar2]],outcol)
  aucsig <- sigr::formatAUCpair(frame,xvar1,xvar2,truthVar,truthTarget,
    pLargeCutoff=1,
    nrep=100,format = 'ascii')
  nm1 <- paste0(xvar1,', AUC=',aucsig$scoreString1)
  nm2 <- paste0(xvar2,', AUC=',aucsig$scoreString2)
  rocList1$pointGraph$model <- nm1
  rocList1$lineGraph$model <- nm1
  rocList2$pointGraph$model <- nm2
  rocList2$lineGraph$model <- nm2
  pointGraph <- rbind(rocList1$pointGraph,rocList2$pointGraph)
  lineGraph <- rbind(rocList1$lineGraph,rocList2$lineGraph)
  palletName = "Dark2"
  plot= ggplot2::ggplot() +
    ggplot2::geom_point(data=pointGraph,
                        ggplot2::aes(x=FalsePositiveRate,y=TruePositiveRate,
                                     color=model,shape=model),
                        alpha=0.5) +
    ggplot2::geom_line(data=lineGraph,
                       ggplot2::aes(x=FalsePositiveRate,y=TruePositiveRate,
                                    color=model,linetype=model)) +
    ggplot2::geom_abline(slope=1,intercept=0,color='gray') +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(paste0(title,'\n',
                  truthVar, '==', truthTarget, ' ~ model\n',
                  'alternative hypothesis: AUC(',xvar1,')>AUC(pooled)\n',
                  aucsig$pString)) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  plot
}

