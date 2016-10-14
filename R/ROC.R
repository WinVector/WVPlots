

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
#' frm = data.frame(x=x,y=y,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' frm$absY <- abs(frm$y)
#' frm$posY = frm$y > 0
#' frm$costX = 1
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
                  'AUC: ',sprintf("%.2g",auc))) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  plot
}
