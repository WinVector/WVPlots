

#' calculate precision/recall curve.
#'
#' Based on:
#'  http://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
#'
#'  See also https://github.com/WinVector/sigr
#'
#' @param modelPredictions numeric predictions (not empty)
#' @param yValues logical truth (not empty, same lenght as model predictions)
#' @return line graph, and point graph
#'
calcPR <- function(modelPredictions,yValues) {
  prevalence = mean(yValues)
  ord <- order(modelPredictions, decreasing=TRUE)
  yValues <- yValues[ord]
  modelPredictions <- modelPredictions[ord]
  # Precision is the x-axis, TPR/Recall the y.
  x <- cumsum(yValues)/seq_len(length(yValues)) # Precision
  y <- cumsum(yValues)/max(1,sum(yValues))      # TPR/Recall
  pointGraph <- data.frame(Precision=x,Recall=y,
                           stringsAsFactors = FALSE)
  # each point should be fully after a bunch of points or fully before a
  # decision level. remove dups to achieve this.
  dup <- c(modelPredictions[-1]>=modelPredictions[-length(modelPredictions)],
           FALSE)
  # And add in ideal endpoints just in case (redundancy here is not a problem).
  x <- c(1,x[!dup],prevalence)
  y <- c(0,y[!dup],1)
  lineGraph <- data.frame(Precision=x,Recall=y,
                          stringsAsFactors = FALSE)
  # get the best F1
  f1 <- 2*pointGraph$Recall* pointGraph$Precision/
    (pointGraph$Recall + pointGraph$Precision)
  bestF1 <- max(f1)
  list(lineGraph=lineGraph,pointGraph=pointGraph,bestF1=bestF1)
}


#' Plot Precision-Recall plot.
#'
#' See http://www.nature.com/nmeth/journal/v13/n8/full/nmeth.3945.html
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
#' WVPlots::PRPlot(frm, "x", "yC", TRUE, title="Example Precision-Recall plot")
#'
#' @export
PRPlot <- function(frame, xvar, truthVar, truthTarget, title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  outcol <- frame[[truthVar]]==truthTarget
  if(length(unique(outcol))!=2) {
    return(NULL)
  }

  prevalence = mean(as.numeric(outcol) == max(as.numeric(outcol)))
  predcol <- frame[[xvar]]
  prList <- calcPR(predcol,outcol)
  pf <- prList$pointGraph
  pf <- pf[order(pf$Recall),]
  f1 <- 2*pf$Recall*pf$Precision/(pf$Recall+pf$Precision)
  bestX <- which.max(f1)
  bestF1 <- f1[[bestX]]
  pF1 <- pf[bestX,]

  # curves of constant F1 with precision as a function of recall.
  isoFrame <- data.frame(Recall=seq(0.01,1,by=0.01))
  isoFrame$Precision <- bestF1*isoFrame$Recall/(2*isoFrame$Recall-bestF1)
  isoFrame <- isoFrame[(isoFrame$Precision<=1) & (isoFrame$Precision>0),]
  #f1check <- 2*isoFrame$Recall*isoFrame$Precision/(isoFrame$Recall+isoFrame$Precision)


  palletName = "Dark2"
  plot= ggplot2::ggplot() +
    ggplot2::geom_point(data=pf,
                        ggplot2::aes_string(x='Recall',y='Precision'),
                        color='darkblue',alpha=0.5) +
    ggplot2::geom_point(data=pF1,
                        ggplot2::aes_string(x='Recall',y='Precision'),
                        color='blue',size=2,shape=15) +
    ggplot2::geom_line(data=prList$lineGraph,
                       ggplot2::aes_string(x='Recall',y='Precision'),
                       color='darkblue') +
    ggplot2::geom_line(data=isoFrame,
                       ggplot2::aes_string(x='Recall',y='Precision'),
                       color='blue',alpha=0.5,linetype=2) +
    ggplot2::geom_hline(yintercept=prevalence, linetype=2) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(paste0(title,'\n',
                           'best F1 ',format(bestF1, digits=2, nsmall=2),
                           '\n',
                           truthVar,'==',truthTarget, '~', xvar)) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  plot
}
