


#' Calculate precision/recall curve.
#'
#' Based on:
#'  https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
#'
#'  See also https://github.com/WinVector/sigr
#'
#' @param modelPredictions numeric predictions (not empty)
#' @param yValues logical truth (not empty, same length as model predictions)
#' @return line graph, point graph, and summaries
#'
#' @noRd
#'
calcPR <- function(modelPredictions,yValues) {
  prevalence = mean(yValues)
  ord <- order(modelPredictions, decreasing=TRUE)
  yValues <- yValues[ord]
  modelPredictions <- modelPredictions[ord]
  precision <- cumsum(yValues)/seq_len(length(yValues)) # Precision
  recall <- cumsum(yValues)/max(1,sum(yValues))      # TPR/Recall
  pointGraph <- data.frame(Precision=precision,Recall=recall,
                           stringsAsFactors = FALSE)
  # each point should be fully after a bunch of points or fully before a
  # decision level. remove dups to achieve this.
  dup <- c(modelPredictions[-1]>=modelPredictions[-length(modelPredictions)],
           FALSE)
  # And add in ideal endpoints just in case (redundancy here is not a problem).
  precision <- c(1,precision[!dup],prevalence)
  recall <- c(0,recall[!dup],1)
  lineGraph <- data.frame(Precision=precision,Recall=recall,
                          stringsAsFactors = FALSE)
  # get the best F1
  f1 <- 2*lineGraph$Recall* lineGraph$Precision/
    (lineGraph$Recall + lineGraph$Precision)
  bestF1 <- max(f1,na.rm=TRUE)
  if(is.na(bestF1) || is.infinite(bestF1)) {
    print("break")
  }
  lineGraph <- lineGraph[order(lineGraph$Recall),]
  pointGraph <- pointGraph[order(pointGraph$Recall),]
  n <- nrow(lineGraph)
  area <- sum( ((lineGraph$Precision[-1]+lineGraph$Precision[-n])/2) *
                 (lineGraph$Recall[-1]-lineGraph$Recall[-n]) )
  list(lineGraph=lineGraph,pointGraph=pointGraph,
       bestF1=bestF1,area=area)
}


#' Plot Precision-Recall plot.
#'
#' Plot Precision-Recall plot.
#'
#' See https://www.nature.com/articles/nmeth.3945 for a discussion of precision and recall,
#' and how the precision/recall plot relates to the ROC plot.
#'
#' In addition to plotting precision versus recall, \code{PRPlot} reports the best
#' achieved F1 score, and plots an isoline corresponding to that F1 score.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param estimate_sig logical, if TRUE compute significance
#'
#' @seealso \code{\link{ROCPlot}}
#'
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
PRPlot <- function(frame, xvar, truthVar, truthTarget, title,
                   ...,
                   estimate_sig = FALSE) {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = list(xvar = xvar, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::PRPlot")
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

  pString <- ''
  if(estimate_sig && requireNamespace('sigr',quietly = TRUE)) {
    sp <- sigr::permutationScoreModel(predcol,outcol,
                                      function(modelValues,yValues) {
                                        calcPR(modelValues,yValues)$bestF1
                                      })
    pString <- sigr::render(sigr::wrapSignificance(sp$pValue),format='ascii')
    pString <- paste0('\n alt. hyp.: F1(',xvar,')>permuted F1, ',pString)
  }
  palletName = "Dark2"
  plot <- ggplot2::ggplot()
  if(nrow(pF1)<1000) {
    plot <- plot +
      ggplot2::geom_point(data=pf,
                          ggplot2::aes_string(x='Recall',y='Precision'),
                          color='darkblue',alpha=0.5)
  }
  plot <- plot +
    ggplot2::geom_point(data=pF1,
                        ggplot2::aes_string(x='Recall',y='Precision'),
                        color='blue',size=2,shape=15) +
    ggplot2::geom_line(data=prList$lineGraph,
                       ggplot2::aes_string(x='Recall',y='Precision'),
                       color='darkblue') +
    ggplot2::geom_line(data=isoFrame,
                       ggplot2::aes_string(x='Recall',y='Precision'),
                       color='blue',alpha=0.5,linetype=2) +
    ggplot2::geom_hline(yintercept=prevalence, linetype=3, alpha=0.2) +
    ggplot2::geom_vline(xintercept=1, linetype=3, alpha=0.2) +
    ggplot2::geom_hline(yintercept=1, linetype=3, alpha=0.2) +
    ggplot2::geom_vline(xintercept=0, linetype=3, alpha=0.2) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(paste0(title,'\n',
                            truthVar,'==',truthTarget, '~', xvar),
                     subtitle = paste0(
                           'best F1 ',format(bestF1, digits=2, nsmall=2),
                            pString)) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  plot
}
