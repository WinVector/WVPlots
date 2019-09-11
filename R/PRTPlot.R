


#' Calculate precision/recall/enrichment as a function of threshold
#' Based on:
#'  http://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
#'
#'  See also https://github.com/WinVector/sigr
#'
#' @param modelPredictions numeric predictions (not empty, zero assumed to be "all")
#' @param yValues logical truth (not empty, same length as model predictions)
#' @return data frame of precision, enrichment, and recall as a function of threshold
#'
#' @noRd
#'
calcPRT <- function(modelPredictions, yValues) {
  prevalence = mean(yValues)
  ord <- order(modelPredictions, decreasing=TRUE)
  yValues <- yValues[ord]
  modelPredictions <- modelPredictions[ord]
  precision <- cumsum(yValues)/seq_len(length(yValues)) # Precision
  recall <- cumsum(yValues)/max(1,sum(yValues))      # TPR/Recall
  data.frame(threshold = modelPredictions,
             precision = precision,
             enrichment = precision/prevalence,
             recall = recall)
}



#' Plot Precision-Recall or Enrichment-Recall as a function of threshold.
#'
#' Plot Precision-Recall or Enrichment-Recall as a function of threshold.
#'
#' For a classifier, the precision is what fraction of predicted positives
#' are true positives; the recall is what fraction of true positives the
#' classifier finds, and the enrichment is the ratio of classifier precision to
#' the average rate of positives. Plotting precision-recall or enrichment-recall
#' as a function of classifier score helps identify a score threshold that achieves
#' an acceptable tradeoff between precision and recall, or enrichment and recall.
#'
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param plotvars variables to plot, must be at least one of "precision", "recall" and "enrichment". Defaults to c("precision", "recall")
#' @param thresholdrange range of thresholds to plot.
#' @param linecolor line color for the plot
#'
#' @seealso \code{\link{PRPlot}}
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
#' WVPlots::PRTPlot(frm, "x", "yC", TRUE, title="Example Precision-Recall threshold plot")
#'
#' @export
PRTPlot <- function(frame, xvar, truthVar, truthTarget, title,
                   ...,
                   plotvars = c("precision", "recall"),
                   thresholdrange = c(-Inf, Inf),
                   linecolor = 'black'
                   ) {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = list(xvar = xvar, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::PRPlot")
  outcol <- frame[[truthVar]]==truthTarget
  if(length(unique(outcol))!=2) {
    return(NULL)
  }
  predcol <- frame[[xvar]]
  prtFrame <- calcPRT(predcol,outcol)

  # mark not unbound
  threshold <- value <- NULL

  thresholds = with(prtFrame, thresholdrange[1] <= threshold & threshold <= thresholdrange[2])
  toPlot = prtFrame[thresholds, c("threshold", plotvars)]

  prtlong = cdata::unpivot_to_blocks(toPlot,
                                     nameForNewKeyColumn = "measure",
                                     nameForNewValueColumn = "value",
                                     columnsToTakeFrom = plotvars)

  ggplot2::ggplot(prtlong, ggplot2::aes(x=threshold, y=value)) +
    ggplot2::geom_line(color=linecolor) +
    ggplot2::facet_wrap(~measure,
                        scale = "free_y",
                        ncol=1) +
    ggplot2::ggtitle(title)
}
