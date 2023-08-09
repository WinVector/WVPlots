


#' Calculate precision/recall/enrichment as a function of threshold
#' Based on:
#'  https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
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

  notY <- 1 - yValues # falses
  fpr <- cumsum(notY)/max(1, sum(notY)) # (pred T AND actually F)/(all F)
  specificity <- 1 - fpr


  data.frame(threshold = modelPredictions,
             precision = precision,
             enrichment = precision/prevalence,
             recall = recall,
             sensitivity = recall,
             false_positive_rate = fpr,
             specificity = specificity)
}



#' Plot Precision-Recall or Enrichment-Recall as a function of threshold.
#'
#' Plot classifier performance metrics as a function of threshold.
#'
#'@details
#' For a classifier, the precision is what fraction of predicted positives
#' are true positives; the recall is what fraction of true positives the
#' classifier finds, and the enrichment is the ratio of classifier precision to
#' the average rate of positives. Plotting precision-recall or enrichment-recall
#' as a function of classifier score helps identify a score threshold that achieves
#' an acceptable tradeoff between precision and recall, or enrichment and recall.
#'
#' In addition to precision/recall, \code{PRTPlot} can plot a number of other metrics:
#'
#' \itemize{
#'   \item{precision: fraction of predicted positives that are true positives}
#'   \item{recall: fraction of true positives that were predicted to be true}
#'   \item{enrichment: ratio of classifier precision to prevalence of positive class}
#'   \item{sensitivity: the same as recall (also known as the true positive rate)}
#'   \item{specificity: fraction of true negatives to all negatives (or 1 - false_positive_rate)}
#'   \item{false_positive_rate: fraction of negatives predicted to be true over all negatives}
#' }
#'
#' For example, plotting sensitivity/false_positive_rate as functions of threshold will "unroll" an ROC Plot.
#'
#' Plots are in a single column, in the order specified by \code{plotvars}.
#'
#'
#' @param frame data frame to get values from
#' @param predVar name of the column of predicted scores
#' @param truthVar name of the column of actual outcomes in frame
#' @param truthTarget value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param plotvars variables to plot, must be at least one of the measures listed below. Defaults to c("precision", "recall")
#' @param thresholdrange range of thresholds to plot.
#' @param linecolor line color for the plot
#'
#' @seealso \code{\link{ThresholdPlot}}, \code{\link{ROCPlot}}
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' df <- iris
#' df$isVersicolor <- with(df, Species=='versicolor')
#' model = glm(isVersicolor ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
#'             data=df, family=binomial)
#' df$pred = predict(model, newdata=df, type="response")
#'
#' WVPlots::PRTPlot(df, "pred", "isVersicolor", TRUE, title="Example Precision-Recall threshold plot")
#'
#' if (FALSE) {
#' WVPlots::PRTPlot(df, "pred", "isVersicolor", TRUE,
#'                  plotvars = c("sensitivity", "specificity", "false_positive_rate"),
#'                  title="Sensitivity/specificity/FPR as functions of threshold")
#' }
#'
#' @export
PRTPlot <- function(frame, predVar, truthVar, truthTarget, title,
                   ...,
                   plotvars = c("precision", "recall"),
                   thresholdrange = c(-Inf, Inf),
                   linecolor = 'black'
                   ) {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = list(predVar = predVar, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::PRPlot")
  outcol <- frame[[truthVar]]==truthTarget
  if(length(unique(outcol))!=2) {
    return(NULL)
  }
  predcol <- frame[[predVar]]
  prtFrame <- calcPRT(predcol,outcol)

  # mark not unbound
  threshold <- value <- NULL

  thresholds = with(prtFrame, thresholdrange[1] <= threshold & threshold <= thresholdrange[2])
  toPlot = prtFrame[thresholds, c("threshold", plotvars)]

  prtlong = cdata::unpivot_to_blocks(toPlot,
                                     nameForNewKeyColumn = "measure",
                                     nameForNewValueColumn = "value",
                                     columnsToTakeFrom = plotvars)

  # set the factor order to be the same as given in plotvars
  prtlong$measure = factor(prtlong$measure, levels=plotvars)

  ggplot2::ggplot(prtlong, ggplot2::aes(x=threshold, y=value)) +
    ggplot2::geom_line(color=linecolor) +
    ggplot2::facet_wrap(~measure,
                        scale = "free_y",
                        ncol=1) +
    ggplot2::ggtitle(title)
}
