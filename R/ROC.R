



novelPointPositionsL <- function(x) {
  len <- length(x)
  if(len<=1) {
    return(rep(TRUE,len))
  }
  c(TRUE,abs(x[-1]-x[-len])>1.0e-6)
}

novelPointPositionsR <- function(x) {
  len <- length(x)
  if(len<=1) {
    return(rep(TRUE,len))
  }
  c(abs(x[-1]-x[-len])>1.0e-6,TRUE)
}

#' Graph AUC.
#'
#' Based on:
#'  http://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
#'
#'  See also https://github.com/WinVector/sigr
#
#' @param modelPredictions numeric predictions (not empty)
#' @param yValues logical truth (not empty, same length as model predictions)
#' @return line graph, point graph, and area under curve
#'
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' WVPlots::graphROC(frm$x, frm$yC, estimate_sig = TRUE)
#'
#' @noRd
#'
graphROC <- function(modelPredictions, yValues) {
  if(!is.numeric(modelPredictions)) {
    stop("WVPlots::graphROC modelPredictions must be numeric")
  }
  if(!is.logical(yValues)) {
    stop("WVPlots::graphROC yValues must be logical")
  }
  if(length(modelPredictions)!=length(yValues)) {
    stop("WVPlots::graphROC must have length(modelPredictions)==length(yValues)")
  }
  ord <- order(modelPredictions, decreasing=TRUE)
  yValues <- yValues[ord]
  modelPredictions <- modelPredictions[ord]
  # FPR is the x-axis, TPR the y.
  x <- cumsum(!yValues)/max(1,sum(!yValues)) # FPR = x-axis
  y <- cumsum(yValues)/max(1,sum(yValues))   # TPR = y-axis
  # each point should be fully after a bunch of points or fully before a
  # decision level. remove dups to achieve this.
  dup <- c(modelPredictions[-1]>=modelPredictions[-length(modelPredictions)],
           FALSE)
  # And add in ideal endpoints just in case (redundancy here is not a problem).
  x <- x[!dup]
  y <- y[!dup]
  prediction <- modelPredictions[!dup]
  pointGraph <- data.frame(FalsePositiveRate= x,
                           TruePositiveRate= y,
                           model= prediction,
                           stringsAsFactors = FALSE)
  x <- c(0,x,1)
  y <- c(0,y,1)
  prediction <- c(0,prediction,1)
  lineGraph <- data.frame(FalsePositiveRate= x,
                          TruePositiveRate= y,
                          model= prediction,
                          stringsAsFactors = FALSE)
  # further de-dup points
  # care about changes in x or y
  goodPointRows <- novelPointPositionsL(pointGraph$FalsePositiveRate) |
    novelPointPositionsL(pointGraph$TruePositiveRate)
  pointGraph <- pointGraph[goodPointRows, , drop=FALSE]
  # only care about points near height changes
  goodLineRows <- novelPointPositionsL(lineGraph$TruePositiveRate) |
    novelPointPositionsR(lineGraph$TruePositiveRate)
  lineGraph <- lineGraph[goodLineRows, , drop=FALSE]
  list(lineGraph=lineGraph,
       pointGraph=pointGraph,
       area=sigr::calcAUC(modelPredictions,yValues))
}

#' Plot receiver operating characteristic plot.
#'
#' Plot receiver operating characteristic plot.
#'
#' See http://www.nature.com/nmeth/journal/v13/n8/full/nmeth.3945.html for a discussion of
#' true positive and false positive rates,
#' and how the ROC plot relates to the precision/recall plot.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param estimate_sig logical, if TRUE estimate and display significance of difference from AUC 0.5.
#' @param returnScores logical if TRUE return detailed permutedScores
#' @param nrep number of permutation repetitions to estimate p values.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#'
#' @seealso \code{\link{PRPlot}}
#'
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' WVPlots::ROCPlot(frm, "x", "yC", TRUE, title="Example ROC plot", estimate_sig = TRUE)
#'
#' @export
ROCPlot <- function(frame, xvar, truthVar, truthTarget, title,
                    ...,
                    estimate_sig = FALSE,
                    returnScores = FALSE,
                    nrep = 100,
                    parallelCluster = NULL) {
  # check and narrow frame
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = list(xvar = xvar, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::ROCPlot")
  outcol <- frame[[truthVar]]==truthTarget
  if(length(unique(outcol))!=2) {
    return(NULL)
  }
  predcol <- frame[[xvar]]
  rocList <- graphROC(predcol,outcol)
  auc <- rocList$area
  palletName = "Dark2"
  aucsig <- NULL
  pString <- NULL
  subtitle <- NULL
  if(estimate_sig) {
    aucsig <- sigr::permutationScoreModel(modelValues=predcol,
                                          yValues=outcol,
                                          scoreFn=sigr::calcAUC,
                                          returnScores=returnScores,
                                          nRep=nrep,
                                          parallelCluster=parallelCluster)
    pString <- sigr::render(sigr::wrapSignificance(aucsig$pValue),format='ascii')
    aucString <- sprintf('%.2g',auc)
    subtitle <- paste0(
      'AUC = ',aucString,
      '\nalt. hyp.: AUC(',xvar,')>permuted AUC, ',
      pString)
  } else {
    aucString <- sprintf('%.2g',auc)
    subtitle <- paste('AUC =', aucString)
  }
  plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=rocList$lineGraph,
                         ggplot2::aes_string(x='FalsePositiveRate',
                                             ymax='TruePositiveRate',ymin=0),
                         alpha=0.2,color=NA)
  if(nrow(rocList$pointGraph)<=1000) {
    plot <- plot +
      ggplot2::geom_point(data=rocList$pointGraph,
                          ggplot2::aes_string(x='FalsePositiveRate',
                                              y='TruePositiveRate'),
                          color='darkblue', alpha=0.5)
  }
  plot <- plot +
    ggplot2::geom_line(data=rocList$lineGraph,
                       ggplot2::aes_string(x='FalsePositiveRate',
                                           y='TruePositiveRate'),
                       color='darkblue') +
    ggplot2::geom_abline(slope=1,intercept=0) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(paste0(title,'\n',
                            truthVar, '==', truthTarget, ' ~ ', xvar),
                     subtitle = subtitle) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  if(returnScores) {
    return(list(plot=plot,rocList=rocList,aucsig=aucsig,pString=pString))
  }
  plot
}




#' Compare two ROC plots.
#'
#' Plot two receiver operating characteristic curves from the same data.frame.
#'
#' The use case for this function is to compare the performance of two
#' models when applied to a data set, where the predictions from both models
#' are columns of the same data frame.
#'
#' @param frame data frame to get values from
#' @param xvar1 name of the first independent (input or model) column in frame
#' @param xvar2 name of the second independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param estimate_sig logical, if TRUE estimate and display significance of difference from AUC 0.5.
#' @param returnScores logical if TRUE return detailed permutedScores
#' @param nrep number of permutation repetitions to estimate p values.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#'
#' @seealso \code{\link{ROCPlot}}
#'
#' @examples
#'
#' set.seed(34903490)
#' x1 = rnorm(50)
#' x2 = rnorm(length(x1))
#' y = 0.2*x2^2 + 0.5*x2 + x1 + rnorm(length(x1))
#' frm = data.frame(x1=x1,x2=x2,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' # WVPlots::ROCPlot(frm, "x1", "yC", TRUE, title="Example ROC plot")
#' # WVPlots::ROCPlot(frm, "x2", "yC", TRUE, title="Example ROC plot")
#' WVPlots::ROCPlotPair(frm, "x1", "x2", "yC", TRUE,
#'    title="Example ROC pair plot", estimate_sig = TRUE)
#'
#' @export
ROCPlotPair <- function(frame, xvar1, xvar2, truthVar, truthTarget, title,
                        ...,
                        estimate_sig=FALSE,
                        returnScores=FALSE,
                        nrep=100,
                        parallelCluster=NULL) {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = list(xvar1 = xvar1, xvar2 = xvar2, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::ROCPlotPair")
  outcol <- frame[[truthVar]]==truthTarget
  if(length(unique(outcol))!=2) {
    return(NULL)
  }
  rocList1 <- graphROC(frame[[xvar1]],outcol)
  rocList2 <- graphROC(frame[[xvar2]],outcol)
  aucsig <- NULL
  eString <- NULL

  nm1 <- paste0('1: ',xvar1,', AUC=',sprintf('%.2g',rocList1$area))
  nm2 <- paste0('2: ',xvar2,', AUC=',sprintf('%.2g',rocList2$area))

  subtitle <- NULL

  if(estimate_sig) {
    aucsig <- sigr::resampleScoreModelPair(frame[[xvar1]],
                                           frame[[xvar2]],
                                           frame[[truthVar]]==truthTarget,
                                           scoreFn=sigr::calcAUC,
                                           returnScores=returnScores,
                                           nRep=nrep,
                                           parallelCluster=parallelCluster)
    eString <- sigr::render(sigr::wrapSignificance(aucsig$eValue,symbol = 'e'),
                            format='ascii',
                            pLargeCutoff=2.0)
    subtitle <- paste0(
      'testing: AUC(1)>AUC(2)\n on same data\n ',
      eString)
  }

  rocList1$pointGraph$model <- nm1
  rocList1$lineGraph$model <- nm1
  rocList2$pointGraph$model <- nm2
  rocList2$lineGraph$model <- nm2

  pointGraph <- rbind(rocList1$pointGraph,rocList2$pointGraph)
  lineGraph <- rbind(rocList1$lineGraph,rocList2$lineGraph)
  palletName = "Dark2"
  plot <- ggplot2::ggplot()
  if(nrow(pointGraph)<=1000) {
    plot <- plot + ggplot2::geom_point(data=pointGraph,
                        ggplot2::aes_string(x='FalsePositiveRate',
                                            y='TruePositiveRate',
                                     color='model',shape='model'),
                        alpha=0.5)
  }
  plot <- plot +
    ggplot2::geom_line(data=lineGraph,
                       ggplot2::aes_string(x='FalsePositiveRate',
                                           y='TruePositiveRate',
                                    color='model',linetype='model')) +
    ggplot2::geom_abline(slope=1,intercept=0,color='gray') +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(paste0(title,'\n',
                  truthVar, '==', truthTarget, ' ~ model'),
                  subtitle = subtitle) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  if(returnScores) {
    return(list(plot=plot,
                rocList1=rocList1,rocList2=rocList2,
                aucsig=aucsig,eString=eString))
  }
  plot
}


#' Compare two ROC plots.
#'
#' Plot two receiver operating characteristic curves from different data frames.
#'
#' Use this curve to compare model predictions to true outcome from two
#' data frames, each of which has its own model predictions and true outcome columns.
#'
#' @param nm1 name of first model
#' @param frame1 data frame to get values from
#' @param xvar1 name of the first independent (input or model) column in frame
#' @param truthVar1 name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget1 value we consider to be positive
#' @param nm2 name of second model
#' @param frame2 data frame to get values from
#' @param xvar2 name of the first independent (input or model) column in frame
#' @param truthVar2 name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget2 value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param estimate_sig logical, if TRUE estimate and display significance of difference from AUC 0.5.
#' @param returnScores logical if TRUE return detailed permutedScores
#' @param nrep number of permutation repetitions to estimate p values.
#' @param parallelCluster (optional) a cluster object created by package parallel or package snow.
#'
#' @seealso \code{\link{ROCPlot}}
#'
#' @examples
#'
#' set.seed(34903490)
#' x1 = rnorm(50)
#' x2 = rnorm(length(x1))
#' y = 0.2*x2^2 + 0.5*x2 + x1 + rnorm(length(x1))
#' frm = data.frame(x1=x1,x2=x2,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' # WVPlots::ROCPlot(frm, "x1", "yC", TRUE, title="Example ROC plot")
#' # WVPlots::ROCPlot(frm, "x2", "yC", TRUE, title="Example ROC plot")
#' WVPlots::ROCPlotPair2('train',frm, "x1", "yC", TRUE,
#'                       'test', frm, "x2", "yC", TRUE,
#'                       title="Example ROC pair plot", estimate_sig = TRUE)
#'
#' @export
ROCPlotPair2 <- function(nm1, frame1, xvar1, truthVar1, truthTarget1,
                         nm2, frame2, xvar2, truthVar2, truthTarget2,
                         title,
                         ...,
                         estimate_sig= TRUE,
                         returnScores= FALSE,
                         nrep= 100,
                         parallelCluster= NULL) {
  frame1 <- check_frame_args_list(...,
                                  frame = frame1,
                                  name_var_list = list(xvar1 = xvar1, truthVar1 = truthVar1),
                                  title = title,
                                  funname = "WVPlots::ROCPlotPair2")
  frame2 <- check_frame_args_list(...,
                                  frame = frame2,
                                  name_var_list = list(xvar2 = xvar2, truthVar2 = truthVar2),
                                  title = title,
                                  funname = "WVPlots::ROCPlotPair2")
  test <- NULL # used as a symbol, declare not an unbound variable
  outcol1 <- frame1[[truthVar1]]==truthTarget1
  if(length(unique(outcol1))!=2) {
    return(NULL)
  }
  outcol2 <- frame2[[truthVar2]]==truthTarget2
  if(length(unique(outcol2))!=2) {
    return(NULL)
  }
  rocList1 <- graphROC(frame1[[xvar1]],outcol1)
  rocList2 <- graphROC(frame2[[xvar2]],outcol2)

  aucsig <- NULL
  eString <- NULL
  nm1 <- paste0('1: ',nm1,' ',xvar1,', AUC=',sprintf('%.2g',rocList1$area))
  nm2 <- paste0('2: ',nm2,' ',xvar2,', AUC=',sprintf('%.2g',rocList2$area))
  subtitle <- NULL
  if(estimate_sig) {
    d1 <- sigr::resampleTestAUC(frame1,xvar1,truthVar1,truthTarget1,
                                nrep=nrep,returnScores = TRUE)
    d2 <- sigr::resampleTestAUC(frame2,xvar2,truthVar2,truthTarget2,
                                nrep=nrep,returnScores = TRUE)
    aucsig <- sigr::estimateDifferenceZeroCrossing(d1$eScore$resampledScores -
                                                     d2$eScore$resampledScores)
    eString <- sigr::render(sigr::wrapSignificance(aucsig$eValue,symbol='e'),
                            format='ascii',
                            pLargeCutoff=0.5)

    subtitle <- paste0(
      'testing: AUC(1)>AUC(2)\n on different data\n ',
      eString)
  }

  rocList1$pointGraph$dataset <- nm1
  rocList1$lineGraph$dataset <- nm1
  rocList2$pointGraph$dataset <- nm2
  rocList2$lineGraph$dataset <- nm2
  pointGraph <- rbind(rocList1$pointGraph,rocList2$pointGraph)
  lineGraph <- rbind(rocList1$lineGraph,rocList2$lineGraph)
  palletName = "Dark2"
  plot <- ggplot2::ggplot()
  if(nrow(pointGraph)<=1000) {
    plot <- plot +
      ggplot2::geom_point(data=pointGraph,
                          ggplot2::aes_string(x='FalsePositiveRate',
                                              y='TruePositiveRate',
                                              color='dataset',shape='dataset'),
                          alpha=0.5)
  }
  plot <- plot +
    ggplot2::geom_line(data=lineGraph,
                       ggplot2::aes_string(x='FalsePositiveRate',
                                           y='TruePositiveRate',
                                           color='dataset',linetype='dataset')) +
    ggplot2::geom_abline(slope=1,intercept=0,color='gray') +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(title,
                     subtitle = subtitle) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  if(returnScores) {
    return(list(plot=plot,
                rocList1=rocList1,rocList2=rocList2,
                d1=d1,d2=d2,
                test=test,
                aucsig=aucsig,
                eString=eString))
  }
  plot
}



#' Use \code{plotly} to produce a ROC plot.
#'
#' Use \code{plotly} to produce a ROC plot.
#'
#'
#' @param d dataframe
#' @param predCol name of column with numeric predictions
#' @param outcomeCol name of column with truth
#' @param outcomeTarget value considered true
#' @param title character title for plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param estimate_sig logical, if TRUE estimate and display significance of difference from AUC 0.5.
#' @return plotly plot
#'
#' @seealso \code{\link{ROCPlot}}
#'
#' @examples
#'
#' if(requireNamespace("plotly", quietly = TRUE)) {
#'    set.seed(34903490)
#'    x = rnorm(50)
#'    y = 0.5*x^2 + 2*x + rnorm(length(x))
#'    frm = data.frame(x=x,yC=y>=as.numeric(quantile(y,probs=0.8)))
#'    plotlyROC(frm, 'x', 'yC', TRUE, 'example plot', estimate_sig = TRUE)
#' }
#'
#'
#' @export
#'
plotlyROC <- function(d, predCol, outcomeCol, outcomeTarget, title,
                      ...,
                      estimate_sig = FALSE) {
  if(!(requireNamespace("plotly", quietly = TRUE))) {
    stop("WVPlots::plotlyROC requires the plotly package be installed")
  }
  d <- check_frame_args_list(...,
                             frame = d,
                             name_var_list = list(predCol = predCol, outcomeCol = outcomeCol),
                             title = title,
                             funname = "WVPlots::plotlyROC")
  prediction <- d[[predCol]]
  if(!is.numeric(prediction)) {
    stop("WVPlots:plotlyROC prediction must be numeric")
  }
  outcome <- d[[outcomeCol]]==outcomeTarget
  rocFrame <- graphROC(prediction, outcome)

  palletName = "Dark2"

  auc <- rocFrame$area
  returnScores=FALSE
  nrep=100
  parallelCluster=NULL
  aucsig <- NULL
  pString <- NULL
  aucString <- NULL
  subtitle <- NULL
  if(estimate_sig) {
    aucsig <- sigr::permutationScoreModel(modelValues=prediction,
                                          yValues=outcome,
                                          scoreFn=sigr::calcAUC,
                                          returnScores=returnScores,
                                          nRep=nrep,
                                          parallelCluster=parallelCluster)
    pString <- sigr::render(sigr::wrapSignificance(aucsig$pValue),format='ascii')
    aucString <- sprintf('%.2g',auc)
    subtitle = paste0(
      'AUC=',aucString,
      '\n</br>alt. hyp.: AUC(',predCol,')>permuted AUC, ',
      pString)
  } else {
    aucString <- sprintf('%.2g',auc)
    subtitle <- paste0('AUC=', aucString)
  }

  # see https://plot.ly/r/text-and-annotations/
  plotly::plot_ly(rocFrame$pointGraph,
                  x = ~FalsePositiveRate,
                  y = ~TruePositiveRate,
                  type='scatter',
                  mode='lines+markers',
                  hoverinfo= 'text',
                  text= ~ paste('</br>threshold: ', model,
                                '</br>False Positive Rate:', FalsePositiveRate,
                                '</br>True Positive Rate:', TruePositiveRate)) ->.;
    plotly::layout(., title = paste(title,
                                    '\n</br>',
                                    outcomeCol, '==', outcomeTarget, ' ~ ', predCol,
                                    '\n</br>', subtitle))
}

