

#' @importFrom wrapr unpack
NULL


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
#'  https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
#'
#'  See also https://github.com/WinVector/sigr
#
#' @param modelPredictions numeric predictions (not empty)
#' @param yValues logical truth (not empty, same length as model predictions)
#' @return line graph, point graph, and area under curve
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' WVPlots:::graphROC(frm$x, frm$yC)
#'
#' @noRd
#'
graphROC <- function(modelPredictions, yValues) {
  positive_prevalence <- mean(yValues, na.rm = TRUE)
  roc <- sigr::build_ROC_curve(
    modelPredictions = modelPredictions,
    yValues = yValues,
    yTarget = TRUE,
    na.rm = TRUE)
  roc <- sigr::add_ROC_derived_columns(roc, positive_prevalence)
  pointGraph <- data.frame(FalsePositiveRate= roc$FalsePositiveRate,
                           TruePositiveRate= roc$TruePositiveRate,
                           model= roc$Score,
                           stringsAsFactors = FALSE)
  pointGraph <- pointGraph[!is.na(pointGraph$model), , drop = FALSE]
  lineGraph <- data.frame(FalsePositiveRate= roc$FalsePositiveRate,
                          TruePositiveRate= roc$TruePositiveRate,
                          model= roc$Score,
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


#' @importFrom grDevices chull
NULL


#' Plot receiver operating characteristic plot.
#'
#' Plot receiver operating characteristic plot.
#'
#' See https://www.nature.com/articles/nmeth.3945 for a discussion of
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
#' @param curve_color color of the ROC curve
#' @param fill_color shading color for the area under the curve
#' @param diag_color color for the AUC=0.5 line (x=y)
#' @param add_beta_ideal_curve logical, if TRUE add the beta(a, b), beta(c, d) ideal curve found by moment matching.
#' @param beta_ideal_curve_color color for ideal curve.
#' @param add_beta1_ideal_curve logical, if TRUE add the beta(1, a), beta(b, 2) ideal curve defined in \url{https://journals.sagepub.com/doi/abs/10.1177/0272989X15582210}
#' @param beta1_ideal_curve_color color for ideal curve.
#' @param add_symmetric_ideal_curve logical, if TRUE add the ideal curve as discussed in \url{https://win-vector.com/2020/09/13/why-working-with-auc-is-more-powerful-than-one-might-think/}.
#' @param symmetric_ideal_curve_color color for ideal curve.
#' @param add_convex_hull logical, if TRUE add convex hull to plot
#' @param convex_hull_color color for convex hull curve
#' @param ideal_plot_step_size step size used in ideal plots
#'
#' @seealso \code{\link{PRTPlot}}, \code{\link{ThresholdPlot}}
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' beta_example <- function(
#'   n,
#'   shape1_pos, shape2_pos,
#'   shape1_neg, shape2_neg) {
#'   d <- data.frame(
#'     y = sample(
#'       c(TRUE, FALSE),
#'       size = n,
#'       replace = TRUE),
#'     score = 0.0
#'   )
#'   d$score[d$y] <- rbeta(sum(d$y), shape1 = shape1_pos, shape2 = shape2_pos)
#'   d$score[!d$y] <- rbeta(sum(!d$y), shape1 = shape1_neg, shape2 = shape2_neg)
#'   d
#' }
#'
#' d1 <- beta_example(
#'   100,
#'   shape1_pos = 6,
#'   shape2_pos = 5,
#'   shape1_neg = 1,
#'   shape2_neg = 2)
#'
#' ROCPlot(
#'    d1,
#'    xvar = "score",
#'    truthVar = "y", truthTarget = TRUE,
#'    title="Example ROC plot",
#'    estimate_sig = TRUE,
#'    add_beta_ideal_curve = TRUE,
#'    add_convex_hull = TRUE)
#'
#' @export
ROCPlot <- function(frame, xvar, truthVar, truthTarget, title,
                    ...,
                    estimate_sig = FALSE,
                    returnScores = FALSE,
                    nrep = 100,
                    parallelCluster = NULL,
                    curve_color='darkblue',
                    fill_color='black',
                    diag_color='black',
                    add_beta_ideal_curve = FALSE,
                    beta_ideal_curve_color = "#fd8d3c",
                    add_beta1_ideal_curve = FALSE,
                    beta1_ideal_curve_color = "#f03b20",
                    add_symmetric_ideal_curve = FALSE,
                    symmetric_ideal_curve_color = "#bd0026",
                    add_convex_hull = FALSE,
                    convex_hull_color = "#404040",
                    ideal_plot_step_size = 0.001) {
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
  rocList <- graphROC(predcol, outcol)
  auc <- rocList$area
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
  FalsePositiveRate <- NULL  # don't look unbound in CRAN check
  TruePositiveRate <- NULL  # don't look unbound in CRAN check
  plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=rocList$lineGraph,
                         mapping=ggplot2::aes(x=FalsePositiveRate,
                                             ymax=TruePositiveRate,ymin=0),
                         alpha=0.2,color=NA, fill=fill_color)
  if(nrow(rocList$pointGraph)<=1000) {
    plot <- plot +
      ggplot2::geom_point(data=rocList$pointGraph,
                          mapping=ggplot2::aes(x=FalsePositiveRate,
                                              y=TruePositiveRate),
                          color=curve_color, alpha=0.5)
  }
  plot <- plot +
    ggplot2::geom_line(data=rocList$lineGraph,
                       mapping=ggplot2::aes(x=FalsePositiveRate,
                                           y=TruePositiveRate),
                       color=curve_color) +
    ggplot2::geom_abline(slope=1,intercept=0,color=diag_color) +
    ggplot2::coord_fixed() +
    ggplot2::ggtitle(paste0(title,'\n',
                            truthVar, '==', truthTarget, ' ~ ', xvar),
                     subtitle = subtitle) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1) +
    ggplot2::ylab('TruePositiveRate (Sensitivity)') +
    ggplot2::xlab('FalsePositiveRate (1 - Specificity)')
  Specificity <- NULL  # don't look unbound in CRAN check
  Sensitivity <- NULL  # don't look unbound in CRAN check
  if(add_beta1_ideal_curve) {
    # match the displayed curve
    a <- b <- NULL  # don't look unbound
    unpack[a, b] <-
       sigr::find_ROC_matching_ab1(modelPredictions = predcol, yValues = outcol)
    # print(paste(a, b))
    ideal_roc <- sigr::sensitivity_and_specificity_s12p12n(
      seq(0, 1, by = ideal_plot_step_size),
      shape1_pos = a,
      shape2_pos = 1,
      shape1_neg = 1,
      shape2_neg = b)
    plot <- plot +
      ggplot2::geom_line(
        data = ideal_roc,
        mapping = ggplot2::aes(x = 1 - Specificity, y = Sensitivity),
        color = beta_ideal_curve_color,
        alpha = 0.8,
        linetype = 2)
  }
  if(add_beta_ideal_curve) {
    # fit by moment matching
    shape1 <- shape1_neg <- shape1_pos <- shape2 <- shape2_neg <- shape2_pos <- NULL  # don't look unbound
    unpack[shape1_pos, shape2_pos, shape1_neg, shape2_neg] <-
      sigr::find_ROC_matching_ab(modelPredictions = predcol, yValues = outcol)
    ideal_roc <- sigr::sensitivity_and_specificity_s12p12n(
      seq(0, 1, by = ideal_plot_step_size),
      shape1_pos = shape1_pos,
      shape2_pos = shape2_pos,
      shape1_neg = shape1_neg,
      shape2_neg = shape2_neg)
    plot <- plot +
      ggplot2::geom_line(
        data = ideal_roc,
        mapping = ggplot2::aes(x = 1 - Specificity, y = Sensitivity),
        color = beta_ideal_curve_color,
        alpha = 0.8,
        linetype = 2)
  }
  if(add_symmetric_ideal_curve) {
    # add in an ideal AUC curve with same area
    # From: https://win-vector.com/2020/09/13/why-working-with-auc-is-more-powerful-than-one-might-think/
    q <- sigr::find_AUC_q(frame[[xvar]], frame[[truthVar]] == truthTarget)
    ideal_roc <- data.frame(Specificity = seq(0, 1, by = ideal_plot_step_size))
    ideal_roc$Sensitivity <- sigr::sensitivity_from_specificity_q(ideal_roc$Specificity, q)  # TODO: move back to sigr
    plot <- plot +
      ggplot2::geom_line(
         data = ideal_roc,
         mapping = ggplot2::aes(x = 1 - Specificity, y = Sensitivity),
         color = symmetric_ideal_curve_color,
         alpha = 0.8,
         linetype = 2)
  }
  if(add_convex_hull) {
    FalsePositiveRate <- TruePositiveRate <- NULL # don't look unbound
    ch_frm <- rocList$pointGraph[ , c('FalsePositiveRate', 'TruePositiveRate')]
    ch_frm <- rbind(
      data.frame(FalsePositiveRate = 0, TruePositiveRate = 0),
      ch_frm,
      data.frame(FalsePositiveRate = c(1, 1), TruePositiveRate = c(1, 0)))
    ch_idxs <- grDevices::chull(x = ch_frm$FalsePositiveRate,
                                y = ch_frm$TruePositiveRate)
    pts <- ch_frm[sort(unique(ch_idxs)), ]
    pts <- pts[-nrow(pts), ]
    plot <- plot +
      ggplot2::geom_line(
        data = pts,
        mapping = ggplot2::aes(x = FalsePositiveRate, y = TruePositiveRate),
        color = convex_hull_color,
        alpha = 0.75,
        linetype = 3)
  }
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
#' If \code{palette} is NULL, plot colors will be chosen from the default ggplot2 palette. Setting \code{palette} to NULL
#' allows the user to choose a non-Brewer palette, for example with \code{\link[ggplot2:scale_manual]{scale_color_manual}}.
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
#' @param palette name of a brewer palette (NULL for ggplot2 default coloring)
#'
#' @seealso \code{\link{ROCPlot}}
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
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
                        parallelCluster=NULL,
                        palette="Dark2") {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = list(xvar1 = xvar1, xvar2 = xvar2, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::ROCPlotPair")
  outcol <- frame[[truthVar]]==truthTarget
  FalsePositiveRate <- NULL  # don't look unbound in CRAN check
  TruePositiveRate <- NULL  # don't look unbound in CRAN check
  model <- NULL  # don't look unbound in CRAN check
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

  pointGraph <- rbind(rocList1$pointGraph, rocList2$pointGraph, stringsAsFactors = FALSE)
  lineGraph <- rbind(rocList1$lineGraph, rocList2$lineGraph, stringsAsFactors = FALSE)
  palletName = palette
  plot <- ggplot2::ggplot()
  if(nrow(pointGraph)<=1000) {
    plot <- plot + ggplot2::geom_point(data=pointGraph,
                                       mapping=ggplot2::aes(x=FalsePositiveRate,
                                                           y=TruePositiveRate,
                                                           color=model,shape=model),
                                       alpha=0.5)
  }
  plot <- plot +
    ggplot2::geom_line(data=lineGraph,
                       mapping=ggplot2::aes(x=FalsePositiveRate,
                                           y=TruePositiveRate,
                                           color=model,linetype=model)) +
    ggplot2::geom_abline(slope=1,intercept=0,color='gray') +
    ggplot2::coord_fixed()

  if(!is.null(palette)) {
    plot <- plot +
      ggplot2::scale_fill_brewer(palette=palletName) +
      ggplot2::scale_color_brewer(palette=palletName)
  }
  plot <- plot + ggplot2::ggtitle(paste0(title,'\n',
                                         truthVar, '==', truthTarget, ' ~ model'),
                                  subtitle = subtitle) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1) +
    ggplot2::ylab('TruePositiveRate (Sensitivity)') +
    ggplot2::xlab('FalsePositiveRate (1 - Specificity)')
  if(returnScores) {
    return(list(plot=plot,
                rocList1=rocList1,rocList2=rocList2,
                aucsig=aucsig,eString=eString))
  }
  plot
}


#' Compare multiple ROC plots.
#'
#' Plot multiple receiver operating characteristic curves from the same data.frame.
#'
#' The use case for this function is to compare the performance of two
#' models when applied to a data set, where the predictions from both models
#' are columns of the same data frame.
#'
#' If \code{palette} is NULL, plot colors will be chosen from the default ggplot2 palette. Setting \code{palette} to NULL
#' allows the user to choose a non-Brewer palette, for example with \code{\link[ggplot2:scale_manual]{scale_color_manual}}.
#'
#' @param frame data frame to get values from
#' @param xvar_names names of the independent (input or model) columns in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param palette name of a brewer palette (NULL for ggplot2 default coloring)
#'
#' @seealso \code{\link{ROCPlot}}, \code{\link{ROCPlotPair}}, \code{\link{ROCPlotPair2}}
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' set.seed(34903490)
#' x1 = rnorm(50)
#' x2 = rnorm(length(x1))
#' x3 = rnorm(length(x1))
#' y = 0.2*x2^2 + 0.5*x2 + x1 + rnorm(length(x1))
#' frm = data.frame(
#'    x1 = x1,
#'    x2 = x2,
#'    x3 = x3,
#'    yC = y >= as.numeric(quantile(y,probs=0.8)))
#' WVPlots::ROCPlotList(
#'    frame = frm,
#'    xvar_names = c("x1", "x2", "x3"),
#'    truthVar = "yC", truthTarget = TRUE,
#'    title = "Example ROC list plot")
#'
#' @export
ROCPlotList <- function(
  frame, xvar_names, truthVar, truthTarget, title,
  ...,
  palette="Dark2") {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = c(xvar_names := xvar_names, c(truthVar = truthVar)),
                                 title = title,
                                 funname = "WVPlots::ROCPlotPair")
  outcol <- frame[[truthVar]]==truthTarget
  if(length(unique(outcol))!=2) {
    return(NULL)
  }
  rocLists <- lapply(
    xvar_names := xvar_names,
    function(v) {graphROC(frame[[v]], outcol)})

  nmList <- lapply(
    xvar_names := xvar_names,
    function(v) {paste0(v,', AUC=',sprintf('%.2g',rocLists[[v]]$area))}
  )

  FalsePositiveRate <- NULL  # don't look unbound in CRAN check
  TruePositiveRate <- NULL  # don't look unbound in CRAN check
  model <- NULL  # don't look unbound in CRAN check
  dataset <- NULL  # don't look unbound in CRAN check

  for(v in xvar_names) {
    rocLists[[v]]$pointGraph$model <- nmList[[v]]
    rocLists[[v]]$lineGraph$model <- nmList[[v]]
  }

  pointGraph <- do.call(rbind,
                        c(lapply(xvar_names, function(v) {rocLists[[v]]$pointGraph}),
                          c(stringsAsFactors = FALSE)))
  lineGraph <- do.call(rbind,
                       c(lapply(xvar_names, function(v) {rocLists[[v]]$lineGraph}),
                         c(stringsAsFactors = FALSE)))
  # set the factor order to be the same as given in metrics
  pointGraph$model = factor(pointGraph$model, levels=nmList)
  lineGraph$model = factor(lineGraph$model, levels=nmList)

  subtitle <- NULL

  palletName = palette
  plot <- ggplot2::ggplot()
  if(nrow(pointGraph)<=1000) {
    plot <- plot + ggplot2::geom_point(data=pointGraph,
                                       mapping=ggplot2::aes(x=FalsePositiveRate,
                                                           y=TruePositiveRate,
                                                           color=model,shape=model),
                                       alpha=0.5)
  }
  plot <- plot +
    ggplot2::geom_line(data=lineGraph,
                       mapping=ggplot2::aes(x=FalsePositiveRate,
                                           y=TruePositiveRate,
                                           color=model,linetype=model)) +
    ggplot2::geom_abline(slope=1,intercept=0,color='gray') +
    ggplot2::coord_fixed()

  if(!is.null(palette)) {
    plot <- plot +
      ggplot2::scale_fill_brewer(palette=palletName) +
      ggplot2::scale_color_brewer(palette=palletName)
  }
  plot <- plot + ggplot2::ggtitle(paste0(title,'\n',
                                         truthVar, '==', truthTarget, ' ~ model'),
                                  subtitle = subtitle) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1) +
    ggplot2::ylab('TruePositiveRate (Sensitivity)') +
    ggplot2::xlab('FalsePositiveRate (1 - Specificity)')
  plot
}

#' @export
#' @rdname ROCPlotList
ROCPlotPairList <- ROCPlotList

#' @export
#' @rdname ROCPlotList
ROCListPlot <- ROCPlotList


#' Compare two ROC plots.
#'
#' Plot two receiver operating characteristic curves from different data frames.
#'
#' Use this curve to compare model predictions to true outcome from two
#' data frames, each of which has its own model predictions and true outcome columns.
#'
#' If \code{palette} is NULL, plot colors will be chosen from the default ggplot2 palette. Setting \code{palette} to NULL
#' allows the user to choose a non-Brewer palette, for example with \code{\link[ggplot2:scale_manual]{scale_color_manual}}.
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
#' @param palette name of Brewer palette to color curves (can be NULL)
#'
#' @seealso \code{\link{ROCPlot}}
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
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
                         parallelCluster= NULL,
                         palette="Dark2") {
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

  FalsePositiveRate <- NULL  # don't look unbound in CRAN check
  TruePositiveRate <- NULL  # don't look unbound in CRAN check
  model <- NULL  # don't look unbound in CRAN check
  dataset <- NULL  # don't look unbound in CRAN check

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
  pointGraph <- rbind(rocList1$pointGraph, rocList2$pointGraph, stringsAsFactors = FALSE)
  lineGraph <- rbind(rocList1$lineGraph, rocList2$lineGraph, stringsAsFactors = FALSE)
  palletName = palette
  plot <- ggplot2::ggplot()
  if(nrow(pointGraph)<=1000) {
    plot <- plot +
      ggplot2::geom_point(data=pointGraph,
                          mapping=ggplot2::aes(x=FalsePositiveRate,
                                              y=TruePositiveRate,
                                              color=dataset,shape=dataset),
                          alpha=0.5)
  }
  plot <- plot +
    ggplot2::geom_line(data=lineGraph,
                       mapping=ggplot2::aes(x=FalsePositiveRate,
                                           y=TruePositiveRate,
                                           color=dataset,linetype=dataset)) +
    ggplot2::geom_abline(slope=1,intercept=0,color='gray') +
    ggplot2::coord_fixed()

  if(!is.null(palette)) {
    plot <- plot +
      ggplot2::scale_fill_brewer(palette=palletName) +
      ggplot2::scale_color_brewer(palette=palletName)
  }
  plot <- plot + ggplot2::ggtitle(title,
                                  subtitle = subtitle) +
    ggplot2::ylim(0,1) + ggplot2::xlim(0,1) +
    ggplot2::ylab('TruePositiveRate (Sensitivity)') +
    ggplot2::xlab('FalsePositiveRate (1 - Specificity)')
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
#' Note: any \code{arrange_} warning is a version incompatibility between \code{plotly} and \code{dplyr}.
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
#' if(FALSE && requireNamespace("plotly", quietly = TRUE)) {
#'    if (requireNamespace('data.table', quietly = TRUE)) {
#'		   # don't multi-thread during CRAN checks
#' 		   data.table::setDTthreads(1)
#'    }
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
                                '</br>False Positive Rate (1 - Specificity):', FalsePositiveRate,
                                '</br>True Positive Rate (Sensitivity):', TruePositiveRate)) ->.;
    plotly::layout(., title = paste(title,
                                    '\n</br>',
                                    outcomeCol, '==', outcomeTarget, ' ~ ', predCol,
                                    '\n</br>', subtitle))
}

