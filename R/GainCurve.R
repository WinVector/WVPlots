


# define some helper and reporting functions
# calculate area under the curve of numeric vectors x,y
# length(x)==length(y)
# y>=0, 0<=x<=1 x non-empty, and x strictly increasing
areaCalc <- function(x, y) {
  # append extra points to get rid of degenerate cases
  if (!all(diff(x) > 0)) {
    stop("areaCalc x wasn't strinctly increasing")
  }
  if (x[1] < 0) {
    x <- c(0, x)
    y <- c(0, y)
  }
  if (x[length(x)] < 1) {
    x <- c(x, 1)
    y <- c(y, 1)
  }
  n <- length(x)
  sum(0.5 * (y[-1] + y[-n]) * (x[-1] - x[-n]))
}

relativeGiniScore <- function(modelValues, yValues) {
  d = data.frame(predcol = modelValues, truthcol = yValues)
  n <- nrow(d)
  predord = order(d[['predcol']],
                  sample.int(n, n, replace = FALSE),
                  decreasing = TRUE) # reorder, with highest first
  wizard = order(d[['truthcol']],
                 sample.int(n, n, replace = FALSE),
                 decreasing = TRUE)
  npop = dim(d)[1]

  # data frame the cumulative prediction/truth as a function
  # of the fraction of the population we're considering, highest first
  results = data.frame(
    pctpop = (1:npop) / npop,
    model = cumsum(d[predord, 'truthcol']) / sum(d[['truthcol']]),
    wizard = cumsum(d[wizard, 'truthcol']) / sum(d[['truthcol']])
  )

  # calculate the areas under each curve
  # gini score is 2* (area - 0.5)
  idealArea = areaCalc(results$pctpop, results$wizard) - 0.5
  modelArea = areaCalc(results$pctpop, results$model) - 0.5
  modelArea / idealArea # actually, normalized gini score
}

# sample with respect to multiple orders to get smooth sampling
thin_frame_by_orders <- function(d, cols, groupcol, large_count) {
  n <- nrow(d)
  if(n<=(length(cols)+1)*large_count) {
    return(d)
  }
  takes <- c()
  for(ci in cols) {
    ordi <- order(d[[groupcol]],
                  d[[ci]],
                  sample.int(n, n, replace = FALSE))
    takesi <- seq(1, n, length.out = large_count)
    sg <- d[[groupcol]][ordi]
    deltas <- which(sg[-1]!=sg[-n])
    boundsi <- NULL
    if(length(deltas>1)) {
      boundsi <- pmin(n, c(deltas, 1+deltas))
    }
    invperm <- wrapr::invert_perm(ordi)
    takes <- sort(unique(c(takes,
                           invperm[takesi],
                           invperm[boundsi])))
  }
  if(2*length(takes)>=n) {
    return(d)
  }
  d[takes, , drop = FALSE]
}

#' Plot the cumulative gain curve of a sort-order.
#'
#' Plot the cumulative gain curve of a sort-order.
#'
#' The use case for this visualization is to compare a predictive model
#' score to an actual outcome (either binary (0/1) or continuous). In this case the
#' gain curve plot measures how well the model score sorts the data compared
#' to the true outcome value.
#'
#' The x-axis represents the fraction of items seen when sorted by score, and the
#' y-axis represents the cumulative summed true outcome represented by the items seen so far.
#' See, for example,
#' \url{https://www.ibm.com/support/knowledgecenter/SSLVMB_24.0.0/spss/tutorials/mlp_bankloan_outputtype_02.html}.
#'
#' For comparison, \code{GainCurvePlot} also plots the "wizard curve": the gain curve when the
#' data is sorted according to its true outcome.
#'
#' To improve presentation quality, the plot is limited to approximately \code{large_count} points (default: 1000).
#' For larger data sets, the data is appropriately randomly sampled down before plotting.
#'
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model score) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param estimate_sig logical, if TRUE compute significance.
#' @param large_count numeric, upper bound target for number of plotting points.
#' @param truth_target if not NULL compare to this scalar value.
#' @param model_color color for the model curve
#' @param wizard_color color for the "wizard" (best possible) curve
#' @param shadow_color color for the shaded area under the curve
#' @examples
#'
#' set.seed(34903490)
#' y = abs(rnorm(20)) + 0.1
#' x = abs(y + 0.5*rnorm(20))
#' frm = data.frame(model=x, value=y)
#' WVPlots::GainCurvePlot(frm, "model", "value",
#'    title="Example Continuous Gain Curve")
#'
#' @export
GainCurvePlot = function(frame, xvar, truthVar, title,
                         ...,
                         estimate_sig = FALSE,
                         large_count = 1000,
                         truth_target = NULL,
                         model_color='darkblue',
                         wizard_color='darkgreen',
                         shadow_color='darkgray') {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = list(xvar = xvar, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::GainCurvePlot")
  pctpop <- NULL # used as a symbol, declare not an unbound variable
  pct_outcome <-
    NULL # used as a symbol, declare not an unbound variable
  sort_criterion <-
    NULL # used as a symbol, declare not an unbound variable
  if(!is.null(truth_target)) {
    truthcol <- as.numeric(frame[[truthVar]]==truth_target)
  } else {
    truthcol <- as.numeric(frame[[truthVar]])
  }
  predcol <- as.numeric(frame[[xvar]])
  # data frame of pred and truth, sorted in order of the predictions
  d = data.frame(predcol = predcol, truthcol = truthcol)
  n <- nrow(d)
  predord = order(d[['predcol']],
                  sample.int(n, n, replace = FALSE),
                  decreasing = TRUE) # reorder, with highest first
  wizard = order(d[['truthcol']],
                 sample.int(n, n, replace = FALSE),
                 decreasing = TRUE)
  npop = dim(d)[1]

  # data frame the cumulative prediction/truth as a function
  # of the fraction of the population we're considering, highest first
  results = data.frame(
    pctpop = (1:npop) / npop,
    model = cumsum(d[predord, 'truthcol']) / sum(d[['truthcol']]),
    wizard = cumsum(d[wizard, 'truthcol']) / sum(d[['truthcol']])
  )

  # calculate the areas under each curve
  # gini score is 2* (area - 0.5)
  idealArea = areaCalc(results$pctpop, results$wizard) - 0.5
  modelArea = areaCalc(results$pctpop, results$model) - 0.5
  relGiniScore = modelArea / idealArea # actually, normalized gini score

  # transform the frame into the tall form, for plotting
  r1 <- data.frame(pctpop = results$pctpop,
                   pct_outcome = results$model,
                   sort_criterion = "model",
                   stringsAsFactors = FALSE)
  r2 <- data.frame(pctpop = results$pctpop,
                   pct_outcome = results$wizard,
                   sort_criterion = "wizard",
                   stringsAsFactors = FALSE)
  results <- rbind(r1, r2, stringsAsFactors = FALSE)
  # rename sort_criterion
  sortKeyM <- c('model' = paste('model: sort by', xvar),
               'wizard' = paste('wizard: sort by', truthVar))
  results$sort_criterion <- sortKeyM[results$sort_criterion]
  # rename levels of sort criterion
  colorKey = as.character(sortKeyM) %:=% c(model_color, wizard_color)
  names(colorKey) = c(paste('model: sort by', xvar),
                      paste('wizard: sort by', truthVar))
  modelKey = names(colorKey)[[1]]

  pString <- ''
  if(estimate_sig && requireNamespace('sigr', quietly = TRUE)) {
    sp <-
      sigr::permutationScoreModel(predcol, truthcol, relativeGiniScore)
    pString <-
      sigr::render(sigr::wrapSignificance(sp$pValue), format = 'ascii')
    pString <-
      paste0('\nalt. hyp.: relGini(',
             xvar,
             ')>permuted relGini, ',
             pString)
  }

  # cut down the number of points
  results <- thin_frame_by_orders(results,
                                  c("pctpop", "pct_outcome"),
                                  "sort_criterion",
                                  large_count)

  # plot
  gplot = ggplot2::ggplot(data = results) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = pctpop,
        y = pct_outcome,
        color = sort_criterion,
        shape = sort_criterion
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = pctpop,
        y = pct_outcome,
        color = sort_criterion,
        linetype = sort_criterion
      )
    ) +
    ggplot2::geom_abline(
      color = 'gray',
      slope = 1,
      intercept = 0
    ) +
    ggplot2::geom_ribbon(
      data = results[results$sort_criterion == modelKey, , drop = FALSE],
      mapping = ggplot2::aes(
        x = pctpop,
        ymin = pctpop,
        ymax = pct_outcome,
        color = sort_criterion
      ),
      alpha = 0.3,
      color = NA,
      fill = shadow_color
    ) +
    ggplot2::ggtitle(
      paste0(
        title,
        '\n',
        truthVar,
        '~',
        xvar),
      subtitle=paste0(
        'Gini score: ',
        format(modelArea, digits = 2),
        ', relative Gini score: ',
        format(relGiniScore, digits = 2),
        pString
      )
    ) +
    ggplot2::xlab("fraction items in sort order") +
    ggplot2::ylab(paste("fraction total sum", truthVar)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    ggplot2::scale_color_manual(values = colorKey) +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "bottom")
  gplot
}


makeRelativeGiniCostScorer <- function(costcol) {
  force(costcol)
  function(modelValues, yValues) {
    truthcol <- yValues
    predcol <- modelValues
    # data frame of pred and truth, sorted in order of the predictions
    d = data.frame(predcol = predcol,
                   truthcol = truthcol,
                   costcol = costcol)
    n <- nrow(d)
    predord = order(d[['predcol']],
                    sample.int(n, n, replace = FALSE),
                    decreasing = TRUE) # reorder, with highest first
    wizard = order(d[['truthcol']] / d[['costcol']],
                   sample.int(n, n, replace = FALSE),
                   decreasing = TRUE)
    npop = dim(d)[1]

    # data frame the cumulative prediction/truth as a function
    # of the fraction of the population we're considering, highest first
    mName = paste("model: sort by model")
    resultsM = data.frame(
      pctpop = cumsum(d[predord, 'costcol']) / sum(d[['costcol']]),
      pct_outcome = cumsum(d[predord, 'truthcol']) /
        sum(d[['truthcol']]),
      sort_criterion = mName
    )
    wName = paste("wizard: sort by varlue/cost")
    resultsW = data.frame(
      pctpop = cumsum(d[wizard, 'costcol']) / sum(d[['costcol']]),
      pct_outcome = cumsum(d[wizard, 'truthcol']) /
        sum(d[['truthcol']]),
      sort_criterion = wName
    )
    results = rbind(resultsM, resultsW, stringsAsFactors = FALSE)

    # calculate the areas under each curve
    # gini score is 2* (area - 0.5)
    idealArea = areaCalc(resultsW$pctpop, resultsW$pct_outcome) - 0.5
    modelArea = areaCalc(resultsM$pctpop, resultsM$pct_outcome) - 0.5
    modelArea / idealArea # actually, normalized gini score
  }
}

#' Plot the cumulative gain curve of a sort-order with costs.
#'
#' Plot the cumulative gain curve of a sort-order with costs.
#'
#' \code{GainCurvePlotC} plots a cumulative gain curve for the case where
#' items have an additional cost, in addition to an outcome value.
#'
#' The x-axis represents the fraction of total cost experienced when items are sorted by score, and the
#' y-axis represents the cumulative summed true outcome represented by the items seen so far.
#'
#' For comparison, \code{GainCurvePlotC} also plots the "wizard curve": the gain curve when the
#' data is sorted according to its true outcome/cost (the optimal sort order).
#'
#' To improve presentation quality, the plot is limited to approximately \code{large_count} points (default: 1000).
#' For larger data sets, the data is appropriately randomly sampled down before plotting.
#'
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model score) column in frame
#' @param costVar cost of each item (drives x-axis sum)
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param estimate_sig logical, if TRUE compute significance
#' @param large_count numeric, upper bound target for number of plotting points
#' @param model_color color for the model curve
#' @param wizard_color color for the "wizard" (best possible) curve
#' @param shadow_color color for the shaded area under the curve
#'
#' @seealso \code{\link{GainCurvePlot}}
#'
#' @examples
#'
#' set.seed(34903490)
#' y = abs(rnorm(20)) + 0.1
#' x = abs(y + 0.5*rnorm(20))
#' frm = data.frame(model=x, value=y)
#' frm$costs=1
#' frm$costs[1]=5
#' WVPlots::GainCurvePlotC(frm, "model", "costs", "value",
#'    title="Example Continuous Gain CurveC")
#'
#' @export
GainCurvePlotC = function(frame, xvar, costVar, truthVar, title,
                          ...,
                          estimate_sig = FALSE,
                          large_count = 1000,
                          model_color='darkblue',
                          wizard_color='darkgreen',
                          shadow_color='darkgray') {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = list(xvar = xvar, costVar= costVar, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::GainCurvePlotC")
  pctpop <- NULL # used as a symbol, declare not an unbound variable
  pct_outcome <-
    NULL # used as a symbol, declare not an unbound variable
  sort_criterion <-
    NULL # used as a symbol, declare not an unbound variable
  truthcol <- as.numeric(frame[[truthVar]])
  predcol <- as.numeric(frame[[xvar]])
  costcol <- as.numeric(frame[[costVar]])
  # data frame of pred and truth, sorted in order of the predictions
  d = data.frame(predcol = predcol,
                 truthcol = truthcol,
                 costcol = costcol)
  n <- nrow(d)
  predord = order(d[['predcol']],
                  sample.int(n, n, replace = FALSE),
                  decreasing = TRUE) # reorder, with highest first
  wizard = order(d[['truthcol']] / d[['costcol']],
                 sample.int(n, n, replace = FALSE),
                 decreasing = TRUE)
  npop = dim(d)[1]

  # data frame the cumulative prediction/truth as a function
  # of the fraction of the population we're considering, highest first
  mName = paste("model: sort by", xvar)
  resultsM = data.frame(
    pctpop = cumsum(d[predord, 'costcol']) / sum(d[['costcol']]),
    pct_outcome = cumsum(d[predord, 'truthcol']) / sum(d[['truthcol']]),
    sort_criterion = mName
  )
  wName = paste("wizard: sort by ", truthVar, '/', costVar)
  resultsW = data.frame(
    pctpop = cumsum(d[wizard, 'costcol']) / sum(d[['costcol']]),
    pct_outcome = cumsum(d[wizard, 'truthcol']) / sum(d[['truthcol']]),
    sort_criterion = wName
  )
  results = rbind(resultsM, resultsW, stringsAsFactors = FALSE)

  # calculate the areas under each curve
  # gini score is 2* (area - 0.5)
  idealArea = areaCalc(resultsW$pctpop, resultsW$pct_outcome) - 0.5
  modelArea = areaCalc(resultsM$pctpop, resultsM$pct_outcome) - 0.5
  relGiniScore = modelArea / idealArea # actually, normalized gini score


  # map names to colors
  colorKey = c('model' = model_color, 'wizard' = wizard_color)
  names(colorKey) = c(mName, wName)
  modelKey = mName

  pString <- ''
  if (estimate_sig && requireNamespace('sigr', quietly = TRUE)) {
    relativeGiniCostScorer <- makeRelativeGiniCostScorer(costcol)
    sp <-
      sigr::permutationScoreModel(predcol, truthcol, relativeGiniCostScorer)
    pString <-
      sigr::render(sigr::wrapSignificance(sp$pValue), format = 'ascii')
    pString <-
      paste0('\nalt. hyp.: relGini(',
             xvar,
             ')>permuted relGini, ',
             pString)
  }

  # cut down the number of points
  results <- thin_frame_by_orders(results,
                                  c("pctpop", "pct_outcome"),
                                  "sort_criterion",
                                  large_count)

  # plot
  gplot = ggplot2::ggplot(data = results) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = pctpop,
        y = pct_outcome,
        color = sort_criterion,
        shape = sort_criterion
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = pctpop,
        y = pct_outcome,
        color = sort_criterion,
        linetype = sort_criterion
      )
    ) +
    ggplot2::geom_abline(
      color = "gray",
      slope = 1,
      intercept = 0
    ) +
    ggplot2::geom_ribbon(
      data = results[results$sort_criterion == modelKey, , drop = FALSE],
      mapping = ggplot2::aes(
        x = pctpop,
        ymin = pctpop,
        ymax = pct_outcome,
        color = sort_criterion
      ),
      alpha = 0.2,
      color = NA,
      fill = shadow_color
    ) +
    ggplot2::ggtitle(
      paste0(
        title,
        '\n',
        truthVar,
        '~',
        xvar),
      subtitle=paste0(
        'Gini score: ',
        format(modelArea, digits = 2),
        ', relative Gini score: ',
        format(relGiniScore, digits = 2),
        pString
      )
    ) +
    ggplot2::xlab(paste("fraction of sum", costVar, " in sort order")) +
    ggplot2::ylab(paste("fraction total sum", truthVar)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    ggplot2::scale_color_manual(values = colorKey) +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "bottom")
  gplot
}

# --------------------------------------------------------------

# find the y value that approximately corresponds to an x value on the gain curve
get_gainy = function(frame, xvar, truthVar, gainx) {
  # The sort order for predicted salary, decreasing
  n <- nrow(frame)
  ord = order(frame[[xvar]],
              sample.int(n, n, replace = FALSE),
              decreasing = TRUE)

  # top 25 predicted salaries
  n = round(nrow(frame) * gainx)
  topN = ord[1:n]

  truth_topN = sum(frame[topN, truthVar])
  totalY = sum(frame[[truthVar]])
  truth_topN / totalY
}

#' Plot the cumulative gain curve of a sort-order with extra notation
#'
#' Plot the cumulative gain curve of a sort-order with extra notation.
#'
#' This is the standard gain curve plot (see \code{\link{GainCurvePlot}}) with
#' a label attached to a particular value of x. The label is created by
#' a function \code{labelfun}, which takes as inputs the x and y coordinates
#' of a label and returns a string (the label).
#'
#' By default, uses the model to calculate the y value of the calculated point;
#' to use the wizard curve, set \code{sort_by_model = FALSE}
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model score) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param gainx the point on the x axis corresponding to the desired label
#' @param labelfun a function to return a label for the marked point
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param sort_by_model logical, if TRUE use the model to calculate gainy, else use wizard.
#' @param estimate_sig logical, if TRUE compute significance
#' @param large_count numeric, upper bound target for number of plotting points
#' @param model_color color for the model curve
#' @param wizard_color color for the "wizard" (best possible) curve
#' @param shadow_color color for the shaded area under the curve
#' @param crosshair_color color for the annotation location lines
#' @param text_color color for the annotation text
#' @seealso \code{\link{GainCurvePlot}}
#'
#' @examples
#'
#' set.seed(34903490)
#' y = abs(rnorm(20)) + 0.1
#' x = abs(y + 0.5*rnorm(20))
#' frm = data.frame(model=x, value=y)
#' gainx = 0.25  # get the predicted top 25% most valuable points as sorted by the model
#' # make a function to calculate the label for the annotated point
#' labelfun = function(gx, gy) {
#'   pctx = gx*100
#'   pcty = gy*100
#'
#'   paste("The predicted top ", pctx, "% most valuable points by the model\n",
#'         "are ", pcty, "% of total actual value", sep='')
#' }
#' WVPlots::GainCurvePlotWithNotation(frm, "model", "value",
#'    title="Example Gain Curve with annotation",
#'    gainx=gainx,labelfun=labelfun)
#'
#' # now get the top 25% actual most valuable points
#'
#'labelfun = function(gx, gy) {
#'   pctx = gx*100
#'   pcty = gy*100
#'
#'   paste("The actual top ", pctx, "% most valuable points\n",
#'         "are ", pcty, "% of total actual value", sep='')
#' }
#'
#' WVPlots::GainCurvePlotWithNotation(frm, "model", "value",
#'    title="Example Gain Curve with annotation",
#'    gainx=gainx,labelfun=labelfun, sort_by_model=FALSE)
#'
#' @export
GainCurvePlotWithNotation = function(frame,
                                     xvar,
                                     truthVar,
                                     title,
                                     gainx,
                                     labelfun,
                                     ...,
                                     sort_by_model = TRUE,
                                     estimate_sig = FALSE,
                                     large_count = 1000,
                                     model_color='darkblue',
                                     wizard_color='darkgreen',
                                     shadow_color='darkgray',
                                     crosshair_color = 'red',
                                     text_color='black') {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = list(xvar = xvar, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::GainCurvePlotWithNotation")
  if(sort_by_model) {
    gainy = get_gainy(frame, xvar, truthVar, gainx)
  }
  else {
    gainy = get_gainy(frame, truthVar, truthVar, gainx)
  }
  gainy_p = round(100 * gainy) / 100  # two sig figs
  label = labelfun(gainx, gainy_p)
  gp = GainCurvePlot(frame, xvar, truthVar, title,
                     estimate_sig = estimate_sig,
                     large_count = large_count,
                     model_color = model_color,
                     wizard_color = wizard_color,
                     shadow_color = shadow_color) +
    ggplot2::geom_vline(xintercept = gainx,
                        color = crosshair_color,
                        alpha = 0.5) +
    ggplot2::geom_hline(yintercept = gainy,
                        color = crosshair_color,
                        alpha = 0.5) +
    ggplot2::scale_shape_discrete(guide = FALSE) +
    ggplot2::annotate(
      geom = "text",
      x = gainx + 0.01,
      y = gainy - 0.01,
      color = text_color,
      label = label,
      vjust = "top",
      hjust = "left"
    )
  gp
}


#' Plot the cumulative gain curves of a sort-order.
#'
#' Plot the cumulative gain curves of a sort-order.
#'
#' The use case for this visualization is to compare a predictive model
#' score to an actual outcome (either binary (0/1) or continuous). In this case the
#' gain curve plot measures how well the model score sorts the data compared
#' to the true outcome value.
#'
#' The x-axis represents the fraction of items seen when sorted by score, and the
#' y-axis represents the gain seen so far (cumulative value of model over cummulative value of random selection)..
#'
#'
#'
#' @param frame data frame to get values from
#' @param xvars name of the independent (input or model score) columns in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param truth_target if not NULL compare to this scalar value.
#' @param palette color palette for the model curves
#' @examples
#'
#' set.seed(34903490)
#' y = abs(rnorm(20)) + 0.1
#' x = abs(y + 0.5*rnorm(20))
#' frm = data.frame(model=x, value=y)
#' WVPlots::GainCurvePlotList(frm, c("model", "value"), "value",
#'    title="Example Continuous gain Curves")
#'
#' @export
GainCurvePlotList = function(frame, xvars, truthVar, title,
                             ...,
                             truth_target = NULL,
                             palette = 'Dark2') {
  frame <- check_frame_args_list(...,
                                 frame = frame,
                                 name_var_list = c(xvars = xvars, truthVar = truthVar),
                                 title = title,
                                 funname = "WVPlots::GainCurvePlot")
  curve <- percent_total <- NULL  # mark as not unbound
  pct_outcome <- pctpop <- sort_criterion <- NULL  # mark as not unbound variables
  if(!is.null(truth_target)) {
    truthcol <- as.numeric(frame[[truthVar]]==truth_target)
  } else {
    truthcol <- as.numeric(frame[[truthVar]])
  }
  n <- nrow(frame)

  # data frame the cumulative prediction/truth as a function
  # of the fraction of the population we're considering, highest first
  results <- data.frame(
    pctpop = (1:n) / n
  )
  for(xvar in xvars) {
    predcol <- as.numeric(frame[[xvar]])
    # data frame of pred and truth, sorted in order of the predictions
    d = data.frame(predcol = predcol, truthcol = truthcol)
    predord <- order(d$predcol,
                     sample.int(n, n, replace = FALSE),
                     decreasing = TRUE) # reorder, with highest first
    gain <- cumsum(d[predord, 'truthcol']) / sum(d[['truthcol']])
    results[[xvar]] <- gain
  }

  # transform the frame into the tall form, for plotting
  results <- cdata::pivot_to_blocks(results,
                                    nameForNewKeyColumn = 'curve',
                                    nameForNewValueColumn = 'percent_total',
                                    columnsToTakeFrom = setdiff(colnames(results), 'pctpop'))

  # plot
  gplot = ggplot2::ggplot(
    data = results,
    mapping = ggplot2::aes(
      x = pctpop,
      y = percent_total,
      color = curve)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::xlab("fraction items in sort order") +
    ggplot2::ylab("percent of total value") +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "bottom")
  gplot
}


#' @export
#' @rdname GainCurvePlotList
GainCurveListPlot <- GainCurvePlotList
