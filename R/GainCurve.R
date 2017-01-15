

# define some helper and reporting functions
# calculate area under the curve of numeric vectors x,y
# length(x)==length(y)
# y>=0, 0<=x<=1 x non-empty, and x strictly increasing
areaCalc <- function(x,y) {
  # append extra points to get rid of degenerate cases
  if(!all(diff(x)>0)) {
    stop("areaCalc x wasn't strinctly increasing")
  }
  if(x[1]<0) {
    x <- c(0,x)
    y <- c(0,y)
  }
  if(x[length(x)]<1) {
    x <- c(x,1)
    y <- c(y,1)
  }
  n <- length(x)
  sum(0.5*(y[-1]+y[-n])*(x[-1]-x[-n]))
}

relativeGiniScore <- function(modelValues,yValues) {
  d = data.frame(predcol=modelValues,truthcol=yValues)
  predord = order(d[['predcol']], decreasing=TRUE) # reorder, with highest first
  wizard = order(d[['truthcol']], decreasing=TRUE)
  npop = dim(d)[1]

  # data frame the cumulative prediction/truth as a function
  # of the fraction of the population we're considering, highest first
  results = data.frame(pctpop= (1:npop)/npop,
                       model = cumsum(d[predord,'truthcol'])/sum(d[['truthcol']]),
                       wizard = cumsum(d[wizard, 'truthcol'])/sum(d[['truthcol']]))

  # calculate the areas under each curve
  # gini score is 2* (area - 0.5)
  idealArea = areaCalc(results$pctpop,results$wizard) - 0.5
  modelArea = areaCalc(results$pctpop,results$model) - 0.5
  giniScore = modelArea/idealArea # actually, normalized gini score
  giniScore
}


#' Plot the gain curve of a sort-order.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' y = abs(rnorm(20)) + 0.1
#' x = abs(y + 0.5*rnorm(20))
#' frm = data.frame(model=x, value=y)
#' frm$costs=1
#' frm$costs[1]=5
#' frm$rate = with(frm, value/costs)
#' frm$isValuable = (frm$value >= as.numeric(quantile(frm$value, probs=0.8)))
#' WVPlots::GainCurvePlot(frm, "model", "value",
#'    title="Example Continuous Gain Curve")
#'
#' @export
GainCurvePlot = function(frame, xvar, truthVar,title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  if(!requireNamespace('reshape2',quietly = TRUE)) {
    warning("GainCurve needs reshape2")
    return(NULL)
  }
  pctpop <- NULL # used as a symbol, declare not an unbound variable
  pct_outcome <- NULL # used as a symbol, declare not an unbound variable
  sort_criterion <- NULL # used as a symbol, declare not an unbound variable
  truthcol <- as.numeric(frame[[truthVar]])
  predcol <- as.numeric(frame[[xvar]])
  # data frame of pred and truth, sorted in order of the predictions
  d = data.frame(predcol=predcol,truthcol=truthcol)
  predord = order(d[['predcol']], decreasing=TRUE) # reorder, with highest first
  wizard = order(d[['truthcol']], decreasing=TRUE)
  npop = dim(d)[1]

  # data frame the cumulative prediction/truth as a function
  # of the fraction of the population we're considering, highest first
  results = data.frame(pctpop= (1:npop)/npop,
                       model = cumsum(d[predord,'truthcol'])/sum(d[['truthcol']]),
                       wizard = cumsum(d[wizard, 'truthcol'])/sum(d[['truthcol']]))

  # calculate the areas under each curve
  # gini score is 2* (area - 0.5)
  idealArea = areaCalc(results$pctpop,results$wizard) - 0.5
  modelArea = areaCalc(results$pctpop,results$model) - 0.5
  giniScore = modelArea/idealArea # actually, normalized gini score

  # melt the frame into the tall form, for plotting
  results = reshape2::melt(results, id.vars="pctpop", measure.vars=c("model", "wizard"),
                 variable.name="sort_criterion", value.name="pct_outcome")
  # rename levels of sort criterion
  colorKey = c('model'='darkblue', 'wizard'='darkgreen')
  names(colorKey) = c(paste('model: sort by',xvar),paste('wizard: sort by',truthVar))
  modelKey = names(colorKey)[[1]]
  results[["sort_criterion"]] = names(colorKey)[results[["sort_criterion"]]]

  pString <- ''
  if(requireNamespace('sigr',quietly = TRUE)) {
    sp <- sigr::permutationScoreModel(predcol,truthcol,relativeGiniScore)
    pString <- sigr::render(sigr::wrapSignificance(sp$pValue),format='ascii')
    pString <- paste0('\nalt. hyp.: relGini(',xvar,')>permuted relGini, ',    pString)
  }

  # plot
  ges = ggplot2::aes(x=pctpop, y=pct_outcome,
                     color=sort_criterion,
                     shape=sort_criterion,
                     linetype=sort_criterion)
  gplot = ggplot2::ggplot(data=results) +
    ggplot2::geom_point(mapping=ges,alpha=0.5) +
    ggplot2::geom_line(mapping=ges) +
    ggplot2::geom_abline(mapping=ges,color="gray",slope=1,intercept=0) +
    ggplot2::geom_ribbon(data=results[results$sort_criterion==modelKey,,drop=FALSE],
                         mapping=ggplot2::aes(x=pctpop, ymin=pctpop,
                                              ymax=pct_outcome, color=sort_criterion),
                alpha=0.2,color=NA) +
    ggplot2::ggtitle(paste0("Gain curve, ", title, '\n',
                  truthVar, '~', xvar,
                  ', relative Gini score: ', format(giniScore,digits=2),
                  pString)) +
    ggplot2::xlab("fraction items in sort order") +
    ggplot2::ylab(paste("fraction total sum",truthVar)) +
    ggplot2::scale_x_continuous(breaks=seq(0,1,0.1)) +
    ggplot2::scale_y_continuous(breaks=seq(0,1,0.1)) +
    ggplot2::scale_color_manual(values=colorKey) +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position="bottom")
  gplot
}


makeRelativeGiniCostScorer <- function(costcol) {
  force(costcol)
  function(modelValues,yValues) {
    truthcol <- yValues
    predcol <- modelValues
    # data frame of pred and truth, sorted in order of the predictions
    d = data.frame(predcol=predcol,truthcol=truthcol,costcol=costcol)
    predord = order(d[['predcol']], decreasing=TRUE) # reorder, with highest first
    wizard = order(d[['truthcol']]/d[['costcol']], decreasing=TRUE)
    npop = dim(d)[1]

    # data frame the cumulative prediction/truth as a function
    # of the fraction of the population we're considering, highest first
    mName = paste("model: sort by model")
    resultsM = data.frame(pctpop = cumsum(d[predord,'costcol'])/sum(d[['costcol']]),
                          pct_outcome = cumsum(d[predord,'truthcol'])/sum(d[['truthcol']]),
                          sort_criterion=mName)
    wName = paste("wizard: sort by varlue/cost")
    resultsW = data.frame(pctpop = cumsum(d[wizard,'costcol'])/sum(d[['costcol']]),
                          pct_outcome = cumsum(d[wizard,'truthcol'])/sum(d[['truthcol']]),
                          sort_criterion=wName)
    results = rbind(resultsM,resultsW)

    # calculate the areas under each curve
    # gini score is 2* (area - 0.5)
    idealArea = areaCalc(resultsW$pctpop,resultsW$pct_outcome) - 0.5
    modelArea = areaCalc(resultsM$pctpop,resultsM$pct_outcome) - 0.5
    giniScore = modelArea/idealArea # actually, normalized gini score
    giniScore
  }
}


#' Plot the gain curve of a sort-order with costs.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param costVar cost of each item (drives x-axis sum)
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' y = abs(rnorm(20)) + 0.1
#' x = abs(y + 0.5*rnorm(20))
#' frm = data.frame(model=x, value=y)
#' frm$costs=1
#' frm$costs[1]=5
#' frm$rate = with(frm, value/costs)
#' frm$isValuable = (frm$value >= as.numeric(quantile(frm$value, probs=0.8)))
#' WVPlots::GainCurvePlotC(frm, "model", "costs", "value",
#'    title="Example Continuous Gain CurveC")
#'
#' @export
GainCurvePlotC = function(frame, xvar, costVar, truthVar, title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  pctpop <- NULL # used as a symbol, declare not an unbound variable
  pct_outcome <- NULL # used as a symbol, declare not an unbound variable
  sort_criterion <- NULL # used as a symbol, declare not an unbound variable
  truthcol <- as.numeric(frame[[truthVar]])
  predcol <- as.numeric(frame[[xvar]])
  costcol <- as.numeric(frame[[costVar]])
  # data frame of pred and truth, sorted in order of the predictions
  d = data.frame(predcol=predcol,truthcol=truthcol,costcol=costcol)
  predord = order(d[['predcol']], decreasing=TRUE) # reorder, with highest first
  wizard = order(d[['truthcol']]/d[['costcol']], decreasing=TRUE)
  npop = dim(d)[1]

  # data frame the cumulative prediction/truth as a function
  # of the fraction of the population we're considering, highest first
  mName = paste("model: sort by",xvar)
  resultsM = data.frame(pctpop = cumsum(d[predord,'costcol'])/sum(d[['costcol']]),
                        pct_outcome = cumsum(d[predord,'truthcol'])/sum(d[['truthcol']]),
                        sort_criterion=mName)
  wName = paste("wizard: sort by ",truthVar,'/',costVar)
  resultsW = data.frame(pctpop = cumsum(d[wizard,'costcol'])/sum(d[['costcol']]),
                        pct_outcome = cumsum(d[wizard,'truthcol'])/sum(d[['truthcol']]),
                        sort_criterion=wName)
  results = rbind(resultsM,resultsW)

  # calculate the areas under each curve
  # gini score is 2* (area - 0.5)
  idealArea = areaCalc(resultsW$pctpop,resultsW$pct_outcome) - 0.5
  modelArea = areaCalc(resultsM$pctpop,resultsM$pct_outcome) - 0.5
  giniScore = modelArea/idealArea # actually, normalized gini score


  # rename levels of sort criterion
  colorKey = c('model'='darkblue', 'wizard'='darkgreen')
  names(colorKey) = c(mName,wName)
  modelKey = mName
  results[["sort_criterion"]] = names(colorKey)[results[["sort_criterion"]]]

  pString <- ''
  if(requireNamespace('sigr',quietly=TRUE)) {
    relativeGiniCostScorer <- makeRelativeGiniCostScorer(costcol)
    sp <- sigr::permutationScoreModel(predcol,truthcol,relativeGiniCostScorer)
    pString <- sigr::render(sigr::wrapSignificance(sp$pValue),format='ascii')
    pString <- paste0('\nalt. hyp.: relGini(',xvar,')>permuted relGini, ',pString)
  }

  # plot
  ges = ggplot2::aes(x=pctpop, y=pct_outcome,
                     color=sort_criterion,
                     shape=sort_criterion,
                     linetype=sort_criterion)
  gplot = ggplot2::ggplot(data=results) +
    ggplot2::geom_point(mapping=ges,alpha=0.5) +
    ggplot2::geom_line(mapping=ges) +
    ggplot2::geom_abline(mapping=ges,color="gray",slope=1,intercept=0) +
    ggplot2::geom_ribbon(data=results[results$sort_criterion==modelKey,,drop=FALSE],
                         mapping=ggplot2::aes(x=pctpop, ymin=pctpop,
                                              ymax=pct_outcome, color=sort_criterion),
                alpha=0.2,color=NA) +
    ggplot2::ggtitle(paste0("Gain curve, ", title, '\n',
                  truthVar, '~', xvar,
            ', relative Gini score: ', format(giniScore,digits=2),
            pString)) +
    ggplot2::xlab(paste("fraction of sum",costVar," in sort order")) +
    ggplot2::ylab(paste("fraction total sum",truthVar)) +
    ggplot2::scale_x_continuous(breaks=seq(0,1,0.1)) +
    ggplot2::scale_y_continuous(breaks=seq(0,1,0.1)) +
    ggplot2::scale_color_manual(values=colorKey) +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position="bottom")
  gplot
}

# --------------------------------------------------------------

# find the y value that approximately corresponds to an x value on the gain curve
get_gainy = function(frame, xvar, truthVar, gainx) {
  # The sort order for predicted salary, decreasing
  ord = order(frame[[xvar]], decreasing=TRUE)

  # top 25 predicted salaries
  n = round(nrow(frame)*gainx)
  topN = ord[1:n]

  truth_topN= sum(frame[topN, truthVar])
  totalY = sum(frame[[truthVar]])
  round(100*truth_topN/totalY)/100  # two sig figs
}

#' Take the standard WVPlots gain curve and add extra notation
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param gainx the point on the x axis corresponding to the desired label
#' @param labelfun a function to return a label for the marked point
#' @param ...  no unarmed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' y = abs(rnorm(20)) + 0.1
#' x = abs(y + 0.5*rnorm(20))
#' frm = data.frame(model=x, value=y)
#' frm$costs=1
#' frm$costs[1]=5
#' frm$rate = with(frm, value/costs)
#' frm$isValuable = (frm$value >= as.numeric(quantile(frm$value, probs=0.8)))
#' gainx = 0.10  # get the top 10% most valuable points as sorted by the model
#' # make a function to calculate the label for the annotated point
#' labelfun = function(gx, gy) {
#'   pctx = gx*100
#'   pcty = gy*100
#'
#'   paste("The top ", pctx, "% most valuable points by the model\n",
#'         "are ", pcty, "% of total actual value", sep='')
#' }
#' WVPlots::GainCurvePlotWithNotation(frm, "model", "value",
#'    title="Example Gain Curve with annotation",
#'    gainx=gainx,labelfun=labelfun)
#'
#' @export
GainCurvePlotWithNotation = function(frame, xvar, truthVar, title, gainx, labelfun, ...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  gainy = get_gainy(frame, xvar, truthVar, gainx)
  label = labelfun(gainx, gainy)
  gp = GainCurvePlot(frame, xvar, truthVar, title) +
    ggplot2::geom_vline(xintercept=gainx, color="red", alpha=0.5) +
    ggplot2::geom_hline(yintercept=gainy, color="red", alpha=0.5) +
    ggplot2::scale_shape_discrete(guide=FALSE) +
    ggplot2::annotate(geom="text", x=gainx+0.01, y=gainy-0.01,
              color="black", label=label, vjust="top", hjust="left")
  gp
}

