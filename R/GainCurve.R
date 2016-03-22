
#' Plot the gain curve of a sort-order.
#'
#' @param frame data frame to get values from
#' @param xvar name of the indepement (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,y=y,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' frm$absY <- abs(frm$y)
#' frm$posY = frm$y > 0
#' frm$costX = 1
#' WVPlots::GainCurvePlot(frm, "x", "absY", title="Example Continuous Gain Curve")
#'
#' @export
GainCurvePlot = function(frame, xvar, truthVar,title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
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
    ggplot2::ggtitle(paste("Gain curve,", title, '\n',
                  truthVar, '~', xvar, '\n',
                  'relative Gini score', format(giniScore,digits=2))) +
    ggplot2::xlab("fraction items in sort order") +
    ggplot2::ylab(paste("fraction total sum",truthVar)) +
    ggplot2::scale_x_continuous(breaks=seq(0,1,0.1)) +
    ggplot2::scale_y_continuous(breaks=seq(0,1,0.1)) +
    ggplot2::scale_color_manual(values=colorKey) +
    ggplot2::coord_fixed()
  gplot
}

#' Plot the gain curve of a sort-order in horizontal orientation.
#'
#' @param frame data frame to get values from
#' @param xvar name of the indepement (input or model) column in frame
#' @param costVar cost of each item (drives x-axis sum)
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,y=y,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' frm$absY <- abs(frm$y)
#' frm$posY = frm$y > 0
#' frm$costX = 1
#' WVPlots::GainCurvePlotC(frm, "x", "costX", "absY", title="Example Continuous Gain CurveC")
#'
#' @export
GainCurvePlotC = function(frame, xvar, costVar, truthVar, title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
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
    ggplot2::ggtitle(paste("Gain curve,", title, '\n',
                  truthVar, '~', xvar, '\n',
            'relative Gini score', format(giniScore,digits=2))) +
    ggplot2::xlab(paste("fraction ",costVar,"cost in sort order")) +
    ggplot2::ylab(paste("fraction total sum",truthVar)) +
    ggplot2::scale_x_continuous(breaks=seq(0,1,0.1)) +
    ggplot2::scale_y_continuous(breaks=seq(0,1,0.1)) +
    ggplot2::scale_color_manual(values=colorKey) +
    ggplot2::coord_fixed()
  gplot
}

