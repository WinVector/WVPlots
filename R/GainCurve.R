


#' @export
GainCurvePlot = function(frame, xvar, truthVar,title='Gain Curve') {
  checkArgs(frame,xvar,truthVar)
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
  results = melt(results, id.vars="pctpop", measure.vars=c("model", "wizard"),
                 variable.name="sort_criterion", value.name="pct_outcome")
  # rename levels of sort criterion
  colorKey = c('model'='darkblue', 'wizard'='darkgreen')
  names(colorKey) = c(paste('model: sort by',xvar),paste('wizard: sort by',truthVar))
  modelKey = names(colorKey)[[1]]
  results[["sort_criterion"]] = names(colorKey)[results[["sort_criterion"]]]
  # plot
  gplot = ggplot(data=results, aes(x=pctpop, y=pct_outcome,
                                   color=sort_criterion,
                                   shape=sort_criterion,
                                   linetype=sort_criterion)) +
    geom_point() + geom_line() +
    geom_abline(color="gray") +
    geom_ribbon(data=results[results$sort_criterion==modelKey,,drop=FALSE],
                aes(x=pctpop, ymin=pctpop,ymax=pct_outcome, color=sort_criterion),
                alpha=0.2,color=NA) +
    ggtitle(paste("Gain curve,", title, '\n',
                  truthVar, '~', xvar, '\n',
                  'relative Gini score', format(giniScore,digits=2))) +
    xlab("% items in sort order") + ylab(paste("% total sum",truthVar)) +
    scale_x_continuous(breaks=seq(0,1,0.1)) +
    scale_y_continuous(breaks=seq(0,1,0.1)) +
    scale_color_manual(values=colorKey)
  gplot
}
