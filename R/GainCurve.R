
# define some helper and reporting functions
# calulcate area under the curve of numeric vectors x,y
# length(x)==length(y)
# y>=0, 0<=x<=1 and x increasing
areaCalc <- function(x,y) {
  # append extra points to get rid of degenerate cases
  x <- c(0,x,1)
  y <- c(0,y,1)
  n <- length(x)
  sum(0.5*(y[-1]+y[-n])*(x[-1]-x[-n]))
}


gainCurve = function(frame, xvar, truthVar,title='Gain Curve') {
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

  gplot = ggplot(data=results, aes(x=pctpop, y=pct_outcome, color=sort_criterion)) +
    geom_point() + geom_line() +
    geom_abline(color="gray") +
    geom_ribbon(data=results[results$sort_criterion=='model',,drop=FALSE],
                aes(x=pctpop, ymin=pctpop,ymax=pct_outcome, color=sort_criterion),
                alpha=0.2,color=NA) +
    ggtitle(paste("Gain curve,", title, '\n',
                  'relative Gini score', format(giniScore,digits=2))) +
    xlab("% items in score order") + ylab("% total category") +
    scale_x_continuous(breaks=seq(0,1,0.1)) +
    scale_y_continuous(breaks=seq(0,1,0.1)) +
    scale_color_manual(values=c('model'='darkblue', 'wizard'='darkgreen'))
  gplot
}
