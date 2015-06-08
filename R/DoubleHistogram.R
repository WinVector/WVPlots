
#' @export
DoubleHistogramPlot <- function(frame, xvar, truthVar,breaks=40,title='double histogram plot') {
  checkArgs(frame,xvar,truthVar)
  df <- data.frame(x=as.numeric(frame[[xvar]]),
                   y=as.character(frame[[truthVar]]),
                   stringsAsFactors=FALSE)
  breaksV <- hist(df[['x']],breaks=breaks,plot=FALSE)$breaks
  yVals <- sort(unique(df[['y']]))
  signs <- (-1)^seq_len(length(yVals))
  names(signs) <- yVals
  pf <- ddply(df,'y',function(sf) {
    yGroup <- sf$y[[1]]
    si <- signs[[yGroup]]
    counts <- hist(sf[['x']],breaks=breaksV,plot=FALSE)
    rf <- data.frame(count=counts$counts,
               stringsAsFactors=FALSE)
    rf[[xvar]] <- counts$mids
    rf[[truthVar]] <- yGroup
    sm <- loess(paste('count','~',xvar),rf)
    rf$smooth <- pmax(0,predict(sm,rf,se=FALSE))
    # crudely match areas
    scale <- sum(rf$count)/sum(rf$smooth)
    rf$smooth <- si*rf$smooth*scale
    rf[['count']] <- si*rf[['count']]
    rf
  })
  # library(RColorBrewer)
  # display.brewer.all()
  palletName <- "Dark2"
  # build a net effect curve
  netF <- ddply(pf,'x',summarize,
                      count=sum(count))
  sm <- loess(paste('count','~','x'),netF)
  pf$net <- predict(sm,pf,se=FALSE)
  # ConditionalDistributionPlot assumes no xlim set
  # ConditionalDistributionPlot assumes no scale_y_continuous set
  plot <- ggplot(data=pf,mapping=aes_string(x=xvar,
                                    color=truthVar,fill=truthVar,linetype=truthVar)) +
    geom_bar(mapping=aes_string(y='count'),
             stat='identity',alpha=0.5,position='identity') +
    geom_line(mapping=aes_string(y='smooth')) +
    geom_line(mapping=aes_string(y='net'),linetype=3,color='black') +
    scale_fill_brewer(palette=palletName) + scale_color_brewer(palette=palletName) +
    ggtitle(title)
  plot
}
