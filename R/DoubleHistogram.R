

DoubleHistogramPlot <- function(frame, xvar, truthVar,breaks=40,title='double histogram plot') {
  df <- data.frame(x=as.numeric(frame[[xvar]]),
                   y=as.character(frame[[truthVar]]),
                   stringsAsFactors=FALSE)
  breaksV <- hist(df[['x']],breaks=breaks,plot=FALSE)$breaks
  pf <- ddply(df,'y',function(sf) {
    counts = hist(sf[['x']],breaks=breaksV,plot=FALSE)
    rf = data.frame(count=counts$counts,
               stringsAsFactors=FALSE)
    rf[[xvar]] = counts$mids
    rf[[truthVar]] = sf$y[[1]]
    sm = loess(paste('count','~',xvar),rf)
    rf$smooth = pmax(0,predict(sm,rf,se=FALSE))
    # crudely match areas
    scale = sum(rf$count)/sum(rf$smooth)
    rf$smooth = rf$smooth*scale
    rf
  })
  # library(RColorBrewer)
  # display.brewer.all()
  palletName = "Dark2"
  # ConditionalDistributionPlot assumes no xlim set
  ggplot(data=pf,mapping=aes_string(x=xvar,
                                    color=truthVar,fill=truthVar,linetype=truthVar)) +
    geom_bar(mapping=aes_string(y='count'),
             stat='identity',alpha=0.5,position='identity') +
    geom_line(mapping=aes_string(y='smooth')) +
    scale_fill_brewer(palette=palletName) + scale_color_brewer(palette=palletName) +
    ggtitle(title)
}
