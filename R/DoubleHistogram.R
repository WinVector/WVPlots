

doubleHistogram <- function(frame, xvar, truthVar,breaks=40,title='double histogram plot') {
  df <- data.frame(x=as.numeric(frame[[xvar]]),
                   y=as.character(frame[[truthVar]]),
                   stringsAsFactors=FALSE)
  breaksV <- hist(df[['x']],breaks=breaks,plot=FALSE)$breaks
  pf <- ddply(df,'y',function(sf) {
    counts = hist(sf[['x']],breaks=breaksV,plot=FALSE)
    rf = data.frame(truth=sf$y[[1]],
               pred=counts$mids,
               count=counts$counts,
               stringsAsFactors=FALSE)
    sm = loess(count~pred,rf)
    rf$smooth = pmax(0,predict(sm,rf,se=FALSE))
    rf
  })
  ggplot() +
    geom_bar(data=pf,mapping=aes_string(x='pred',color='truth',fill='truth',y='count'),
             stat='identity',alpha=0.5,position='identity') +
    geom_line(data=pf,mapping=aes_string(x='pred',color='truth',y='smooth')) +
    ggtitle(title)
}
