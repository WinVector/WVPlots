

doubleDensity <- function(frame, xvar, truthVar,title='double density plot') {
  df <- data.frame(x=as.numeric(frame[[xvar]]),
                   y=as.character(frame[[truthVar]]),
                   stringsAsFactors=FALSE)
  pf <- ddply(df,'y',function(sf) {
    dens <- density(sf[['x']],adjust=0.5,
                   from=min(sf[['x']]),to=max(sf[['x']]))
    rf <- data.frame(density=dens$y,
               stringsAsFactors=FALSE)
    rf[[xvar]] <- dens$x
    rf[[truthVar]] <- sf$y[[1]]
    rf
  })
  ggplot(pf,mapping=aes_string(x=xvar,y='density',ymax='density',color=truthVar,fill=truthVar)) +
    geom_line() + geom_ribbon(mapping=aes(ymin=0),alpha=0.5,color=NA) + ggtitle(title)
}
