

doubleDensity <- function(frame, xvar, truthVar,title='double density plot') {
  df <- data.frame(x=as.numeric(frame[[xvar]]),
                   y=as.character(frame[[truthVar]]),
                   stringsAsFactors=FALSE)
  pf <- ddply(df,'y',function(sf) {
    dens = density(sf[['x']],adjust=0.5,
                   from=min(sf[['x']]),to=max(sf[['x']]))
    data.frame(truth=sf$y[[1]],
               pred=dens$x,density=dens$y,
                stringsAsFactors=FALSE)
  })
  ggplot(pf, aes(x=pred,ymin=0,y=density,ymax=density,color=truth,fill=truth)) +
    geom_line() + geom_ribbon(alpha=0.5,color=NA)
}
