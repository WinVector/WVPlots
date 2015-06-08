
#' @export
DoubleDensityPlot <- function(frame, xvar, truthVar,title='double density plot') {
  checkArgs(frame,xvar,truthVar)
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
  # library(RColorBrewer)
  # display.brewer.all()
  palletName = "Dark2"
  # ConditionalDistributionPlot assumes no xlim set
  # ConditionalDistributionPlot assumes no scale_y_continuous set
  ggplot(pf,mapping=aes_string(x=xvar,y='density',ymax='density',
                               color=truthVar,fill=truthVar,linetype=truthVar)) +
    geom_line() +
    geom_ribbon(mapping=aes(ymin=0),alpha=0.5,color=NA) +
    scale_fill_brewer(palette=palletName) + scale_color_brewer(palette=palletName) +
    ggtitle(title)
}
