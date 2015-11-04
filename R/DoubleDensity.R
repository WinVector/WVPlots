
#' Plot two density plots conditioned on truthVar.
#'
#' @param frame data frame to get values from
#' @param xvar name of the indepement (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unamed argument, added to force named binding of later arguments.
#'
#' @export
DoubleDensityPlot <- function(frame, xvar, truthVar,title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
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
