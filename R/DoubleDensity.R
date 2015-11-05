
#' Plot two density plots conditioned on truthVar.
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
#' WVPlots::DoubleDensityPlot(frm, "x", "yC", title="Example double density plot")
#'
#' @export
DoubleDensityPlot <- function(frame, xvar, truthVar,title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  df <- data.frame(x=as.numeric(frame[[xvar]]),
                   y=as.character(frame[[truthVar]]),
                   stringsAsFactors=FALSE)
  pf <- plyr::ddply(df,'y',function(sf) {
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
  ggplot2::ggplot(pf,mapping=ggplot2::aes_string(x=xvar,y='density',ymax='density',
                               color=truthVar,fill=truthVar,linetype=truthVar)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=0),alpha=0.5,color=NA) +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(title)
}
