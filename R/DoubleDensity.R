


#' Plot two density plots conditioned on truthVar.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(score=x,
#'    truth=(y>=as.numeric(quantile(y,probs=0.8))),
#'    stuck=TRUE,
#'    rare=FALSE)
#' frm[1,'rare'] = TRUE
#' WVPlots::DoubleDensityPlot(frm, "score", "truth", title="Example double density plot")
#' WVPlots::DoubleDensityPlot(frm, "score", "stuck", title="Example double density plot")
#' WVPlots::DoubleDensityPlot(frm, "score", "rare", title="Example double density plot")
#'
#' @export
DoubleDensityPlot <- function(frame, xvar, truthVar,title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  df <- data.frame(x=as.numeric(frame[[xvar]]),
                   y=as.character(frame[[truthVar]]),
                   stringsAsFactors=FALSE)
  pf <- wv_gapply(df,'y',
                    partitionMethod='split',
                    function(sf) {
                      uvals <- unique(sf[['x']])
                      nunique <- length(uvals)
                      if(nunique>1) {
                        dens <- density(sf[['x']],adjust=0.5,
                                        from=min(sf[['x']]),to=max(sf[['x']]))
                        rf <- data.frame(density=dens$y,xintercept=NA,
                                         stringsAsFactors=FALSE)
                        rf[[xvar]] <- dens$x
                        rf[[truthVar]] <- sf$y[[1]]
                      } else {
                        rf <- data.frame(density=NA,xintercept=uvals,
                                         stringsAsFactors=FALSE)
                        rf[[xvar]] <- NA
                        rf[[truthVar]] <- sf$y[[1]]
                      }
                      rf
                    })
  pf$zero = 0
  # library(RColorBrewer)
  # display.brewer.all()
  palletName = "Dark2"
  plt <- ggplot2::ggplot(data=pf,
                         mapping=ggplot2::aes_string(x=xvar,y='density',
                                                     ymin='zero',ymax='density',
                                                     color=truthVar,fill=truthVar,
                                                     linetype=truthVar
                                                     ))
  if(sum(!is.na(pf$density))>0) {
    plt <- plt + ggplot2::geom_line() +
      ggplot2::geom_ribbon(alpha=0.5,color=NA)
  }
  if(sum(!is.na(pf$xintercept))>0) {
    plt <- plt + ggplot2::geom_vline(data=pf,
                                     mapping=ggplot2::aes_string(color=truthVar,linetype=truthVar,
                                                                 xintercept='xintercept'))
  }
  plt + ggplot2::ggtitle(title) +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName)
}
