

#' @importFrom graphics hist
NULL


#' Plot two histograms conditioned on truthVar.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param breaks breaks to pass to histogram
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,y=y,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' frm$absY <- abs(frm$y)
#' frm$posY = frm$y > 0
#' frm$costX = 1
#' WVPlots::DoubleHistogramPlot(frm, "x", "yC", title="Example double histogram plot")
#'
#' @export
DoubleHistogramPlot <- function(frame, xvar, truthVar, title, ...,
                                breaks=40) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  if(!requireNamespace('graphics',quietly = TRUE)) {
    return("WVPlots::DoubleHistogramPlot needs graphics")
  }
  count <- NULL # used as a symbol, declare not an unbound variable
  df <- data.frame(x=as.numeric(frame[[xvar]]),
                   y=as.character(frame[[truthVar]]),
                   stringsAsFactors=FALSE)
  breaksV <- graphics::hist(df[['x']],breaks=breaks,plot=FALSE)$breaks
  yVals <- sort(unique(df[['y']]))
  signs <- (-1)^seq_len(length(yVals))
  names(signs) <- yVals
  pf <- wv_gapply(df,'y',
                       partitionMethod='split',
                       function(sf) {
                         yGroup <- sf$y[[1]]
                         si <- signs[[yGroup]]
                         counts <- graphics::hist(sf[['x']],breaks=breaksV,plot=FALSE)
                         rf <- data.frame(count=counts$counts,
                                          stringsAsFactors=FALSE)
                         rf[[xvar]] <- counts$mids
                         rf[[truthVar]] <- yGroup
                         sm <- tryCatch({
                           smf <- loess(paste('count','~',xvar),rf)
                           sm <- pmax(0,predict(smf,rf,se=FALSE))
                         },
                         error = function(e) { NA }
                         )
                         rf$smooth <- sm
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
  netF <- wv_gapply(pf,xvar,partitionMethod = 'split',
                         function(fi) {
                           di <- data.frame(count=sum(fi$count))
                           di[[xvar]] <- fi[[xvar]][[1]]
                           di
                         })
  netF <- netF[order(netF[[xvar]]),,drop=FALSE]
  sm <- tryCatch({
    smf <- loess(paste('count','~',xvar),netF)
    sm <- predict(smf,pf,se=FALSE)
  },
    error = function(e) { NA }
  )
  pf$net <- sm
  plot <- ggplot2::ggplot(data=pf,mapping=ggplot2::aes_string(x=xvar,
                                    color=truthVar,fill=truthVar,linetype=truthVar)) +
    ggplot2::geom_bar(mapping=ggplot2::aes_string(y='count'),
             stat='identity',alpha=0.5,position='identity')
  if(sum(!is.na(pf$smooth))>1) {
    plot <- plot +
      ggplot2::geom_line(mapping=ggplot2::aes_string(y='smooth'))
  }
  if(sum(!is.na(pf$net))>1) {
    plot <- plot +
      ggplot2::geom_line(mapping=ggplot2::aes_string(y='net'),linetype=3,color='black')
  }
  plot + ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(title)
}
