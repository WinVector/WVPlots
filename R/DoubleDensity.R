


#' Plot two density plots conditioned on an outcome variable.
#'
#' Plot two density plots conditioned on a binary outcome variable.
#'
#' The use case for this visualization is to plot the distribution of a predictive model score
#' (usually the predicted probability
#' of a desired outcome) conditioned on the actual outcome. However, you can use it to compare the distribution of any
#' numerical quantity conditioned on a binary feature. See the examples.
#'
#' The plot will degrade gracefully in degenerate conditions, for example when only
#' one category is present.
#'
#' If \code{palette} is NULL, plot colors will be chosen from the default ggplot2 palette. Setting \code{palette} to NULL
#' allows the user to choose a non-Brewer palette, for example with \code{\link[ggplot2]{scale_fill_manual}}.
#'
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param truth_target if not NULL compare to this scalar value.
#' @param palette name of Brewer palette (can be NULL)
#' @examples
#'
#' mpg = ggplot2::mpg
#' mpg$trans = gsub("\\(.*$", '', mpg$trans)
#' WVPlots::DoubleDensityPlot(mpg, "cty", "trans", "City driving mpg by transmission type")
#'
#'# redo the last plot with a custom palette
#' cmap = c("auto" = "#b2df8a", "manual" = "#1f78b4")
#' plt = WVPlots::DoubleDensityPlot(mpg, "cty", "trans",
#'               palette = NULL,
#'               title="City driving mpg by transmission type")
#' plt + ggplot2::scale_color_manual(values=cmap) +
#'       ggplot2::scale_fill_manual(values=cmap)
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
#'
#' @export
DoubleDensityPlot <- function(frame, xvar, truthVar, title,
                              ...,
                              truth_target = NULL,
                              palette = "Dark2") {
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar, truthVar = truthVar),
                        title = title,
                        funname = "WVPlots::DoubleDensityPlot")
  if(is.null(truth_target)) {
    df <- data.frame(x=as.numeric(frame[[xvar]]),
                     y=as.character(frame[[truthVar]]),
                     stringsAsFactors=FALSE)
  } else {
    df <- data.frame(x=as.numeric(frame[[xvar]]),
                     y=ifelse(frame[[truthVar]]==truth_target,
                              truth_target,
                              paste0("!", truth_target)),
                     stringsAsFactors=FALSE)
  }
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
  palletName = palette
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
  if(!is.null(palette)) {
  plt = plt +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName)
  }
  plt + ggplot2::ggtitle(title)
}
