

#' Plot a conditional scatter plot with marginals.  xvar is the independent variable (input or model), and yvar is the dependent variable, and cvar is the condition code
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param yvar name of the dependent (output or result to be modeled) column in frame
#' @param cvar name of condition variable
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param annot_size numeric scale annotation text (if present)
#' @param colorPalette name of a Brewer palette (see http://colorbrewer2.org/ )
#' @param adjust_x  numeric adjust x density plot
#' @param adjust_y  numeric adjust y density plot
#' @examples
#'
#' set.seed(34903490)
#' frm = data.frame(x=rnorm(50),y=rnorm(50))
#' frm$cat <- frm$x+frm$y>0
#' WVPlots::ScatterHistC(frm, "x", "y", "cat", title="Example Conditional Distribution")
#'
#' @export
ScatterHistC = function(frame, xvar, yvar, cvar, title, ...,
                        annot_size=3,
                        colorPalette="Dark2",
                       adjust_x = 1,
                       adjust_y = 1) {
  checkArgs(frame=frame,xvar=xvar,yvar=yvar,title=title,...)
  minimal_labels = TRUE

  # Use this plot to print the legend.
  if(is.factor(frame[[cvar]])) {
    labs = levels(frame[[cvar]])
    labs = factor(labs,
                  levels=labs,
                  labels=paste(" ", labs, sep='')  # add a blank to the front
    )
    # this preserves the factor order, if it's not alphabetical
  } else {
    # if frame[[cvar]] isn't a factor, then it will sort alphabetical anyway
    labs = levels(as.factor(frame[[cvar]]))
    labs = paste(" ", labs, sep='')  # add a blank to the front
  }
  nlab = length(labs)

  legendframe = data.frame(x=0, y=seq_len(nlab), cvar=labs)
  emptyframe = data.frame(x=c(-0.01,1), y=c(0,nlab+2))

  legendplt =  ggplot2::ggplot() +
    ggplot2::annotate("text", x=0.1, y=nlab+1, label=cvar, size=annot_size) +
    ggplot2::geom_point(data=legendframe, ggplot2::aes(x=x,y=y,color=cvar),
                        size=3) +
    ggplot2::geom_text(data=legendframe, ggplot2::aes(x=x,y=y,label=cvar),
                       nudge_x=0.025, hjust="left", size=annot_size) +
    ggplot2::geom_point(data=emptyframe, ggplot2::aes(x=x,y=y),
                        colour = "white") +
    ggplot2::theme(plot.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(1, 1, 0, 0), "lines"),
                   legend.position="none") +
   ggplot2:: scale_color_brewer(palette=colorPalette)


  # scatterplot of x and y
  plot_center = ggplot2::ggplot(frame,
                                ggplot2::aes_string(x=xvar,y=yvar,color=cvar)) +
    ggplot2::geom_point() +
    ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "lines")) +
    ggplot2::scale_color_brewer(palette=colorPalette) +
    ggplot2::scale_fill_brewer(palette=colorPalette)

  # get the data range, to help align plots
  x = frame[[xvar]]
  y = frame[[yvar]]
  xlims =  c(min(x), max(x))
  ylims =  c(min(y), max(y))

  #  print(xlims)
  # print(ggplot_build(plot_center)$panel$ranges[[1]]$x.range)

  plot_center = plot_center + ggplot2::xlim(xlims) +
    ggplot2::theme(legend.position="none")

  # print(ggplot_build(plot_center)$panel$ranges[[1]]$x.range)

  # marginal density of x - plot on top
  #
  # 0,0,0,0 -- title squooshed down
  # 1,0,0,0 -- title has space
  # 0,1,0,0 -- right side is shorter
  # 0,0,1,0 -- bottom gap bigger
  # 0,0,0,1 -- left side is shorter
  #
  plot_top <- ggplot2::ggplot(frame,
                              ggplot2::aes_string(x=xvar,color=cvar)) +
    ggplot2::geom_line(stat='density',adjust=adjust_x) +
    ggplot2::xlim(xlims)
  if(minimal_labels) {
    plot_top = plot_top +
      ggplot2::theme(legend.position = "none",
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     plot.margin = grid::unit(c(1, 0, 0, 0), "lines"))
  } else {
    plot_top = plot_top +
      ggplot2::theme(plot.margin = grid::unit(c(1, 0, 0, 0), "lines"))
  }
  plot_top <- plot_top + ggplot2::scale_color_brewer(palette=colorPalette) +
    ggplot2::scale_fill_brewer(palette=colorPalette)


  # marginal density of y - plot on the right
  plot_right <- ggplot2::ggplot(frame,
                                ggplot2::aes_string(x=yvar,color=cvar)) +
    ggplot2::geom_line(stat='density', adjust=adjust_y) +
    ggplot2::xlim(ylims) +
    ggplot2::coord_flip()
  if(minimal_labels) {
    plot_right = plot_right +
      ggplot2::theme(legend.position = "none",
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     plot.margin = grid::unit(c(0, 1, 0, 0), "lines"))
  } else {
    plot_right = plot_right +
      ggplot2::theme(plot.margin = grid::unit(c(0, 1, 0, 0), "lines"))
  }
  plot_right <- plot_right + ggplot2::scale_color_brewer(palette=colorPalette) +
    ggplot2::scale_fill_brewer(palette=colorPalette)

  yPadFn <- designYLabelPadFunction(plot_center + ggplot2::ylim(ylims),plot_top)
  plot_center <- plot_center + ggplot2::scale_y_continuous(limits=ylims,label=yPadFn)
  plot_top <- plot_top + ggplot2::scale_y_continuous(label=yPadFn)

  # arrange the plots together, with appropriate height and width
  # for each row and column

  gridExtra::grid.arrange(plot_top, legendplt, plot_center, plot_right,
                          top=grid::textGrob(title),
                          ncol = 2, nrow = 2, widths = c(4,1), heights = c(1, 4))
}

#' Plot a height scatter plot with marginals.  xvar is the independent variable (input or model), and yvar is the dependent variable, and zvar is the condition height.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param yvar name of the dependent (output or result to be modeled) column in frame
#' @param zvar name of height variable
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param annot_size numeric scale annotation text (if present)
#' @param colorPalette name of a Brewer palette (see http://colorbrewer2.org/ )
#' @param nclus scalar number of z-clusters to plot
#' @param adjust_x  numeric adjust x density plot
#' @param adjust_y  numeric adjust y density plot
#' @examples
#'
#' set.seed(34903490)
#' frm = data.frame(x=rnorm(50),y=rnorm(50))
#' frm$z <- frm$x+frm$y
#' WVPlots::ScatterHistN(frm, "x", "y", "z", title="Example Joint Distribution")
#'
#' @export
ScatterHistN = function(frame, xvar, yvar, zvar, title, ...,
                        annot_size=3,
                        colorPalette="RdYlBu",
                        nclus=3,
                        adjust_x = 1,
                        adjust_y = 1) {
  checkArgs(frame=frame,xvar=xvar,yvar=yvar,title=title,...)
  q <- quantile(frame[[zvar]],seq(0, 1, 1/nclus))
  yC <- cut(frame[[zvar]],q,include.lowest = TRUE)
  frame[[zvar]] <- yC
  ScatterHistC(frame, xvar, yvar, zvar, title, ...,
               annot_size=annot_size,
               colorPalette=colorPalette,
               adjust_x = adjust_x,
               adjust_y = adjust_y)
}