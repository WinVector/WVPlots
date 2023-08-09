

#' Plot a conditional scatter plot with marginals.
#'
#' Plot a scatter plot conditioned on a discrete variable, with marginal conditional density plots.
#'
#' \code{xvar} and \code{yvar} are the coordinates of the points, and \code{cvar} is the
#' discrete conditioning variable that indicates which category each point (x,y) belongs to.
#'
#' @param frame data frame to get values from
#' @param xvar name of the x variable
#' @param yvar name of the y variable
#' @param cvar name of condition variable
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param annot_size numeric scale annotation text (if present)
#' @param colorPalette name of a Brewer palette (see https://colorbrewer2.org/ )
#' @param adjust_x  numeric: adjust x density plot
#' @param adjust_y  numeric: adjust y density plot
#' @return plot grid
#'
#' @seealso \code{\link{ScatterHist}}
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' set.seed(34903490)
#' frm = data.frame(x=rnorm(50),y=rnorm(50))
#' frm$cat <- frm$x+frm$y>0
#' WVPlots::ScatterHistC(frm, "x", "y", "cat",
#'                       title="Example Conditional Distribution")
#'
#' @export
ScatterHistC = function(frame, xvar, yvar, cvar, title, ...,
                        annot_size=3,
                        colorPalette="Dark2",
                        adjust_x = 1,
                        adjust_y = 1) {
  if(is.null(colorPalette)) {
    colorPalette = "Dark2"
  }
  if((!requireNamespace("grid", quietly = TRUE)) ||
     (!requireNamespace("gridExtra", quietly = TRUE))) {
    return("WVPlots::ScatterHistC requires the grid and gridExtra packages be installed")
  }
  frame <- as.data.frame(frame)
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar, yvar = yvar, cvar = cvar),
                        title = title,
                        funname = "WVPlots::ScatterHistC")
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
  plot_center = ggplot2::ggplot(data = frame,
                                mapping = ggplot2::aes(!!!simulate_aes_string(x=xvar,y=yvar,color=cvar))) +
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

  plot_center = plot_center +
    ggplot2::coord_cartesian(xlim=xlims) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
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
  plot_top <- ggplot2::ggplot(data = frame,
                              mapping = ggplot2::aes(!!!simulate_aes_string(x=xvar,color=cvar))) +
    ggplot2::geom_line(stat='density',adjust=adjust_x) +
    ggplot2::coord_cartesian(xlim=xlims) +
    ggplot2::scale_x_continuous(expand = c(0,0))
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
  plot_right <- ggplot2::ggplot(data = frame,
                                mapping = ggplot2::aes(!!!simulate_aes_string(x=yvar,color=cvar))) +
    ggplot2::geom_line(stat='density', adjust=adjust_y) +
    #  ggplot2::coord_cartesian(xlim=ylims) + # causes a warning with ggplot2 2.2.1.9000
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::coord_flip(xlim=ylims, expand=0)
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

  # esimate size
  yPadFn <- designYLabelPadFunction(plot_center +
                                      ggplot2::scale_y_continuous(limits=ylims, expand = c(0,0)),
                                    plot_top)
  # adjust using estimte
  plot_center <- plot_center +
    ggplot2::scale_y_continuous(limits=ylims, label=yPadFn, expand = c(0,0))
  plot_top <- plot_top +
    ggplot2::scale_y_continuous(label=yPadFn)

  # arrange the plots together, with appropriate height and width
  # for each row and column

  gridExtra::grid.arrange(plot_top, legendplt, plot_center, plot_right,
                          top=grid::textGrob(title),
                          ncol = 2, nrow = 2, widths = c(4,1), heights = c(1, 4))
}

#' Plot a height scatter plot with marginals.
#'
#' Plot a scatter plot conditioned on a continuous variable, with marginal conditional density plots.
#'
#' \code{xvar} and \code{yvar} are the coordinates of the points, and \code{zvar} is the
#' continuous conditioning variable. \code{zvar} is partitioned into \code{nclus} disjoint
#' ranges (by default, 3), which are then treated as discrete categories.The scatterplot and marginal density plots
#' are color-coded by these categories.
#'
#' @param frame data frame to get values from
#' @param xvar name of the x variable
#' @param yvar name of the y variable
#' @param zvar name of height variable
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param annot_size numeric: scale annotation text (if present)
#' @param colorPalette name of a Brewer palette (see https://colorbrewer2.org/ )
#' @param nclus scalar: number of z-clusters to plot
#' @param adjust_x  numeric: adjust x density plot
#' @param adjust_y  numeric: adjust y density plot
#'
#' @seealso \code{\link{ScatterHistC}}
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
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
  if(is.null(colorPalette)) {
    colorPalette = "RdYlBu"
  }
  if((!requireNamespace("grid", quietly = TRUE)) ||
     (!requireNamespace("gridExtra", quietly = TRUE))) {
    return("WVPlots::ScatterHistN requires the grid and gridExtra packages be installed")
  }
  frame <- as.data.frame(frame)
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar, yvar = yvar, zvar = zvar),
                        title = title,
                        funname = "WVPlots::ScatterHistN")
  q <- sort(unique(quantile(frame[[zvar]],seq(0, 1, 1/nclus))))
  yC <- cut(frame[[zvar]],q,include.lowest=TRUE)
  if(length(unique(yC))<=1) {
    q <- sort(unique(c(q,median(unique(frame[[zvar]])))))
    yC <- cut(frame[[zvar]],q,include.lowest=TRUE)
  }
  frame[[zvar]] <- yC
  ScatterHistC(frame, xvar, yvar, zvar, title, ...,
               annot_size=annot_size,
               colorPalette=colorPalette,
               adjust_x = adjust_x,
               adjust_y = adjust_y)
}
