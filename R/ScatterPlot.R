

#' Plot a scatter plot.  xvar is the independent variable (input or model) and yvar is the dependent variable
#' @param frame data frame to get values from
#' @param xvar name of the indepement (input or model) column in frame
#'
#' @export
ScatterPlot = function(frame, xvar, yvar,
                       smoothmethod="auto", # only works for auto and lm
                       title='', annot_size=5,
                       minimal_labels = TRUE,
                       binwidth_x = NULL,
                       binwidth_y = NULL,
                       adjust_x = 1,
                       adjust_y = 1) {



  usePresentationRanges = FALSE
  # placeholder plot - prints nothing at all
  empty =  ggplot() + geom_point(aes(c(0,1), c(0,1)), colour = "white") +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())

  # if we are showing a linear fit, print the fit's parameters
  if(smoothmethod=="lm") {
    # get goodness of linear relation
    model = lm(paste(yvar,"~",xvar), data=frame)
    fstat = summary(model)$fstatistic
    rsqr = summary(model)$r.squared
    pval = pf(fstat[["value"]], fstat[["numdf"]], fstat[["dendf"]], lower.tail=FALSE)

    # print(summary(model))
    fitstring = paste("R-squared = ", format(rsqr, digits=3))
    sigstring = paste("Significance = ", format(pval, digits=3))

    empty = empty + annotate("text", x=0.5, y=0.75, label=fitstring, size=annot_size) +
      annotate("text", x=0.5, y=0.5, label=sigstring, size=annot_size)

  }

  # scatterplot of x and y
  plot_center = ggplot(frame, aes_string(x=xvar,y=yvar)) +
    geom_point(alpha=0.5) + geom_smooth(method=smoothmethod)

  # In current ggplot2, presentation area is always a bit bigger
  # than data and a bit bigger than assigned limits- but by
  # different scaling factors.  So for even the plots to have
  # a changes to line up (due to similar renderings, and not
  # gauranteed by shared coordinate maps) we need set limits
  # everwhere, including on the original controlling plot.
  if(usePresentationRanges) {
    # get plot_center's presentation data ranges
    info = ggplot_build(plot_center)
    xlims = info$panel$ranges[[1]]$x.range
    ylims = info$panel$ranges[[1]]$y.range
  } else {
    # get the data range
    x = frame[[xvar]]
    y = frame[[yvar]]
    xlims =  c(min(x), max(x))
    ylims =  c(min(y), max(y))
  }



  #  print(xlims)
  # print(ggplot_build(plot_center)$panel$ranges[[1]]$x.range)

  plot_center = plot_center + xlim(xlims) +  ylim(ylims)

  # print(ggplot_build(plot_center)$panel$ranges[[1]]$x.range)

  # marginal density of x - plot on top
  plot_top <- ggplot(frame, aes_string(x=xvar)) +
    geom_histogram(aes(y=..density..), fill="gray",
                   color="white", binwidth=binwidth_x) +
   geom_line(stat='density',color="blue", adjust=adjust_x) +
    xlim(xlims)
  if(minimal_labels) {
    plot_top = plot_top +
      theme(legend.position = "none", axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }


  # marginal density of y - plot on the right
  plot_right <- ggplot(frame, aes_string(x=yvar)) +
    geom_histogram(aes(y=..density..), fill="gray",
                   color="white", binwidth=binwidth_y) +
   geom_line(stat='density',color="blue", adjust=adjust_y) +
    xlim(ylims) +
    coord_flip()
  if(minimal_labels) {
    plot_right = plot_right +
      theme(legend.position = "none", axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }

  newPlots <- alignPlotYlabels(plot_center,plot_top)
  # plot_center <- newPlots$p1 # seem to be getting wrong breaks in center plot, probably set too many scales/ranges/theme/axes by now
  plot_top <- newPlots$p2

  # arrange the plots together, with appropriate height and width
  # for each row and column
  grid.arrange(plot_top, empty, plot_center, plot_right,
               ncol = 2, nrow = 2, widths = c(4,1), heights = c(1, 4),
               main=title)
}
