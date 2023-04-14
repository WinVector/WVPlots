#' Build a pair plot
#'
#' Creates a matrix of scatterplots, one for each possible pair of variables.
#'
#' If \code{palette} is NULL, and \code{group_var} is non-NULL, plot colors will be chosen from the default ggplot2 palette.
#' Setting \code{palette} to NULL
#' allows the user to choose a non-Brewer palette, for example with \code{\link[ggplot2:scale_manual]{scale_color_manual}}.
#'
#' @param d data frame
#' @param meas_vars the variables to be plotted
#' @param title plot title
#' @param ... not used, forces later arguments to bind by name
#' @param group_var variable for grouping and colorcoding
#' @param alpha alpha for points on plot
#' @param palette name of a brewer palette (NULL for ggplot2 default coloring)
#' @param point_color point color for monochrome plots (no grouping)
#' @return a ggplot2 pair plot
#'
#' @examples
#'
#' # PairPlot(iris, colnames(iris)[1:4], "Example plot", group_var = "Species")
#'
#' # custom palette
#' colormap = c('#a6611a', '#dfc27d', '#018571')
#' PairPlot(iris, colnames(iris)[1:4], "Example plot",
#'          group_var = "Species", palette=NULL) +
#'          ggplot2::scale_color_manual(values=colormap)
#'
#' # # no color-coding
#' # PairPlot(iris, colnames(iris)[1:4], "Example plot")
#'
#' @export
#'
PairPlot <- function(d, meas_vars,  title,
                     ...,
                     group_var = NULL,
                     alpha = 1,
                     palette = "Dark2",
                     point_color = 'darkgray') {
  check_frame_args_list(...,
                        frame = d,
                        name_var_list = c(meas_vars, group_var),
                        title = title,
                        funname = "WVPlots::PairPlot")

  controlTable <- data.frame(expand.grid(meas_vars, meas_vars,
                                         stringsAsFactors = FALSE))
  # rename the columns
  colnames(controlTable) <- c("x", "y")

  # add the key column
  controlTable <- cbind(
    data.frame(pair_key = paste(controlTable[[1]], controlTable[[2]]),
               stringsAsFactors = FALSE),
    controlTable)

  d_aug = cdata::rowrecs_to_blocks(
    d,
    controlTable,
    columnsToCopy = group_var)

  splt <- strsplit(d_aug$pair_key, split = " ", fixed = TRUE)
  d_aug$xv <- vapply(splt, function(si) si[[1]], character(1))
  d_aug$yv <- vapply(splt, function(si) si[[2]], character(1))

  # reorder the key columns to be the same order
  # as the base version above
  d_aug$xv <- factor(as.character(d_aug$xv),
                     meas_vars)
  d_aug$yv <- factor(as.character(d_aug$yv),
                     meas_vars)

  x <- y <- NULL # don't look like unbound references to checker

  plt = ggplot2::ggplot(data = d_aug, mapping = ggplot2::aes(x=x, y=y))

  if(length(group_var) == 1) {
    plt = plt +
      ggplot2::geom_point(mapping = ggplot2::aes(!!!simulate_aes_string(color=group_var), alpha=alpha))
  } else {
    plt = plt + ggplot2::geom_point(alpha=alpha, color=point_color)
  }

  plt = plt +
    ggplot2::facet_grid(yv~xv, scale = "free") +
    ggplot2::ggtitle(title) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL)

  if(!is.null(palette)) {
    plt = plt + ggplot2::scale_color_brewer(palette = palette)
  }

  plt
}

