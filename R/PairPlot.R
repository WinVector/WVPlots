#' Build a pair plot
#'
#'
#' @param d data frame
#' @param meas_vars the variables to be plotted
#' @param title plot title
#' @param ... not used, forces later arguments to bind by name
#' @param group_var variable for grouping and colorcoding
#' @param palette name of a brewer palette (NULL for ggplot2 default coloring)
#' @return a ggplot2 pair plot
#'
#' @examples
#'
#' PairPlot(iris, colnames(iris)[1:4], "Example plot", group_var = "Species")
#'
#' # no color-coding
#' PairPlot(iris, colnames(iris)[1:4], "Example plot")
#'
#' @export
#'
PairPlot <- function(d, meas_vars,  title,
                     ...,
                     group_var = NULL,
                     palette = "Dark2") {
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

  plt = ggplot2::ggplot(d_aug, ggplot2::aes(x=x, y=y))

  if(length(group_var) == 1) {
    plt = plt +
      ggplot2::geom_point(ggplot2::aes_string(color=group_var))
  } else {
    plt = plt + ggplot2::geom_point()
  }

  plt = plt +
    ggplot2::facet_grid(yv~xv, scale = "free") +
    ggplot2::ggtitle(title) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL)

  if(length(palette) == 1) {
    plt = plt + ggplot2::scale_color_brewer(palette = palette)
  }

  plt
}

