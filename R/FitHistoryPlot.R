
#' Plot the trajectory of a model fit.
#'
#' Plot a history of fit over a trajetory of times.  Example below gives
#' a fit plot for a history report from Keras R package.
#' Please see \url{http://www.win-vector.com/blog/2017/12/plotting-deep-learning-model-performance-trajectories/}
#' for some examples and details.
#'
#' @param d data frame to get values from.
#' @param column_description description of column measures (data.frame with columns measure, validation, and training).
#' @param title character title for plot.
#' @param ... force later arguments to be bound by name
#' @param epoch_name name for epoch or trajectory column.
#' @param needs_flip charater array of measures that need to be flipped.
#' @param pick_metric character metric to maximize.
#' @param discount_rate numeric what fraction of over-fit to subtract from validation performance.
#' @return ggplot2 plot
#'
#' @examples
#'
#' d <- data.frame(
#'   epoch    = c(1,         2,         3,         4,         5),
#'   val_loss = c(0.3769818, 0.2996994, 0.2963943, 0.2779052, 0.2842501),
#'   val_acc  = c(0.8722000, 0.8895000, 0.8822000, 0.8899000, 0.8861000),
#'   loss     = c(0.5067290, 0.3002033, 0.2165675, 0.1738829, 0.1410933),
#'   acc      = c(0.7852000, 0.9040000, 0.9303333, 0.9428000, 0.9545333) )
#'
#' cT <- data.frame(
#'   measure =    c("minus binary cross entropy", "accuracy"),
#'   training =   c("loss",                       "acc"),
#'   validation = c("val_loss",                   "val_acc"),
#'   stringsAsFactors = FALSE)
#'
#' plt <- plot_fit_trajectory(
#'   d,
#'   column_description = cT,
#'   needs_flip = "minus binary cross entropy",
#'   title = "model performance by epoch, dataset, and measure",
#'   epoch_name = "epoch",
#'   pick_metric = "minus binary cross entropy",
#'   discount_rate = 0.1)
#'
#' suppressWarnings(print(plt)) # too few points for loess
#'
#' @export
#'
plot_fit_trajectory <- function(d,
                                column_description,
                                title,
                                ...,
                                epoch_name = "epoch",
                                needs_flip = c(),
                                pick_metric = NULL,
                                discount_rate = NULL) {
  if(!is.data.frame(d)) {
    stop("WVPlots::plot_fit_trajectory d must be a data.frame")
  }
  if(!is.data.frame(column_description)) {
    stop("WVPlots::plot_fit_trajectory column_description must be a data.frame")
  }
  if(length(list(...))>0) {
    stop("WVPlots::plot_fit_trajectory unexpected arguments")
  }
  # make sure measure is first column for moveValuesToRowsD()
  column_description <- column_description[,
                                           c("measure", "training", "validation"),
                                           drop = FALSE]
  discounted <- NULL # don't look like an unbound variable
  training <- NULL # don't look like an unbound variable
  cols_to_flip <-
    column_description[column_description$measure %in% needs_flip,
                       ,
                       drop = FALSE]
  cols_to_flip$measure <- NULL
  cols_to_flip <- unique(as.character(unlist(cols_to_flip)))
  for(ci in cols_to_flip) {
    d[[ci]] <- -d[[ci]]
  }

  d <- cdata::moveValuesToRowsD(
    d,
    controlTable = column_description,
    columnsToCopy = epoch_name)

  # get factors into user order
  d$measure <- factor(d$measure,
                       levels = column_description$measure)

  d$rmin <- ifelse(d$validation <= d$training, d$validation, NA)
  d$rmax <- ifelse(d$validation <= d$training, d$training, NA)
  if(!is.null(discount_rate)) {
    d$discounted <- ifelse(d$validation <= d$training,
                           d$validation - 0.1*(d$training-d$validation),
                           d$validation)
  }

  plt <- ggplot2::ggplot(data = d,
                  ggplot2::aes_string(x = epoch_name,
                               xend = epoch_name,
                               y = "validation",
                               yend = "training",
                               ymin = "rmin",
                               ymax = "rmax")) +
    ggplot2::geom_segment(alpha = 0.5) +
    ggplot2::geom_point() +
    ggplot2::geom_point( ggplot2::aes(y = training),
                         shape = 3, alpha = 0.5) +
    ggplot2::stat_smooth(geom = "line",
                         se = FALSE,
                         color  = "#d95f02",
                         alpha = 0.8,
                         method = "loess") +
    ggplot2::geom_ribbon(alpha=0.2, fill = "#1b9e77") +
    ggplot2::facet_wrap(~measure, ncol=1, scales = 'free_y') +
    ggplot2::ylab("performance") +
    ggplot2::ggtitle(title)
  if(!is.null(pick_metric)) {
    pd <- d[d$measure == pick_metric, , drop = FALSE]
    if(!is.null(discount_rate)) {
      pick <- pd[[epoch_name]][[which.max(pd$discounted)]]
    } else {
      pick <- pd[[epoch_name]][[which.max(pd$validation)]]
    }
    plt <- plt +
      ggplot2::geom_vline(xintercept = pick, alpha=0.7, color='#e6ab02')
  }
  if(!is.null(discount_rate)) {
    plt <- plt +
      ggplot2::stat_smooth(geom = "line",
                aes(y = discounted),
                se = FALSE,
                color  = "#d95f02",
                alpha = 0.2,
                method = "loess",
                linetype = 2)
  }
  plt
}


#' Plot the trajectory of a Keras model fit.
#'
#' Plot a history of fit over a trajetory of times.  Example below gives
#' a fit plot for a history report from Keras R package.
#' Please see \url{http://winvector.github.io/FluidData/PlotExample/KerasPerfPlot.html}
#' for some details.
#'
#' @param d data frame to get values from.
#' @param title character title for plot.
#' @param ... force later arguments to be bound by name
#' @param epoch_name name for epoch or trajectory column.
#' @param pick_metric character metric to maximize.
#' @param discount_rate numeric what fraction of over-fit to subtract from validation performance.
#' @return ggplot2 plot
#'
#' @examples
#'
#' # example data (from Keras)
#' d <- data.frame(
#'   val_loss = c(0.3769818, 0.2996994, 0.2963943, 0.2779052, 0.2842501),
#'   val_acc  = c(0.8722000, 0.8895000, 0.8822000, 0.8899000, 0.8861000),
#'   loss     = c(0.5067290, 0.3002033, 0.2165675, 0.1738829, 0.1410933),
#'   acc      = c(0.7852000, 0.9040000, 0.9303333, 0.9428000, 0.9545333) )
#'
#' plt <- plot_Keras_fit_trajectory(
#'   d,
#'   title = "model performance by epoch, dataset, and measure")
#'
#' suppressWarnings(print(plt)) # too few points for loess
#'
#' @export
#'
plot_Keras_fit_trajectory <- function(d,
                                title,
                                ...,
                                epoch_name = "epoch",
                                pick_metric = "minus binary cross entropy",
                                discount_rate = 0.1) {
  d[[epoch_name]] <- seq_len(nrow(d))
  column_description <-  data.frame(
       measure =    c("minus binary cross entropy", "accuracy"),
       training =   c("loss",                       "acc"),
       validation = c("val_loss",                   "val_acc"),
       stringsAsFactors = FALSE)
  needs_flip <- "minus binary cross entropy"
  plot_fit_trajectory(d = d,
                      title = title,
                      column_description = column_description,
                      needs_flip = needs_flip,
                      epoch_name = epoch_name,
                      pick_metric = pick_metric,
                      discount_rate = discount_rate)
}
