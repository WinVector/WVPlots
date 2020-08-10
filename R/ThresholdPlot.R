

# take a sub-selection of a sorted list of positions (sort can be ascending or descending)
winnow_sorted_list <- function(sorted_values, rough_target) {
  n <- length(sorted_values)
  if((n<=4) || (length(rough_target) != 1) || (is.na(rough_target)) || (2*rough_target >= n)) {
    return(!logical(n))
  }
  take <- logical(n)
  # take ends, want first 2 on each end for our application
  take[c(1, 2, n-1, n)] <- TRUE
  # take by stride
  take[seq(1, n, by = ceiling(2*n/rough_target))] <- TRUE
  # take by delta
  step <- 2*(max(sorted_values) - min(sorted_values))/rough_target
  last_take <- sorted_values[[2]]
  for(i in seq_len(n)) {
    if(abs(sorted_values[[i]] - last_take) >= step) {
      take[[i]] <- TRUE
      last_take <- sorted_values[[i]]
    }
  }
  return(take)
}


# Example:
#
# d <- data.frame(x =  c(1, 2, 3, 4, 5), y = c(FALSE, FALSE, TRUE, TRUE, FALSE))
# WVPlots:::ThresholdStats(d, 'x', 'y')
#
ThresholdStats <- function(frame, xvar, truthVar,
                           ...,
                           truth_target = TRUE,
                           compute_dists = TRUE,
                           winnow_size = 200) {
  wrapr::stop_if_dot_args(substitute(list(...)), "WVPlots:::ThresholdStats")

  # make a thin frame to re-sort for cumulative statistics
  sorted_frame <- data.frame(
    threshold = frame[[xvar]],
    truth = as.numeric(frame[[truthVar]]==truth_target),
    stringsAsFactors = FALSE)
  sorted_frame$orig_index = wrapr::seqi(1, nrow(frame))
  sorted_frame <- sorted_frame[order(-sorted_frame$threshold, sorted_frame$orig_index), , drop = FALSE]
  sorted_frame$notY = 1 - sorted_frame$truth  # falses
  sorted_frame$one = 1
  sorted_frame$orig_index <- NULL
  prevalence <- mean(sorted_frame$truth)

  if(compute_dists) {
    # cdf/pdf estimate
    cdf <- stats::ecdf(sorted_frame$threshold)
    dens <- stats::density(sorted_frame$threshold)
    pdf <- stats::approxfun(dens$x, dens$y)
    pos_rate <- stats::spline(sorted_frame$threshold, sorted_frame$truth, n=100)
    pos_rate <- stats::approxfun(pos_rate$x, pos_rate$y)
  }

  # pseudo-observation to get end-case (accept nothing case)
  eps = 1.0e-6
  sorted_frame <- rbind(
      data.frame(
          threshold = max(sorted_frame$threshold) + eps,
          truth = 0,
          notY = 0,
          one = 0),
      sorted_frame,
      data.frame(
        threshold = min(sorted_frame$threshold) - eps,
        truth = 0,
        notY = 0,
        one = 0)
  )

  # basic cumulative facts
  sorted_frame$count = cumsum(sorted_frame$one)  # predicted true so far
  sorted_frame$fraction = sorted_frame$count / max(1, sum(sorted_frame$one))
  sorted_frame$precision = cumsum(sorted_frame$truth) / pmax(1, sorted_frame$count)
  sorted_frame$true_positive_rate = (
    cumsum(sorted_frame$truth) / pmax(1, sum(sorted_frame$truth))
  )
  sorted_frame$false_positive_rate = (
    cumsum(sorted_frame$notY) / pmax(1, sum(sorted_frame$notY))
  )
  sorted_frame$true_negative_rate = (
    sum(sorted_frame$notY) - cumsum(sorted_frame$notY)
  ) / pmax(1, sum(sorted_frame$notY))
  sorted_frame$false_negative_rate = (
    sum(sorted_frame$truth) - cumsum(sorted_frame$truth)
  ) / pmax(1, sum(sorted_frame$truth))

  if((length(winnow_size)==1) && (winnow_size>100) && (nrow(sorted_frame) > 2*winnow_size)) {
    take <- winnow_sorted_list(sorted_frame$threshold, winnow_size)
    sorted_frame <- sorted_frame[take, , drop = FALSE]
  }

  if(compute_dists) {
    # approximate cdf/pdf work
    sorted_frame$cdf <- cdf(sorted_frame$threshold)
    sorted_frame$pdf <- pmax(0, pdf(sorted_frame$threshold))
    sorted_frame$positive_rate <- pmin(1, pmax(0, pos_rate(sorted_frame$threshold)))
  }

  # derived facts and synonyms
  sorted_frame$recall = sorted_frame$true_positive_rate
  sorted_frame$sensitivity = sorted_frame$recall
  sorted_frame$specificity = 1 - sorted_frame$false_positive_rate
  sorted_frame$enrichment <- sorted_frame$precision / prevalence

  # re-order for plotting
  sorted_frame$new_index = wrapr::seqi(1, nrow(sorted_frame))
  sorted_frame <- sorted_frame[order(sorted_frame$new_index, decreasing = TRUE), , drop = FALSE]

  # clean up
  sorted_frame$notY <- NULL
  sorted_frame$one <- NULL
  sorted_frame$new_index <- NULL
  sorted_frame$truth <- NULL
  rownames(sorted_frame) <- NULL
  return(sorted_frame)
}


#' Plot distribution of metrics as a function of being greater than or equal to thresholds.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the column to be predicted
#' @param title title to place on plot
#' @param ...  no unarmed argument, added to force named binding of later arguments.
#' @param metrics metrics to be computed, allowed: 'threshold', 'count', 'fraction', 'precision', 'true_positive_rate', 'false_positive_rate', 'true_negative_rate', 'false_negative_rate', 'recall', 'sensitivity', 'specificity', 'cdf', 'pdf', 'positive_rate'.
#' @param truth_target truth value considered to be positive.
#'
#' @export
#'
#' @examples
#'
#' d <- data.frame(
#'  x =  rnorm(1000),
#'  y = sample(c(TRUE, FALSE), size = 1000, replace = TRUE))
#' ThresholdPlot(d, 'x', 'y', 'positivity rates, and data distribution',
#'    metrics = c('sensitivity', 'specificity', 'pdf'))
#' ThresholdPlot(d, 'x', 'y', 'precision/recall, and data distribution',
#'    metrics = c('recall', 'precision', 'pdf'))
#' MetricPairPlot(d, 'x', 'y', 'ROC equivalent')
#' ROCPlot(d, 'x', 'y', TRUE, 'ROC example')
#'
ThresholdPlot <- function(frame, xvar, truthVar, title,
                          ...,
                          metrics = c('sensitivity', 'specificity'),
                          truth_target = TRUE) {
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::ThresholdPlot")
  stats <- ThresholdStats(frame = frame, xvar = xvar, truthVar = truthVar, truth_target = truth_target)
  stats <- cdata::pivot_to_blocks(
    stats,
    nameForNewKeyColumn = 'metric',
    nameForNewValueColumn = 'value',
    columnsToTakeFrom = setdiff(colnames(stats), 'threshold'))
  universe <- sort(unique(stats$metric))
  bad_metrics <- setdiff(metrics, universe)
  if(length(bad_metrics) > 0) {
    stop(paste0("allowed metrics are: ", paste(universe), collapse = ', '), ", saw: ", paste(bad_metrics, collapse = ', '))
  }
  stats <- stats[stats$metric %in% metrics, , drop = FALSE]
  ggplot2::ggplot(data = stats, mapping = ggplot2::aes_string(x = 'threshold', y = 'value')) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~metric, ncol = 1, scales = 'free_y') +
    ggplot2::ggtitle(title)
}


#' Plot the relationship between two metrics.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the column to be predicted
#' @param title title to place on plot
#' @param ...  no unarmed argument, added to force named binding of later arguments.
#' @param x_metric metric to be plotted, allowed: 'threshold', 'count', 'fraction', 'precision', 'true_positive_rate', 'false_positive_rate', 'true_negative_rate', 'false_negative_rate', 'recall', 'sensitivity', 'specificity', 'cdf', 'pdf', 'positive_rate'.
#' @param y_metric metric to be plotted, allowed: 'threshold', 'count', 'fraction', 'precision', 'true_positive_rate', 'false_positive_rate', 'true_negative_rate', 'false_negative_rate', 'recall', 'sensitivity', 'specificity', 'cdf', 'pdf', 'positive_rate'.
#' @param truth_target truth value considered to be positive.
#'
#' @export
#'
#' @examples
#'
#' # data with two different regimes of behavior
#' d <- rbind(
#'   data.frame(
#'     x =  rnorm(1000),
#'     y = sample(c(TRUE, FALSE), prob = c(0.02, 0.98), size = 1000, replace = TRUE)),
#'   data.frame(
#'     x =  rnorm(200) + 5,
#'     y = sample(c(TRUE, FALSE), size = 200, replace = TRUE))
#' )
#' # sensitivity/1-specificity examples
#' ThresholdPlot(d, 'x', 'y', 'positivity rates, and data distribution',
#'    metrics = c('sensitivity', 'specificity', 'pdf'))
#' MetricPairPlot(d, 'x', 'y', 'ROC equivalent')
#' ROCPlot(d, 'x', 'y', TRUE, 'ROC example')
#' # precision/recall examples
#' ThresholdPlot(d, 'x', 'y', 'precision/recall, and data distribution',
#'    metrics = c('recall', 'precision', 'pdf'))
#' MetricPairPlot(d, 'x', 'y', 'recall/precision', x_metric = 'recall', y_metric = 'precision')
#'
MetricPairPlot <- function(frame, xvar, truthVar, title,
                          ...,
                          x_metric = 'false_positive_rate',
                          y_metric = 'true_positive_rate',
                          truth_target = TRUE) {
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::MetricPairPlot")
  stats <- ThresholdStats(frame = frame, xvar = xvar, truthVar = truthVar, truth_target = truth_target)
  universe <- sort(unique(colnames(stats)))
  bad_metrics <- setdiff(c(x_metric, y_metric), universe)
  if(length(bad_metrics) > 0) {
    stop(paste0("allowed metrics are: ", paste(universe), collapse = ', '), ", saw: ", paste(bad_metrics, collapse = ', '))
  }
  stats <- stats[ , c(x_metric, y_metric), drop = FALSE]
  # re-order for plotting
  stats <- stats[order(stats[[x_metric]], stats[[y_metric]]), , drop = FALSE]
  ggplot2::ggplot(data = stats, mapping = ggplot2::aes_string(x = x_metric, y = y_metric)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(title)
}

