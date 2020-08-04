
# Example:
#
# d <- data.frame(
#   x =  c(1, 2, 3, 4, 5),
#   y = c(FALSE, FALSE, TRUE, TRUE, FALSE))
# ThresholdStats(d, 'x', 'y')
#
ThresholdStats <- function(frame, xvar, truthVar, truth_target=TRUE) {
  # make a thin frame to re-sort for cumulative statistics
  sorted_frame <- data.frame(
    threshold = frame[[xvar]],
    truth = as.numeric(frame[[truthVar]]==truth_target),
    stringsAsFactors = FALSE)
  sorted_frame["orig_index"] = wrapr::seqi(1, nrow(frame))
  sorted_frame <- sorted_frame[order(-sorted_frame$threshold, sorted_frame$orig_index), , drop = FALSE]
  sorted_frame["notY"] = 1 - sorted_frame["truth"]  # falses
  sorted_frame["one"] = 1
  sorted_frame["orig_index"] <- NULL

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
  sorted_frame["count"] = cumsum(sorted_frame["one"])  # predicted true so far
  sorted_frame["fraction"] = sorted_frame["count"] / sum(sorted_frame["one"])
  sorted_frame["precision"] = cumsum(sorted_frame["truth"]) / sorted_frame["count"]
  sorted_frame["true_positive_rate"] = (
    cumsum(sorted_frame["truth"]) / sum(sorted_frame["truth"])
  )
  sorted_frame["false_positive_rate"] = (
    cumsum(sorted_frame["notY"]) / sum(sorted_frame["notY"])
  )
  sorted_frame["true_negative_rate"] = (
    sum(sorted_frame["notY"]) - cumsum(sorted_frame["notY"])
  ) / sum(sorted_frame["notY"])
  sorted_frame["false_negative_rate"] = (
    sum(sorted_frame["truth"]) - cumsum(sorted_frame["truth"])
  ) / sum(sorted_frame["truth"])

  # derived facts and synonyms
  sorted_frame["recall"] = sorted_frame["true_positive_rate"]
  sorted_frame["sensitivity"] = sorted_frame["recall"]
  sorted_frame["specificity"] = 1 - sorted_frame["false_positive_rate"]

  # re-order for neatness
  sorted_frame["new_index"] = wrapr::seqi(1, nrow(sorted_frame))
  sorted_frame <- sorted_frame[order(sorted_frame[["new_index"]], decreasing = TRUE), , drop = FALSE]

  # clean up
  sorted_frame["notY"] <- NULL
  sorted_frame["one"] <- NULL
  sorted_frame["new_index"] <- NULL
  sorted_frame["truth"] <- NULL
  return(sorted_frame)
}


#' Plot distribution of metrics as a function of being greater than or equal to thresholds.
#'
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the column to be predicted
#' @param title title to place on plot
#' @param ...  no unarmed argument, added to force named binding of later arguments.
#' @param metrics metrics to be computed, allowed: 'threshold', 'count', 'fraction', 'precision', 'true_positive_rate', 'false_positive_rate', 'true_negative_rate', 'false_negative_rate', 'recall', 'sensitivity', 'specificity'.
#' @param truth_target truth value considered to be positive.
#'
#' @export
#'
#' @examples
#'
#' d <- data.frame(
#'  x =  c(1, 2, 3, 4, 5),
#'   y = c(FALSE, FALSE, TRUE, TRUE, FALSE))
#' ThresholdPlot(d, 'x', 'y', 'example plot')
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
  ggplot2::ggplot(data = stats, mapping = ggplot2::aes(x = threshold, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~metric, ncol = 1, scales = 'free_y') +
    ggplot2::ggtitle(title)
}

