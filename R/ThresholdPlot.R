
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
  sorted_frame <- sorted_frame[order(sorted_frame$threshold, sorted_frame$orig_index), , drop = FALSE]
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
  sorted_frame["enrichment"] = (
    sorted_frame["precision"] / mean(sorted_frame[["truth"]])
  )

  # derived facts and synonyms
  sorted_frame["gain"] = sorted_frame["enrichment"]
  sorted_frame["lift"] = sorted_frame["gain"] / sorted_frame["fraction"]
  sorted_frame["recall"] = sorted_frame["true_positive_rate"]
  sorted_frame["sensitivity"] = sorted_frame["recall"]
  sorted_frame["specificity"] = 1 - sorted_frame["false_positive_rate"]

  # re-order for neatness
  sorted_frame["new_index"] = wrapr::seqi(1, nrow(sorted_frame))
  sorted_frame <- sorted_frame[order(-sorted_frame[["new_index"]]), , drop = FALSE]

  # clean up
  sorted_frame["notY"] <- NULL
  sorted_frame["one"] <- NULL
  sorted_frame["new_index"] <- NULL
  sorted_frame["truth"] <- NULL
  return(sorted_frame)
}



ThresholdPlot <- function(frame, xvar, truthVar, title,
                          ...,
                          truth_target = TRUE) {
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::ThresholdPlot")
  stats <- ThresholdStats(frame = frame, xvar = xvar, truthVar = truthVar, truth_target = truth_target)
  # TODO: plot this stuff
}

