
# code from sigr ROC.R to decouple dependence for a bit


#' @importFrom stats pbeta
NULL

# calculate ROC curve.
#
# Based on:
#  \url{https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html}
#
# @param modelPredictions numeric predictions (not empty)
# @param yValues truth values (not empty, same length as model predictions)
# @param ... force later arguments to bind by name.
# @param na.rm logical, if TRUE remove NA values.
# @param yTarget value considered to be positive.
# @return the ROC graph of Score (model score), Sensitivity, and Specificity. Guaranteed to have the (0, 0) and (1, 1) (1-Specificity,Sensitivity) endpoints.
#
# @examples
#
# WVPlots:::build_ROC_curve(1:4, c(TRUE,FALSE,TRUE,TRUE))
#
# @export
build_ROC_curve <- function(modelPredictions, yValues,
                            ...,
                            na.rm = FALSE,
                            yTarget = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "WVPlots::build_ROC_curve")
  if(!is.numeric(modelPredictions)) {
    stop("WVPlots::calcAUC modelPredictions must be numeric")
  }
  yValues <- yValues==yTarget
  if(!is.logical(yValues)) {
    stop("WVPlots::build_ROC_curve yValues must be logical")
  }
  if(length(modelPredictions)!=length(yValues)) {
    stop("WVPlots::build_ROC_curve must have length(modelPredictions)==length(yValues)")
  }
  if(na.rm) {
    goodPosns <- (!is.na(modelPredictions)) & (!is.na(yValues))
    modelPredictions <- modelPredictions[goodPosns]
    yValues <- yValues[goodPosns]
  }
  x <- NULL
  y <- NULL
  positive_prevalence <- 0
  if(length(yValues) <= 0) {
    modelPredictions <- c(NA_real_, NA_real_)
    x = c(0, 1)
    y = c(0, 1)
  } else {
    positive_prevalence <- mean(yValues)
    ord <- order(modelPredictions, decreasing=TRUE)
    yValues <- yValues[ord]
    modelPredictions <- modelPredictions[ord]
    # FPR is the x-axis, TPR the y.
    x <- cumsum(!yValues)/max(1,sum(!yValues)) # FPR = x-axis
    y <- cumsum(yValues)/max(1,sum(yValues))   # TPR = y-axis
    # each point should be fully after a bunch of points or fully before a
    # decision level. remove dups to achieve this.
    dup <- c(modelPredictions[-1]>=modelPredictions[-length(modelPredictions)],
             FALSE)
    x <- x[!dup]
    y <- y[!dup]
    modelPredictions <- modelPredictions[!dup]
    # And add in ideal endpoints just in case
    if((x[[1]] > 0) || (y[[1]] > 0)) {
      x <- c(0, x)
      y <- c(0, y)
      modelPredictions <- c(NA_real_, modelPredictions)
    }
    if((x[[length(x)]] < 1) || (y[[length(x)]] < 1)) {
      x <- c(x, 1)
      y <- c(y, 1)
      modelPredictions <- c(modelPredictions, NA_real_)
    }
  }
  data.frame(
    Score = modelPredictions,
    Sensitivity = y,
    Specificity = 1 - x)
}


# Add ROC derived columns.
#
# Add ROC columns derived from sensitivity and specificity.
#
# @param d input data frame, must at lest of columns Sensitivity and Specificity
# @param positive_prevalence scalar, the prevalence of the positive class or prior odds
# @return extended data frame with more columns
#
# @examples
#
# d <- data.frame(pred = 1:4, truth = c(TRUE,FALSE,TRUE,TRUE))
# roc <- WVPlots:::build_ROC_curve(d$pred, d$truth)
# WVPlots:::add_ROC_derived_columns(roc, mean(d$truth))
#
# @export
add_ROC_derived_columns <- function(d, positive_prevalence) {
  # standard definitions
  d$FalsePositiveRate <- 1 - d$Specificity
  d$TruePositiveRate <- d$Sensitivity
  d$TrueNegativeRate <- 1 - d$FalsePositiveRate
  d$FalseNegativeRate <- 1 - d$TruePositiveRate
  # to prevalences (rates normalized by total population, not by class)
  d$false_positive_prevalence = d$FalsePositiveRate * (1 - positive_prevalence)
  d$true_positive_prevalence = d$TruePositiveRate * positive_prevalence
  d$true_negative_prevalence = d$TrueNegativeRate * (1 - positive_prevalence)
  d$false_negative_prevalence = d$FalseNegativeRate * positive_prevalence
  # return value
  d
}


area_from_roc_graph <- function(d) {
  # sum areas of segments (triangle topped vertical rectangles)
  n <- nrow(d)
  area <- sum( ((d$Sensitivity[-1]+d$Sensitivity[-n])/2) * abs(d$Specificity[-1]-d$Specificity[-n]) )
  area
}


# calculate AUC.
#
# Based on:
#  \url{https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html}
#
# @param modelPredictions numeric predictions (not empty), ordered (either increasing or decreasing)
# @param yValues truth values (not empty, same length as model predictions)
# @param ... force later arguments to bind by name.
# @param na.rm logical, if TRUE remove NA values.
# @param yTarget value considered to be positive.
# @return area under curve
#
# @examples
#
# WVPlots:::calcAUC(1:4, c(TRUE,FALSE,TRUE,TRUE)) # should be 2/3
#
# @export
calcAUC <- function(modelPredictions, yValues,
                    ...,
                    na.rm = FALSE,
                    yTarget = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "WVPlots::calcAUC")
  d <- build_ROC_curve(
    modelPredictions = modelPredictions,
    yValues = yValues,
    na.rm = na.rm,
    yTarget = yTarget)
  # sum areas of segments (triangle topped vertical rectangles)
  area <- area_from_roc_graph(d)
  area
}

# Compute the q-graph.
#
# Based on:
#  \url{https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html}
#
# @param Specificity vector of sensitivities to evaluate
# @param q shape parameter for \code{1 - (1 - (1-Specificity)^q)^(1/q)}
# @return Sensitivity
#
# @examples
#
# WVPlots:::sensitivity_from_specificity_q(seq(0, 1, 0.1), 0.61)
#
# @export
sensitivity_from_specificity_q <- function(Specificity, q) {
  Sensitivity <- 1 - (1 - (1-Specificity)^q)^(1/q)
  Sensitivity
}


# Find area matching polynomial curve.
#
# Based on \url{https://win-vector.com/2020/09/13/why-working-with-auc-is-more-powerful-than-one-might-think/}
#
# @param area area to match
# @return q that such that curve \code{1 - (1 - (1-Specificity)^q)^(1/q)} matches area
#
# @examples
#
# WVPlots:::find_area_q(0.75)
#
# @export
find_area_q <- function(area) {
  q_eps <- 1.e-6
  q_low <- 0
  q_high <- 1
  ex_frame <- data.frame(
    Specificity = seq(0, 1, length.out = 101))
  while(q_low + q_eps < q_high) {
    q_mid <- (q_low + q_high)/2
    ex_frame$Sensitivity <- sensitivity_from_specificity_q(ex_frame$Specificity, q_mid)
    q_mid_area <- area_from_roc_graph(ex_frame)
    if(q_mid_area <= area) {
      q_high <- q_mid
    } else {
      q_low <- q_mid
    }
  }
  (q_low + q_high)/2
}


# Find area matching polynomial curve.
#
# Based on \url{https://win-vector.com/2020/09/13/why-working-with-auc-is-more-powerful-than-one-might-think/}
#
# @param modelPredictions numeric predictions (not empty), ordered (either increasing or decreasing)
# @param yValues truth values (not empty, same length as model predictions)
# @param ... force later arguments to bind by name.
# @param na.rm logical, if TRUE remove NA values.
# @param yTarget value considered to be positive.
# @return q that such that curve 1 - (1 -  (1-ideal_roc$Specificity)^q)^(1/q) matches area
#
# @examples
#
# d <- data.frame(pred = 1:4, truth = c(TRUE,FALSE,TRUE,TRUE))
# q <- WVPlots:::find_AUC_q(d$pred, d$truth)
# roc <- WVPlots:::build_ROC_curve(d$pred, d$truth)
# ideal_roc <- data.frame(Specificity = seq(0, 1, length.out = 101))
# ideal_roc$Sensitivity <- WVPlots:::sensitivity_from_specificity_q(ideal_roc$Specificity, q)
# # library(ggplot2)
# # ggplot(mapping = aes(x = 1 - Specificity, y = Sensitivity)) +
# #   geom_line(data = roc, color = "DarkBlue") +
# #   geom_line(data  = ideal_roc, color = "Orange") +
# #   theme(aspect.ratio=1) +
# #   ggtitle("example actual and ideal curve")
#
# @export
find_AUC_q <- function(modelPredictions, yValues,
                       ...,
                       na.rm = FALSE,
                       yTarget = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "WVPlots::find_AUC_q")
  d <- build_ROC_curve(
    modelPredictions = modelPredictions,
    yValues = yValues,
    na.rm = na.rm,
    yTarget = yTarget)
  area <- area_from_roc_graph(d)
  q <- find_area_q(area)
  q
}


# Fit beta parameters from data.
#
# Fit shape1, shape2 using the method of moments.
#
# @param x numeric predictions
# @return beta shape1, shape2 parameters in a named list
#
# @examples
#
# x <- rbeta(1000, shape1 = 3, shape2 = 5.5)
# WVPlots:::fit_beta_shapes(x) # should often be near [3, 5.5]
#
# @export
fit_beta_shapes <- function(x) {
  Ex <- mean(x)
  Vx <- mean((x - Ex)^2)
  shape1 <- Ex * Ex * (1 - Ex) / Vx - Ex
  shape2 <- (Ex * (1 - Ex) / Vx - 1) * (1 - Ex)
  c(shape1 = shape1, shape2 = shape2)
}


# Compute the shape1_pos, shape2_pos, shape1_neg, shape2_neg graph.
#
# Compute specificity and sensitivity given specificity and model fit parameters.
#
# @param Score vector of sensitivities to evaluate
# @param ... force later arguments to bind by name.
# @param shape1_pos beta shape1 parameter for positive examples
# @param shape2_pos beta shape2 parameter for positive examples
# @param shape1_neg beta shape1 parameter for negative examples
# @param shape2_neg beta shape1 parameter for negative examples
# @return Score, Specificity and Sensitivity data frame
#
# @examples
#
# library(wrapr)
#
# empirical_data <- rbind(
#   data.frame(
#     Score = rbeta(1000, shape1 = 3, shape2 = 2),
#     y = TRUE),
#   data.frame(
#     Score = rbeta(1000, shape1 = 5, shape2 = 4),
#     y = FALSE)
# )
#
# unpack[shape1_pos = shape1, shape2_pos = shape2] <-
#   WVPlots:::fit_beta_shapes(empirical_data$Score[empirical_data$y])
#
# shape1_pos
# shape2_pos
#
# unpack[shape1_neg = shape1, shape2_neg = shape2] <-
#   WVPlots:::fit_beta_shapes(empirical_data$Score[!empirical_data$y])
#
# shape1_neg
# shape2_neg
#
# ideal_roc <- WVPlots:::sensitivity_and_specificity_s12p12n(
#   seq(0, 1, 0.1),
#   shape1_pos = shape1_pos,
#   shape1_neg = shape1_neg,
#   shape2_pos = shape2_pos,
#   shape2_neg = shape2_neg)
#
#
# empirical_roc <- WVPlots:::build_ROC_curve(
#   modelPredictions = empirical_data$Score,
#   yValues = empirical_data$y
# )
#
# # # should look very similar
# # library(ggplot2)
# # ggplot(mapping = aes(x = 1 - Specificity, y = Sensitivity)) +
# #   geom_line(data = empirical_roc, color='DarkBlue') +
# #   geom_line(data = ideal_roc, color = 'Orange')
#
# @export
sensitivity_and_specificity_s12p12n <- function(
  Score,
  ...,
  shape1_pos, shape2_pos,
  shape1_neg, shape2_neg) {
  wrapr::stop_if_dot_args(substitute(list(...)), "WVPlots::sensitivity_and_specificity_s12p12n")

  Specificity <- pbeta(Score, shape1 = shape1_neg, shape2 = shape2_neg)
  Sensitivity <- 1 - pbeta(Score, shape1 = shape1_pos, shape2 = shape2_pos)
  data.frame(
    Score = Score,
    Specificity = Specificity,
    Sensitivity = Sensitivity)
}


