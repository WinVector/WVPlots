# toy data
set.seed(23590)
class_size = 35
nclasses = 100
true_frate = 0.45

fdata = data.frame(n_female = rbinom(nclasses, class_size, true_frate))

library(WVPlots)
library(ggplot2)
library(rqdatatable)


#'
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param trial_size the number of "coin flips" in a trial
#' @param title title to place on plot
#' @param ...  no unarmed argument, added to force named binding of later arguments.
#' @param p mean of the binomial. If NULL, use empirical mean
#' @param limit_to_observed_range If TRUE, limit plot to observed counts
#' @param count_color color of empirical distribution
#' @param binom_color color of theoretical binomial
PlotDistCountBinomial = function(frm, xvar, trial_size, title, ...,
                                 p = NULL, limit_to_observed_range=FALSE,
                                 count_color = 'black',
                                 binom_color = 'blue') {
  frm <- as.data.frame(frm)
  # # todo: fix this when I'm done -- no :::
  # WVPlots:::check_frame_args_list(...,
  #                       frame = frm,
  #                       name_var_list = list(xvar = xvar),
  #                       title = title,
  #                       funname = "WVPlots::PlotDistCountBinomial")
  # ntrials = nrow(frm)
  # if(!is.null(p) && (p <= 0 || p >= 1)) {
  #   stop("p must be in the open interval (0,1)")
  # }
  #
  # if(is.null(p)) {
  #   # get the empirical mean
  #   nheads = sum(fdata[[xvar]])
  #   nflips = trial_size * ntrials
  #   p = nheads/nflips
  # }
  #
  # # theoretical counts (not necessarily integral)
  # dtheory = data.frame(x = 0:trial_size,
  #                      y = ntrials*dbinom(0:trial_size, trial_size, p))
  # colnames(dtheory) = c(xvar, "number_of_observations")

  # empirical counts
  demp = frm %.>%
    extend(., one=1) %.>%
    project(., number_of_observations = sum(one), groupby="n_female") %.>%
    as.data.frame(.)
  #
  # subtitle = paste("Empirical observations compared to binomial with p =", p)
  #
  # plt = ggplot(mapping=aes_string(x=xvar, y="number_of_observations", ymax="number_of_observations")) +
  #   geom_linerange(data=dtheory, ymin=0, size=4, color=binom_color, alpha=0.5) +
  #   geom_linerange(data=demp, ymin=0, size=2, color=count_color) +
  #   geom_point(data=demp, size=4, color=count_color)
  #
  # if(limit_to_observed_range) {
  #   xrange = range(frm[[xvar]])
  #   plt = plt + coord_cartesian(xlim = xrange)
  # }
  #
  # plt + ggtitle(title, subtitle=subtitle)
}

# compare to empirical p
PlotDistCountBinomial(fdata, "n_female", class_size, "Distribution of count of female students, class size = 50")

frm = fdata
xvar = "n_female"
trial_size = class_size
title = "title"
p = NULL
imit_to_observed_range=FALSE
count_color = 'black'
binom_color = 'blue'

f <- function(frm, ...) {
  demp = frm %.>%
    extend(., one=1) %.>%
    project(., number_of_observations = sum(one), groupby="n_female") %.>%
    as.data.frame(.)
  return(demp)
}

f <- function(frm, ...) {
  t = frm
  t = extend(t, one=1)
  t = project(t, number_of_observations = sum(one), groupby="n_female")
  t = as.data.frame(t)
  demp = t
  return(demp)
}

f <- function(...) {
  t = extend(data.frame(x=2), one=1)
  return(t)
}




