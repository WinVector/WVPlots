
#' @importFrom stats dbinom
NULL

#' Plot count data with a theoretical binomial
#'
#' Compares empirical count data to a binomial distribution
#'
#' This function is useful for comparing the number of successes that occur
#' in a series of trials, all of the same size, to a binomial of a given
#' success-probability.
#'
#' Plots the empirical distribution of successes, and a theoretical matching binomial. If
#' the mean of the binomial, \code{p}, is given, the binomial with success-probability
#' \code{p} is plotted. Otherwise, \code{p} is taken to be the pooled success rate
#' of the data: \code{sum(frm[[xvar]]) / (trial_size*nrow(frm))}. The mean of
#' the binomial is reported in the subtitle of the plot (to three significant figures).
#'
#' If \code{limit_to_observed_range} is TRUE, the range of the plot will only cover
#' the range of the empirical data. Otherwise, the range of the plot will be
#' \code{0:trial_size} (the default).
#'
#' @param frm data frame to get values from
#' @param xvar column of frm that counts the number of successes for each trial
#' @param trial_size the number of "coin flips" in a trial
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param p mean of the binomial. If NULL, use empirical mean
#' @param limit_to_observed_range If TRUE, limit plot to observed counts
#' @param count_color color of empirical distribution
#' @param binom_color color of theoretical binomial
#'
#' @seealso \code{\link{PlotDistHistBeta}}, \code{\link{PlotDistDensityBeta}},
#'
#' @examples
#' set.seed(23590)
#' class_size = 35
#' nclasses = 100
#' true_frate = 0.4
#' fdata = data.frame(n_female = rbinom(nclasses, class_size, true_frate))
#'
#' title = paste("Distribution of count of female students, class size =", class_size)
#' # compare to empirical p
#' PlotDistCountBinomial(fdata, "n_female", class_size, title)
#'
#' # compare to theoretical p of 0.5
#' PlotDistCountBinomial(fdata, "n_female", class_size, title,
#'                       p = 0.5)
#'
#' # Example where the distribution is not of a true single binomial
#' fdata2 = rbind(data.frame(n_female = rbinom(50, class_size, 0.25)),
#'                data.frame(n_female = rbinom(10, class_size, 0.60)) )
#' PlotDistCountBinomial(fdata2, "n_female", class_size, title)
#'
#' @export
PlotDistCountBinomial = function(frm, xvar, trial_size, title, ...,
                                 p = NULL, limit_to_observed_range=FALSE,
                                 count_color = 'black',
                                 binom_color = 'blue') {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
                        frame = frm,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::PlotDistCountBinomial")
  ntrials = nrow(frm)
  if(!is.null(p) && (p <= 0 || p >= 1)) {
    stop("p must be in the open interval (0,1)")
  }

  if(is.null(p)) {
    # get the empirical mean
    nheads = sum(frm[[xvar]])
    nflips = trial_size * ntrials
    p = nheads/nflips
  }

  # theoretical counts (not necessarily integral)
  dtheory = data.frame(x = 0:trial_size,
                       y = ntrials*stats::dbinom(0:trial_size, trial_size, p))
  colnames(dtheory) = c(xvar, "number_of_observations")

  one = 1 # don't look unbound
  # empirical counts
  demp = frm %.>%
    extend(., one=1) %.>%
    project(., number_of_observations = sum(one), groupby="n_female") %.>%
    as.data.frame(.)

  pstr = format(p, digits=3)
  subtitle = paste("Empirical observations compared to binomial with p =", pstr)

  plt = ggplot2::ggplot(mapping=aes_string(x=xvar, y="number_of_observations", ymax="number_of_observations")) +
    ggplot2::geom_linerange(data=dtheory, ymin=0, size=4, color=binom_color, alpha=0.5) +
    ggplot2::geom_linerange(data=demp, ymin=0, size=2, color=count_color) +
    ggplot2::geom_point(data=demp, size=4, color=count_color)

  if(limit_to_observed_range) {
    xrange = range(frm[[xvar]])
    plt = plt + ggplot2::coord_cartesian(xlim = xrange)
  }

  plt + ggplot2::ggtitle(title, subtitle=subtitle)
}



