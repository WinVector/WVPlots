
#' Plot a Shadow Histogram Plot
#'
#' Plot a histogram of a continuous variable \code{xvar},
#' faceted on a categorical conditioning variable, \code{condvar}. Each faceted plot
#' also shows a "shadow plot" of the unconditioned histogram for comparison.
#'
#' Currently supports only the \code{bins} and \code{binwidth} arguments (see \code{\link[ggplot2]{geom_histogram}}),
#' but not the \code{center}, \code{boundary}, or \code{breaks} arguments.
#'
#' By default, the facet plots are arranged in a single column. This can be changed
#' with the optional \code{ncol} argument.
#'
#' If \code{palette} is NULL, plot colors will be chosen from the default ggplot2 palette. Setting \code{palette} to NULL
#' allows the user to choose a non-Brewer palette, for example with \code{\link[ggplot2]{scale_fill_manual}}.
#'
#' @param frm data frame to get values from.
#' @param xvar name of the primary continuous variable
#' @param condvar name of conditioning variable (categorical variable, controls faceting).
#' @param title title to place on plot.
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param ncol numeric: number of columns in facet_wrap.
#' @param palette character: name of brewer color palette (can be NULL)
#' @param bins number of bins. Defaults to thirty.
#' @param binwidth width of the bins. Overrides bins.
#' @return a ggplot2 histogram plot
#'
#' @examples
#'
#' ShadowHist(iris, "Petal.Length", "Species",
#'            title = "Petal Length distribution by Species")
#'
#' @export
ShadowHist = function(frm, xvar, condvar, title, ...,
                      ncol = 1, palette = "Dark2",
                      bins = 30, binwidth = NULL) {
  frm <- check_frame_args_list(...,
                               frame = frm,
                               name_var_list = list(xvar = xvar, condvar = condvar),
                               title = title,
                               funname = "WVPlots::ShadowHist")

  if(is.numeric(frm[[condvar]])) {
    frm[[condvar]] = as.factor(as.character(frm[[condvar]]))
  }

  frmthin = frm
  frmthin[[condvar]] = NULL

  CVAR = NULL # make sure this does not look like an unbound reference
  XVAR = NULL # make sure this does not look like an unbound reference

  p = wrapr::let(
    c(XVAR = xvar,
      CVAR = condvar),
    ggplot2::ggplot(frm, ggplot2::aes(x=XVAR)) +
      ggplot2::geom_histogram(data = frmthin, bins = bins, binwidth = binwidth,
                              fill="lightgray", color = "lightgray", alpha = 0.5) +
      ggplot2::geom_histogram(data = frm, ggplot2::aes(fill=CVAR), color = NA,
                              bins = bins, binwidth = binwidth) +
      ggplot2::facet_wrap(~CVAR, ncol=ncol, labeller = ggplot2::label_both) +
      ggplot2::guides(fill = FALSE)
  )

  if(!is.null(palette)) {
    p = p + ggplot2::scale_fill_brewer(palette = palette)
  }
  p + ggplot2::ggtitle(title)
}
