
#' Plot a Shadow Bar Plot
#'
#' Plot a bar chart of row counts conditioned on the categorical variable \code{condvar},
#' faceted on a second categorical variable, \code{refinevar}. Each faceted plot
#' also shows a "shadow plot" of the totals conditioned on \code{condvar} alone.
#'
#' This plot enables comparisons of subpopulation totals across both
#' \code{condvar} and \code{refinevar} simultaneously.
#'
#' By default, the facet plots are arranged in a single column. This can be changed
#' with the optional \code{ncol} argument.
#'
#' @param frm data frame to get values from.
#' @param condvar name of the primary conditioning variable (a categorical variable, controls x-axis).
#' @param refinevar name of the second or refining conditioning variable (also a categorical variable, controls faceting).
#' @param title title to place on plot.
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param ncol numeric: number of columns in facet_wrap.
#' @return a ggplot2 bar chart counting examples grouped by condvar, faceted by refinevar.
#'
#' @examples
#'
#' ShadowPlot(mtcars, "carb", "cyl",
#'            title = "Number of example cars by carb and cyl counts")
#'
#' @export
ShadowPlot = function(frm, condvar, refinevar, title, ...,
                      ncol = 1) {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
                        frame = frm,
                        name_var_list = list(condvar = condvar, refinevar = refinevar),
                        title = title,
                        funname = "WVPlots::ShadowPlot")

  if(is.numeric(frm[[condvar]])) {
    frm[[condvar]] = as.factor(as.character(frm[[condvar]]))
  }

  if(is.numeric(frm[[refinevar]])) {
    frm[[refinevar]] = as.factor(as.character(frm[[refinevar]]))
  }

  frmthin = frm
  frmthin[[refinevar]] = NULL

  CVAR = NULL # make sure this does not look like an unbound reference
  FVAR = NULL # make sure this does not look like an unbound reference
  total = NULL # make sure this does not look like an unbound reference
  count = NULL # make sure this does not look like an unbound reference

  p = wrapr::let(
    c(FVAR = condvar,
      CVAR = refinevar),
    ggplot2::ggplot(frm, ggplot2::aes(x=FVAR)) +
      ggplot2::geom_bar(data = frmthin, fill="lightgray", color = "lightgray", alpha = 0.5) +
      ggplot2::geom_bar(data = frm, fill="darkblue", color=NA) +
      ggplot2::facet_wrap(~CVAR, ncol=ncol, labeller = ggplot2::label_both)
  )


  p + ggplot2::ggtitle(title)
}
