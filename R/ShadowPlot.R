
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
#' If \code{palette} is NULL, and \code{monochrome} is FALSE, plot colors will be chosen from the default ggplot2 palette. Setting \code{palette} to NULL
#' allows the user to choose a non-Brewer palette, for example with \code{\link[ggplot2]{scale_fill_manual}}.
#' For consistency with previous releases, \code{ShadowPlot} defaults to \code{monochrome = TRUE}, while
#' \code{\link{ShadowHist}} defaults to \code{monochrome = FALSE}.
#'
#' Please see here for some interesting discussion \url{https://drsimonj.svbtle.com/plotting-background-data-for-groups-with-ggplot2}.
#'
#' @param frm data frame to get values from.
#' @param condvar name of the primary conditioning variable (a categorical variable, controls x-axis).
#' @param refinevar name of the second or refining conditioning variable (also a categorical variable, controls faceting).
#' @param title title to place on plot.
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param monochrome logical: if TRUE, all facets filled with same color
#' @param palette character: if monochrome==FALSE, name of brewer color palette (can be NULL)
#' @param fillcolor character: if monochrome==TRUE, name of fill color for bars
#' @param ncol numeric: number of columns in facet_wrap.
#' @return a ggplot2 bar chart counting examples grouped by condvar, faceted by refinevar.
#'
#' @examples
#'
#' ShadowPlot(mtcars, "carb", "cyl",
#'            title = "Number of example cars by carb and cyl counts")
#'
#' # colorcode the facets
#' ShadowPlot(mtcars, "carb", "cyl",
#'            monochrome = FALSE,
#'            title = "Number of example cars by carb and cyl counts")
#'
#' @export
ShadowPlot = function(frm, condvar, refinevar, title, ...,
                      monochrome = TRUE, palette = "Dark2",
                      fillcolor = "darkblue", ncol = 1) {
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

  if(monochrome) {
    p = wrapr::let(
      c(FVAR = condvar,
        CVAR = refinevar),
      ggplot2::ggplot(frm, ggplot2::aes(x=FVAR)) +
        ggplot2::geom_bar(data = frmthin, fill="lightgray", color = "lightgray", alpha = 0.5) +
        ggplot2::geom_bar(data = frm, fill=fillcolor, color=NA) +
        ggplot2::facet_wrap(~CVAR, ncol=ncol, labeller = ggplot2::label_both)
    )
  } else {
    p = wrapr::let(
      c(FVAR = condvar,
        CVAR = refinevar),
      ggplot2::ggplot(frm, ggplot2::aes(x=FVAR)) +
        ggplot2::geom_bar(data = frmthin, fill="lightgray", color = "lightgray", alpha = 0.5) +
        ggplot2::geom_bar(data = frm, ggplot2::aes(fill=CVAR), color = NA) +
        ggplot2::facet_wrap(~CVAR, ncol=ncol, labeller = ggplot2::label_both) +
        ggplot2::guides(fill = FALSE)
    )

    if(!is.null(palette)) {
      p = p + ggplot2::scale_fill_brewer(palette = palette)
    }
  }

  p + ggplot2::ggtitle(title)
}
