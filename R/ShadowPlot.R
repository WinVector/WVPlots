
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
  checkArgs(frame=frm,xvar=condvar,yvar=refinevar,title=title,...)

  if(is.numeric(frm[[condvar]])) {
    frm[[condvar]] = as.factor(as.character(frm[[condvar]]))
  }

  if(is.numeric(frm[[refinevar]])) {
    frm[[refinevar]] = as.factor(as.character(frm[[refinevar]]))
  }

  df = tapply(rep(1, nrow(frm)),
              list(frm[[condvar]], frm[[refinevar]]),
              FUN = sum,
              simplify = TRUE,
              default = 0)
  df = as.data.frame(df)
  df$total = rowSums(df)
  df[[condvar]] = rownames(df)

  cols = as.character(unique(frm[[refinevar]]))

  df = cdata::unpivot_to_blocks(df,
                                nameForNewKeyColumn = refinevar,
                                nameForNewValueColumn = "count",
                                columnsToTakeFrom = cols)

  CVAR = NULL # make sure this does not look like an unbound reference
  FVAR = NULL # make sure this does not look like an unbound reference
  total = NULL # make sure this does not look like an unbound reference
  count = NULL # make sure this does not look like an unbound reference
  p = wrapr::let(
    c(FVAR = condvar,
      CVAR = refinevar),
    ggplot2::ggplot(df, ggplot2::aes(x=FVAR)) +
      ggplot2::geom_col(ggplot2::aes(y=total), fill="lightgray", color = "lightgray", alpha = 0.5) +
      ggplot2::geom_col(ggplot2::aes(y=count), fill="darkblue") +
      ggplot2::facet_wrap(~CVAR, ncol=ncol, labeller = ggplot2::label_both)
  )

  p + ggplot2::ggtitle(title)
}
