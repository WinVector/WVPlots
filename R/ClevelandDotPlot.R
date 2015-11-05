# move this to sharedFunctions if someone else needs it
isScalar = function(x) {
  return (is.numeric(x) && length(x)==1)
}



stemdotstats = function(ycol) {
  data.frame(y=length(ycol),
             ymax=length(ycol),
             ymin=0)
}

#' Plot a Cleveland dot plot.
#'
#' assumes that xvar is a factor variable
#' sort < 0 sorts the factor levels in decreasing order (most frequent level first)
#' sort > 0 sorts the factor levels in increasing order (good when used in conjunction with coord_flip())
#' sort = 0 leaves the factor levels in "natural order" -- usually alphabetical
#' stem = FALSE will plot only the dots, without the stem to the y=0 line.
#' limit_n = NULL plots all the levels, N an integer limits to the top N most populous levels
#' @param frm data frame to get values from
#' @param xvar name of the indepement (input or model) column in frame
#' @param title title to place on plot
#' @param ...  no unamed argument, added to force named binding of later arguments.
#' @param sort if TRUE sort data
#' @param limit_n if not NULL number of items to plot
#' @param stem if TRUE add stems/whiskers to plot
#'
#' @export
ClevelandDotPlot = function(frm, xvar, title, ...,
                            sort=-1, limit_n = NULL, stem=TRUE) {
  checkArgs(frame=frm,xvar=xvar,yvar=xvar,title=title,...)
  if(!(is.null(limit_n) || isScalar(limit_n))) {
    stop("parameter limit_n must either be null or a numeric scalar")
  }
  if(isScalar(limit_n) && (limit_n < 1)) {
    stop("parameter limit_n must be at least 1")
  }

  # to get the top N, we always use decreasing sort order
  if(!is.null(limit_n)) {
    tab = table(frm[[xvar]])
    levelnames = names(tab)
    ord = order(tab, decreasing=TRUE)
    N = min(c(limit_n, length(tab)))
    topN = levelnames[ord][1:N]

    frm = subset(frm, frm[[xvar]] %in% topN)
    frm[[xvar]] = droplevels(frm[[xvar]])
  }

  if(abs(sort) > 0) {
    n = length(frm[[xvar]])
    frm[[xvar]] = reorder(frm[[xvar]], numeric(n)+sort, FUN=sum)
  }
  frm$count = 1
  if(stem) {
    p = ggplot2::ggplot(frm, ggplot2::aes_string(x=xvar, y="count")) +
      ggplot2::stat_summary(fun.data=stemdotstats, geom="pointrange")
  } else {
    p = ggplot2::ggplot(frm, ggplot2::aes_string(x=xvar)) + ggplot2::geom_point(stat="bin")
  }
  p + ggplot2::ggtitle(title)
}
