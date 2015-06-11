# move this to sharedFunctions if someone else needs it
isScalar = function(x) {
  return (is.numeric(x) && length(x)==1)
}



stemdotstats = function(ycol) {
  data.frame(y=length(ycol),
             ymax=length(ycol),
             ymin=0)
}

# assumes that xvar is a factor variable
# sort < 0 sorts the factor levels in decreasing order (most frequent level first)
# sort > 0 sorts the factor levels in increasing order (good when used in conjunction with coord_flip())
# sort = 0 leaves the factor levels in "natural order" -- usually alphabetical
# stem = FALSE will plot only the dots, without the stem to the y=0 line.
# limit_n = NULL plots all the levels, N an integer limits to the top N most populous levels
#' @export
ClevelandDotPlot = function(frm, xvar, sort=-1, limit_n = NULL, stem=TRUE, title='Cleveland Dot plot: Count Data') {
  checkArgs(frm,xvar,xvar)
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
    p = ggplot(frm, aes_string(x=xvar, y="count")) +
      stat_summary(fun.data=stemdotstats, geom="pointrange")
  } else {
    p = ggplot(frm, aes_string(x=xvar)) + geom_point(stat="bin")
  }
  p + ggtitle(title)
}
