

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

#' @export
ClevelandDotPlot = function(frm, xvar, sort=-1, stem=TRUE, title='Cleveland Dot plot: Count Data'){
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
