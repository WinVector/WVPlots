
padToK <- function(k) {
  force(k)
  function(x) {str_pad(x,k,pad='_') }
}

# assumes p1 and p2 have not set scale_y_*
alignPlotYlabels <- function(p1,p2) {
  info1 <- ggplot_build(p1)
  origlabs1 <- info1$panel$ranges[[1]]$y.labels
  info2 <- ggplot_build(p2)
  origlabs2 <- info2$panel$ranges[[1]]$y.labels
  lengthTarget <- max(nchar(c(origlabs1,origlabs2)))
  padFn <- padToK(lengthTarget)
  list(p1=p1+scale_y_continuous(label=padFn),
       p2=p2+scale_y_continuous(label=padFn))
}
