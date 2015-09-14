is_integral = function(x) {
  if(!is.numeric(x)) return(FALSE)
  if(is.integer(x)) return(TRUE)

  xint = as.integer(x)
  return(sum(xint != x)==0)
}


#' @export
DiscreteDistribution = function(frm, xvar, title='Discrete Distribution', ...,
                                stem=TRUE) {
  checkArgs(frm,xvar,xvar)
  if(!is_integral(frm[[xvar]])) {
    stop(paste("Column", xvar, "must have integer values"))
  }

  frm$unit=1

  if(stem) {
    geom="pointrange"
  } else {
    geom="point"
  }
  ggplot(frm, aes_string(xvar, "unit")) +
    stat_summary(fun.y=sum, fun.ymax=sum, fun.ymin=function(x){0}, geom=geom) +
    ggtitle(title)
}
