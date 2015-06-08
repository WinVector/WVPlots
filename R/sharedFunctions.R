
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


# define some helper and reporting functions
# calulcate area under the curve of numeric vectors x,y
# length(x)==length(y)
# y>=0, 0<=x<=1 and x increasing
areaCalc <- function(x,y) {
  # append extra points to get rid of degenerate cases
  x <- c(0,x,1)
  y <- c(0,y,1)
  n <- length(x)
  sum(0.5*(y[-1]+y[-n])*(x[-1]-x[-n]))
}

cdeviance <- function(truth,pred,epsilon=1.e-6) {
  pred = pmax(pred, epsilon)
  pred = pmin(pred, 1-epsilon)
  S = 0.0 # assumed log-likelihood of saturated model
  -2*(sum(ifelse(truth,log(pred),log(1-pred)))-S)
}


