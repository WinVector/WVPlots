

# check the arguments are the types our functions commonly expect
checkArgs <- function(frame,xvar,yvar) {
  if((!is.data.frame(frame))||(nrow(frame)<0)||(ncol(frame)<=0)) {
    stop("frame must be a non-empty data frame")
  }
  if((!is.character(xvar))||(length(xvar)!=1)) {
    stop("xvar must be a length 1 character vector")
  }
  if(!(xvar %in% colnames(frame))) {
    stop("xvar must be the name of a column in frame")
  }
  if((!is.character(yvar))||(length(yvar)!=1)) {
    stop("yvar must be a length 1 character vector")
  }
  if(!(yvar %in% colnames(frame))) {
    stop("yvar must be the name of a column in frame")
  }
}

# Curry without leaking
padToK <- function(k) {
  force(k)
  function(x) {str_pad(x,k,pad='_') }
}

# assumes p1 and p2 have not set scale_y_*
designYLabelPadFunction <- function(p1,p2) {
  info1 <- ggplot_build(p1)
  origlabs1 <- info1$panel$ranges[[1]]$y.labels
  info2 <- ggplot_build(p2)
  origlabs2 <- info2$panel$ranges[[1]]$y.labels
  lengthTarget <- max(nchar(c(origlabs1,origlabs2)))
  padToK(lengthTarget)
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


