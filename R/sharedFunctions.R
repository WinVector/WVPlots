

#' @importFrom stats aggregate anova as.formula binomial glm lm lm.wfit pchisq pf quantile
#' @importFrom ROCR prediction performance
#' @importFrom ggplot2 ggplot_build aes aes_string
#' @importFrom grid textGrob unit
#' @importFrom gridExtra grid.arrange
#' @importFrom plyr ddply summarize
#' @importFrom reshape2 melt
#' @importFrom stringr str_pad
NULL

# check the arguments are the types our functions commonly expect
checkArgs <- function(frame,xvar,yvar,title,...) {
  args <- list(...)
  if(missing(frame)||(!is.data.frame(frame))||(nrow(frame)<0)||(ncol(frame)<=0)) {
    stop("frame must be a non-empty data frame")
  }
  if(missing(title)||(!is.character(title))||(length(title)!=1)) {
    stop("title must be a length 1 character vector")
  }
  if(missing(xvar)||(!is.character(xvar))||(length(xvar)!=1)) {
    stop("xvar must be a length 1 character vector")
  }
  if(missing(yvar)||(!is.character(yvar))||(length(yvar)!=1)) {
    stop("yvar must be a length 1 character vector")
  }
  if(!(xvar %in% colnames(frame))) {
    stop("xvar must be the name of a column in frame")
  }
  if(!(yvar %in% colnames(frame))) {
    stop("yvar must be the name of a column in frame")
  }
  if(length(args)!=0) {
    nm <- setdiff(paste(names(args),collapse=", "),'')
    nv <- length(args)-length(nm)
    stop(paste("unexpected arguments",nm,"(and",nv,"unexpected values)"))
  }
}

# Curry without leaking
padToK <- function(k) {
  force(k)
  function(x) {stringr::str_pad(x,k,pad='_') }
}

# assumes p1 and p2 have not set scale_y_*
designYLabelPadFunction <- function(p1,p2) {
  info1 <- ggplot2::ggplot_build(p1)
  origlabs1 <- info1$panel$ranges[[1]]$y.labels
  info2 <- ggplot2::ggplot_build(p2)
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

# this function counts logical as discrete
isDiscrete = function(x) {
  return (is.factor(x) || is.character(x) || is.integer(x) || is.logical(x))
}


