

#' @importFrom stats aggregate anova as.formula binomial glm lm lm.wfit pchisq pf quantile density dnorm ecdf loess pnorm predict reorder smooth var
#' @importFrom ROCR prediction performance
#' @importFrom graphics hist
#' @importFrom ggplot2 ggplot_build aes aes_string
#' @importFrom grid textGrob unit
#' @importFrom gridExtra grid.arrange
#' @importFrom plyr ddply summarize
#' @importFrom reshape2 melt
#' @importFrom stringr str_pad
#' @importFrom stats median
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
    nm <- names(args)
    if(length(nm)>0) {
      stop(paste("unexpected named arguments",paste(nm,collapse = ', ')))
    }
    stop(paste("saw",length(args),"unexpected argument values: ",paste(args,collapse=', ')))
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


