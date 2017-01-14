
#' @importFrom stats dbeta
NULL

#' plot distribution details as a density plus matching normal
#'
#' assumes that xvar is a factor variable
#' sort < 0 sorts the factor levels in decreasing order (most frequent level first)
#' sort > 0 sorts the factor levels in increasing order (good when used in conjunction with coord_flip())
#' sort = 0 leaves the factor levels in "natural order" -- usually alphabetical
#' stem = FALSE will plot only the dots, without the stem to the y=0 line.
#' limit_n = NULL plots all the levels, N an integer limits to the top N most populous levels
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(52523)
#' d <- data.frame(wt=100*rnorm(100))
#' PlotDistDensityNormal(d,'wt','example')
#'
#' @export
PlotDistDensityNormal <- function(frm,xvar,title,...) {
  x <- frm[[xvar]]
  dPlot <- data.frame(x=x)
  colnames(dPlot) <- xvar
  # get the normal approximation
  xmin <- min(x)
  xmax <- max(x)
  meanx <- mean(x)
  sdx <- sqrt(var(x))
  dDist <- data.frame(x=seq(xmin,xmax,length.out=30))
  colnames(dDist) <- xvar
  dDist$density <- dnorm(dDist[[xvar]],mean=meanx,sd=sdx)
  ggplot2::ggplot() +
    ggplot2::geom_density(data=dPlot,
                          mapping=ggplot2::aes_string(x=xvar),
                          adjust=0.5) +
    ggplot2::geom_line(data=dDist,
                       mapping=ggplot2::aes_string(x=xvar,y='density'),
                       color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx+sdx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx-sdx,color='blue',linetype=2) +
    ggplot2::ggtitle(title)
}


#' plot distribution details as a histogram plus matching normal
#'
#' assumes that xvar is a factor variable
#' sort < 0 sorts the factor levels in decreasing order (most frequent level first)
#' sort > 0 sorts the factor levels in increasing order (good when used in conjunction with coord_flip())
#' sort = 0 leaves the factor levels in "natural order" -- usually alphabetical
#' stem = FALSE will plot only the dots, without the stem to the y=0 line.
#' limit_n = NULL plots all the levels, N an integer limits to the top N most populous levels
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @param ...  no unarmed argument, added to force named binding of later arguments.
#' @param binWidth with of histogram bins
#' @examples
#'
#' set.seed(52523)
#' d <- data.frame(wt=100*rnorm(100))
#' PlotDistCountNormal(d,'wt','example')
#'
#' @export
PlotDistCountNormal <- function(frm,xvar,title,...,binWidth=c()) {
  x <- frm[[xvar]]
  if(is.null(binWidth)) {
    range <- max(x)-min(x)
    if(range<=0) {
      binWith <- 1
    } else {
      binWidth <- 10^ceiling(log(range,10))/50
    }
  }
  # build our own histogram
  dCalc <- data.frame(x=binWidth*round(x/binWidth),one=1)
  dHist <- aggregate(one~x,data=dCalc,FUN=sum)
  colnames(dHist) <- c(xvar,'count')
  # get the normal approximation
  meanx <- mean(x)
  sdx <- sqrt(var(x))
  # get the normal counts
  dTheory <- data.frame(x=seq(min(dCalc$x),max(dCalc$x),by=binWidth))
  colnames(dTheory) <- xvar
  dTheory$count <- length(x)*(
    pnorm(dTheory[[xvar]]-binWidth/2,
          mean=meanx,
          sd=sdx,
          lower.tail=FALSE) -
      pnorm(dTheory[[xvar]]+binWidth/2,
            mean=meanx,
            sd=sdx,
            lower.tail=FALSE))
  # get the scaled density (not area is misleading if binWidth!=1)
  xmin <- max(x)
  xmax <- max(x)
  dDist <- data.frame(x=seq(xmin,xmax,length.out=40))
  colnames(dDist) <- xvar
  # plotted area is going to be re-scale by both number of observations and binWidth
  dDist$count <- length(x)*binWidth*dnorm(dDist[[xvar]],mean=meanx,sd=sdx)
  # plot
  ggplot2::ggplot(data=dHist,
                  mapping=ggplot2::aes_string(x=xvar,y='count',ymax='count')) +
    ggplot2::geom_linerange(data=dTheory,aes(ymin=0),size=4,alpha=0.5,color='blue') +
    ggplot2::geom_point(size=4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin=0),size=2) +
    ggplot2::geom_line(data=dDist,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx+sdx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx-sdx,color='blue',linetype=2) +
    ggplot2::ggtitle(paste(title,'\n','binWidth=',format(binWidth)))
}


#' plot distribution details as a density plus matching beta
#'
#' assumes that xvar is a factor variable
#' sort < 0 sorts the factor levels in decreasing order (most frequent level first)
#' sort > 0 sorts the factor levels in increasing order (good when used in conjunction with coord_flip())
#' sort = 0 leaves the factor levels in "natural order" -- usually alphabetical
#' stem = FALSE will plot only the dots, without the stem to the y=0 line.
#' limit_n = NULL plots all the levels, N an integer limits to the top N most populous levels
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(52523)
#' d <- data.frame(wt=rbeta(100,shape1=1,shape2=0.5))
#' PlotDistDensityBeta(d,'wt','example')
#'
#' @export
PlotDistDensityBeta <- function(frm,xvar,title,...) {
  x <- frm[[xvar]]
  dPlot <- data.frame(x=x)
  colnames(dPlot) <- xvar
  # get the beta approximation
  meanx <- mean(x)
  varx <- var(x)
  sdx <- sqrt(varx)
  # try simple moment matching https://en.wikipedia.org/wiki/Beta_distribution#Method_of_moments
  shape1 <- meanx*(meanx*(1-meanx)/varx-1)
  shape2 <- (1-meanx)*(meanx*(1-meanx)/varx-1)
  checkM <- shape1/(shape1+shape2)
  checkV <- shape1*shape2/((shape1+shape2)^2*(shape1+shape2+1))
  dDist <- data.frame(x=seq(1/30,1-1/30,length.out=30))
  colnames(dDist) <- xvar
  dDist$density <- dbeta(dDist[[xvar]],shape1=shape1,shape2=shape2)
  ggplot2::ggplot() +
    ggplot2::geom_density(data=dPlot,
                          mapping=ggplot2::aes_string(x=xvar),
                          adjust=0.5) +
    ggplot2::geom_line(data=dDist,
                       mapping=ggplot2::aes_string(x=xvar,y='density'),
                       color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx+sdx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx-sdx,color='blue',linetype=2) +
    ggplot2::ggtitle(title)
}


#' plot distribution details as a density plus matching beta
#'
#' assumes that xvar is a factor variable
#' sort < 0 sorts the factor levels in decreasing order (most frequent level first)
#' sort > 0 sorts the factor levels in increasing order (good when used in conjunction with coord_flip())
#' sort = 0 leaves the factor levels in "natural order" -- usually alphabetical
#' stem = FALSE will plot only the dots, without the stem to the y=0 line.
#' limit_n = NULL plots all the levels, N an integer limits to the top N most populous levels
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(52523)
#' d <- data.frame(wt=rbeta(100,shape1=0.5,shape2=0.5))
#' PlotDistHistBeta(d,'wt','example')
#'
#' @export
PlotDistHistBeta <- function(frm,xvar,title,...) {
  x <- frm[[xvar]]
  dPlot <- data.frame(x=x)
  colnames(dPlot) <- xvar
  # get the beta approximation
  meanx <- mean(x)
  varx <- var(x)
  sdx <- sqrt(varx)
  # try simple moment matching https://en.wikipedia.org/wiki/Beta_distribution#Method_of_moments
  shape1 <- meanx*(meanx*(1-meanx)/varx-1)
  shape2 <- (1-meanx)*(meanx*(1-meanx)/varx-1)
  checkM <- shape1/(shape1+shape2)
  checkV <- shape1*shape2/((shape1+shape2)^2*(shape1+shape2+1))
  dDist <- data.frame(x=seq(1/200,1-1/200,length.out=50))
  colnames(dDist) <- xvar
  dDist$density <- dbeta(dDist[[xvar]],shape1=shape1,shape2=shape2)
  dDist$count <- length(x)*dDist$density/sum(dDist$densit)
  ggplot2::ggplot() +
    ggplot2::geom_histogram(data=dPlot,
                          mapping=ggplot2::aes_string(x=xvar)) +
    ggplot2::geom_line(data=dDist,
                       mapping=ggplot2::aes_string(x=xvar,y='count'),
                       color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx+sdx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx-sdx,color='blue',linetype=2) +
    ggplot2::ggtitle(title)
}
