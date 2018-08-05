
#' @importFrom stats dbeta
NULL

#' Plot an empirical density with the matching normal distribution
#'
#' Compares empirical data to a normal distribution with the same mean and standard deviation.
#'
#' Plots the empirical density, the theoretical matching normal, the mean value,
#' and plus/minus one standard deviation from the mean.
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @examples
#'
#' set.seed(52523)
#' d <- data.frame(wt=100*rnorm(100))
#' PlotDistDensityNormal(d,'wt','example')
#'
#' @export
PlotDistDensityNormal <- function(frm, xvar, title) {
  frm <- as.data.frame(frm)
  check_frame_args_list(# ...,
    frame = frm,
    name_var_list = list(xvar = xvar),
    title = title,
    funname = "WVPlots::PlotDistDensityNormal")
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


#' Plot distribution details as a histogram plus matching normal
#'
#' Compares empirical data to a normal distribution with the same mean and standard deviation.
#'
#' Plots the histograms of the empirical distribution and of the matching normal distribution.
#' Also plots the mean and plus/minus one standard deviation.
#'
#' Bin width for the histogram is calculated automatically to yield approximately 50 bins across the
#' range of the data, unless the \code{binWidth} argument is explicitly passed in. \code{binWidth} is reported
#' in the subtitle of the plot.
#'
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @param ...  no unarmed argument, added to force named binding of later arguments.
#' @param binWidth width of histogram bins
#' @examples
#'
#' set.seed(52523)
#' d <- data.frame(wt=100*rnorm(100))
#' PlotDistCountNormal(d,'wt','example')
#'
#' @export
PlotDistCountNormal <- function(frm, xvar, title,
                                ...,
                                binWidth=c()) {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
                        frame = frm,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::PlotDistCountNormal")
  x <- frm[[xvar]]
  if(is.null(binWidth)) {
    range <- max(x)-min(x)
    if(range<=0) {
      binWidth <- 1
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

  subtitle = paste('binWidth =', format(binWidth))

  ggplot2::ggplot(data=dHist,
                  mapping=ggplot2::aes_string(x=xvar,y='count',ymax='count')) +
    ggplot2::geom_linerange(data=dTheory,ggplot2::aes(ymin=0),size=4,alpha=0.5,color='blue') +
    ggplot2::geom_point(size=4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin=0),size=2) +
    ggplot2::geom_line(data=dDist,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx+sdx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx-sdx,color='blue',linetype=2) +
    ggplot2::ggtitle(title, subtitle=subtitle)
}

#' Plot an empirical density with the matching beta distribution
#'
#' Compares empirical data to a beta distribution with the same mean and standard deviation.
#'
#' Plots the empirical density, the theoretical matching beta, the mean value,
#' and plus/minus one standard deviation from the mean.
#'
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @examples
#'
#' set.seed(52523)
#' d <- data.frame(wt=rbeta(100,shape1=1,shape2=0.5))
#' PlotDistDensityBeta(d,'wt','example')
#'
#' @export
PlotDistDensityBeta <- function(frm, xvar, title) {
  frm <- as.data.frame(frm)
  check_frame_args_list(#...,
    frame = frm,
    name_var_list = list(xvar = xvar),
    title = title,
    funname = "WVPlots::PlotDistDensityBeta")
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
                          adjust=0.5,
                          fill = "lightgray",
                          color = NA) +
    ggplot2::geom_line(data=dDist,
                       mapping=ggplot2::aes_string(x=xvar,y='density'),
                       color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx+sdx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx-sdx,color='blue',linetype=2) +
    ggplot2::ggtitle(title)
}

#' Plot distribution details as a histogram plus matching beta
#'
#' Compares empirical data to a beta distribution with the same mean and standard deviation.
#'
#' Plots the histogram of the empirical distribution and the density of the matching beta distribution.
#' Also plots the mean and plus/minus one standard deviation.
#'
#' The number of bins for the histogram defaults to 30.
#' The binwidth can also be passed in instead of the number of bins.
#'
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @param ... force later arguments to bind by name
#' @param binwidth passed to geom_histogram(). If passed in, overrides bins.
#' @param bins passed to geom_histogram(). Default: 30
#' @return ggplot2 plot
#'
#'
#' @examples
#'
#' set.seed(52523)
#' d <- data.frame(wt=rbeta(100,shape1=0.5,shape2=0.5))
#' PlotDistHistBeta(d,'wt','example')
#'
#' @export
PlotDistHistBeta <- function(frm, xvar, title,
                             ...,
                             binwidth = NULL, bins = 30) {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
                        frame = frm,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::PlotDistHistBeta")
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
                            mapping=ggplot2::aes_string(x=xvar),
                            binwidth = binwidth, bins = bins) +
    ggplot2::geom_line(data=dDist,
                       mapping=ggplot2::aes_string(x=xvar,y='count'),
                       color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx+sdx,color='blue',linetype=2) +
    ggplot2::geom_vline(xintercept=meanx-sdx,color='blue',linetype=2) +
    ggplot2::ggtitle(title)
}
