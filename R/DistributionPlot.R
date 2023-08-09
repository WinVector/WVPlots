
#' @importFrom stats dbeta
NULL

#' Plot an empirical density with the matching normal distribution
#'
#' Compares empirical data to a normal distribution with the same mean and standard deviation.
#'
#' Plots the empirical density, the theoretical matching normal, the mean value,
#' and plus/minus one standard deviation from the mean.
#'
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param adjust passed to geom_density; controls smoothness of density plot
#' @param curve_color color for empirical density curve
#' @param normal_color color for theoretical matching normal
#' @param mean_color color of mean line
#' @param sd_color color for 1-standard deviation lines (can be NULL)
#'
#' @seealso \code{\link[ggplot2]{geom_density}}
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' set.seed(52523)
#' d <- data.frame(wt=100*rnorm(100))
#' PlotDistDensityNormal(d,'wt','example')
#'
#' # # no sd lines
#' # PlotDistDensityNormal(d, 'wt', 'example', sd_color=NULL)
#'
#' @export
PlotDistDensityNormal <- function(frm, xvar, title, ...,
                                  adjust = 0.5,
                                  curve_color = 'lightgray',
                                  normal_color = 'blue',
                                  mean_color = 'blue',
                                  sd_color = 'darkgray') {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
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
  plt = ggplot2::ggplot() +
    ggplot2::geom_density(data=dPlot,
                          mapping=ggplot2::aes(!!!simulate_aes_string(x=xvar)),
                          fill = curve_color,
                          color = NA,
                          adjust=adjust) +
    ggplot2::geom_line(data=dDist,
                       mapping=ggplot2::aes(!!!simulate_aes_string(x=xvar,y='density')),
                       color=normal_color,linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color=mean_color,linetype=2)
  if(!is.null(sd_color)) {
    plt = plt +
      ggplot2::geom_vline(xintercept=meanx+sdx,color=sd_color,linetype=2) +
      ggplot2::geom_vline(xintercept=meanx-sdx,color=sd_color,linetype=2)
  }
  plt +  ggplot2::ggtitle(title)
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
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param binWidth width of histogram bins
#' @param hist_color color of empirical histogram
#' @param normal_color color of matching theoretical normal
#' @param mean_color color of mean line
#' @param sd_color color of 1-standard deviation lines (can be NULL)
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' set.seed(52523)
#' d <- data.frame(wt=100*rnorm(100))
#' PlotDistCountNormal(d,'wt','example')
#'
#' # # no sd lines
#' # PlotDistCountNormal(d, 'wt', 'example', sd_color=NULL)
#'
#' @export
PlotDistCountNormal <- function(frm, xvar, title,
                                ...,
                                binWidth=c(),
                                hist_color='black',
                                normal_color='blue',
                                mean_color='blue',
                                sd_color='blue') {
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

  plt = ggplot2::ggplot(data=dHist,
                        mapping=ggplot2::aes(!!!simulate_aes_string(x=xvar,y='count',ymax='count'))) +
    ggplot2::geom_linerange(data=dTheory,ggplot2::aes(ymin=0),size=4,alpha=0.5,color=normal_color) +
    ggplot2::geom_point(color=hist_color, size=4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin=0),color=hist_color, size=2) +
    ggplot2::geom_line(data=dDist,color=normal_color,linetype=2) +   # check this
    ggplot2::geom_vline(xintercept=meanx,color=mean_color,linetype=2)

  if(!is.null(sd_color)) {
    plt = plt +
      ggplot2::geom_vline(xintercept=meanx+sdx,color=sd_color,linetype=2) +
      ggplot2::geom_vline(xintercept=meanx-sdx,color=sd_color,linetype=2)
  }
  plt + ggplot2::ggtitle(title, subtitle=subtitle)
}

#' Plot empirical rate data as a density with the matching beta distribution
#'
#' Compares empirical rate data to a beta distribution with the same mean and standard deviation.
#'
#' Plots the empirical density, the theoretical matching beta, the mean value,
#' and plus/minus one standard deviation from the mean.
#'
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @param ... force later arguments to bind by name
#' @param curve_color color for empirical density curve
#' @param beta_color color for matching theoretical beta
#' @param mean_color color for mean line
#' @param sd_color color for 1-standard deviation lines (can be NULL)
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' set.seed(52523)
#' N = 100
#' pgray = 0.1  # rate of gray horses in the population
#' herd_size = round(runif(N, min=25, 50))
#' ngray = rbinom(N, herd_size, pgray)
#' hdata = data.frame(n_gray=ngray, herd_size=herd_size)
#'
#' # observed rate of gray horses in each herd
#' hdata$rate_gray = with(hdata, ngray/herd_size)
#'
#' title = "Observed prevalence of gray horses in population"
#'
#' PlotDistDensityBeta(hdata, "rate_gray", title) +
#'   ggplot2::geom_vline(xintercept = pgray, linetype=4, color="maroon") +
#'   ggplot2::annotate("text", x=pgray+0.01, y=0.01, hjust="left",
#'                     label = paste("True prevalence =", pgray))
#'
#' # # no sd lines
#' # PlotDistDensityBeta(hdata, "rate_gray", title,
#' #                     sd_color=NULL)
#' @export
PlotDistDensityBeta <- function(frm, xvar, title, ...,
                                curve_color='lightgray',
                                beta_color='blue',
                                mean_color='blue',
                                sd_color='darkgray') {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
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
  # dDist <- data.frame(x=seq(1/30,1-1/30,length.out=30))
  dDist <- data.frame(x=seq(1/200,1-1/200,length.out=100))
  colnames(dDist) <- xvar
  dDist$density <- dbeta(dDist[[xvar]],shape1=shape1,shape2=shape2)
  plt = ggplot2::ggplot() +
    ggplot2::geom_density(data=dPlot,
                          mapping=ggplot2::aes(!!!simulate_aes_string(x=xvar)),
                          adjust=0.5,
                          fill = curve_color,
                          color = NA) +
    ggplot2::geom_line(data=dDist,
                       mapping=ggplot2::aes(!!!simulate_aes_string(x=xvar,y='density')),
                       color=beta_color,linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color=mean_color,linetype=2)

  if(!is.null(sd_color)) {
    plt = plt +
      ggplot2::geom_vline(xintercept=meanx+sdx,color=sd_color,linetype=2) +
      ggplot2::geom_vline(xintercept=meanx-sdx,color=sd_color,linetype=2)
  }
  plt + ggplot2::ggtitle(title)
}

#' Plot empirical rate data as a histogram plus matching beta
#'
#' Compares empirical rate data to a beta distribution with the same mean and standard deviation.
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
#' @param bins passed to geom_histogram(). Default: 30
#' @param hist_color color of empirical histogram
#' @param beta_color color of matching theoretical beta
#' @param mean_color color of mean line
#' @param sd_color color of 1-standard devation lines (can be NULL)
#' @return ggplot2 plot
#'
#' @examples
#'
#' if (requireNamespace('data.table', quietly = TRUE)) {
#'		# don't multi-thread during CRAN checks
#' 		data.table::setDTthreads(1)
#' }
#'
#' set.seed(52523)
#' N = 100
#' pgray = 0.1  # rate of gray horses in the population
#' herd_size = round(runif(N, min=25, 50))
#' ngray = rbinom(N, herd_size, pgray)
#' hdata = data.frame(n_gray=ngray, herd_size=herd_size)
#'
#' # observed rate of gray horses in each herd
#' hdata$rate_gray = with(hdata, n_gray/herd_size)
#'
#' title = "Observed prevalence of gray horses in population"
#'
#' PlotDistHistBeta(hdata, "rate_gray", title) +
#'   ggplot2::geom_vline(xintercept = pgray, linetype=4, color="maroon") +
#'   ggplot2::annotate("text", x=pgray+0.01, y=0.01, hjust="left",
#'                     label = paste("True prevalence =", pgray))
#'
#' # # no sd lines
#' # PlotDistHistBeta(hdata, "rate_gray", title,
#' #                     sd_color=NULL)
#' @export
PlotDistHistBeta <- function(frm, xvar, title,
                             ...,
                             bins = 30,
                             hist_color='darkgray',
                             beta_color='blue',
                             mean_color='blue',
                             sd_color='darkgray') {
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
  dDist <- data.frame(x=seq(1/200,1-1/200,length.out=100))
  colnames(dDist) <- xvar
  dDist$density <- dbeta(dDist[[xvar]],shape1=shape1,shape2=shape2)
  # each observation adds 1/bins area to the histogram, as the range
  # is 0:1. So rescale density to have similar area for presentation.

  histogramArea = length(x)*(1/bins) # each obsevation grows one bin, bin-width = width([0, 1])/bins
  dDist$count = histogramArea*dDist$density


  plt = ggplot2::ggplot() +
    ggplot2::geom_histogram(data=dPlot,
                            mapping=ggplot2::aes(!!!simulate_aes_string(x=xvar)),
                            bins = bins,
                            fill=hist_color, color=NA) +
    ggplot2::geom_line(data=dDist,
                       mapping=ggplot2::aes(!!!simulate_aes_string(x=xvar,y='count')),
                       color=beta_color,linetype=2) +
    ggplot2::geom_vline(xintercept=meanx,color=mean_color,linetype=2)
  if(!is.null(sd_color)) {
    plt = plt +
      ggplot2::geom_vline(xintercept=meanx+sdx,color=sd_color,linetype=2) +
      ggplot2::geom_vline(xintercept=meanx-sdx,color=sd_color,linetype=2)
  }
  plt + ggplot2::ggtitle(title)
}
