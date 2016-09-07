

# define some helper and reporting functions
# calculate fraction of area above prevalance floor that is under curve
# length(x)>=2
# length(x)==length(y)
# y>=0, x increasing and min(x)==0,max(x)==1
prArea <- function(x,y) {
  n <- length(x)
  sum(0.5*(y[-1]+y[-n])*(x[-1]-x[-n]))
}

#' Plot Precision-Recall plot.
#'
#' See http://www.nature.com/nmeth/journal/v13/n8/full/nmeth.3945.html
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param truthVar name of the dependent (output or result to be modeled) column in frame
#' @param truthTarget value we consider to be positive
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @examples
#'
#' set.seed(34903490)
#' x = rnorm(50)
#' y = 0.5*x^2 + 2*x + rnorm(length(x))
#' frm = data.frame(x=x,y=y,yC=y>=as.numeric(quantile(y,probs=0.8)))
#' frm$absY <- abs(frm$y)
#' frm$posY = frm$y > 0
#' frm$costX = 1
#' WVPlots::PRPlot(frm, "x", "yC", title="Example Precision-Recall plot")
#'
#' @export
PRPlot <- function(frame, xvar, truthVar, truthTarget, title,...) {
  checkArgs(frame=frame,xvar=xvar,yvar=truthVar,title=title,...)
  outcol <- frame[[truthVar]]==truthTarget
  if(length(unique(outcol))!=2) {
    return(NULL)
  }

  prevalence = mean(as.numeric(outcol) == max(as.numeric(outcol)))
  predcol <- frame[[xvar]]
  pred <- ROCR::prediction(predcol,outcol)
  perf <-  ROCR::performance(pred,'prec','rec')

  pf <- data.frame(
    Recall=perf@x.values[[1]],
    Precision=perf@y.values[[1]])

  # ROCR marks the pt where recall=0 as precision=NaN
  # get rid of that, and make it 1
  badPosns <-is.nan(pf$Precision) | is.na(pf$Precision) |
                      is.infinite(pf$Precision) |
                      (pf$Precision<0) | (pf$Precision>1) |
                      is.nan(pf$Recall) | is.na(pf$Recall) |
                      is.infinite(pf$Recall) |
                      (pf$Recall<=0) | (pf$Recall>1)
  pf <- pf[!badPosns,]
  # add in ideal points
  pf <- rbind(pf,data.frame(
    Recall=c(0,1),
    Precision=c(prevalence,prevalence)
  ))
  pf <- pf[order(pf$Recall),]
  f1 <- 2*pf$Recall*pf$Precision/(pf$Recall+pf$Precision)
  bestX <- which.max(f1)
  bestF1 <- f1[[bestX]]
  pF1 <- pf[bestX,]

  pra <- prArea(pf$Recall,pf$Precision)

  # curves of constant F1 with precision as a function of recall.
  isoFrame <- data.frame(Recall=seq(0.01,1,by=0.01))
  isoFrame$Precision <- bestF1*isoFrame$Recall/(2*isoFrame$Recall-bestF1)
  isoFrame <- isoFrame[(isoFrame$Precision<=1) & (isoFrame$Precision>0),]
  f1check <- 2*isoFrame$Recall*isoFrame$Precision/(isoFrame$Recall+isoFrame$Precision)


  palletName = "Dark2"
  plot= ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=pf,
                         ggplot2::aes(x=Recall,ymax=Precision,ymin=0),
                         alpha=0.3) +
    ggplot2::geom_point(data=pf,
                        ggplot2::aes(x=Recall,y=Precision),
                        alpha=0.8) +
    ggplot2::geom_point(data=pF1,
                        ggplot2::aes(x=Recall,y=Precision),
                        color='blue',size=2,shape=15) +
    ggplot2::geom_line(data=pf,
                       ggplot2::aes(x=Recall,y=Precision)) +
    ggplot2::geom_line(data=isoFrame,
                       ggplot2::aes(x=Recall,y=Precision),
                       color='blue',alpha=0.5,linetype=2) +
    ggplot2::geom_hline(yintercept=prevalence, linetype=2) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_brewer(palette=palletName) +
    ggplot2::scale_color_brewer(palette=palletName) +
    ggplot2::ggtitle(paste0(title,'\n',
                           'best f1 ',format(bestF1, digits=2, nsmall=2),
                           ', area ',format(pra, digits=2,nsmall=2),
                           '\n',
                           truthVar, '~', xvar))
  plot
}
