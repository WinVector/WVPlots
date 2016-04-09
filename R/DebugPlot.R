
#' Capture arguments of exception throwing plot for later debugging.
#'
#' Run and print plot.  Catch any errors and return either plot or arguments to re-run.
#'
#' @param plotFn
#'
#' @examples
#'
#' d <- data.frame(x=1:5)
#' res = DebugPlot('PlotDistCountNormal',d,xvar='x','example')
#' res = DebugPlot('PlotDistCountNormal',d,xvar='xmisspelled','example')
#' if(!('ggplot' %in% class(res))) {
#'   print("error, re-run by running: do.call(res$plotFn,res$args)")
#' }
#'
#' @export
DebugPlot <- function(plotFn,...) {
  args <- list(...)
  tryCatch({
    plt = do.call(plotFn,args)
    print(plt)
    plt
  },
  error = function(e) { list(plotFn=plotFn,args=args) })
}
