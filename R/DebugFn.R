
#' Capture arguments of exception throwing plot for later debugging.
#'
#' Run fn, save arguments on failure.
#'
#' @param saveFile path to save RDS to.
#' @param fn function to call
#' @param ... arguments for fn
#' @return fn(...) normally, but if f(...) throws an exception save to saveFile RDS of list r such that do.call(r$fn,r$args) repeats the call to fn with args.
#'
#' @examples
#'
#' d <- data.frame(x=1:5)
#' DebugFn('problem.RDS','PlotDistCountNormal',d,xvar='x','example')
#' tryCatch(
#'    DebugFn('problem.RDS','PlotDistCountNormal',
#'       d,xvar='xmisspelled','example'),
#'    error = function(e) { print(e) })
#'
#' @export
DebugFn <- function(saveFile,fn,...) {
  args <- list(...)
  tryCatch({
    res = do.call(fn,args)
    res
  },
  error = function(e) {
    saveRDS(object=list(fn=fn,args=args),file=saveFile)
    stop(paste0("Wrote '",saveFile,"' on catching '",as.character(e),"'"))
    })
}

#' Capture arguments of exception throwing plot for later debugging.
#'
#' Run fn and print result, save arguments on failure.  Use on systems like ggplot()
#' where some calculation is delayed until print().
#'
#' @param saveFile path to save RDS to.
#' @param fn function to call
#' @param ... arguments for fn
#' @return fn(...) normally, but if f(...) throws an exception save to saveFile RDS of list r such that do.call(r$fn,r$args) repeats the call to fn with args.
#'
#' @examples
#'
#' d <- data.frame(x=1:5)
#' DebugPrintFn('problem.RDS','PlotDistCountNormal',d,xvar='x','example')
#' tryCatch(
#'    DebugPrintFn('problem.RDS','PlotDistCountNormal',
#'       d,xvar='xmisspelled','example'),
#'    error = function(e) { print(e) })
#'
#' @export
DebugPrintFn <- function(saveFile,fn,...) {
  args <- list(...)
  tryCatch({
    res = do.call(fn,args)
    print(res)
    res
  },
  error = function(e) {
    saveRDS(object=list(fn=fn,args=args),file=saveFile)
    stop(paste0("Wrote '",saveFile,"' on catching '",as.character(e),"'"))
  })
}
