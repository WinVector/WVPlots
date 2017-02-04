
#' Capture arguments of exception throwing plot for later debugging.
#'
#' Run fn, save arguments on failure.
#' @seealso \code{\link{DebugPrintFn}} .
#'
#' Note: newer versions of the debug functions are available in the replyr package.
#'
#' @param saveFile path to save RDS to.
#' @param fn function to call
#' @param ... arguments for fn
#' @return fn(...) normally, but if fn(...) throws an exception save to saveFile RDS of list r such that do.call(r$fn,r$args) repeats the call to fn with args.
#'
#' @examples
#'
#' d <- data.frame(x=1:5)
#' saveName <- paste0(tempfile('debug'),'.RDS')
#' # correct run
#' DebugFn(saveName, 'PlotDistCountNormal', d, xvar='x', 'example')
#' # now re-run
#' # capture error on incorrect run
#' tryCatch(
#'    DebugFn(saveName,'PlotDistCountNormal',
#'       d,xvar='xmisspelled','example'),
#'    error = function(e) { print(e) })
#' # examine details
#' situation <- readRDS(saveName)
#' str(situation)
#' # fix and re-run
#' situation$args$xvar <- 'x'
#' do.call(situation$fn,situation$args)
#' # clean up
#' file.remove(saveName)
#'
#' @export
DebugFn <- function(saveFile,fn,...) {
  args <- list(...)
  envir = parent.frame()
  tryCatch({
    res = do.call(fn,args, envir=envir)
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
#' Note: newer versions of the debug functions are available in the replyr package.
#'
#' @seealso \code{\link{DebugFn}}
#'
#' @param saveFile path to save RDS to.
#' @param fn function to call
#' @param ... arguments for fn
#' @return fn(...) normally, but if fn(...) throws an exception save to saveFile RDS of list r such that do.call(r$fn,r$args) repeats the call to fn with args.
#'
#' @examples
#'
#' d <- data.frame(x=1:5)
#' saveName <- paste0(tempfile('debug'),'.RDS')
#' # correct run
#' DebugPrintFn(saveName, 'PlotDistCountNormal', d, xvar='x', 'example')
#' # now re-run
#' # capture error on incorrect run
#' tryCatch(
#'    DebugPrintFn(saveName,'PlotDistCountNormal',
#'       d,xvar='xmisspelled','example'),
#'    error = function(e) { print(e) })
#' # examine details
#' situation <- readRDS(saveName)
#' str(situation)
#' # fix and re-run
#' situation$args$xvar <- 'x'
#' do.call(situation$fn,situation$args)
#' # clean up
#' file.remove(saveName)
#'
#' @export
DebugPrintFn <- function(saveFile,fn,...) {
  args <- list(...)
  envir = parent.frame()
  tryCatch({
    res = do.call(fn,args, envir=envir)
    print(res)
    res
  },
  error = function(e) {
    saveRDS(object=list(fn=fn,args=args),file=saveFile)
    stop(paste0("Wrote '",saveFile,"' on catching '",as.character(e),"'"))
  })
}
