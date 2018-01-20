#' WVPlots: Common Plots for Analysis
#'
#' Select data analysis plots, under a standardized calling interface implemented on top of 'ggplot2' and 'plotly'.
#' Plots of interest include: 'ROC', gain curve, scatter plot with marginal distributions,
#' conditioned scatter plot with marginal densities.
#' box and stem with matching theoretical distribution, density with matching theoretical distribution.
#'
#' To learn more about replyr, please start with the vignette:
#' \code{vignette('replyr','replyr')}
#'
#'
#'For more information:
#' \itemize{
#'   \item \code{vignette(package='WVPlots')}
#'   \item \code{RShowDoc('WVPlots_examples',package='WVPlots')}
#'   \item Website: \url{https://github.com/WinVector/WVPlots} }
#'
#' @docType package
#' @name WVPlots
NULL

#' @importFrom wrapr :=
NULL

#' @importFrom utils packageVersion
NULL

#' @importFrom mgcv gam
NULL

#' @importFrom seplyr novelName
NULL

# re-export so old code and demos work (from when functions were here)


#' @importFrom wrapr DebugFn
#' @export
wrapr::DebugFn

#' @importFrom wrapr DebugFnE
#' @export
wrapr::DebugFnE

#' @importFrom wrapr DebugFnW
#' @export
wrapr::DebugFnW

#' @importFrom wrapr DebugFnWE
#' @export
wrapr::DebugFnWE

#' @importFrom wrapr DebugPrintFn
#' @export
wrapr::DebugPrintFn

#' @importFrom wrapr DebugPrintFnE
#' @export
wrapr::DebugPrintFnE
