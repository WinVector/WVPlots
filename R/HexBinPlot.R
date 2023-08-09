


#' Build a hex bin plot
#'
#' Build a hex bin plot with rational color coding.
#'
#' Builds a standard ggplot2 hexbin plot,
#' with a color scale such that dense areas are colored darker (the default
#' ggplot2 fill scales will color dense areas lighter).
#'
#' The user can choose an alternate color scale with endpoints \code{lightcolor}
#' and \code{darkcolor}; it is up to the user to make sure that \code{lightcolor}
#' is lighter than \code{darkcolor}.
#'
#' Requires the \code{hexbin} package.
#'
#' @param d data frame
#' @param xvar name of x variable column
#' @param yvar name of y variable column
#' @param title plot title
#' @param ... not used, forces later arguments to bind by name
#' @param lightcolor light color for least dense areas
#' @param darkcolor dark color for most dense areas
#' @param bins passed to geom_hex
#' @param binwidth passed to geom_hex
#' @param na.rm passed to geom_hex
#' @return a ggplot2 hexbin plot
#'
#' @seealso \code{\link[ggplot2]{geom_hex}}
#'
#' @examples
#'
#' if(requireNamespace("hexbin", quietly = TRUE)) {
#'    if (requireNamespace('data.table', quietly = TRUE)) {
#'		   # don't multi-thread during CRAN checks
#' 		   data.table::setDTthreads(1)
#'    }
#'    set.seed(634267)
#'    dframe = data.frame(x = rnorm(1000), y = rnorm(1000))
#'    print(HexBinPlot(dframe, "x", "y", "Example hexbin"))
#'
#'    diamonds = ggplot2::diamonds
#'    print(HexBinPlot(diamonds, "carat", "price", "Diamonds example"))
#'
#'    # change the colorscale
#'     print(HexBinPlot(diamonds, "carat", "price", "Diamonds example",
#'                      lightcolor="#fed98e",
#'                      darkcolor="#993404"))
#' }
#'
#' @export
#'
HexBinPlot <- function(d, xvar, yvar,  title,
                      ...,
                      lightcolor = "#deebf7",
                      darkcolor = "#000000",
                      bins = 30, binwidth = NULL, na.rm = FALSE) {
  if(!(requireNamespace("hexbin", quietly = TRUE))) {
    stop("WVPlots::HexBinPlot requires the hexbin package be installed")
  }
  check_frame_args_list(...,
                             frame = d,
                             name_var_list = list(xvar = xvar, yvar = yvar),
                             title = title,
                             funname = "WVPlots::HexBinPlot")

  ggplot2::ggplot(
      data = d,
      mapping = ggplot2::aes(!!!simulate_aes_string(x=xvar, y=yvar))) +
    ggplot2::geom_hex(bins = bins, binwidth = binwidth, na.rm = na.rm) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_fill_gradient(low = lightcolor, high = darkcolor, space = "Lab") +
    ggplot2::theme_bw()
}



