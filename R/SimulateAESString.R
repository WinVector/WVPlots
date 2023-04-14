

#' Simulate the needlessly deprecated \code{ggplot2::aes_string()}.
#'
#' Use to allow (needlessly) replacing code of the form \code{ggplot2::aes_string(...)}
#' with code of the form \code{ggplot2::aes(!!!simulate_aes_string(...))}.
#' Purpose is to get out of the way of the deprecation and possible future removal of \code{ggplot2::aes_string()}.
#' This will itself break when \code{rlang} again chantes its API and deprecates the affordances used.
#' Inspired by the research of \url{https://stackoverflow.com/a/74424353/6901725}.
#'
#'
#' @param ... named string arguments to turn into symbols using `rlang::data_sym()`.
#' @return some rlang NSE that simulates values at great complexity (but needed for newer ggplot2()).
#'
#' @examples
#'
#' d <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' xvar <- 'x'  # the idea is, this is passed in and not known at coding time
#' yvar <- 'y'
#' # what we want:
#' #  ggplot2::ggplot(data = d, mapping = ggplot2::aes_string(x = xvar, y = yvar)) +
#' #     ggplot2::geom_point()
#' # The required "tidy evaluation ideoms[sic] with `aes()`".
#' ggplot2::ggplot(data = d, mapping = ggplot2::aes(!!!simulate_aes_string(x = xvar, y = yvar))) +
#'    ggplot2::geom_point()
#'
#' @export
simulate_aes_string <- function(...) {
  # replace the needlessly deprecated aes_string
  # from:
  #  https://stackoverflow.com/a/74424353/6901725
  args <- list(...)
  for (v in args) {
    if (is.character(v)) {
      stopifnot(length(v) == 1)
    }
  }
  lapply(args, function(x) {if (is.character(x)) rlang::data_sym(x) else x})
}
