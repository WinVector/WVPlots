

#' @import wrapr
#' @import rquery
#' @import rqdatatable
NULL

. = NULL # don't look unbound

test_rqdatatable <- function() {
  d = data.frame(x = c(1, 2))
  x = NULL  # don't look unbound
  d %.>%
    extend(., y = x*x)
  return(d)
}
