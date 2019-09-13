


.onAttach <- function(libname, pkgname) {
  prev_exec <- getOption("rquery.rquery_executor", default = NULL)
  if(is.null(prev_exec) || (is.list(prev_exec) && isTRUE(prev_exec$name == "rqdatable"))) {
    options(list("rquery.rquery_executor" = list(f = rqdatatable::ex_data_table, name = "rqdatable")))
  } else {
    packageStartupMessage("rqdatatable loaded, but did not register itself as the executor (already one registered)")
  }
  invisible()
}


.onLoad <- function(libname, pkgname) {
  prev_exec <- getOption("rquery.rquery_executor", default = NULL)
  if(is.null(prev_exec) || (is.list(prev_exec) && isTRUE(prev_exec$name == "rqdatable"))) {
    options(list("rquery.rquery_executor" = list(f = rqdatatable::ex_data_table, name = "rqdatable")))
  }
  invisible()
}

