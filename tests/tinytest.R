
if (requireNamespace("tinytest", quietly=TRUE) ) {
  if (requireNamespace('data.table', quietly = TRUE)) {
    # don't multi-thread during CRAN checks
    data.table::setDTthreads(1)
  }
  tinytest::test_package("WVPlots")
}

