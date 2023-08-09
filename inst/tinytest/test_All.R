
if (requireNamespace('data.table', quietly = TRUE)) {
  # don't multi-thread during CRAN checks
  data.table::setDTthreads(1)
}


test_a_1 <- function() {
  set.seed(52523)
  d = data.frame(meas=rnorm(100))
  threshold = -1.5
  print(WVPlots::ShadedDensity(d, "meas", threshold,
                         title="Example shaded density plot, left tail"))
  print(WVPlots::ShadedDensity(d, "meas", -threshold, tail="right",
                         title="Example shaded density plot, right tail"))

  invisible(NULL)
}

test_a_1()


test_a_2 <- function() {
  set.seed(52523)
  d <- data.frame(PC1=rnorm(10),PC2=rnorm(10))
  d$y <- as.numeric(d$PC1+d$PC2>0)
  plt <- WVPlots::ScatterHistN(d,'PC1','PC2','y',"test")
  expect_true(!is.null(plt))
  print(plt)
  invisible(NULL)
}

test_a_2()
