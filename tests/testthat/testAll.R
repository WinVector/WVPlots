
context("Excercise Operations")

test_that("testBO: Works As Expected", {
  set.seed(52523)
  d = data.frame(meas=rnorm(100))
  threshold = -1.5
  print(WVPlots::ShadedDensity(d, "meas", threshold,
                         title="Example shaded density plot, left tail"))
  print(WVPlots::ShadedDensity(d, "meas", -threshold, tail="right",
                         title="Example shaded density plot, right tail"))
})
