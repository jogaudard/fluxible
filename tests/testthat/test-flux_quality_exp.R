test_that("quality works on exp fit", {
  expect_snapshot(flux_quality_exp(slopes0, fluxID_col = "fluxID", conc_col = "conc", b_col = "b", time_col = "time", fit_col = "fit", slope_col = "slope_tz", cut_col = "cut"))
})
