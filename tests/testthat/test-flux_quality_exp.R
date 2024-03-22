test_that("quality works on exp fit", {
  expect_snapshot(flux_quality_exp(slopes0,
    slope_col = "f_slope_tz"
  ))
})
