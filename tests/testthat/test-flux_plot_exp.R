test_that("exponential plot works", {
  expect_snapshot(flux_plot_exp(slopes30_flag,
    datetime_col = "datetime",
    fit_slope_col = "fit_slope", start_col = "start"
  ))
})
