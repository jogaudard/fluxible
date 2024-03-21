test_that("linear plot works", {
  expect_snapshot(flux_plot_lin(slopes30lin_flag,
    datetime_col = "datetime",
    cut_col = "cut", fit_col = "fit", start_col = "start"
  ))
})
