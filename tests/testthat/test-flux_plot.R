test_that("plot for exponential fit", {
  expect_snapshot(
    flux_plot(slopes0_flag,
      fit_type = "exp",
      fit_slope_col = "f_fit_slope",
      f_plotname = "test_exp_plot", print_plot = TRUE
    )
  )
})

test_that("plot for linear fit", {
  expect_snapshot(
    flux_plot(slopes30lin_flag,
      fit_type = "lin",
      fit_slope_col = "f_fit_slope",
      f_plotname = "test_lin_plot", print_plot = TRUE
    )
  )
})


# test_that("plot for exponential fit", {
#   expect_equal(
#     flux_plot(slopes0_flag,
#       fit_type = "exp",
#       fit_slope_col = "f_fit_slope",
#       f_plotname = "test_exp_plot", print_plot = TRUE
#     ),
#     flux_plot_exp(slopes0_flag,
#       fit_slope_col = "f_fit_slope",
#       f_plotname = "test_exp_plot", print_plot = TRUE
#     )
#   )
# })

# test_that("plot for linear fit", {
#   expect_equal(
#     flux_plot(slopes30lin_flag,
#       fit_type = "lin",
#       f_plotname = "test_lin_plot", print_plot = TRUE
#     ),
#     flux_plot_lin(slopes30lin_flag, f_plotname = "test_lin_plot",
#       print_plot = TRUE
#     )
#   )
# })
