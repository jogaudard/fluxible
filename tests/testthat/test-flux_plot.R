test_that("plot for exponential fit", {
  expect_snapshot(
    suppressMessages( #because the progress bar is messing with check()
      flux_plot(slopes0_flag,
      fit_type = "exp",
      fit_slope_col = "f_fit_slope",
      f_plotname = "test_exp_plot", print_plot = FALSE
    )
  )
  )
  # the plots are quite heavy so we do not keep them
    unlink("f_quality_plots/", recursive = TRUE, force = TRUE)
})

test_that("plot for linear fit", {
  expect_snapshot(
    suppressMessages( #because the progress bar is messing with check()
    flux_plot(slopes30lin_flag,
      fit_type = "lin",
      fit_slope_col = "f_fit_slope",
      f_plotname = "test_lin_plot", print_plot = FALSE
    )
  )
  )
  # the plots are quite heavy so we do not keep them
    unlink("f_quality_plots/", recursive = TRUE, force = TRUE)
})

test_that("plot for linear fit with jpg extension works", {
  expect_snapshot(
    suppressMessages( #because the progress bar is messing with check()
    flux_plot(slopes30lin_flag,
      fit_type = "lin",
      fit_slope_col = "f_fit_slope",
      f_plotname = "test_lin_plot",
      print_plot = FALSE,
      output = "ggsave",
      device = "jpg"
    )
  )
  )
  # the plots are quite heavy so we do not keep them
  unlink("f_quality_plots/", recursive = TRUE, force = TRUE)
})

# test_that("plot for linear fit with jpg extension works (without device)", {
#   expect_snapshot(
#     flux_plot(slopes30lin_flag,
#       fit_type = "lin",
#       fit_slope_col = "f_fit_slope",
#       f_plotname = "test_lin_plot.jpg",
#       print_plot = TRUE,
#       output = "ggsave"
#     )
#   )
# })


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

# removing the f_quality_plot folder because it is taking quite some space
