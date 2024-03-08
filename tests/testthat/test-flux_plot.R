test_that("plot for exponential fit", {

    expect_equal(
        flux_plot(slopes0_flag, fit_type = "exp", datetime_col = "datetime", fit_slope_col = "fit_slope", start_col = "start", f_plotname = "test_exp_plot", print_plot = TRUE),
        flux_plot_exp(slopes0_flag, datetime_col = "datetime", fit_slope_col = "fit_slope", start_col = "start", f_plotname = "test_exp_plot", print_plot = TRUE)
    )

})

test_that("plot for linear fit", {

    expect_equal(
        flux_plot(slopes30lin_flag, fit_type = "lin", datetime_col = "datetime", cut_col = "cut", fit_col = "fit", start_col = "start", f_plotname = "test_lin_plot", print_plot = TRUE),
        flux_plot_lin(slopes30lin_flag, datetime_col = "datetime", cut_col = "cut", fit_col = "fit", start_col = "start", f_plotname = "test_lin_plot", print_plot = TRUE)
    )

})