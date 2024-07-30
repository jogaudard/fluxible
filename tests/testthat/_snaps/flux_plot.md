# plot for exponential fit

    Code
      suppressMessages(flux_plot(slopes0_flag, fit_type = "exp", fit_slope_col = "f_fit_slope",
        f_plotname = "test_exp_plot", print_plot = FALSE))

# plot for linear fit

    Code
      suppressMessages(flux_plot(slopes30lin_flag, fit_type = "lin", fit_slope_col = "f_fit_slope",
        f_plotname = "test_lin_plot", print_plot = FALSE))

# plot for linear fit with jpg extension works

    Code
      suppressMessages(flux_plot(slopes30lin_flag, fit_type = "lin", fit_slope_col = "f_fit_slope",
        f_plotname = "test_lin_plot", print_plot = FALSE, output = "ggsave", device = "jpg"))

