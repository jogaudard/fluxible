# plot for exponential fit

    Code
      flux_plot(slopes0_flag, fit_type = "exp", fit_slope_col = "f_fit_slope",
        f_plotname = "test_exp_plot", print_plot = FALSE)
    Output
      [1] "Saving plots in f_quality_plots folder."

# plot for linear fit

    Code
      flux_plot(slopes30lin_flag, fit_type = "lin", fit_slope_col = "f_fit_slope",
        f_plotname = "test_lin_plot", print_plot = FALSE)
    Output
      [1] "Saving plots in f_quality_plots folder."

# plot for linear fit with jpg extension works

    Code
      flux_plot(slopes30lin_flag, fit_type = "lin", fit_slope_col = "f_fit_slope",
        f_plotname = "test_lin_plot", print_plot = FALSE, output = "ggsave", device = "jpg")
    Message
      Saving 7 x 7 in image
    Output
      [1] "Saving plots in f_quality_plots folder."

