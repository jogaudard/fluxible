# plot for exponential fit

    Code
      flux_plot(slopes0_flag, fit_type = "exp", fit_slope_col = "f_fit_slope",
        f_plotname = "test_exp_plot", print_plot = FALSE)
    Message
      Plotting in progress
      Plots saved in f_quality_plots folder.

# plot for linear fit

    Code
      flux_plot(slopes30lin_flag, fit_type = "lin", fit_slope_col = "f_fit_slope",
        f_plotname = "test_lin_plot", print_plot = FALSE)
    Message
      Plotting in progress
      Plots saved in f_quality_plots folder.

# plot for linear fit with jpg extension works

    Code
      flux_plot(slopes30lin_flag, fit_type = "lin", fit_slope_col = "f_fit_slope",
        f_plotname = "test_lin_plot", print_plot = FALSE, output = "ggsave", device = "jpg")
    Message
      Plotting in progress
      Saving plots with ggsave.
      Saving 7 x 7 in image
      Plots saved in f_quality_plots folder.

