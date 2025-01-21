# plot for exponential fit

    Code
      suppressMessages(flux_plot(slopes0_flag, f_plotname = "test_exp_plot",
        print_plot = FALSE, output = "pdfpages"))

# plot for linear fit

    Code
      vdiffr::expect_doppelganger("plot for linear fit", flux_plot(slopes30lin_flag))
    Message
      Plotting in progress

# plot for linear fit with jpg extension works

    Code
      suppressMessages(flux_plot(slopes30lin_flag, f_plotname = "test_lin_plot",
        print_plot = FALSE, output = "ggsave", ggsave_args = list(device = "jpg")))

# plot for segments

    Code
      vdiffr::expect_doppelganger("plot for segments", flux_plot(slopes_pftc7_flags))
    Message
      Part of the fit will not be displayed
          because f_ylim_lower is too high.
      Plotting in progress

