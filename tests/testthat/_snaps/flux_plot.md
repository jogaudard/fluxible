# plot for exponential fit

    Code
      suppressMessages(flux_plot(slopes0_flag, conc, datetime, f_plotname = "test_exp_plot",
        print_plot = FALSE, output = "pdfpages"))

# plot for linear fit

    Code
      vdiffr::expect_doppelganger("plot for linear fit", flux_plot(slopes30lin_flag,
        conc, datetime))
    Message
      Plotting in progress

# plot for linear fit with jpg extension works

    Code
      suppressMessages(flux_plot(slopes30lin_flag, conc, datetime, f_plotname = "test_lin_plot",
        print_plot = FALSE, output = "ggsave", ggsave_args = list(device = "jpg")))

# plot for exp_tz fit

    Code
      vdiffr::expect_doppelganger("plot for exp_tz fit", flux_plot(flux_quality(
        flux_fitting(co2_conc, conc, datetime, fit_type = "exp_tz", end_cut = 60,
          t_zero = 20), conc), conc, datetime))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
      
       Total number of measurements: 6
      
       ok 	 5 	 83 %
       discard 	 1 	 17 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
      Plotting in progress

# plot for exp_tz fit with mid missing data

    Code
      vdiffr::expect_doppelganger("plot for exp_tz fit with mid missing data",
        flux_plot(flux_quality(flux_fitting(co2_conc_mid_missing, conc, datetime,
          fit_type = "exp_tz", end_cut = 60, t_zero = 20), conc), conc, datetime))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_exptz()`:
      
       fluxID 1 : slope was estimated on 139 points out of 150 seconds
       fluxID 2 : slope was estimated on 114 points out of 150 seconds
       fluxID 4 : slope was estimated on 103 points out of 150 seconds
    Message
      
       Total number of measurements: 6
      
       ok 	 5 	 83 %
       discard 	 1 	 17 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
      Plotting in progress

# plot for exp_zhao18 fit with mid missing data

    Code
      vdiffr::expect_doppelganger("plot for exp_zhao18 fit with mid missing data",
        flux_plot(flux_quality(flux_fitting(co2_conc_mid_missing, conc, datetime,
          fit_type = "exp_zhao18", end_cut = 60, t_zero = 20), conc), conc, datetime))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
    Condition
      Warning in `flux_fitting_zhao18()`:
      
       fluxID 1 : slope was estimated on 139 points out of 150 seconds
       fluxID 2 : slope was estimated on 114 points out of 150 seconds
       fluxID 4 : slope was estimated on 103 points out of 150 seconds
    Message
      
       Total number of measurements: 6
      
       ok 	 6 	 100 %
       discard 	 0 	 0 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
      Part of the fit will not be displayed
          because f_ylim_lower is too high.
      Plotting in progress

# plot for quadratic fit with mid missing data

    Code
      vdiffr::expect_doppelganger("plot for quadratic fit with mid missing data",
        flux_plot(flux_quality(flux_fitting(co2_conc_mid_missing, conc, datetime,
          fit_type = "quadratic", end_cut = 60, t_zero = 20), conc), conc, datetime))
    Condition
      Warning in `flux_fitting_quadratic()`:
      
       fluxID 1 : slope was estimated on 139 points out of 150 seconds
       fluxID 2 : slope was estimated on 114 points out of 150 seconds
       fluxID 4 : slope was estimated on 103 points out of 150 seconds
    Message
      
       Total number of measurements: 6
      
       ok 	 6 	 100 %
       discard 	 0 	 0 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
      Plotting in progress

# plot for exp_hm fit

    Code
      vdiffr::expect_doppelganger("plot for exp_hm fit", flux_plot(flux_quality(
        flux_fitting(co2_conc, conc, datetime, fit_type = "exp_hm", end_cut = 60,
          t_zero = 20), conc), conc, datetime))
    Message
      Cutting measurements...
      Estimating starting parameters for optimization...
      Optimizing fitting parameters...
      Calculating fits and slopes...
      Done.
      
       Total number of measurements: 6
      
       discard 	 4 	 67 %
       ok 	 2 	 33 %
       zero 	 0 	 0 %
       force_discard 	 0 	 0 %
       start_error 	 0 	 0 %
       no_data 	 0 	 0 %
       force_ok 	 0 	 0 %
       force_zero 	 0 	 0 %
      Plotting in progress

