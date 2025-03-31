test_that("plot for exponential fit", {
  expect_snapshot(
    suppressMessages( # because the progress bar is messing with check()
      flux_plot(slopes0_flag,
        conc,
        datetime,
        f_plotname = "test_exp_plot",
        print_plot = FALSE,
        output = "pdfpages"
      )
    )
  )
  # the plots are quite heavy so we do not keep them
  unlink("f_quality_plots/", recursive = TRUE, force = TRUE)
})

test_that("plot for linear fit", {
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for linear fit",
      flux_plot(slopes30lin_flag, conc, datetime)
    )
  )
})

test_that("plot for linear fit with jpg extension works", {
  expect_snapshot(
    suppressMessages( # because the progress bar is messing with check()
      flux_plot(slopes30lin_flag,
        conc,
        datetime,
        f_plotname = "test_lin_plot",
        print_plot = FALSE,
        output = "ggsave",
        ggsave_args = list(device = "jpg")
      )
    )
  )
  # the plots are quite heavy so we do not keep them
  unlink("f_quality_plots/", recursive = TRUE, force = TRUE)
})

test_that("plot can be exported as an object", {
  plot_object <- flux_plot(slopes30lin_flag, conc, datetime)
  vdiffr::expect_doppelganger("plot as an object", plot_object)
})

test_that("plot for exp_tz fit", {
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for exp_tz fit",
      flux_fitting(
        co2_conc,
        conc,
        datetime,
        fit_type = "exp_tz",
        end_cut = 60,
        t_zero = 20
      ) |>
        flux_quality(conc) |>
        flux_plot(conc, datetime)
    )
  )
})
test_that("plot for exp_hm fit", {
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for exp_hm fit",
      flux_fitting(
        co2_conc,
        conc,
        datetime,
        fit_type = "exp_hm",
        end_cut = 60,
        t_zero = 20
      ) |>
        flux_quality(conc) |>
        flux_plot(conc, datetime)
    )
  )
})
