test_that("works for exponential fitting", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "expo") |>
      select(f_fluxID, f_slope) |>
      distinct()
  )
})

test_that("works for linear fitting", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "lin") |>
      select(f_fluxID, f_slope) |>
      distinct()
  )
})

test_that("works for quadratic fitting", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "qua") |>
      select(f_fluxID, f_slope) |>
      distinct()
  )
})

test_that("works for exponential fitting with cut", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "expo", start_cut = 20) |>
      select(f_fluxID, f_slope) |>
      distinct()
  )
})

test_that("works for linear fitting with cut", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "line", start_cut = 20) |>
      select(f_fluxID, f_slope) |>
      distinct()
  )
})

test_that("removing duplicated datetime", {
  rep <- co2_conc[rep(1, 200), ] |>
    mutate(
      f_conc = runif(n = 200, min = 420, max = 460)
    )

  rep_data <- rbind(co2_conc, rep) |>
    arrange(f_datetime)

  expect_snapshot(
    flux_fitting(rep_data, fit_type = "exp")
  )
})

test_that("correct flux with duplicated datetime", {
  rep <- co2_conc[rep(1, 200), ] |>
    mutate(
      f_conc = runif(n = 200, min = 420, max = 460)
    )

  rep_data <- rbind(co2_conc, rep) |>
    arrange(f_datetime)

  qflux_fitting <- purrr::quietly(flux_fitting)
  expect_equal(
    qflux_fitting(rep_data, fit_type = "exp"),
    qflux_fitting(co2_conc, fit_type = "exp")
  )
})

# test_that("segmentation tool", {
#   pftc7_short <- pftc7_short |>
#     mutate(
#       f_end = start_time + 120
#     )
  
#   pftc7_segmented_short <- pftc7_segmented_short |>
#     rename(
#       f_conc = "co2_conc",
#       f_datetime = "date_time",
#       f_start = "start_time",
#       f_fluxID = "file_name"
#     )

#   expect_equal(
#     flux_fitting(
#       pftc7_short,
#       fit_type = "segments",
#       start_col = "start_time",
#       end_col = "f_end",
#       start_cut = 6,
#       end_cut = 0,
#       conc_col = "co2_conc",
#       par_col = "par",
#       datetime_col = "date_time",
#       h2o_col = "h2o_conc",
#       signal_strength_col = "signal_strength",
#       fluxid_col = "file_name",
#       h2o_correction = TRUE,
#       min_seg_length = 30
#     ),
#     pftc7_segmented_short
#   )
# })

# segmentation without par
# segmentation without signal strength
# segmentation with chamber data
# segmentation tool without h2o column