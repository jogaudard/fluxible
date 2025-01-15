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

test_that("segmentation tool", {

  
  pftc7_segmented_short <- pftc7_segmented_short |>
    rename(
      f_conc = "co2_conc",
      f_datetime = "date_time",
      f_start = "start_time",
      f_fluxID = "file_name",
      f_rsquared = "f_rsq",
      f_adj_rsquared = "f_rsq_adj",
      f_pvalue = "f_pval",
      f_mean_slope_corr = "f_slope"
    ) |>
    select(f_conc, f_datetime, f_fluxID, f_fit, f_mean_slope_corr, f_rsquared, f_adj_rsquared, f_pvalue)

    # pftc7_short_segmented_test <- 

  expect_equal(
    flux_fitting(
      pftc7_short,
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 6,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      signal_strength_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    ) |>
    flux_quality(par_threshold = 600,
  sign_str_threshold = 98,
  pvalue_threshold = 0.3,
  rsquared_threshold = 0.7,
  sd_threshold = 0.1,
  ratio_threshold = 0) |>
    select(f_conc, f_datetime, f_fluxID, f_fit, f_mean_slope_corr, f_rsquared, f_adj_rsquared, f_pvalue),
    pftc7_segmented_short
  )
})

# segmentation without par
# segmentation without signal strength
# segmentation with chamber data
# segmentation tool without h2o column