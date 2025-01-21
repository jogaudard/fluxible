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

  
  pftc7_segmented_short_expected <- pftc7_segmented_short |>
    rename(
      f_conc = "co2_conc",
      f_datetime = "date_time",
      f_start = "start_time",
      f_fluxID = "file_name",
      f_rsquared = "f_rsq",
      f_adj_rsquared = "f_rsq_adj",
      f_pvalue = "f_pval",
      f_par = "par"
    ) |>
    group_by(f_fluxID) |>
    mutate(
      f_cut = as.factor(f_cut),
      f_fluxID = as.factor(f_fluxID),
      f_mean_slope = mean(f_slope, na.rm = TRUE)
    ) |>
    ungroup() |>
    arrange(f_datetime) |>
    select(f_par, f_datetime, f_fluxID, f_mean_slope, temperature_c, pressure) |>
       data.frame()

    pftc7_short_segmented_test <- pftc7_short |>
    group_by(file_name) |>
    dplyr::slice(8:n()) |>
    ungroup() |>
    flux_fitting(
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 0,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    ) |>
    flux_quality(par_threshold = 650,
  sign_str_threshold = 95,
  pvalue_threshold = 0,
  rsquared_threshold = 0,
  sd_threshold = 1,
  ratio_threshold = 0) |>
    arrange(f_datetime) |>
    select(f_par, f_datetime, f_fluxID, f_mean_slope, temperature_c, pressure) |>
    data.frame()

  expect_equal(
    pftc7_short_segmented_test,
    pftc7_segmented_short_expected,
    tolerance = 0.06
  )
})

test_that("segmentation tool original data", {

  pftc7_segmented_short_expected <- pftc7_segmented_short |>
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
    mutate(
      f_fluxID = as.factor(f_fluxID)
    ) |>
    select(f_conc, f_datetime, par, f_fluxID, temperature_c, pressure) |>
    arrange(f_datetime)

    pftc7_short_segmented_test <- pftc7_short |>
    rename(
      f_conc = "co2_conc",
      f_datetime = "date_time",
      f_fluxID = "file_name"
    ) |>
    group_by(f_fluxID) |>
    dplyr::slice(8:n()) |>
    ungroup() |>
    mutate(
      f_fluxID = as.factor(f_fluxID)
    ) |>
    select(f_conc, f_datetime, par, f_fluxID, temperature_c, pressure) |>
    arrange(f_datetime)

  expect_equal(
    pftc7_short_segmented_test,
    pftc7_segmented_short_expected
  )
})

test_that("segmentation tool snapshot", {
  expect_snapshot(
    flux_fitting(
      conc_df = pftc7_short,
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 0,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    ) |>
      dplyr::select(
        f_fluxID,
        f_slope,
        f_rsquared,
        f_adj_rsquared,
        f_pvalue,
        f_segment_length
      ) |>
      dplyr::distinct()
  )
})


test_that("fitting segment works without par", {

  test_data <- pftc7_short |>
    dplyr::select(!par)

  expect_snapshot(
    flux_fitting(
      conc_df = test_data,
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 0,
      end_cut = 0,
      conc_col = "co2_conc",
      # par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    ) |>
      dplyr::select(
        f_fluxID,
        f_slope,
        f_rsquared,
        f_adj_rsquared,
        f_pvalue,
        f_segment_length
      ) |>
      dplyr::distinct()
  )
})


test_that("fitting segment works without signal strength", {

  test_data <- pftc7_short |>
    dplyr::select(!signal_strength)

  expect_snapshot(
    flux_fitting(
      conc_df = test_data,
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 0,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      # sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    ) |>
      dplyr::select(
        f_fluxID,
        f_slope,
        f_rsquared,
        f_adj_rsquared,
        f_pvalue,
        f_segment_length
      ) |>
      dplyr::distinct()
  )
})


test_that("fitting segment works without h2o concentration", {

  test_data <- pftc7_short |>
    dplyr::select(!h2o_conc)

  expect_snapshot(
    flux_fitting(
      conc_df = test_data,
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 0,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      # h2o_col = "h2o_conc",
      sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = FALSE,
      min_seg_length = 30
    ) |>
      dplyr::select(
        f_fluxID,
        f_slope,
        f_rsquared,
        f_adj_rsquared,
        f_pvalue,
        f_segment_length
      ) |>
      dplyr::distinct()
  )
})

test_that("error on col format", {

  test_data <- pftc7_short |>
    dplyr::mutate(
      h2o_conc = "humide"
    )

  expect_error(
    flux_fitting(
      conc_df = test_data,
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 0,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = FALSE,
      min_seg_length = 30
    )
  )
})

# segmentation with chamber data
