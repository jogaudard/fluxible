test_that("works for exponential fitting", {
  expect_snapshot(
    flux_quality(slopes0,
      fit_type = "expo",
      slope_col = "f_slope"
    )
  )
})

test_that("works for linear fitting", {
  expect_snapshot(
    flux_quality(slopes0lin,
      fit_type = "li"
    )
  )
})

test_that("works for quadratic fitting", {
  expect_snapshot(
    flux_quality(slopes30qua,
      fit_type = "quadratic"
    )
  )
})

test_that("segmentation tool", {
  expect_snapshot(
    flux_quality(slopes_pftc7
    ) |>
    select(
      f_fluxID, f_mean_slope, f_mean_slope_corr, f_quality_flag, f_sd_slope
      ) |>
    dplyr::distinct()
  )
})