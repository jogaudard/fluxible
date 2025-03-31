test_that("works for exponential fitting", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "exp_zhao18"
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for linear fitting", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "lin"
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for quadratic fitting", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "qua"
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for exponential fitting with cut", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "exp_zhao18",
      start_cut = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for linear fitting with cut", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "line",
      start_cut = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("removing duplicated datetime", {
  rep <- co2_conc[rep(1, 200), ] |>
    mutate(
      conc = runif(n = 200, min = 420, max = 460)
    )

  rep_data <- rbind(co2_conc, rep) |>
    arrange(datetime)

  expect_snapshot(
    flux_fitting(
      rep_data,
      conc,
      datetime,
      fit_type = "exponential"
    )
  )
})

test_that("correct flux with duplicated datetime", {
  rep <- co2_conc[rep(1, 200), ] |>
    mutate(
      conc = runif(n = 200, min = 420, max = 460)
    )

  rep_data <- rbind(co2_conc, rep) |>
    arrange(datetime)

  qflux_fitting <- purrr::quietly(flux_fitting)
  expect_equal(
    qflux_fitting(
      rep_data,
      conc,
      datetime,
      fit_type = "exponential"
    ),
    qflux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "exponential"
    )
  )
})

test_that("works for exp_tz fitting", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "exp_tz"
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})


test_that("works for exp_zhao18 with missing data", {
  expect_snapshot(
    flux_fitting(
      co2_conc_missing,
      conc,
      datetime,
      fit_type = "exp_zhao18",
      end_cut = 60,
      t_zero = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for exp_zhao18 with mid missing data", {
  expect_snapshot(
    flux_fitting(
      co2_conc_mid_missing,
      conc,
      datetime,
      fit_type = "exp_zhao18",
      end_cut = 60,
      t_zero = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for exp_tz with mid missing data", {
  expect_snapshot(
    flux_fitting(
      co2_conc_mid_missing,
      conc,
      datetime,
      fit_type = "exp_tz",
      end_cut = 60,
      t_zero = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for quadratic with mid missing data", {
  expect_snapshot(
    flux_fitting(
      co2_conc_mid_missing,
      conc,
      datetime,
      fit_type = "quadratic",
      end_cut = 60,
      t_zero = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

# producing an error where optim can not optimize the equation
test_that("exp_tz: optim produces non-finite values", {

  test_data <- co2_conc_missing |>
    dplyr::mutate(
      conc = replace(
        conc,
        c(297:425, 427:490, 495:506),
        NA
      )
    )

  expect_snapshot(
    flux_fitting(
      test_data,
      conc,
      datetime,
      fit_type = "exp_tz",
      end_cut = 60,
      t_zero = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("exp_zhao18: optim produces non-finite values", {

  test_data <- co2_conc_missing |>
    dplyr::mutate(
      conc = replace(
        conc,
        c(297:425, 427:490, 495:506),
        NA
      )
    )

  expect_snapshot(
    flux_fitting(
      test_data,
      conc,
      datetime,
      fit_type = "exp_zhao18",
      end_cut = 60,
      t_zero = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for exp_hm fitting", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "exp_hm"
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})
