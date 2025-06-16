test_that("mmol conversion works", {
    coef <- flux_units("mmol/m2/s")

    expect_equal(
        coef,
        0.001
    )
})

test_that("mmol and hours conversion works", {
    coef <- flux_units("mmol/m2/d")

    expect_equal(
        coef,
        86.4
    )
})

test_that("mol and minutes conversion works", {
    coef <- flux_units("mol/m2/mn")

    expect_equal(
        coef,
        6e-5
    )
})

test_that("superseeded but still works", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  output <- flux_calc(
    slopes0,
    f_slope,
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    cols_ave = c("PAR", "temp_soil"),
    setup_volume = 24.575,
    atm_pressure = 1,
    plot_area = 0.0625,
    cut = FALSE
  ) |>
    dplyr::select(
      f_fluxid, f_temp_air_ave, datetime, f_flux, PAR_ave, temp_soil_ave
    )


  expect_snapshot(output)
})
