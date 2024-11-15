test_that("GEP calculation", {
  expect_snapshot(
    flux_gep(co2_fluxes,
        id_cols = "turfID",
        flux_col = "flux",
        type_col = "type",
        datetime_col = "f_start",
        par_col = "PAR",
        cols_keep = c()
        )
  )
})