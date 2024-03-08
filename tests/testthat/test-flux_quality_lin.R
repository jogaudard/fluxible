test_that("quality works on linear fit", {
    expect_snapshot(flux_quality_lin(slopes0lin, fluxID_col = "fluxID", slope_col = "slope", conc_col = "conc"))
})