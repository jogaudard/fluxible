test_that("mmol conversion works", {
    coef <- flux_units("mmol/s/m2")

    expect_equal(
        coef,
        0.001
    )
})

test_that("mmol and hours conversion works", {
    coef <- flux_units("mmol/d/m2")

    expect_equal(
        coef,
        86.4
    )
})

test_that("mol and minutes conversion works", {
    coef <- flux_units("mol/mn/m2")

    expect_equal(
        coef,
        6e-5
    )
})
