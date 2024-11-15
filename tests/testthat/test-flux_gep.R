test_that("GEP calculation", {
  fluxes <- co2_fluxes  |>
    filter(
      turfID == "74 WN2C 155" |
        turfID == "109 AN3C 109" |
        turfID == "29 WN3C 106"
    )

  expect_snapshot(
    flux_gep(fluxes,
      id_cols = "turfID",
      flux_col = "flux",
      type_col = "type",
      datetime_col = "f_start",
      par_col = "PAR",
      cols_keep = c("temp_soil")
    )
  )
})

test_that("GEP calculation works with several id cols", {
  campaign <- c(1, 1, 2, 2, 3, 3)
  turfid <- c("A", "A", "A", "A", "B", "B")
  type <- c("NEE", "ER", "NEE", "ER", "NEE", "ER")
  flux <- c(3, 5, 2, 7, 9, 11)
  datetime <- c(
    "2024-02-11 10:00:00",
    "2024-02-11 10:00:10",
    "2024-02-11 10:00:20",
    "2024-02-11 10:00:30",
    "2024-02-11 10:00:40",
    "2024-02-11 10:00:50"
  )
  par <- c(300, 2, 250, 5, 320, 1)

  fluxes <- tibble(
    campaign,
    turfid,
    type,
    flux,
    datetime,
    par
  )

  expect_snapshot(
    flux_gep(fluxes,
      id_cols = c("turfid", "campaign"),
      flux_col = "flux",
      type_col = "type",
      datetime_col = "datetime",
      par_col = "par"
    )
  )
})

test_that("GEP error message for non numeric flux", {
  campaign <- c(1, 1, 2, 2, 3, 3)
  turfid <- c("A", "A", "A", "A", "B", "B")
  type <- c("NEE", "ER", "NEE", "ER", "NEE", "ER")
  flux <- c(3, "jasdg", 2, "a", 9, 11)
  datetime <- c(
    "2024-02-11 10:00:00",
    "2024-02-11 10:00:10",
    "2024-02-11 10:00:20",
    "2024-02-11 10:00:30",
    "2024-02-11 10:00:40",
    "2024-02-11 10:00:50"
  )
  par <- c(300, 2, 250, 5, 320, 1)

  fluxes <- tibble(
    campaign,
    turfid,
    type,
    flux,
    datetime,
    par
  )

  expect_error(
    flux_gep(fluxes,
      id_cols = c("turfid", "campaign"),
      flux_col = "flux",
      type_col = "type",
      datetime_col = "datetime",
      par_col = "par"
    )
  )
})
