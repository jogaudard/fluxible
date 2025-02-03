test_that("GEP calculation", {
  expect_snapshot(
    flux_gep(co2_fluxes,
      flux,
      type,
      f_start,
      PAR,
      id_cols = "turfID",
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
      flux,
      type,
      datetime,
      par,
      id_cols = c("turfid", "campaign")
    )
  )
})

test_that("missing NEE and several id cols", {
  campaign <- c(1, 1, 2, 2, 3, 3, 3, 4, 4)
  turfid <- c("A", "A", "A", "A", "B", "B", "C", "D", "A")
  type <- c("NEE", "ER", "NEE", "ER", "NEE", "ER", "ER", "ER", "ER")
  flux <- c(3, 5, 2, 7, 9, 11, 10, 13, 8)
  datetime <- c(
    "2024-02-11 10:00:00",
    "2024-02-11 10:00:10",
    "2024-02-11 10:00:20",
    "2024-02-11 10:00:30",
    "2024-02-11 10:00:40",
    "2024-02-11 10:00:50",
    "2024-02-11 10:01:00",
    "2024-02-11 10:01:10",
    "2024-02-11 10:01:20"
  )
  par <- c(300, 2, 250, 5, 320, 1, 0, 3, 4)

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
      flux,
      type,
      datetime,
      par,
      id_cols = c("turfid", "campaign")
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
      flux,
      type,
      datetime,
      par,
      id_cols = c("turfid", "campaign")
    )
  )
})
