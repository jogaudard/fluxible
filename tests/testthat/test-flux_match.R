test_that("matching works", {
  expect_snapshot(flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    measurement_length = 180
  ) |>
    dplyr::select(f_fluxid, f_start, f_end) |>
    dplyr::distinct()
  )
})

test_that("time_diff works", {
  co2_df_short_180 <- co2_df_short %>%
    dplyr::mutate(
      datetime = datetime - 180 # logger is lagging 3 minutes behind
    )

  # test

  expect_snapshot(flux_match(
    co2_df_short_180,
    record_short,
    datetime,
    start,
    measurement_length = 220,
    time_diff = 180
  ))
})

test_that("renaming variables works", {
  co2_df_short <- co2_df_short %>%
    dplyr::rename(
      CO2_conc = conc,
      date_time = datetime
    )

  record_short <- record_short %>%
    dplyr::rename(
      starting = start
    )



  expect_snapshot(
    flux_match(
      co2_df_short,
      record_short,
      date_time,
      starting,
      measurement_length = 220
    )
  )
})



test_that("flags on nb of data", {
  expect_snapshot(
    suppressWarnings( # warnings are expected, they are tested in another test
      flux_match(
        co2_df_missing,
        record_short,
        datetime,
        start,
        measurement_length = 220
      )
    )
  )
})




# test that the data type checking works (all the error messages)

test_that("error on datetime", {
  co2_df_short <- co2_df_short %>%
    dplyr::mutate(
      datetime = lubridate::date(datetime)
    )

  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      measurement_length = 220
    ),
    "Please correct the arguments"
  )
})


test_that("error on start", {
  record_short <- record_short %>%
    dplyr::mutate(
      start = lubridate::hour(start)
    )

  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      measurement_length = 220
    ),
    "Please correct the arguments"
  )
})

test_that("error on measurement_length", {
  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      measurement_length = "blip"
    ),
    "Please correct the arguments"
  )
})


test_that("error on time_diff", {
  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      measurement_length = 220,
      time_diff = "comment est votre blanquette?"
    ),
    "Please correct the arguments"
  )
})

test_that("matching works with end col", {

  record_short_end <- record_short |>
    dplyr::mutate(
      end = dplyr::case_when(
        type == "ER" ~ start + 120,
        type == "NEE" ~ start + 180
      )
    )

  expect_snapshot(flux_match(
    co2_df_short,
    record_short_end,
    datetime,
    start,
    end
  ) |>
    dplyr::select(f_fluxid, f_start, f_end) |>
    dplyr::distinct()
  )
})

test_that("startcrop deprecated", {
  expect_error(flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    measurement_length = 180,
    startcrop = 10
  ),
  "The `startcrop` argument of `flux_match()` was deprecated in fluxible 1.2.1 and is now defunct.",
  fixed = TRUE
  )
})

test_that("ratio_threshold deprecated", {
  expect_warning(flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    measurement_length = 180,
    ratio_threshold = 0.8
  ),
  "The `ratio_threshold` argument of `flux_match()` is deprecated as of fluxible 1.2.2.",
  fixed = TRUE
  )
})

test_that("f_conc deprecated", {
  expect_warning(flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    measurement_length = 180,
    f_conc = "conc"
  ),
  "The `f_conc` argument of `flux_match()` is deprecated as of fluxible 1.2.2.",
  fixed = TRUE
  )
})

test_that("error on end col", {
  expect_error(flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    end_col = turfID
  ),
  "Please correct the arguments",
  )
})

test_that("fixe length deprecated", {
  record_short_end <- record_short |>
    dplyr::mutate(
      end = dplyr::case_when(
        type == "ER" ~ start + 120,
        type == "NEE" ~ start + 180
      )
    )

  expect_warning(flux_match(
    co2_df_short,
    record_short_end,
    datetime,
    start,
    end,
    fixed_length = FALSE
  ),
  "The `fixed_length` argument of `flux_match()` is deprecated as of fluxible 1.2.7.",
  fixed = TRUE
  )
})
