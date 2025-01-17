flux_tent_output <- readr::read_csv("data-raw/pftc7_site5_photo_flux_estimates_og_function.csv")
flux_tent_output <- tibble::as_tibble(flux_tent_output)
str(flux_tent_output)

usethis::use_data(flux_tent_output, overwrite = TRUE)

pftc7_short <- readr::read_csv("data-raw/pftc7_site5_photo.csv")
pftc7_short <- tibble::as_tibble(pftc7_short) |>
dplyr::group_by(file_name) |>
  dplyr::mutate(
    f_end = max(date_time)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    co2_conc,
    h2o_conc,
    temperature_c,
    pressure_kpa,
    signal_strength,
    date_time,
    start_time,
    file_name,
    plot_id,
    par,
    f_end
  )
str(pftc7_short)
usethis::use_data(pftc7_short, overwrite = TRUE)

pftc7_long <- readr::read_csv("data-raw/pftc7_site5.csv")
pftc7_long <- tibble::as_tibble(pftc7_long)
pftc7_long <- pftc7_long |>
  dplyr::group_by(file_name) |>
  dplyr::mutate(
    f_end = max(date_time)
  ) |>
  dplyr::ungroup()
str(pftc7_long)
usethis::use_data(pftc7_long, overwrite = TRUE)

pftc7_segmented_short <- readr::read_csv("data-raw/pftc7_site5_photo_flux_segment_output.csv")
pftc7_segmented_short <- tibble::as_tibble(pftc7_segmented_short)
str(pftc7_segmented_short)
usethis::use_data(pftc7_segmented_short, overwrite = TRUE)

slopes_pftc7 <- flux_fitting(
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
      signal_strength_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    )
usethis::use_data(slopes_pftc7, overwrite = TRUE)

