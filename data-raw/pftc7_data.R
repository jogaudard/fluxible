flux_tent_output <- readr::read_csv("data-raw/pftc7_site5_photo_flux_estimates_og_function.csv")
flux_tent_output <- tibble::as_tibble(flux_tent_output)
str(flux_tent_output)

usethis::use_data(flux_tent_output, overwrite = TRUE)

pftc7_short <- readr::read_csv("data-raw/pftc7_site5_photo.csv")
pftc7_short <- tibble::as_tibble(pftc7_short)
str(pftc7_short)
usethis::use_data(pftc7_short, overwrite = TRUE)

pftc7_long <- readr::read_csv("data-raw/pftc7_site5.csv")
pftc7_long <- tibble::as_tibble(pftc7_long)
pftc7_long <- pftc7_long |>
  dplyr::mutate(
    f_end = start_time + 119
  )
str(pftc7_long)
usethis::use_data(pftc7_long, overwrite = TRUE)

pftc7_segmented_short <- readr::read_csv("data-raw/pftc7_site5_photo_flux_segment_output.csv")
pftc7_segmented_short <- tibble::as_tibble(pftc7_segmented_short)
str(pftc7_segmented_short)
usethis::use_data(pftc7_segmented_short, overwrite = TRUE)
