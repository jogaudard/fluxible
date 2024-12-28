flux_tent_output <- readr::read_csv("data-raw/pftc7_site5_photo_flux_estimates_og_function.csv")
# str(flux_tent_output)

usethis::use_data(flux_tent_output, overwrite = TRUE)

pftc7_short <- readr::read_csv("data-raw/pftc7_site5_photo.csv")
str(pftc7_short)
usethis::use_data(pftc7_short, overwrite = TRUE)

pftc7_long <- readr::read_csv("data-raw/pftc7_site5.csv")
str(pftc7_long)
usethis::use_data(pftc7_long, overwrite = TRUE)

pftc7_segmented_short <- readr::read_csv("data-raw/pftc7_site5_photo_flux_segment_output.csv")
str(pftc7_segmented_short)
usethis::use_data(pftc7_segmented_short, overwrite = TRUE)
