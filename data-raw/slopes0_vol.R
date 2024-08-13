# making a dataset with volume as a variable
slopes0_vol <- slopes0 |>
  mutate(
    volume = case_when(
      f_fluxID == 1 ~ 18,
      f_fluxID == 2 ~ 28,
      f_fluxID == 3 ~ 20,
      f_fluxID == 4 ~ 24,
      f_fluxID == 5 ~ 4,
      f_fluxID == 6 ~ 35
    )
  )

# write_csv(slopes0_vol, "data-raw/slopes0_vol.csv")

usethis::use_data(slopes0_vol, overwrite = TRUE)
