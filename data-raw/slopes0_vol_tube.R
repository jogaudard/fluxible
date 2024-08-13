# making a dataset with volume as a variable
slopes0_vol_tube <- slopes0_vol |>
  mutate(
    tube_vol = case_when(
      f_fluxID == 1 ~ 1,
      f_fluxID == 2 ~ 4,
      f_fluxID == 3 ~ 0.2,
      f_fluxID == 4 ~ 0.1,
      f_fluxID == 5 ~ 2,
      f_fluxID == 6 ~ 0.5
    )
  )

# write_csv(slopes0_vol, "data-raw/slopes0_vol.csv")

usethis::use_data(slopes0_vol_tube, overwrite = TRUE)
