# making a dataset with volume as a variable
slopes0_vol <- slopes0 |>
  mutate(
    volume = case_when(
      f_fluxid == 1 ~ 18,
      f_fluxid == 2 ~ 28,
      f_fluxid == 3 ~ 20,
      f_fluxid == 4 ~ 24,
      f_fluxid == 5 ~ 4,
      f_fluxid == 6 ~ 35
    )
  )


usethis::use_data(slopes0_vol, overwrite = TRUE)
