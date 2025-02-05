# making a dataset with volume as a variable
slopes0_vol_tube <- slopes0_vol |>
  mutate(
    tube_vol = case_when(
      f_fluxid == 1 ~ 1,
      f_fluxid == 2 ~ 4,
      f_fluxid == 3 ~ 0.2,
      f_fluxid == 4 ~ 0.1,
      f_fluxid == 5 ~ 2,
      f_fluxid == 6 ~ 0.5
    )
  )


usethis::use_data(slopes0_vol_tube, overwrite = TRUE)
