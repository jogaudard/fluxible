slopes30qua <- flux_fitting(co2_conc, fit_type = "quadratic", t_zero = 10, end_cut = 30) |>
   flux_quality(fit_type = "quadratic")

   usethis::use_data(slopes30qua, overwrite = TRUE)
