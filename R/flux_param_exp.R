flux_param_exp <- function(slopes_df,
                          conc_col = "f_conc",
                          quality_flag_col = "f_quality_flag",
                          fluxID_col = "f_fluxID",
                          start_col = "f_start",
                          b_col = "f_b",
                          cor_coef_col = "f_cor_coef",
                          RMSE_col = "f_RMSE",
                          cut_col = "f_cut",
                          cut_arg = "cut"

){

    


    param_df <- slopes_df |>
    select(
      "f_conc", "f_start", "f_fluxID", "f_RMSE", "f_cor_coef", "f_b", "f_cut",
      "f_quality_flag"
    ) |>
    filter(.data$f_cut != ((cut_arg))) |>
    group_by(.data$f_fluxID) |>
    mutate(
      conc_start = .data$f_conc[1]
    ) |>
    ungroup() |>
    select(!"f_conc") |>
    distinct() |>
    mutate(
      f_RMSE = round(.data$f_RMSE, digits = 1),
      f_cor_coef = round(.data$f_cor_coef, digits = 2),
      f_b = round(.data$f_b, digits = 5),
      print_col = paste(
        .data$f_quality_flag, "\n",
        "RMSE = ", .data$f_RMSE, "\n", "Corr coef = ",
        .data$f_cor_coef, "\n", "b = ", .data$f_b,
        sep = ""
      )
    ) |>
    select("f_fluxID", "conc_start", "print_col", "f_quality_flag")

    param_df
}