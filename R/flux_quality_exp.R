#' assessing fluxes quality calculated with an exponential fit
#' 
#' 

flux_quality_exp <- function(slopes_df,
                            fluxID_col = "f_fluxID",
                            conc_col = "f_conc",
                            b_col = "f_b",
                            time_col = "f_time",
                            fit_col = "f_fit",
                            slope_col = "f_slope_tz",
                            weird_fluxesID = c(), # a vector of fluxes to discard because they are obviously wrong, this shoudl be moved to the quality check function
                            RMSE_threshold = 25, # threshold above which data are discarded
                            cor_threshold = 0.5, # delimits the window in which CO2 is considered not correlated with time
                            b_threshold = 1, # this value and its opposite define a window out of which data are being discarded
                            ambient_conc = 421, #by default for CO2, does it make sense for other fluxes??
                            error = 100 # error of the setup in ppm. fluxes starting outside of the window ambient_CO2 +/- error will be discarded
){

    fluxes_df <- fluxes_df |>
        rename(
            f_fluxID = all_of((fluxID_col)),
            f_conc = all_of((conc_col)),
            f_b = all_of((b_col)),
            f_time = all_of((time_col)),
            f_fit = all_of((fit_col)),
            f_slope_tz = all_of((slope_col))
        )

    
    quality_par <- fluxes_df |>
        group_by(.data$fluxID) |>
        nest() |>
        rowwise() |>
        summarise(
            f_cor_coef = cor(data$f_conc, data$f_time),
            f_RMSE = sqrt((1/length(data$f_time)) * sum((data$f_fit - data$f_conc)^2)),
            f_start_error = case_when(
                data$f_conc[1] < (((ambient_conc)) - error) ~ "error",
                data$f_conc[1] > (((ambient_conc)) + error) ~ "error",
        TRUE ~ "ok"
      )
    ) |>
    unnest() |>
    ungroup() |>
    mutate(
        f_fit_quality = case_when(
            .data$f_b >= ((b_threshold)) ~ "bad_b",
            .data$f_RMSE > ((RMSE_threshold)) ~ "bad_RMSE"
        ),
        f_correlation = case_when(
        abs(.data$f_cor_coef) < ((cor_threshold)) ~ "no",
        TRUE ~ "yes"
      ),
      f_flag_fit = case_when(
        .data$f_fluxID %in% ((weird_fluxesID)) ~ "weird_flux",
        .data$f_start_error == "error" ~ "start_error",
        .data$f_fit_quality == "bad_b" & .data$f_correlation == "yes" ~ "discard",
        .data$f_fit_quality == "bad_b" & .data$f_correlation == "no" ~ "zero",
        .data$f_fit_quality == "bad_RMSE" & .data$f_correlation == "yes" ~ "discard",
        .data$f_fit_quality == "bad_RMSE" & .data$f_correlation == "no" ~ "zero"
      ),
      f_slope_corr = case_when(
        .data$f_flag == "weird_flux" ~ NA_real_,
        .data$f_flag == "start_error" ~ NA_real_,
        .data$f_flag == "discard" ~ NA_real_,
        .data$f_flag == "zero" ~ 0,
        TRUE ~ .data$f_slope_tz,
      )
    )

    quality_par


}