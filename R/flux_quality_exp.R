#' assessing fluxes quality calculated with an exponential fit
#' quality assessment for the fluxes calculated with the exponential model
#' @description indicates if fluxes should be discarded or replaced by 0 according to parameters set by user
#' @param slopes_df dataset containing slopes, fluxID, and parameters of the exponential expression
#' @param fluxID_col column containing unique IDs for each flux
#' @param conc_col column containing the measured gas concentration
#' @param b_col column containing the b parameter of the exponential expression
#' @param time_col column containing the time of each measurement in seconds
#' @param fit_col column containing the modeled data
#' @param slope_col column containing the slope of the exponential expression used for the calculation
#' @param weird_fluxesID vector of fluxIDs that should be discarded by the user's decision
#' @param RMSE_threshold threshold for the RMSE of each flux above which the fit is considered unsatisfactory
#' @param cor_threshold threshold for the correlation coefficient of gas concentration with time below which the correlation is considered non significant
#' @param b_threshold threshold for the b parameter. Defines a window with its opposite inside which the fit is considered good enough.
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename group_by rowwise summarise ungroup
#' @importFrom tidyr nest unnest
#' @importFrom stats cor
#' @examples 
#' data(slopes0)
#' flux_quality_exp(slopes0,
#' fluxID_col = "fluxID",
#' conc_col = "conc",
#' b_col = "b",
#' time_col = "time",
#' fit_col = "fit",
#' slope_col = "slope_tz"
#' )
#' @export 
#' 
#' 

# need to check time vs time_cut, we want the quality assessment only on the part that we keep

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

    slopes_df <- slopes_df |>
        rename(
            f_fluxID = all_of((fluxID_col)),
            f_conc = all_of((conc_col)),
            f_b = all_of((b_col)),
            f_time = all_of((time_col)),
            f_fit = all_of((fit_col)),
            f_slope_tz = all_of((slope_col))
        )

    
    quality_par <- slopes_df |>
        group_by(.data$f_fluxID) |>
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
        .data$f_flag_fit == "weird_flux" ~ NA_real_,
        .data$f_flag_fit == "start_error" ~ NA_real_,
        .data$f_flag_fit == "discard" ~ NA_real_,
        .data$f_flag_fit == "zero" ~ 0,
        TRUE ~ .data$f_slope_tz,
      )
    )

    quality_par


}