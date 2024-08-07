#' assessing quality of slopes calculated
#' @description indicates if fluxes should be discarded or replaced
#' by 0 according to parameters set by user
#' @param slopes_df dataset containing slopes
#' @param fit_type expression used to fit the data, linear or exponential
#' @param ambient_conc ambient gas concentration in ppm at the site of
#' measurement (used to detect measurement that started with a polluted setup)
#' @param error error of the setup, defines a window in which
#' the starting values are considered acceptable
#' @param fluxID_col column containing unique IDs for each flux
#' @param slope_col column containing the slope of each flux
#' (as calculated by the flux_fitting function)
#' @param weird_fluxesID vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_okID vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param ratio_threshold ratio of gas concentration data points over length of
#' measurment (in second) below which the measurement will be considered as 
#' not having enough data points to be considered for calculations
#' @param pvalue_col column containing the p-value of each flux (linear fit)
#' @param rsquared_col column containing the r squared to be used for
#' the quality assessment (linear fit)
#' @param pvalue_threshold threshold of p-value below which the change of
#' gas concentration over time is considered not significant (linear fit)
#' @param rsquared_threshold threshold of r squared value below which
#' the linear model is considered an unsatisfactory fit (linear fit)
#' @param conc_col column containing the measured gas concentration
#' (exponential fit)
#' @param b_col column containing the b parameter of the exponential expression
#' (exponential fit)
#' @param time_col column containing the time of each measurement in seconds
#' (exponential fit)
#' @param fit_col column containing the modeled data (exponential fit)
#' @param RMSE_threshold threshold for the RMSE of each flux above which
#' the fit is considered unsatisfactory (exponential fit)
#' @param cor_threshold threshold for the correlation coefficient of
#' gas concentration with time below which the correlation
#' is considered non significant (exponential fit)
#' @param cut_col column containing the cutting information
#' @param cut_arg argument defining that the data point should be cut out
#' @param b_threshold threshold for the b parameter.
#' Defines a window with its opposite inside which the fit is
#' considered good enough (exponential fit)
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename group_by rowwise summarise ungroup
#' @importFrom tidyr nest unnest
#' @importFrom stats cor
#' @examples
#' data(slopes0lin)
#' flux_quality(slopes0lin, fit_type = "li")
#' @export
#'
#'

flux_quality <- function(slopes_df,
                         fit_type, # need to use attribute here
                         ambient_conc = 421,
                         error = 100,
                         fluxID_col = "f_fluxID",
                         slope_col = "f_slope",
                         weird_fluxesID = c(),
                         force_okID = c(),
                         ratio_threshold = 0,
                         pvalue_col = "f_pvalue",
                         rsquared_col = "f_rsquared",
                         pvalue_threshold = 0.3,
                         rsquared_threshold = 0.7,
                         conc_col = "f_conc",
                         b_col = "f_b",
                         time_col = "f_time",
                         fit_col = "f_fit",
                         cut_col = "f_cut",
                         RMSE_threshold = 25,
                         cor_threshold = 0.5,
                         b_threshold = 1,
                         cut_arg = "cut"
                         ) {
  fit_type <- match.arg(((fit_type)), c("exponential", "linear", "quadratic"))

  slopes_df <- slopes_df |>
    rename(
      f_fluxID = all_of(((fluxID_col))),
      f_conc = all_of(((conc_col))),
      f_time = all_of(((time_col))),
      f_fit = all_of(((fit_col))),
      f_cut = all_of(((cut_col)))
    )

  slopes_df <- slopes_df |>
  group_by(.data$f_fluxID, .data$f_cut) |>
    mutate(
      f_n_conc = sum(!is.na(.data$f_conc)),
            f_ratio = .data$f_n_conc / as.double((difftime(.data$f_end, .data$f_start, units = "secs"))),
      f_flag_ratio = case_when(
        .data$f_ratio == 0 ~ "no_data",
        .data$f_ratio <= ((ratio_threshold)) ~ "too_low",
        TRUE ~ "ok"
      )
    ) |>
    ungroup()

  quality_par_start <- slopes_df |>
    # for the start error we take the entire flux into account
    group_by(.data$f_fluxID) |>
    nest() |>
    rowwise() |>
    summarise(
      f_start_error = case_when(
        data$f_conc[1] < (((ambient_conc)) - ((error))) ~ "error",
        data$f_conc[1] > (((ambient_conc)) + ((error))) ~ "error",
        TRUE ~ "ok"
      ),
      .groups = "drop"
    ) |>
    unnest("f_fluxID")
    # ungroup()

    slopes_df <- slopes_df |>
      left_join(quality_par_start, by = "f_fluxID")

  if (((fit_type)) == "exponential") {
    quality_flag <- flux_quality_exp(
      ((slopes_df)),
      slope_col = ((slope_col)),
      weird_fluxesID = ((weird_fluxesID)),
      force_okID = ((force_okID)),
      b_col = ((b_col)),
      RMSE_threshold = ((RMSE_threshold)),
      cor_threshold = ((cor_threshold)),
      b_threshold = ((b_threshold))
    )
  }


  if (((fit_type)) %in% c("linear", "quadratic")) {
    quality_flag <- flux_quality_lm(
      ((slopes_df)),
      slope_col = ((slope_col)),
      weird_fluxesID = ((weird_fluxesID)),
      force_okID = ((force_okID)),
      pvalue_col = ((pvalue_col)),
      rsquared_col = ((rsquared_col)),
      pvalue_threshold = ((pvalue_threshold)),
      rsquared_threshold = ((rsquared_threshold))
    )
  }

  # if (((fit_type)) == "quadratic") {
  #   quality_flag <- flux_quality_lm(
  #     ((slopes_df)),
  #     weird_fluxesID = ((weird_fluxesID)),
  #     pvalue_col = ((pvalue_col)),
  #     rsquared_col = ((rsquared_col)),
  #     pvalue_threshold = ((pvalue_threshold)),
  #     rsquared_threshold = ((rsquared_threshold))
  #   )
  # }

  flag_count <- flux_flag_count(
    ((quality_flag)),
    cut_arg = ((cut_arg))
  )

flag_msg <- flag_count |>
                mutate(
                  message = paste("\n", .data$f_quality_flag, "\t", .data$n, "\t", round(.data$ratio, 2) * 100, "%")
                ) |>
                pull(message)

message(paste("\n", "Total number of measurements:", sum(flag_count$n)))
message(flag_msg)


  quality_flag
}


