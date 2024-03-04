#' quality assessment for the fluxes calculated with the linear model
#' @description indicates if fluxes should be discarded or replaced by 0 according to parameters set by user
#' @param slopes_df dataset containing slopes, fluxID, p.value and r.squared
#' @param pvalue_threshold threshold of p-value below which the change of gas concentration over time is considered not significant (user decided)
#' @param rsquared_threshold threshold of r squared value below which the linear model is considered an unsatisfactory fit
#' @param fluxID_col column containing unique IDs for each flux
#' @param slope_col column containing the slope of each flux (ideally as calculated by the flux_fitting function)
#' @param pvalue_col column containing the p-value of each flux
#' @param rsquared_col column containing the r squared to be used for the quality assessment
#' @param ambient_CO2 ambient CO2 concentration in ppm at the site of measurement (used to detect measurement that started with a polluted setup)
#' @param error error of the setup, defines a window in which the starting values are considered acceptable
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename
#' @examples 
#' data(slopes0lin)
#' flux_quality_lin(slopes0lin, fluxID_col = "fluxID", slope_col = "slope")
#' @export 


flux_quality_lin <- function(slopes_df,
                            pvalue_threshold = 0.3,
                            rsquared_threshold = 0.7,
                            fluxID_col = "f_fluxID",
                            slope_col = "f_slope",
                            pvalue_col = "p.value",
                            rsquared_col = "r.squared",
                            ambient_CO2 = 421,
                            error = 100 # error of the setup in ppm. fluxes starting outside of the window ambient_CO2 +/- error will be discarded
){

    slopes_df <- slopes_df |>
        rename(
            f_fluxID = all_of((fluxID_col)),
            f_slope = all_of((slope_col)),
            f_pvalue = all_of((pvalue_col)),
            f_rsquared = all_of((rsquared_col))
        )

    slopes_df |>
        mutate(
            f_quality_flag = case_when(
                .data$f_rsquared >= ((rsquared_threshold)) ~ "ok",
                .data$f_rsquared < ((rsquared_threshold)) & .data$f_pvalue >= ((pvalue_threshold)) ~ "discard",
                .data$f_rsquared < ((rsquared_threshold)) & .data$f_pvalue < ((pvalue_threshold)) ~ "zero"
            ),
            f_slope_corr = case_when(
                    .data$f_quality_flag == "ok" ~ .data$f_slope,
                    .data$f_quality_flag == "discard" ~ NA_real_,
                    .data$f_quality_flag == "zero" ~ 0
            )
        )

    slopes_df
}