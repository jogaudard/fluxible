#' assessing quality of slopes calculated with the exponential or the linear model
#' @description indicates if fluxes should be discarded or replaced by 0 according to parameters set by user
#' @param slopes_df dataset containing slopes
#' @param fit_type expression used to fit the data, linear or exponential
#' @param ambient_conc ambient gas concentration in ppm at the site of measurement (used to detect measurement that started with a polluted setup)
#' @param error error of the setup, defines a window in which the starting values are considered acceptable
#' @param fluxID_col column containing unique IDs for each flux
#' @param slope_col column containing the slope of each flux (ideally as calculated by the flux_fitting function)
#' @param weird_fluxesID vector of fluxIDs that should be discarded by the user's decision
#' @param pvalue_col column containing the p-value of each flux (linear fit)
#' @param rsquared_col column containing the r squared to be used for the quality assessment (linear fit)
#' @param pvalue_threshold threshold of p-value below which the change of gas concentration over time is considered not significant (linear fit)
#' @param rsquared_threshold threshold of r squared value below which the linear model is considered an unsatisfactory fit (linear fit)
#' @param conc_col column containing the measured gas concentration (exponential fit)
#' @param b_col column containing the b parameter of the exponential expression (exponential fit)
#' @param time_col column containing the time of each measurement in seconds (exponential fit)
#' @param fit_col column containing the modeled data (exponential fit)
#' @param RMSE_threshold threshold for the RMSE of each flux above which the fit is considered unsatisfactory (exponential fit)
#' @param cor_threshold threshold for the correlation coefficient of gas concentration with time below which the correlation is considered non significant (exponential fit)
#' @param cut_col column containing the cutting information
#' @param b_threshold threshold for the b parameter. Defines a window with its opposite inside which the fit is considered good enough (exponential fit)
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename group_by rowwise summarise ungroup
#' @importFrom tidyr nest unnest
#' @importFrom stats cor
#' @examples 
#' data(slopes0lin)
#' flux_quality(slopes0lin, fit_type = "li", fluxID_col = "fluxID",
#' slope_col = "slope", conc_col = "conc")
#' @export 
#' 
#' 

flux_quality <- function(slopes_df,
                        fit_type, #need to use attribute here
                        ambient_conc = 421,
                        error = 100,
                        fluxID_col = "f_fluxID",
                        slope_col = "f_slope",
                        weird_fluxesID = c(),
                        pvalue_col = "p.value",
                        rsquared_col = "r.squared",
                        pvalue_threshold = 0.3,
                        rsquared_threshold = 0.7,
                        conc_col = "f_conc",
                        b_col = "f_b",
                        time_col = "f_time",
                        fit_col = "f_fit",
                        cut_col = "f_cut",
                        RMSE_threshold = 25,
                        cor_threshold = 0.5,
                        b_threshold = 1

){
    fit_type <- match.arg(((fit_type)), c("exponential", "linear"))

    if(((fit_type)) == "exponential") {
        quality_flag <- flux_quality_exp(
            ((slopes_df)),
            ambient_conc = ((ambient_conc)),
            error = ((error)),
            fluxID_col = ((fluxID_col)),
            slope_col = ((slope_col)),
            weird_fluxesID = ((weird_fluxesID)),
            conc_col = ((conc_col)),
            b_col = ((b_col)),
            time_col = ((time_col)),
            fit_col = ((fit_col)),
            cut_col = ((cut_col)),
            RMSE_threshold = ((RMSE_threshold)),
            cor_threshold = ((cor_threshold)),
            b_threshold = ((b_threshold))
            )
        }


    if(((fit_type)) == "linear") {
        quality_flag <- flux_quality_lin(
            ((slopes_df)),
            ambient_conc = ((ambient_conc)),
            error = ((error)),
            fluxID_col = ((fluxID_col)),
            slope_col = ((slope_col)),
            weird_fluxesID = ((weird_fluxesID)),
            pvalue_col = ((pvalue_col)),
            rsquared_col = ((rsquared_col)),
            pvalue_threshold = ((pvalue_threshold)),
            rsquared_threshold = ((rsquared_threshold)),
            conc_col = ((conc_col))
            )
        }


    quality_flag
}