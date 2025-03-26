#'
#' 

stupeflux <- function(raw_conc,
                       field_record,
                       datetime_col,
                       start_col,
                       conc_col,
                       startcrop,
                       measurement_length,
                       fit_type,
                       temp_air_col,
                      chamber_volume,
                      atm_pressure,
                      plot_area,
                      conc_unit,
                      flux_unit,
                      cols_keep = c(),
                      cols_ave = c(),
                      tube_volume,
                       ratio_threshold = 0.5,
                       time_diff = 0,
                       start_cut = 0,
                         end_cut = 0,
                         cz_window = 15,
                         b_window = 10,
                         a_window = 10,
                         roll_width = 15,
                         t_zero = 0,
                         force_discard = c(),
                         force_ok = c(),
                         force_zero = c(),
                         ambient_conc = 421,
                         error = 100,
                         pvalue_threshold = 0.3,
                         rsquared_threshold = 0.7,
                         rmse_threshold = 25,
                         cor_threshold = 0.5,
                         b_threshold = 1,
                      temp_air_unit = "celsius",
                      cut = TRUE){
 conc_df <- flux_match(
    raw_conc,
    field_record,
    {{datetime_col}},
    {{start_col}},
    {{conc_col}},
    startcrop = startcrop,
    measurement_length = measurement_length,
    ratio_threshold = ratio_threshold,
    time_diff = time_diff
 )

conc_fitting <- flux_fitting(
    conc_df,
    {{conc_col}},
    {{datetime_col}},
    start_cut = start_cut,
    end_cut = end_cut,
    cz_window = cz_window,
    b_window = b_window,
    a_window = a_window,
    roll_width = roll_width,
    t_zero = t_zero,
    fit_type = fit_type
)

quality_flag <- flux_quality(
    conc_fitting,
    {{conc_col}},
    force_discard = force_discard,
    force_ok = force_ok,
    force_zero = force_zero,
    ratio_threshold = ratio_threshold,
    ambient_conc = ambient_conc,
    error = error,
    pvalue_threshold = pvalue_threshold,
    rsquared_threshold = rsquared_threshold,
    rmse_threshold = rmse_threshold,
    cor_threshold = cor_threshold,
    b_threshold = b_threshold
)

fluxes <- flux_calc(
    quality_flag,
    slope_col = f_slope_corr,
    {{datetime_col}},
    {{temp_air_col}},
    chamber_volume = chamber_volume,
    atm_pressure = atm_pressure,
    plot_area = plot_area,
    conc_unit = conc_unit,
    flux_unit = flux_unit,
    cols_keep = cols_keep,
    cols_ave = cols_ave,
    tube_volume = tube_volume,
    temp_air_unit = temp_air_unit,
    cut = cut
)

fluxes
}