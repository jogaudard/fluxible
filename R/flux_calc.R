#' calculates ecosystem gas fluxes
#' @description calculates a flux based on the rate of change
#' of gas concentration over time
#' @param slopes_df dataframe of flux slopes
#' @param slope_col column containing the slope to calculate the flux
#' (in ppm*s^(-1))
#' @param datetime_col column containing the datetime of each gas concentration
#' measurements in slopes_df. The first one after cutting will be kept as
#' datetime of each flux in the output.
#' @param conc_unit unit in which the concentration of gas was measured
#' ppm or ppb
#' @param flux_unit unit in which the calculated flux will be
#' `mmol` outputs fluxes in mmol*m^(-2)*h^(-1);
#' `micromol` outputs fluxes in micromol*m^(-2)*h^(-1)
#' @param cut_col column containing cutting information
#' @param keep_arg name in cut_col of data to keep
#' @param chamber_volume volume of the flux chamber in L,
#' default for Three-D project chamber (25x24.5x40cm),
#' can also be a column in case it is a variable
#' @param tube_volume volume of the tubing in L, default for summer 2020 setup,
#' can also be a column in case it is a variable
#' @param atm_pressure atmospheric pressure, assumed 1 atm,
#' can be a constant (numerical) or a variable (column name)
#' @param plot_area area of the plot in m^2, default for Three-D,
#' can also be a column in case it is a variable
#' @param cols_keep columns to keep from the input to the output.
#' Those columns need to have unique values for each flux,
#' as distinct() is applied.
#' @param cols_ave columns with values that should be averaged
#' for each flux in the output. Note that NA are removed in mean calculation.
#' @param fluxid_col column containing the fluxID
#' @param temp_air_col column containing the air temperature used
#' to calculate fluxes. Will be averaged with NA removed.
#' @param temp_air_unit units in which air temperature was measured.
#' Has to be either Celsius, Fahrenheit or Kelvin.
#' @param fit_type (optional) model used in flux_fitting, exponential,
#' quadratic or linear.
#' Will be automatically filled if slopes_df was produced using flux_quality().
#' @return a dataframe containing fluxID, fluxes (in mmol*m^(-2)*h^(-1)
#' or micromol*m^(-2)*h^(-1), depending on the value of `flux_unit`),
#' temperature average for each flux,
#' slope used for each flux calculation,
#' the model used in `flux_fitting`,
#' and any columns specified in cols_keep and cols_ave.
#' @importFrom rlang .data :=
#' @importFrom dplyr select group_by summarise
#' ungroup mutate case_when distinct left_join across everything
# #' @importFrom tidyselect any_of all_of
#' @examples
#' data(slopes0)
#' flux_calc(slopes0,
#' f_slope,
#' datetime,
#' temp_air,
#' conc_unit = "ppm",
#' flux_unit = "mmol",
#' chamber_volume = 24.5,
#' tube_volume = 0.075,
#' atm_pressure = 1,
#' plot_area = 0.0625)
#' @export



flux_calc <- function(slopes_df,
                      slope_col,
                      datetime_col,
                      temp_air_col,
                      chamber_volume,
                      atm_pressure,
                      plot_area,
                      fluxid_col = f_fluxID,
                      conc_unit,
                      flux_unit,
                      cols_keep = c(),
                      cols_ave = c(),
                      tube_volume,
                      temp_air_unit = "celsius",
                      cut_col = f_cut,
                      keep_arg = "keep",
                      cut = TRUE,
                      fit_type = c()) {

  name_df <- deparse(substitute(slopes_df))

  colnames <- colnames(slopes_df)
  if (length(setdiff(cols_keep, colnames)) > 0) {
    stop("some names in cols_keep cannot be found in slopes_df")
  }
  if (length(setdiff(cols_ave, colnames)) > 0) {
    stop("some names in cols_ave cannot be found in slopes_df")
  }


  slopes_df_check <- slopes_df |>
    select(
      {{slope_col}},
      {{temp_air_col}},
      {{datetime_col}}
    )

  df_ok <- flux_fun_check(slopes_df_check,
                          fn = list(
                            is.numeric,
                            is.numeric,
                            is.POSIXct
                          ),
                          msg = rep(c(
                            "has to be numeric",
                            "has to be POSIXct"
                          ),
                          c(2, 1)
                          ),
                          name_df = name_df)


  if (any(!df_ok))
    stop("Please correct the arguments", call. = FALSE)


  fit_type <- flux_fit_type(
    slopes_df,
    fit_type = fit_type
  )

  temp_air_unit <- match.arg(
    temp_air_unit,
    c("celsius", "fahrenheit", "kelvin")
  )

  conc_unit <- match.arg(
    conc_unit,
    c("ppm", "ppb")
  )

  flux_unit <- match.arg(
    flux_unit,
    c("micromol", "mmol")
  )


  if (cut == TRUE) {
    message("Cutting data according to 'keep_arg'...")
    slopes_df <- flux_cut(
      slopes_df,
      {{cut_col}},
      keep_arg
    )
  }


  name_vol <- deparse(substitute(chamber_volume))
  name_atm <- deparse(substitute(atm_pressure))
  name_plot <- deparse(substitute(plot_area))

  message("Averaging air temperature for each flux...")
  slope_temp <- slopes_df |>
    select(
      {{fluxid_col}},
      {{temp_air_col}},
      {{datetime_col}},
      {{slope_col}},
      any_of(c(name_vol, name_atm, name_plot))
    ) |>
    summarise(
      temp_air_ave = mean({{temp_air_col}}, na.rm = TRUE),
      start_datetime = min({{datetime_col}}),
      .by = c(
        {{fluxid_col}}, {{slope_col}}, any_of(c(name_vol, name_atm, name_plot))
      )
    ) |>
    mutate(
      temp_air_ave = case_when(
        temp_air_unit == "celsius" ~ .data$temp_air_ave + 273.15,
        temp_air_unit == "fahrenheit"
        ~ (.data$temp_air_ave + 459.67) * (5 / 9),
        temp_air_unit == "kelvin" ~ .data$temp_air_ave
      )
    )

  # a df with all the columns we just want to keep and join back in the end
  if (length(cols_keep) > 0) {
    message("Creating a df with the columns from 'cols_keep' argument...")
    slope_keep <- slopes_df |>
      select(all_of(cols_keep), {{fluxid_col}}) |>
      distinct() |>
      left_join(slope_temp, by = dplyr::join_by(
        {{fluxid_col}} == {{fluxid_col}}
      ))
  } else {
    slope_keep <- slope_temp
  }

  # a df with the columns that have to be averaged
  if (length(cols_ave) > 0) {
    message("Creating a df with the columns from 'cols_ave' argument...")
    slope_ave <- slopes_df |>
      select(all_of(cols_ave), {{fluxid_col}}) |>
      summarise(across(
        everything(),
        ~ mean(.x, na.rm = TRUE)
      ),
      .by = {{fluxid_col}}
      ) |>
      left_join(slope_keep, by = dplyr::join_by(
        {{fluxid_col}} == {{fluxid_col}}
      ))
  } else {
    slope_ave <- slope_keep
  }

  message("Calculating fluxes...")

  r_const <- 0.082057
  message("R constant set to 0.082057")


  # putting slope in ppm/s
  if (conc_unit == "ppm") {
    message("Concentration was measured in ppm")
  }
  if (conc_unit == "ppb") {
    message("Concentration was measured in ppb")
    slope_ave <- slope_ave |>
      mutate(
        {{slope_col}} := {{slope_col}} * 0.001 # now the slope is in ppm/s
      )
  }


  fluxes <- slope_ave |>
    mutate(
      volume_setup = {{chamber_volume}} + tube_volume,
      flux =
        ({{slope_col}} * {{atm_pressure}} * .data$volume_setup)
        / (r_const *
           .data$temp_air_ave
           * {{plot_area}}) # flux in micromol/s/m^2
        * 3600, # secs to hours, flux is now in micromol/m^2/h
      temp_air_ave = case_when(
        temp_air_unit == "celsius" ~ .data$temp_air_ave - 273.15,
        temp_air_unit == "fahrenheit"
        ~ (.data$temp_air_ave - 273.15) * (9 / 5) + 32,
        temp_air_unit == "kelvin" ~ .data$temp_air_ave
      ),
      model = fit_type,
      .by = {{fluxid_col}}
    )

  # output unit
  if (flux_unit == "micromol") {
    message("Fluxes are in micromol/m2/h")
  }
  if (flux_unit == "mmol") {
    fluxes <- fluxes |>
      mutate(
        flux = .data$flux / 1000
      )
    message("Fluxes are in mmol/m2/h")
  }

  fluxes

}
