#' to calculate a flux based on the rate of change of gas concentration over time
#' @description to calculate a flux based on the rate of change of gas concentration over time
#' @param slope_df dataframe of flux slopes
#' @param slope_col column containing the slope to calculate the flux
#' @param chamber_volume volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
#' @param tube_volume volume of the tubing in L, default for summer 2020 setup
#' @param atm_pressure atmoshperic pressure, assumed 1 atm
#' @param plot_area area of the plot in m^2, default for Three-D
#' @param R_const gas constant, in L*atm*K^(-1)*mol^(-1)
#' @param cols_keep columns to keep from the input to the input. Those columns need to have unique values for each flux
#' @param cols_ave columns with values that should be averaged for each flux in the ouput. Not that NA are removed in mean calculation
#' @param fluxID_col column containing the fluxID
#' @param temp_air_col column containing the air temperature used to caculate fluxes. Will be averaged with NA removed.
#' @param temp_air_unit units for air temperature. Has to be either celsius, fahrenheit or kelvin
#' @return a df containing fluxID, fluxes, temperature average for each flux, slope used for each flux calculation, and any columns specified in cols_keep and cols_ave
#' @importFrom rlang .data
#' @importFrom dplyr .data rename all_of select group_by summarise ungroup mutate case_when distinct left_join summarize_all
#' @examples
#' data(slopes0)
#' flux_calc(slopes0, slope_col = "slope_tz")
#' @export

# to do list


flux_calc <- function(slope_df, # dataset of slopes (output of fitting functions)
                      slope_col, # column containing the slope to calculate the flux
                      chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                      tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                      atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                      plot_area = 0.0625, # area of the plot in m^2, default for Three-D
                      R_const = 0.082057, # gas constant, in L*atm*K^(-1)*mol^(-1)
                      #  cols_keep = c("turfID", "type", "start"), # columns to keep in the output (must be constant values for each fluxID, will go through distinct)
                      cols_keep = c(),
                      #  cols_ave = c("PAR"), # columns that should be average in the output
                      cols_ave = c(),
                      fluxID_col = "fluxID",
                      temp_air_col = "temp_air",
                      temp_air_unit = "celsius") {
  if (!is.double(chamber_volume)) stop("chamber_volume has to be a double")
  if (!is.double(tube_volume)) stop("tube_volume has to be a double")
  if (!is.double(atm_pressure)) stop("atm_pressure has to be a double")
  if (!is.double(plot_area)) stop("plot_area has to be a double")
  if (!is.double(R_const)) stop("R_const has to be a double")
  #   if(is.na(slope_col)) stop("slope_col argument is missing") # apparently not necessary, R is checking this by itself
  if (!(temp_air_unit %in% list("celsius", "fahrenheit", "kelvin"))) stop("temp_air_unit has to be either celsius, fahrenheit or kelvin")

  colnames <- colnames(slope_df)
  if (!(slope_col %in% colnames)) stop("could not find slope_col in slope_df")
  if (!(fluxID_col %in% colnames)) stop("could not find fluxID_col in slope_df")
  if (!(temp_air_col %in% colnames)) stop("could not find temp_air_col in slope_df")


  if (length(setdiff(cols_keep, colnames)) > 0) stop("some names in cols_keep cannot be found in slope_df")
  if (length(setdiff(cols_ave, colnames)) > 0) stop("some names in cols_ave cannot be found in slope_df")



  slope_df <- slope_df |>
    rename(
      fluxID = all_of(fluxID_col),
      air_temp = all_of(temp_air_col),
      slope = all_of(slope_col)
    )


  vol <- chamber_volume + tube_volume

  slope_temp <- slope_df |>
    select("slope", "fluxID", "air_temp") |>
    group_by(.data$fluxID, .data$slope) |>
    summarise(
      temp_air_ave = mean(.data$air_temp, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      temp_air_ave = case_when(
        temp_air_unit == "celsius" ~ .data$temp_air_ave + 273.15,
        temp_air_unit == "fahrenheit" ~ (.data$temp_air_ave + 459.67) * (5 / 9),
        temp_air_unit == "kelvin" ~ .data$temp_air_ave
      )
    )



  # a df with all the columns we just want to keep and join back in the end
  if (length((cols_keep)) > 0) {
    slope_keep <- slope_df |>
      select(all_of(cols_keep), "fluxID") |>
      distinct() |>
      left_join(slope_temp, by = "fluxID")
  } else {
    slope_keep <- slope_temp
  }

  # a df with the columns that have to be averaged
  if (length((cols_ave)) > 0) {
    slope_ave <- slope_df |>
      select(all_of(cols_ave), "fluxID") |>
      group_by(.data$fluxID) |>
      summarize_all(mean, na.rm = TRUE) |>
      ungroup() |>
      left_join(slope_keep, by = "fluxID")
  } else {
    slope_ave <- slope_keep
  }

  # # a df with average of air temperature
  # ait_temp_df <- slope_df |>
  #    select(fluxID, temp_air) |>
  #       group_by(fluxID) |>
  #          summarize_all(mean, na.rm = TRUE) |>
  #             ungroup()





  # creates means of all columns specified to be averaged
  #   means <- co2conc |>
  #     group_by(fluxID) |>
  #     summarise(
  #       PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
  #       temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of temp_air for each flux
  #       + 273.15, #transforming in kelvin for calculation
  #       temp_soilavg = mean(temp_soil, na.rm = TRUE) #mean value of temp_soil for each flux
  #     ) |>
  #     ungroup()

  # fluxes_final <- left_join(slope_df, ave_cols, by = "fluxID") |>
  #                   left_join(keeping_cols, by = "fluxID") |>
  #                   left_join(air_temp_df, by = "fluxID") |>
  # need to add match.arg for temp_air_unit
  temp_air_unit <- match.arg(((temp_air_unit)), c("celsius", "fahrenheit", "kelvin"))

  fluxes <- slope_ave |>
    mutate(
      flux = (.data$slope * ((atm_pressure)) * ((vol))) / (((R_const)) * .data$temp_air_ave * ((plot_area))) # gives flux in micromol/s/m^2
        * 3600 # secs to hours
        / 1000, # micromol to mmol #flux is now in mmol/m^2/h, which is more common
      temp_air_ave = case_when(
        ((temp_air_unit)) == "celsius" ~ .data$temp_air_ave - 273.15,
        ((temp_air_unit)) == "fahrenheit" ~ ((.data$temp_air_ave - 273.15) * (9 / 5)) + 32,
        ((temp_air_unit)) == "kelvin" ~ .data$temp_air_ave
      )
    )
  return(fluxes)
}
