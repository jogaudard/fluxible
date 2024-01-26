#' to calculate a flux based on the rate of change of gas concentration over time
#' 

# to do list
# documentation
# dummy check, arguments in right format
# test temperature conversions


flux_calc <- function(slope_df, # dataset of slopes (output of fitting functions)
                       chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                       tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                       atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                       plot_area = 0.0625, # area of the plot in m^2, default for Three-D
                       R_const = 0.082057, #gas constant, in L*atm*K^(-1)*mol^(-1)
                       #  cols_keep = c("turfID", "type", "start"), # columns to keep in the output (must be constant values for each fluxID, will go through distinct)
                       cols_keep = c(),
                       #  cols_ave = c("PAR"), # columns that should be average in the output
                       cols_ave = c(),
                       slope_col, # column containing the slope to calculate the flux
                       fluxID_col = "fluxID",
                       temp_air_col = "temp_air",
                       temp_air_unit = "celsius"

)
{

  if(!is.double(chamber_volume)) stop("chamber_volume has to be a double")
  if(!is.double(tube_volume)) stop("tube_volume has to be a double")
  if(!is.double(atm_pressure)) stop("atm_pressure has to be a double")
  if(!is.double(plot_area)) stop("plot_area has to be a double")
  if(!is.double(R_const)) stop("R_const has to be a double")
  if(is.na(slope_col)) stop("slope_col argument is missing")
  if(!(temp_air_unit %in% list("celsius", "fahrenheit", "kelvin"))) stop("temp_air_unit has to be either celsius, fahrenheit or kelvin")

  

  slope_df <- slope_df %>%
     dplyr::rename(
      fluxID = dplyr::all_of(fluxID_col),
      air_temp = dplyr::all_of(temp_air_col),
      slope = dplyr::all_of(slope_col)
     )


  vol = chamber_volume + tube_volume

slope_temp <- slope_df %>%
   dplyr::select(slope, fluxID, air_temp) %>%
      dplyr::group_by(fluxID, slope) %>%
         dplyr::summarise(
          temp_air_ave = mean(air_temp, na.rm = TRUE)
         ) %>%
            dplyr::ungroup() %>%
               dplyr::mutate(
                temp_air_ave = dplyr::case_when(
                  temp_air_unit == "celsius" ~ temp_air_ave + 273.15,
                  temp_air_unit == "fahrenheit" ~ (temp_air_ave + 459.67) * (5/9),
                  temp_air_unit == "kelvin" ~ temp_air_ave
                )
               )
  
# a df with all the columns we just want to keep and join back in the end
if(length(cols_keep) > 0) {
slope_keep <- slope_df  %>%
   dplyr::select(dplyr::all_of(cols_keep), fluxID) %>%
                dplyr::distinct() %>%
                   dplyr::left_join(slope_temp, by = "fluxID")
} else {
   slope_keep <- slope_temp
}

# a df with the columns that have to be averaged
if(length(cols_ave) > 0) {
slope_ave <- slope_df %>%
   dplyr::select(dplyr::all_of(cols_ave), fluxID) %>%
      dplyr::group_by(fluxID) %>%
         dplyr::summarize_all(mean, na.rm = TRUE) %>%
            dplyr::ungroup() %>%
               dplyr::left_join(slope_keep, by = "fluxID")
} else {
   slope_ave <- slope_keep
}

# # a df with average of air temperature
# ait_temp_df <- slope_df %>%
#    dplyr::select(fluxID, temp_air) %>%
#       dplyr::group_by(fluxID) %>%
#          dplyr::summarize_all(mean, na.rm = TRUE) %>%
#             dplyr::ungroup()





  # creates means of all columns specified to be averaged
#   means <- co2conc %>% 
#     group_by(fluxID) %>% 
#     summarise(
#       PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
#       temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of temp_air for each flux
#       + 273.15, #transforming in kelvin for calculation
#       temp_soilavg = mean(temp_soil, na.rm = TRUE) #mean value of temp_soil for each flux
#     ) %>% 
#     ungroup()
  
  # fluxes_final <- dplyr::left_join(slope_df, ave_cols, by = "fluxID") %>% 
  #                   dplyr::left_join(keeping_cols, by = "fluxID") %>%
  #                   dplyr::left_join(air_temp_df, by = "fluxID") %>%
  fluxes <- slope_ave %>%
                    dplyr::mutate(
                        flux = (slope * atm_pressure * vol)/(R_const * temp_air_ave * plot_area) #gives flux in micromol/s/m^2
                        *3600 #secs to hours
                        /1000, #micromol to mmol #flux is now in mmol/m^2/h, which is more common
                        temp_air_ave = dplyr::case_when(
                          temp_air_unit == "celsius" ~ temp_air_ave - 273.15,
                          temp_air_unit == "fahrenheit" ~ ((temp_air_ave - 273.15) * (9/5)) + 32,
                          temp_air_unit == "kelvin" ~ temp_air_ave
                        )
                    ) 
  return(fluxes)
  
}