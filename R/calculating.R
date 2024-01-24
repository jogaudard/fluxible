#' to calculate a flux based on the rate of change of gas concentration over time
#' 
#


flux_calc <- function(slope_df, # dataset of slopes (output of fitting functions)
                       chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                       tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                       atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                       plot_area = 0.0625, # area of the plot in m^2, default for Three-D
                       R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
                       cols_keep = c("turfID", "type", "start"), # columns to keep in the output (must be constant values for each fluxID, will go through distinct)
                       cols_ave = c("PAR") # columns that should be average in the output
)
{
  vol = chamber_volume + tube_volume
  
# a df with all the columns we just want to keep and join back in the end
keeping_cols <- slope_df  %>%
   dplyr::select(all_of(cols_keep), fluxID) %>%
                dplyr::distinct()

# a df with the columns that have to be averaged
ave_cols <- slope_df %>%
   dplyr::select(all_of(cols_ave), fluxID, temp_air) %>%
      dplyr::group_by(fluxID) %>%
         dplyr::summarize_all(mean, na.rm = TRUE)

slopes <- slope_df %>%
   dplyr::select(slope, fluxID)

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
  
  fluxes_final <- dplyr::left_join(slope_df, ave_df, by = "fluxID") %>% 
                    dplyr::left_join(keeping_cols, by = "fluxID") %>%
                    dplyr::mutate(
                        flux = (slope * atm_pressure * vol)/(R * temp_air * plot_area) #gives flux in micromol/s/m^2
                        *3600 #secs to hours
                        /1000 #micromol to mmol
                    ) %>% #flux is now in mmol/m^2/h, which is more common
  
  return(fluxes_final)
  
}