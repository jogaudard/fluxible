#' Matching continously measured fluxes with measurement IDs

# to slice and match the CO2 concentration data with the correct flux ID from schedule report on the field.

match_flux <- function(raw_flux,
                        field_record,
                        startcrop = 10,
                        measurement_length = 210,
                        ratio_threshold = 0.5
){
raw_flux <- co2_df_short
field_record <- record_short
  
 # need to include a test for the format of the column, especially the date
  
  field_record <- field_record %>%
    dplyr::mutate(
      start = start + startcrop, #cropping the start
      end = start + measurement_length, #creating column End
      fluxID = dplyr::row_number() #adding an individual ID to each flux, useful to join data or graph the fluxes
    )
  
  
  co2conc <- dplyr::full_join(raw_flux, field_record, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    tidyr::fill(fluxID)  %>% # filling fluxID in the raw_flux data set as well
       drop_na(fluxID) # dropping everything that happens before the first flux

  co2conc <- co2conc %>%
      dplyr::group_by(fluxID) %>% # filling the rest, except if there are NA for some fluxes
    tidyr::fill(names(co2conc)) %>%
    dplyr::filter(
      datetime <= end
      & datetime > start #cropping the part of the flux that is after the End and before the Start
      )  %>%
    dplyr::mutate(
      nrow = n(),
      ratio = nrow/measurement_length,
      flags = dplyr::case_when(
        ratio <= ratio_threshold ~ "nb of data too low"
      )
    ) %>%
       dplyr::ungroup()
  
  
  return(co2conc)
}
