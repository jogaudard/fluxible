#' Matching continously measured fluxes with measurement IDs

# to slice and match the CO2 concentration data with the correct flux ID from schedule report on the field.

match_flux <- function(raw_flux,
                        field_record,
                        startcrop = 10,
                        measurement_length = 210
){
 # need to include a test for the format of the column, especially the date
  
  field_record <- field_record %>%
    mutate(
      start = start + startcrop, #cropping the start
      end = start + measurement_length, #creating column End
      fluxID = row_number() #adding an individual ID to each flux, useful to join data or graph the fluxes
    )
  
  
  co2conc <- full_join(raw_flux, field_record, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(fluxID) # filling fluxID in the raw_flux data set as well

  co2conc <- co2conc %>%
      group_by(fluxID) %>% # filling the rest, except if there are NA for some fluxes
    fill(names(co2conc)) %>%
       ungroup() %>%
    filter(
      datetime <= end
      & datetime >= start #cropping the part of the flux that is after the End and before the Start
      ) 
  
  
  return(co2conc)
}
