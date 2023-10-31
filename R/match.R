#' Matching continously measured fluxes with measurement IDs
#' Matching continously measured fluxes with measurement IDs

# to slice and match the CO2 concentration data with the correct flux ID from schedule report on the field.

match_flux <- function(raw_flux,
                        field_record,
                        startcrop = 10,
                        measurement_length = 210,
                        ratio_threshold = 0.5
){
# raw_flux <- co2_df_missing
# field_record <- record_short
  
 # need to include a test for the format of the column, especially the date
  
  field_record <- field_record %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(
      start = start + startcrop, #cropping the start
      end = start + measurement_length, #creating column End
      fluxID = dplyr::row_number() #adding an individual ID to each flux, useful to join data or graph the fluxes
    )
  
  
  co2conc <- dplyr::full_join(raw_flux, field_record, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    dplyr::mutate(
      # datetime = tidyr::replace_na(datetime, start)
      # datetime_wna = datetime, # keep a datetime column with NA to know where data are missing
      datetime = case_when( # to add the fluxID in case the row with matching datetime and start is missing
        !is.na(datetime) ~ datetime,
        is.na(datetime) ~ start
      )
      ) %>%
      dplyr::arrange(datetime) %>%
         tidyr::fill(fluxID)  %>% # filling fluxID to group afterwards
       tidyr::drop_na(fluxID) # dropping everything that happens before the first flux

  co2conc <- co2conc %>%
      dplyr::group_by(fluxID) %>% # filling the rest, except if there are NA for some fluxes
    tidyr::fill(names(field_record)) %>%
    dplyr::filter(
      (datetime <= end
      & datetime >= start) #cropping the part of the flux that is after the End and before the start
      # | is.na(datetime_wna) # we keep datetime = na because we want to see where there is no data
      )  %>%
    dplyr::mutate(
      # nrow = n(),
      n_co2 = sum(!is.na(CO2)), #not sure why I cannot do that with count
      ratio = n_co2/(measurement_length + 1), # add 1 sec because filter is inclusing both limits
      flag = dplyr::case_when(
        ratio == 0 ~ "no data",
        ratio <= ratio_threshold ~ "nb of data too low"
        # is.na(datetime_wna) ~ "no data"
        
      ) # also need to print a warning in the console with fluxID
    ) %>%
       dplyr::ungroup()

       # making sure all columns are in the right format
       co2conc <- co2conc %>%
          dplyr::mutate(
            temp_air = as.double(temp_air),
            temp_soil = as.double(temp_soil),
            PAR = as.double(PAR),
            turfID = as.factor(turfID),
            type = as.factor(type),
            fluxID = as.factor(fluxID),
            flag = as.character(flag)
          ) %>%
             dplyr::arrange(fluxID)
  
  # print warnings when there are flags
  # if(any(!is.na(co2conc$flag))) warning("there is a flag somewhere")

  flags <- co2conc %>%
     select(fluxID, flag) %>%
     tidyr::drop_na(flag) %>%
        dplyr::distinct() %>%
           dplyr::mutate(
            warnings = paste("\n","fluxID", fluxID, ":", flag),
            warnings = as.character(warnings)
           ) %>%
              pull(warnings)
    
    # warnings <- pull(flags, warnings)
    # warnings <- paste(warnings, sep = ";")
    warnings <- str_c(flags)

  # warnings <- 

  # test <- paste("blop","blip", sep = "\n")
        if(any(!is.na(co2conc$flag))) warning(warnings)
        # if(any(!is.na(co2conc$flag))) warning(test)

  
  return(co2conc)
}
