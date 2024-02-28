#' Matching continously measured fluxes with measurement IDs
#' @description Function to match a dataframe of continuously measured CO2 concentration with measurement IDs from another dataframe. Uses datetime to tell which measurements happened when. Extra variables in both dataframes will appear in the output. 
#' @param raw_conc dataframe of CO2 concentration measured continuously. Has to contain at least a datetime column in ymd_hms format and a CO2 concentration column as double.
#' @param field_record dataframe recording which measurement happened when. Has to contain at least a column telling at what time (in ymd_hms) the measurement started, and any other column allowing for identification of measurements.
#' @param startcrop how many seconds should be discarded at the beginning of the measurement
#' @param measurement_length length of the measurement (in seconds) from the start specified in the field_record
#' @param ratio_threshold ratio (number of concentration measurement compared to length of measurement in seconds) below which the data should be flagged as too little
#' @param time_diff time difference (in seconds) between the two datasets. Will be added to the datetime column of the raw_conc dataset
#' @param datetime_col to specify the name of the datetime column in raw_conc
#' @param conc_col to specify the name of the concentration column in raw_conc
#' @param start_col to specify the name of the start column in field_record
#' @return a dataframe with concentration measurements and corresponding datetime, fluxID, start and end of measurement, flags in case of no data or low number of data, and any variables present in both inputs
#' @importFrom dplyr rename arrange mutate row_number full_join case_when group_by filter ungroup select distinct pull
#' @importFrom tidyr fill drop_na
#' @importFrom lubridate is.POSIXct 
# #' @importFrom purrr ::
#' @examples 
#' data(co2_df_short, record_short)
#' flux_match(co2_df_short, record_short)
#' @export


# should I describe all the variables in the output?

flux_match <- function(raw_conc,
                        field_record,
                        startcrop = 10,
                        measurement_length = 220,
                        ratio_threshold = 0.5,
                        time_diff = 0,
                        datetime_col = "datetime",
                        conc_col = "conc",
                        start_col = "start"
){

# raw_flux <- co2_df_short

# raw_flux <- co2_df_short |>
#      rename(
#       # CO2_conc = CO2,
#       date_time = datetime
#      )
# field_record <- record_short

# need to make it that one can have different column names
  
raw_conc <- raw_conc |>
   rename(
    datetime = all_of(datetime_col),
    conc = all_of(conc_col)
   )

field_record <- field_record |>
   rename(
    start = all_of(start_col)
   )

 # need to include a test for the format of the column, especially the date
 if(!is.POSIXct(raw_conc$datetime)) stop("datetime in raw_conc dataframe is not ymd_hms!")
#  if(!is.double(raw_flux$temp_air)) stop("temp_air is not a double")
#  if(!is.double(raw_flux$temp_soil)) stop("temp_soil is not a double")
#  if(!is.double(raw_flux$PAR)) stop("PAR is not a double")
 if(!is.double(raw_conc$conc)) stop("conc is not a double")

 if(!is.POSIXct(field_record$start)) stop("start in field_record dataframe is not ymd_hms!")
 
 if(!is.double(startcrop)) stop("startcrop has to be a double")
 if(!is.double(time_diff)) stop("time_diff has to be a double")
 if(!is.double(measurement_length)) stop("measurement_length has to be a double")
 if(!is.double(ratio_threshold)) stop("ratio_threshold has to be a number between 0 and 1")
 if(
  ratio_threshold < 0 
  | ratio_threshold > 1)
  stop("ratio_threshold has to be a number between 0 and 1")


  field_record <- field_record |>
    arrange(start) |>
    mutate(
      end = start + measurement_length, #creating column End
      start = start + startcrop, #cropping the start
      fluxID = row_number() #adding an individual ID to each flux, useful to join data or graph the fluxes
    )
  raw_conc <- raw_conc |>
     mutate(
      datetime = datetime + time_diff
     )
  
  conc_df <- full_join(raw_conc, field_record, by = c("datetime" = "start"), keep = TRUE) |> #joining both dataset in one
    mutate(
      # datetime = datetime,
      # datetime = replace_na(datetime, start)
      # datetime_wna = datetime, # keep a datetime column with NA to know where data are missing
      datetime = case_when( # to add the fluxID in case the row with matching datetime and start is missing
        !is.na(datetime) ~ datetime,
        is.na(datetime) ~ start
      )
      ) |>
      arrange(datetime) |>
         fill(fluxID)  |> # filling fluxID to group afterwards
       drop_na(fluxID) # dropping everything that happens before the first flux

  conc_df <- conc_df |>
      group_by(fluxID) |> # filling the rest, except if there are NA for some fluxes
    fill(names(field_record)) |>
    filter(
      (datetime < end
      & datetime >= start) #cropping the part of the flux that is after the End and before the start
      # | is.na(datetime_wna) # we keep datetime = na because we want to see where there is no data
      )  |>
    mutate(
      # nrow = n(),
      n_conc = sum(!is.na(conc)), #not sure why I cannot do that with count
      ratio = n_conc/(measurement_length - startcrop), # add 1 sec because filter is including both limits
      flag = case_when(
        ratio == 0 ~ "no data",
        ratio <= ratio_threshold ~ "nb of data too low"
        # is.na(datetime_wna) ~ "no data"
        
      ) # also need to print a warning in the console with fluxID
    ) |>
       ungroup()

       # making sure all columns are in the right format
       conc_df <- conc_df |>
          mutate(
            # temp_air = as.double(temp_air), # we should not work on those columns, because there might not always be there
            # temp_soil = as.double(temp_soil),
            # PAR = as.double(PAR),
            # turfID = as.factor(turfID),
            # type = as.factor(type),
            fluxID = as.factor(fluxID),
            flag = as.character(flag),
            turfID = as.factor(turfID)
          ) |>
             arrange(fluxID)
  
  # print warnings when there are flags
  # if(any(!is.na(co2conc$flag))) warning("there is a flag somewhere")

  flags <- conc_df |>
     select(fluxID, flag) |>
     drop_na(flag) |>
        distinct() |>
           mutate(
            warnings = paste("\n","fluxID", fluxID, ":", flag),
            warnings = as.character(warnings)
           ) |>
              pull(warnings)
    
    # warnings <- pull(flags, warnings)
    # warnings <- paste(warnings, sep = ";")
    warnings <- stringr::str_c(flags)

  # warnings <- 

  # test <- paste("blop","blip", sep = "\n")
        if(any(!is.na(conc_df$flag))) warning(warnings)
        # if(any(!is.na(co2conc$flag))) warning(test)

  
  return(conc_df)
}
