#' Matching continously measured fluxes with measurement IDs
#' @description Function to match a dataframe of continuously measured
#' CO2 concentration with measurement IDs from another dataframe. Uses datetime
#' to tell which measurements happened when. Extra variables in both dataframes
#' will appear in the output.
#' @param raw_conc dataframe of CO2 concentration measured continuously.
#' Has to contain at least a datetime column in ymd_hms format and
#' a gas concentration column as double.
#' @param field_record dataframe recording which measurement happened when.
#' Has to contain at least a column telling at what time measurements started,
#' and any other column allowing for identification of measurements.
#' @param startcrop how many seconds should be discarded at the beginning of
#' the measurement
#' @param measurement_length length of the measurement (in seconds)
#' from the start specified in the field_record
#' @param ratio_threshold ratio (number of concentration measurement compared to
#' length of measurement in seconds) below which the data should be flagged as
#' too little
#' @param time_diff time difference (in seconds) between the two datasets.
#' Will be added to the datetime column of the raw_conc dataset
#' @param datetime_col to specify the name of the datetime column in raw_conc
#' (dmy_hms format)
#' @param conc_col to specify the name of the concentration column in raw_conc
#' @param start_col to specify the name of the start column in field_record
#' (dmy_hms format)
#' @return a dataframe with concentration measurements, corresponding datetime,
#' flux ID, start and end of measurement, flags in case of no data or low number
#' of data, and any variables present in one of the inputs.
#' @importFrom dplyr rename arrange mutate row_number full_join case_when
#' group_by filter ungroup select distinct pull
#' @importFrom tidyr fill drop_na
#' @importFrom lubridate is.POSIXct
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
                       start_col = "start") {
  raw_conc <- raw_conc |>
    rename(
      f_datetime = all_of((datetime_col)),
      f_conc = all_of((conc_col))
    )

  field_record <- field_record |>
    rename(
      f_start = all_of((start_col))
    )

  # this should be moved in a check data function
  # need to include a test for the format of the column, especially the date
  if (!is.POSIXct(raw_conc$f_datetime)) {
    stop("datetime in raw_conc dataframe is not ymd_hms!")
  }
  if (!is.double(raw_conc$f_conc)) stop("conc is not a double")

  if (!is.POSIXct(field_record$f_start)) {
    stop("start in field_record dataframe is not ymd_hms!")
  }

  if (!is.double(((startcrop)))) {
    stop("startcrop has to be a double")
  }
  if (!is.double(((time_diff)))) {
    stop("time_diff has to be a double")
  }
  if (!is.double(((measurement_length)))) {
    stop("measurement_length has to be a double")
  }
  if (!is.double(((ratio_threshold)))) {
    stop("ratio_threshold has to be a number between 0 and 1")
  }
  if (((ratio_threshold)) < 0 || ((ratio_threshold)) > 1) {
    stop("ratio_threshold has to be a number between 0 and 1")
  }


  field_record <- field_record |>
    arrange(.data$f_start) |>
    mutate(
      f_end = .data$f_start + ((measurement_length)),
      f_start = .data$f_start + ((startcrop)),
      f_fluxID = row_number()
    )
  raw_conc <- raw_conc |>
    mutate(
      f_datetime = .data$f_datetime + ((time_diff))
    )

  conc_df <- full_join(
    raw_conc, field_record,
    by = c("f_datetime" = "f_start"), keep = TRUE
  ) |>
    mutate(
      f_datetime = case_when(
        !is.na(.data$f_datetime) ~ .data$f_datetime,
        is.na(.data$f_datetime) ~ .data$f_start
      )
    ) |>
    arrange(.data$f_datetime) |>
    fill("f_fluxID") |>
    drop_na("f_fluxID")

  conc_df <- conc_df |>
    group_by(.data$f_fluxID) |>
    fill(names(field_record)) |>
    filter(
      (.data$f_datetime < .data$f_end &
        .data$f_datetime >= .data$f_start)
    ) |>
    mutate(
      f_n_conc = sum(!is.na(.data$f_conc)),
      f_ratio = .data$f_n_conc / (((measurement_length)) - ((startcrop))),
      f_flag_match = case_when(
        .data$f_ratio == 0 ~ "no data",
        .data$f_ratio <= ((ratio_threshold)) ~ "nb of data too low"
      )
    ) |>
    ungroup()

  conc_df <- conc_df |>
    mutate(
      f_fluxID = as.factor(.data$f_fluxID),
      f_flag_match = as.character(.data$f_flag_match)
    ) |>
    arrange(.data$f_fluxID)

  # print warnings when there are flags

  flags <- conc_df |>
    select("f_fluxID", "f_flag_match") |>
    drop_na("f_flag_match") |>
    distinct() |>
    mutate(
      f_warnings = paste(
        "\n", "fluxID", .data$f_fluxID, ":",
        .data$f_flag_match
      ),
      f_warnings = as.character(.data$f_warnings)
    ) |>
    pull(.data$f_warnings)

  f_warnings <- stringr::str_c(flags)


  if (any(!is.na(conc_df$f_flag_match))) warning(f_warnings)


  conc_df
}
