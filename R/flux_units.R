#' Returns a unit conversion coefficient for flux_calc
#' 
#' 


flux_units <- function(
    flux_units,
    amount_units = c("mol", "mmol", "umol", "nmol", "pmol"),
    time_units = c("day", "hour", "second"),
    surface_units = c("m2", "dm2", "cm2")
) {
    amount <- str_extract(flux_units, "^\\w*")
    time <- str_extract(flux_units, "(?<=\\/)(\\w.*)(?=\\/)")
    surface <- str_extract(flux_units, "\\w*$")

    amount <- match.arg(amount, amount_units)
    time <- match.arg(time, time_units)
    surface <- match.arg(surface, surface_units)

# starting units are micromol/s/m^2

    amount_coeff <- case_when(
        amount == "mol"
        amount == "mmol"
        amount == "umol"
        amount == "nmol"
        amount == "pmol"
    )


    flux_coeff <- amount_coeff * time_coeff * surface_coeff

    flux_coeff
}
