#' Returns a unit conversion coefficient for flux_calc
#' 
#' 


flux_units <- function(
    flux_units,
    amount_units = c("mol", "mmol", "umol", "nmol", "pmol"),
    surface_units = c("m2", "dm2", "cm2"),
    time_units = c("day", "hour", "second")
) {
    amount <- nth(flux_units, 1)
    surface <- nth(flux_units, 2)
    time <- nth(flux_units, 3)

    amount <- match.arg(amount, amount_units)
    surface <- match.arg(surface, surface_units)
    time <- match.arg(time, time_units)

# starting units are micromol/s/m^2

    amount_coeff <- case_when(
        amount == "mol"
        amount == "mmol"
        amount == "umol"
        amount == "nmol"
        amount == "pmol"
    )


    flux_coeff <- amount_coeff * surface_coeff * time_coeff

    flux_coeff
}
