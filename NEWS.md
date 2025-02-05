# fluxible 0.1.0

* Added a vignette describing how to prepare the data for `fluxible`.
* `flux_plot` now adds a vertical line showing where t_zero is.
* scale_x_datetime arguments in 'flux_plot' are now past as a list.
* Functions now recycle user's column names instead of renaming them. Columns created by fluxible functions have the suffix 'f_'.

# fluxible 0.0.6

* Correction in flux_quality: 'zero' and 'discard' flags were inverted when using the linear fit

# fluxible 0.0.5

* fixed the blurriness of quality flags in flux_plot

# fluxible 0.0.4

* flux_gep function to calculate GEP

# fluxible 0.0.3

* All plots are colored the same (colors code for raw condentration data points and fit and slope in black)
* Legend for line types in plots
* Plot subtitles indicating the fit type

# fluxible 0.0.2

* Unit choices in flux_calc
* plot_area can be a variable in flux_calc
* flux_fitting with exponential model works when concentration data are missing in the middle of the measurement