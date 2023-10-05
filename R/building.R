# script with all the steps to build the package, taken from https://biostats-r.github.io/biostats/package/
# and some random notes as well so I don't forget things

library(usethis)
library(available)


# finding out if the package name is available ----------------------------

available("wtflux")


# creating the structure --------------------------------------------------

# path <- "/home/jga051/Documents/01_PhD/06_projects/wtflux"
#
# create_package(path = path)



# tests to check the package ----------------------------------------------

# what do I need to test?
#


# package workflow --------------------------------------------------------
# it seems that to make testing easier I need to split my functions in smaller bits (I can add some wrap-up functions later)
#
# match.flux: will stay one function.
# this function matches continuously logged data into the different measurements and adds a measurement ID.
# it does so by using the field record provided by the user (record of which measurements was taken when with same time as the logger)
# side-note: we could include a time difference parameter in case the logger and the user were not synchronized

# fitting.fluxes: this one will be split into several functions
# slope.flux: a function to calculate the slope of the flux
# quality.flux: to calculate the quality parameters of each flux (RMSE, NRMSE, correlation coefficient and co)
# flag.flux: to add the flags based on the quality parameters and the user's choices


# should I include a function to graph the fluxes or can we assume that people know how to use ggplot?

# calculate.flux: this function is only calculating the fluxes based on the slope and other needed inputs
# GPP and corrections will be other functions
































