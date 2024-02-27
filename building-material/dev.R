#to create the tests
usethis::use_testthat()

#to read raw data
usethis::use_data_raw()

devtools::test()
devtools::document()
devtools::run_examples()
devtools::check()
