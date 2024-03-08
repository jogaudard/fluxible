#to create the tests
usethis::use_testthat()
usethis::use_test("flux_quality_lin")

#to add used packages in the description
usethis::use_package("ggplot2")


devtools::test()
devtools::document()
devtools::run_examples()
devtools::check()
