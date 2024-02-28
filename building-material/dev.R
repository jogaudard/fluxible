#to create the tests
usethis::use_testthat()

#to add used packages in the description
usethis::use_package("purrr")


devtools::test()
devtools::document()
devtools::run_examples()
devtools::check()
