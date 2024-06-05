#to create the tests
usethis::use_testthat()
usethis::use_test("flux_fitting_quadratic")

#to add used packages in the description
usethis::use_package("ggplot2")

# to make the webpage
usethis::use_pkgdown()
pkgdown::build_site()
usethis::use_pkgdown_github_pages()

usethis::use_readme_rmd()
devtools::build_readme()


devtools::test()
devtools::document()
devtools::run_examples()
devtools::check()

# making the code better
styler::style_pkg()
lintr::lint_package()
