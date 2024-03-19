#to create the tests
usethis::use_testthat()
usethis::use_test("flux_quality_lin")

#to add used packages in the description
usethis::use_package("ggplot2")

# to make the webpage
usethis::use_pkgdown()
pkgdown::build_site()
usethis::use_pkgdown_github_pages()

usethis::use_readme_rmd()


devtools::test()
devtools::document()
devtools::run_examples()
devtools::check()

styler::style_pkg()
lintr::lint_package()
