#to create the tests
usethis::use_testthat()
usethis::use_test("flux_fitting_quadratic")

#to add used packages in the description
usethis::use_package("")

# to make the webpage
usethis::use_pkgdown()
pkgdown::build_site()
usethis::use_pkgdown_github_pages()

usethis::use_vignette("my-vignette")

usethis::use_readme_rmd()
devtools::build_readme()
usethis::use_package_doc()

devtools::test()
devtools::document()
devtools::run_examples()
devtools::check()

# making the code better
# styler::style_pkg()
lintr::lint_package()

# display number of downloads from CRAN
cranlogs::cran_downloads(from = "2024-08-01", to = "2024-10-07", packages = "fluxible") |>
    dplyr::summarise(total = sum(count))

# also here https://hadley.shinyapps.io/cran-downloads/
