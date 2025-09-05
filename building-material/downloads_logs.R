# display number of downloads from CRAN
# cranlogs::cran_downloads(from = "2024-08-01", to = "2024-10-07", packages = "fluxible") |>
#     dplyr::summarise(total = sum(count))

# pkgsearch::pkg_search("fluxfinder, fluxible, gasfluxes, HMR")[, c("package", "downloads_last_month")] |>
#     dplyr::arrange(dplyr::desc(downloads_last_month))


library(adjustedcranlogs)
library(tidyverse)

date_end <- today() - 2

adj_cran_downloads(c("fluxible", "fluxfinder", "gasfluxes", "flux", "HMR", "licoread"), from = "2024-08-27", to = date_end) |>
  pivot_longer(c(adjusted_downloads, adjusted_total_downloads)) |>
  ggplot(aes(date, value, color = package)) +
  geom_line(aes(linewidth = package)) +
  theme_bw() +
  facet_grid(name ~ ., scales = "free") +
  scale_linewidth_manual(values = c(rep(0.4, 2), 1, rep(0.4, 2), 1))

adj_cran_downloads(c("fluxible", "fluxfinder", "gasfluxes", "flux", "HMR", "licoread"), "last-month") |>
  pivot_longer(c(adjusted_downloads, adjusted_total_downloads)) |>
  ggplot(aes(date, value, color = package)) +
  geom_line(aes(linewidth = package)) +
  theme_bw() +
  facet_grid(name ~ ., scales = "free") +
  scale_linewidth_manual(values = c(rep(0.4, 2), 1, rep(0.4, 2), 1))
