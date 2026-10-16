library(tidyverse)

table2_idbanks <- c("001759970", "001763852", "001763503")

table2 <- paste(table2_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  filter(TIME_PERIOD %in% c("1990-01", "2024-06")) |>
  transmute(TIME_PERIOD,
            OBS_VALUE = as.numeric(OBS_VALUE),
            TITLE_FR) |>
  spread(TIME_PERIOD, OBS_VALUE) |>
  transmute(TITLE_FR, 
            infl = `2024-06`/`1990-01`-1) |>
  gt::gt() |>
  gt::fmt_percent(
    columns = 2,
    decimals = 1,
    force_sign = T
  ) |>
  gt::cols_align(
    align = "center",
    columns = everything()
  ) |>
  gt::cols_label(
    infl = gt::html("Janvier 1990 - Juin 2024")
  )

table2  |>
  gt::gtsave(filename = here::here("png", "table2.png"))

table2  |>
  gt::gtsave(filename = here::here("pdf", "table2.pdf"))

if (nzchar(Sys.which("pdfcrop"))) system2("pdfcrop", rep(here::here("pdf", "table2.pdf"), 2))

