library(tidyverse)
source(here::here("R", "update-helpers.R"))

table2_idbanks <- c("001759970", "001763852", "001763503") # IPC ensemble, hors tabac, tabac (base 2015)
table2_idbanks_2025 <- c("011814630", "011814056", "011815410") # idem, base 2025

table2_data <- chainer_base2025(table2_idbanks, table2_idbanks_2025)
dernier_mois <- min(tapply(table2_data$TIME_PERIOD, table2_data$IDBANK, max))
mois_fr <- function(x) str_to_title(format(as.Date(paste0(x, "-01")), "%B %Y"))

table2 <- table2_data |>
  filter(TIME_PERIOD %in% c("1990-01", dernier_mois)) |>
  transmute(TIME_PERIOD,
            OBS_VALUE,
            TITLE_FR) |>
  spread(TIME_PERIOD, OBS_VALUE) |>
  transmute(TITLE_FR,
            infl = .data[[dernier_mois]]/`1990-01`-1) |>
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
    infl = gt::html(paste0("Janvier 1990 - ", mois_fr(dernier_mois)))
  )

table2  |>
  gt::gtsave(filename = here::here("png", "table2-update.png"))

table2  |>
  gt::gtsave(filename = here::here("pdf", "table2-update.pdf"))

if (nzchar(Sys.which("pdfcrop"))) system2("pdfcrop", rep(here::here("pdf", "table2-update.pdf"), 2))
