library(tidyverse)
source(here::here("R", "update-helpers.R"))

# Séries mensuelles base 2015 prolongées par la base 2025
mensuel <- chainer_base2025(c("001759970", # IPC mensuel
                              "001759971"), # IPCH mensuel
                            c("011814630", "011812231"))

# Séries annuelles base 2015 (disponibles jusqu'en 2025)
annuel <- insee_bdm(c("001764363", # IPC annuel
                      "001762489")) # IPCH annuel

dernier_mois <- min(tapply(mensuel$TIME_PERIOD, mensuel$IDBANK, max))
il_y_a_3_ans <- format(as.Date(paste0(dernier_mois, "-01")) - years(3), "%Y-%m")
annees <- as.character(2022:max(annuel$TIME_PERIOD))
mois_fr <- function(x) str_to_title(format(as.Date(paste0(x, "-01")), "%B %Y"))

table1 <- bind_rows(mensuel, annuel) |>
  filter(TIME_PERIOD %in% c("1999-06", il_y_a_3_ans, dernier_mois, as.character(2021:2100))) |>
  transmute(TIME_PERIOD,
            OBS_VALUE,
            `IPC ou IPCH ?` = case_when(grepl("harmonisé", TITLE_FR) ~ "Inflation IPCH",
                                        T ~ "Inflation IPC")) |>
  spread(TIME_PERIOD, OBS_VALUE)

for (a in annees) {
  table1[[paste0("infl", a)]] <- table1[[a]] / table1[[as.character(as.numeric(a) - 1)]] - 1
}

table1 <- table1 |>
  mutate(g3a = .data[[dernier_mois]] / .data[[il_y_a_3_ans]] - 1,
         g99 = .data[[dernier_mois]] / `1999-06` - 1) |>
  select(`IPC ou IPCH ?`, starts_with("infl"), g3a, g99) |>
  gt::gt() |>
  gt::fmt_percent(
    columns = -1,
    decimals = 1,
    force_sign = T
  ) |>
  gt::cols_align(
    align = "center",
    columns = everything()
  ) |>
  gt::cols_label(
    .list = c(setNames(map(annees, ~ gt::html(paste0(.x, "<br>Annuelle"))), paste0("infl", annees)),
              list(g3a = gt::html(paste0(mois_fr(il_y_a_3_ans), " - ", mois_fr(dernier_mois), "<br>Glissement sur 3 ans")),
                   g99 = gt::html(paste0("Juin 1999 - ", mois_fr(dernier_mois), "<br>Glissement depuis juin 1999"))))
  ) |>
  gt::tab_footnote("Source: Insee, calculs de l'auteur. Séries base 2015 prolongées par la base 2025 à partir de janvier 2026.")

table1  |>
  gt::gtsave(filename = here::here("png", "table1-update.png"))

table1  |>
  gt::gtsave(filename = here::here("pdf", "table1-update.pdf"))

if (nzchar(Sys.which("pdfcrop"))) system2("pdfcrop", rep(here::here("pdf", "table1-update.pdf"), 2))
