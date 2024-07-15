rm(list = ls())

# Table 1 

## Comparaison Juin 1999-2024 ---------


table1_idbanks <- c("001759970", # Indice des prix à la consommation - Base 2015 - Ensemble des ménages - France - Ensemble
                    "001759971", # Indice des prix à la consommation harmonisé - Base 2015 - Ensemble des ménages - France - Nomenclature Coicop : Ensemble harmonisé
                    "001764363", # Indice annuel des prix à la consommation - Base 2015 - Ensemble des ménages - France - Ensemble
                    "001762489" # Indice des prix à la consommation harmonisé annuel - Base 2015 - Ensemble des ménages - France - Nomenclature Coicop : Ensemble harmonisé 
                    )

table1 <- paste(table1_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  tibble::as_tibble() |>
  dplyr::filter(TIME_PERIOD %in% c("1999-06", "2021-06", "2024-06", "2021", "2022", "2023")) |>
  dplyr::transmute(TIME_PERIOD, OBS_VALUE, 
                   `IPC ou IPCH ?` = dplyr::case_when(IDBANK %in% c("001759970", "001764363") ~ "IPC",
                                                      IDBANK %in% c("001759971", "001762489") ~ "IPCH")) |>
  dplyr::mutate(OBS_VALUE = as.numeric(OBS_VALUE)) |>
  tidyr::spread(TIME_PERIOD, OBS_VALUE) |>
  dplyr::transmute(`IPC ou IPCH ?`, 
                   `2022 Annuelle` = `2022`/`2021`-1,
                   `2023 Annuelle` = `2023`/`2022`-1,
                   `Glissement sur 3 ans` = `2024-06`/`2021-06`-1,
                   `Glissement sur 25 ans` = `2024-06`/`1999-06`-1)
  
saveRDS(table1, file = "table1.rds")

readRDS("table1.rds")

table1 |>
  gt::gt() |>
  gt::fmt_percent(
    columns = 2:5,
    decimals = 1
  ) |>
  gt::gtsave(filename = "table1.png")

