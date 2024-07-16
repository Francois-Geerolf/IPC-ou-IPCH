table1_idbanks <- c("001759970", # IPC mensuel
                    "001759971", # IPCH mensuel
                    "001764363", # IPC annuel
                    "001762489" # IPCH annuel
                    )

table1 <- paste(table1_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  tibble::as_tibble() |>
  dplyr::filter(TIME_PERIOD %in% c("1999-06", "2021-06", "2024-06", "2021", "2022", "2023")) |>
  dplyr::transmute(TIME_PERIOD, OBS_VALUE, 
                   `IPC ou IPCH ?` = dplyr::case_when(grepl("harmonisé", TITLE_FR) ~ "IPCH",
                                                      T ~ "IPC")) |>
  dplyr::mutate(OBS_VALUE = as.numeric(OBS_VALUE)) |>
  tidyr::spread(TIME_PERIOD, OBS_VALUE) |>
  dplyr::transmute(`IPC ou IPCH ?`, 
                   `2022 Annuelle` = `2022`/`2021`-1,
                   `2023 Annuelle` = `2023`/`2022`-1,
                   `Glissement sur 3 ans` = `2024-06`/`2021-06`-1,
                   `Glissement sur 25 ans` = `2024-06`/`1999-06`-1)

table1 |>
  gt::gt() |>
  gt::fmt_percent(
    columns = 2:5,
    decimals = 1
  ) |>
  gt::gtsave(filename = "table1.png")

