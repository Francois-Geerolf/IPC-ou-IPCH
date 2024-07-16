library(tidyverse)

table1_idbanks <- c("001759970", # IPC mensuel
                    "001759971", # IPCH mensuel
                    "001764363", # IPC annuel
                    "001762489" # IPCH annuel
                    )

table1 <- paste(table1_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  filter(TIME_PERIOD %in% c("1999-06", "2021-06", "2024-06", "2021", "2022", "2023")) |>
  transmute(TIME_PERIOD,
            OBS_VALUE = as.numeric(OBS_VALUE), 
            `IPC ou IPCH ?` = case_when(grepl("harmonisé", TITLE_FR) ~ "Inflation IPCH",
                                        T ~ "Inflation IPC")) |>
  spread(TIME_PERIOD, OBS_VALUE) |>
  transmute(`IPC ou IPCH ?`, 
            infl2022 = `2022`/`2021`-1,
            infl2023 = `2023`/`2022`-1,
            g3a = `2024-06`/`2021-06`-1,
            g25a = `2024-06`/`1999-06`-1)

table1 |>
  gt::gt() |>
  gt::fmt_percent(
    columns = 2:5,
    decimals = 1,
    force_sign = T
  ) |>
  gt::cols_align(
    align = "center",
    columns = everything()
  ) |>
  gt::cols_label(
    infl2022 = gt::html("2022<br>Annuelle"),
    infl2023 = gt::html("2023<br>Annuelle"),
    g3a = gt::html("Juin 2021 - Juin 2024<br>Glissement sur 3 ans"),
    g25a = gt::html("Juin 1999 - Juin 2024<br>Glissement sur 25 ans")
  )  |>
  gt::gtsave(filename = "table1.png")
