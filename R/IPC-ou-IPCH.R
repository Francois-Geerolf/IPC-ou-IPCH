library("rsdmx")
library("tidyverse")
library("zoo")
library("readxl")


  
`IPC-ou-IPCH` <- "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001759970+001759971" |>
    readSDMX() |>
    as_tibble() |>
    mutate(IDBANK,
           TIME_PERIOD = as.Date(paste0(TIME_PERIOD, "-01")),
           TIME_PERIOD = paste0(year(TIME_PERIOD), "-Q", quarter(TIME_PERIOD)),
           TIME_PERIOD = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"),
           OBS_VALUE = as.numeric(OBS_VALUE)) |>
    group_by(TITLE_FR, TIME_PERIOD) |>
    filter(n() == 3) |>
    summarise(OBS_VALUE = mean(OBS_VALUE)) |>
    ungroup() |>
    transmute(IPC_ou_IPCH = ifelse(grepl("harmonisé", TITLE_FR), "IPCH", "IPC"), TIME_PERIOD, OBS_VALUE)
  
save(IPC_ou_IPCH_trimestriel, file = "IPC_ou_IPCH_trimestriel.RData")