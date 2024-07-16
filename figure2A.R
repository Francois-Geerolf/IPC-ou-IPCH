rm(list = ls())

figure1_idbanks <- c("001762353", "001762354", "001762357", "001762360",
                     "001766350", "001766351", "001766355")

figure1 <- paste(figure1_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  tibble::as_tibble() |>
  mutate(TITLE_FR = stringr::str_replace(TITLE_FR, "Pondérations de l'indice des prix à la consommation - Base 2015 - Ensemble des ménages - France - Nomenclature Coicop : ", ""),
         TITLE_FR = stringr::str_replace(TITLE_FR, "Pondérations de l'indice des prix à la consommation harmonisé - Base 2015 - Ensemble des ménages - France - Nomenclature Coicop : ", ""))  |>
  dplyr::transmute(Coicop2016 = TITLE_FR, TIME_PERIOD, OBS_VALUE, 
                   `IPC ou IPCH ?` = dplyr::case_when(IDBANK %in% c("001766350", "001766351", "001766355") ~ "IPC",
                                                      IDBANK %in% c("001762353", "001762354", "001762357", "001762360") ~ "IPCH")) |>
  dplyr::mutate(OBS_VALUE = as.numeric(OBS_VALUE))

saveRDS(figure1, file = "figure1.rds")

figure1 <- readRDS("figure1.rds")

figure1 %>%
  mutate(date = paste0(TIME_PERIOD, "-01-01"),
         date = as.Date(date))  |>
  filter(date >= as.Date("1996-01-01")) |>
  arrange(date) |>
  ggplot() + ylab("") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE/10000, color =  Coicop2016, linetype = `IPC ou IPCH ?`)) +
  scale_x_date(breaks = seq(1920, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.78, 0.8),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = 0.01*seq(0, 300, .1),
                     labels = percent_format(accuracy = .1))

ggsave("figure1.png", width = 1.25*6, height = 1.25*3.375)
