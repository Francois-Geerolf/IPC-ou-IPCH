library(tidyverse)

figure1A_idbanks <- c("001762353", "001762354", "001762357", "001762360",
                     "001766350", "001766351", "001766355")

figure1A <- paste(figure1A_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  transmute(Coicop2016 = stringr::str_extract(TITLE_FR, "(?<=Nomenclature Coicop : ).*"),
            TIME_PERIOD,
            OBS_VALUE = as.numeric(OBS_VALUE),
            `IPC ou IPCH ?` = case_when(grepl("harmonisé", TITLE_FR) ~ "IPCH",
                                        T ~ "IPC"))

figure1A %>%
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01-01")))  |>
  filter(date >= as.Date("1996-01-01")) |>
  arrange(date) |>
  ggplot() + ylab("") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE/10000, color =  Coicop2016, linetype = `IPC ou IPCH ?`)) +
  scale_x_date(breaks = as.Date(paste0(seq(1920, 2100, 2), "-01-01")),
               labels = scales::date_format("%Y")) +
  theme(legend.position = c(0.78, 0.8),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = 0.01*seq(0, 300, .1),
                     labels = scales::percent_format(accuracy = .1))

ggsave("figure1A.pdf", width = 1.25*6, height = 1.25*3.375)
ggsave("figure1A.png", width = 1.25*6, height = 1.25*3.375)
