library(tidyverse)

IPC_ou_IPCH_idbanks <- tribble(~ IDBANK, ~ IPC_ou_IPCH,
                           "001764363", "IPC", # IPC annuel
                           "001762489", "IPCH" # IPCH annuel
)

figure2_idbanks <- tribble(~ IDBANK, ~ Metier,
                           "010752321", "Cadres y compris chefs d'entreprise", # Salaire net EQTP cadres privé
                           "010752324", "Professions intermédiaires", # Salaire net EQTP professions intermédiaires privé
                           "010752330", "Ouvriers" # Salaire net EQTP ouvriers privé
)

IPC_ou_IPCH <- IPC_ou_IPCH_idbanks |>
  mutate(data = map(IDBANK, ~ paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", .) |>
                      rsdmx::readSDMX() |>
                      as_tibble() |>
                      transmute(TIME_PERIOD,
                                OBS_VALUE = as.numeric(OBS_VALUE)))) |>
  unnest(cols = c(data)) %>%
  select(-IDBANK) %>%
  spread(IPC_ou_IPCH, OBS_VALUE)

figure2 <- figure2_idbanks |>
  mutate(data = map(IDBANK, ~ paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", .) |>
                      rsdmx::readSDMX() |>
                      as_tibble() |>
                      transmute(TIME_PERIOD,
                                OBS_VALUE = as.numeric(OBS_VALUE)))) |>
  unnest(cols = c(data)) |>
  select(-IDBANK) |>
  left_join(IPC_ou_IPCH, by = "TIME_PERIOD") |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01-01"))) |>
  filter(date >= as.Date("1996-01-01"),
         date <= as.Date("2022-01-01")) |>
  group_by(Metier) %>%
  arrange(date) %>%
  transmute(date,
            Metier,
            `Réel (inflation IPCH)` = 100*(OBS_VALUE/OBS_VALUE[1])/(IPCH/IPCH[1]),
            `Réel (inflation IPC)` = 100*(OBS_VALUE/OBS_VALUE[1])/(IPC/IPC[1])) %>%
  gather(variable, value, -date, -Metier)



figure2 %>%
  ggplot() + geom_line(aes(x = date, y = value,  color = variable, linetype = Metier)) +
  theme_minimal() + ylab("") + xlab("") +
  #scale_color_manual(values = c("darkgrey", "#F8766D", "#619CFF")) +
  scale_x_date(breaks = seq(1996, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.3, 0.8),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(0, 200, 1)) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(caption = "Source: Insee, calculs de l'auteur")

ggsave("figure2.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure2.pdf", width = 1.25*6, height = 1.25*3.375)
