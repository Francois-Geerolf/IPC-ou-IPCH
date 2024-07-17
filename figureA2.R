library(tidyverse)

IPC_ou_IPCH_idbanks <- tribble(~ IDBANK, ~ IPC_ou_IPCH,
                               "001759970", "IPC", # IPC mensuel
                               "001759971", "IPCH" # IPCH mensuel
)

min_date <- as.Date("1999-01-01")
max_date <- as.Date("2024-06-01")

indicefp <- fread("https://www.ipp.eu/wp-content/themes/ipp/baremes/marche-du-travail/remuneration_dans_fonction_publique/indicefp/indicefp.csv") |>
  mutate(date = as.Date(date)) |>
  arrange(desc(date)) |>
  # Add today's date to be equal ------
slice(1, 1:n()) |>
  mutate(date = ifelse(row_number() == 1, Sys.Date(), date)) |>
  mutate(date = as.Date(date))


figureA2_idbanks <- tribble(~ IDBANK, ~ brut_ou_net,
                           "001572130", "indice brut", # https://www.insee.fr/fr/statistiques/serie/001572130
                           "001572134", "indice net"   # https://www.insee.fr/fr/statistiques/serie/001572134
)

IPC_ou_IPCH <- IPC_ou_IPCH_idbanks |>
  mutate(data = map(IDBANK, ~ paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", .) |>
                      rsdmx::readSDMX() |>
                      as_tibble() |>
                      transmute(date = as.Date(paste0(TIME_PERIOD, "-01")),
                                OBS_VALUE = as.numeric(OBS_VALUE)))) |>
  unnest(cols = c(data)) |>
  select(-IDBANK) |>
  spread(IPC_ou_IPCH, OBS_VALUE)

net_brut_mensuel <- figureA2_idbanks |>
  mutate(data = map(IDBANK, ~ paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", .) |>
                      rsdmx::readSDMX() |>
                      as_tibble()  |> 
                      transmute(date = as.Date(zoo::as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")),
                                OBS_VALUE = as.numeric(OBS_VALUE)))) |>
  unnest(cols = c(data)) |>
  select(-IDBANK) |>
  spread(brut_ou_net, OBS_VALUE) |>
  transmute(date, net_brut = `indice net`/`indice brut`) |>
  slice(1, 1:n(), n()) |>
  mutate(date = case_when(row_number() == n() ~ Sys.Date(),
                          row_number() == 1 ~ min_date,
                          TRUE ~ date)) |>
  complete(date = seq.Date(min(date), max(date), by = "month")) |>
  fill(net_brut)


figureA2 <- indicefp |>
  select(date, value = point_indice_en_euros) |>
  arrange(desc(date)) |>
  complete(date = seq.Date(min(date), max(date), by = "month")) |>
  fill(value) |>
  left_join(IPC_ou_IPCH, by = "date") |>
  left_join(net_brut_mensuel, by = "date") |>
  filter(date >= min_date,
         date <= max_date) |>
  transmute(date,
            IPCH = (value/value[1])*(IPCH[1]/IPCH)*net_brut,
            IPC = (value/value[1])*(IPC[1]/IPC)*net_brut) |>
  gather(variable, OBS_VALUE, -date)

figureA2 |>
  ggplot() + geom_line(aes(x = date, y = OBS_VALUE, color = variable)) + theme_minimal() +
  scale_x_date(breaks = as.Date(paste0(c(seq(1999, 2100, 5), seq(1997, 2100, 5)), "-01-01")),
               labels = date_format("%Y")) +
  theme(legend.position = c(0.43, 0.12),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(1, 0.02, -0.02),
                labels = percent(seq(1, 0.02, -0.02)-1, acc = 1)) + 
  ylab("") + xlab("") +
  geom_text(data = figureA2 |> 
              filter((year(date) %in% seq(2009, 2019, 5) & month(date) == 1)),
            aes(x = date, y = OBS_VALUE, label = percent(OBS_VALUE-1, acc = 0.1)), 
            fontface ="bold", color = "black", size = 3) +
  geom_label(data = filter(figureA2, date == max(date)),
             aes(x = date, y = OBS_VALUE, label = percent(OBS_VALUE-1, acc = 0.1), color = variable), 
             fontface ="bold", size = 3)

ggsave("figureA2.png", width = 1.25*6, height = 1.25*3.375)
ggsave("figureA2.pdf", width = 1.25*6, height = 1.25*3.375)
