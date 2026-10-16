library(tidyverse)
library(ggpubr)
library(scales)

# Base 2025 (COICOP 2018), rétropolée depuis 1996 : les séries base 2015 sont arrêtées fin 2025
figure1_idbanks_A <- c("011812813", "011816286") # Pondérations santé IPCH, IPC
figure1_idbanks_B <- c("011812814", "011816281") # Indices santé IPCH, IPC (mensuels)

figure1_A <- paste(figure1_idbanks_A, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  tibble::as_tibble() |>
  transmute(date = as.Date(paste0(TIME_PERIOD, "-01-01")),
         OBS_VALUE = as.numeric(OBS_VALUE),
         `IPC ou IPCH ?` = case_when(grepl("harmonisé", TITLE_FR) ~ "IPCH",
                                     T ~ "IPC")) %>%
  filter(date >= as.Date("1996-01-01")) %>%
  ggplot() + ylab("Pondération, 06 - Santé") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE/10000, color = `IPC ou IPCH ?`)) +
  scale_x_date(breaks = seq(1920, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.7, 0.55),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(breaks = 0.01*seq(0, 300, 1),
                     labels = percent_format(accuracy = 1))

figure1_A



figure1_B <- paste(figure1_idbanks_B, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  tibble::as_tibble() |>
  transmute(date = as.Date(paste0(TIME_PERIOD, "-01")),
            OBS_VALUE = as.numeric(OBS_VALUE),
            `IPC ou IPCH ?` = case_when(grepl("harmonisé", TITLE_FR) ~ "IPCH",
                                        T ~ "IPC")) %>%
  group_by(`IPC ou IPCH ?`) %>%
  filter(date >= as.Date("1996-01-01")) %>%
  arrange(date) %>%
  mutate(OBS_VALUE = 100*OBS_VALUE/OBS_VALUE[1]) %>%
  ggplot() + ylab("Indice de prix - Santé (100 = 1996)") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE, color = `IPC ou IPCH ?`)) +
  scale_x_date(breaks = seq(1920, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.7, 0.55),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_log10(breaks = seq(10, 300, 5),
                labels = dollar_format(accuracy = 1, prefix = "")) +
  labs(caption = "Source: Insee")

figure1_B

ggarrange(figure1_A, figure1_B, common.legend = T)

ggsave(here::here("png", "figure1-update.png"), width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave(here::here("pdf", "figure1-update.pdf"), width = 1.25*6, height = 1.25*3.375)
