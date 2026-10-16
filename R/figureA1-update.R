library(tidyverse)
source(here::here("R", "update-helpers.R"))

figureA1_idbanks <- c("001762353", "001762354", "001762357", "001762360",
                     "001766350", "001766351", "001766355")
# idem en base 2025 (IPCH 10, 10.1, 10.2, 10.4 ; IPC 10, 10.2, 10.4)
figureA1_idbanks_2025 <- c("011813326", "011813329", "011813341", "011813350",
                           "011817590", "011817626", "011817650")

figureA1 <- chainer_base2025(figureA1_idbanks, figureA1_idbanks_2025, rebase = FALSE) |>
  transmute(Coicop2016 = stringr::str_extract(TITLE_FR, "(?<=Nomenclature Coicop : ).*"),
            TIME_PERIOD,
            OBS_VALUE = as.numeric(OBS_VALUE),
            `IPC ou IPCH ?` = case_when(grepl("harmonisé", TITLE_FR) ~ "IPCH",
                                        T ~ "IPC"))

figureA1 %>%
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01-01")))  |>
  filter(date >= as.Date("1996-01-01")) |>
  arrange(date) |>
  ggplot() + ylab("") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE/10000, color =  Coicop2016, linetype = `IPC ou IPCH ?`)) +
  scale_x_date(breaks = as.Date(paste0(seq(1920, 2100, 2), "-01-01")),
               labels = scales::date_format("%Y")) +
  theme(legend.position = c(0.78, 0.78),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = 0.01*seq(0, 300, .1),
                     labels = scales::percent_format(accuracy = .1)) +
  labs(caption = "Source: Insee")

ggsave(here::here("png", "figureA1-update.png"), width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave(here::here("pdf", "figureA1-update.pdf"), width = 1.25*6, height = 1.25*3.375)
