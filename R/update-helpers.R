library(tidyverse)

# Les séries Insee base 2015 sont arrêtées fin 2025 (passage à la base 2025, COICOP 2018).
# Pour les scripts *-update.R, on garde les séries base 2015 utilisées dans la note
# et on les prolonge avec les séries base 2025 au-delà de leur dernière date.

insee_bdm <- function(idbanks) {
  paste(idbanks, collapse = "+") |>
    paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
    rsdmx::readSDMX() |>
    as_tibble() |>
    transmute(IDBANK, TIME_PERIOD, OBS_VALUE = as.numeric(OBS_VALUE),
              TITLE_FR = str_remove(TITLE_FR, " - Séries arrêtées\\s*$"))
}

# old, new : vecteurs d'idbanks appariés (base 2015, base 2025)
# rebase = TRUE pour les indices (raccord à la dernière date commune),
# rebase = FALSE pour les pondérations (simple ajout des années suivantes).
chainer_base2025 <- function(old, new, rebase = TRUE) {
  ancien <- insee_bdm(old)
  nouveau <- insee_bdm(new)
  map2(old, new, function(o, n) {
    a <- filter(ancien, IDBANK == o)
    b <- filter(nouveau, IDBANK == n)
    fin <- max(a$TIME_PERIOD)
    coef <- if (rebase) a$OBS_VALUE[a$TIME_PERIOD == fin] / b$OBS_VALUE[b$TIME_PERIOD == fin] else 1
    b |>
      filter(TIME_PERIOD > fin) |>
      mutate(IDBANK = o, OBS_VALUE = OBS_VALUE * coef, TITLE_FR = a$TITLE_FR[1]) |>
      bind_rows(a, x = _)
  }) |>
    list_rbind() |>
    arrange(IDBANK, TIME_PERIOD)
}
