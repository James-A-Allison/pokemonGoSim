library(tidyverse)

raid_boss_tiers <- tibble::tibble(
  tier = c(6, 5, 4),
  hp = c(22500, 15000, 9000),
  cpm = c(0.7903, 0.7903, 0.7903)
)
usethis::use_data(raid_boss_tiers, overwrite = TRUE)

weather_boosts <- readRDS("C:/Users/james/PokeWebApp/data/weather.rds")
usethis::use_data(weather_boosts, overwrite = TRUE)


# mega_table <- readRDS("C:/Users/james/PokeWebApp/data/mega_table.RDS")
# usethis::use_data(mega_table, overwrite = TRUE)

type_effectiveness <- readRDS("C:/Users/james/PokeWebApp/data/type_effectiveness.RDS") 
usethis::use_data(type_effectiveness, overwrite = TRUE)

level_multipliers <- readRDS("C:/Users/james/PokeWebApp/data/levels.rds") %>%
  select(level = `Level`, cpm = `CP Multiplier`)
usethis::use_data(level_multipliers, overwrite = TRUE)
