####################################################################################################
## Secure Communities Rollout Maps
## Last Edited: 06/27/2026
####################################################################################################

library(tidyverse)
library(tigris)
library(sf)
library(lubridate)

options(tigris_use_cache = TRUE)

### load data ###
sc <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/secure1904.csv")

### compute first rollout year per county (detainer date, falling back to departed date) ###
sc_rollout <- sc |>
  mutate(
    detainer_date = as.Date(detainer_prepare_date, format = "%d%b%y"),
    departed_date = as.Date(departed_date,         format = "%d%b%y"),
    rollout_date  = coalesce(detainer_date, departed_date),
    county        = str_remove(county, " Borough$"),
    county        = str_to_title(county),
    year          = year(rollout_date)
  ) |>
  filter(!is.na(rollout_date), year >= 2008) |>
  group_by(state, county) |>
  summarise(first_detainer_year = min(year, na.rm = TRUE), .groups = "drop")

### get county shapefiles ###
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2010) |>
  shift_geometry() |>
  left_join(
    fips_codes |> distinct(state_code, state),
    by = c("STATEFP" = "state_code")
  ) |>
  mutate(
    county = str_remove(NAME, " County$| Parish$| Borough$| Census Area$| city$| Municipality$"),
    county = str_to_title(county)
  ) |>
  filter(state %in% state.abb)

### join rollout data to county geometries ###
counties_rollout <- counties_sf |>
  left_join(sc_rollout, by = c("state", "county"))

### generate one map per year of SC activity ###
for (yr in 2008:2016) {

  map_yr <- counties_rollout |>
    mutate(status = if_else(
      !is.na(first_detainer_year) & first_detainer_year <= yr,
      "Treated", "Not yet treated"
    ))

  p <- ggplot(map_yr) +
    geom_sf(aes(fill = status), color = NA, linewidth = 0) +
    scale_fill_manual(
      values = c("Not yet treated" = "#e8e8e8", "Treated" = "#1a3a5c"),
      name   = NULL
    ) +
    labs(title = paste("Secure Communities Rollout:", yr)) +
    theme_void() +
    theme(
      plot.title    = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      legend.position  = "bottom",
      legend.text      = element_text(size = 11),
      plot.margin      = margin(10, 10, 10, 10)
    )

  out_path <- paste0(
    "/Users/kieran/Documents/GitHub/labor_and_automation/figs/primary/sc_rollout_", yr, ".png"
  )

  ggsave(out_path, plot = p, width = 12, height = 7, dpi = 300)
  message("Saved: sc_rollout_", yr, ".png")
}
