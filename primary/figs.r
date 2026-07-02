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
sc_activation <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/sc_activation_dates.csv")

### compute first rollout year per county from ICE's official SC activation roster ###
sc_rollout <- sc_activation |>
  mutate(
    county = str_remove(county, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county),
    first_detainer_year = year(as.Date(activation_date))
  ) |>
  group_by(state, county) |>
  summarise(first_detainer_year = min(first_detainer_year, na.rm = TRUE), .groups = "drop")

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
for (yr in 2008:2013) {

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

####################################################################################################
## Secure Communities Rollout Maps: Agricultural Counties
####################################################################################################

### load USDA ERS County Typology Codes (2015 edition) and flag farming-dependent counties ###
ag_counties <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/ers_county_typology_2015.csv") |>
  filter(Farming_2015_Update == 1) |>
  transmute(
    state  = State,
    county = str_remove(County_name, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county),
    ag_county = TRUE
  )

### join ag-county flag onto the rollout data and generate one map per year ###
counties_rollout_ag <- counties_rollout |>
  left_join(ag_counties, by = c("state", "county")) |>
  mutate(ag_county = replace_na(ag_county, FALSE))

for (yr in 2008:2013) {

  map_yr <- counties_rollout_ag |>
    mutate(status = case_when(
      !ag_county                                             ~ "Non-ag county",
      !is.na(first_detainer_year) & first_detainer_year <= yr ~ "Ag - Treated",
      TRUE                                                    ~ "Ag - Not yet treated"
    ))

  p <- ggplot(map_yr) +
    geom_sf(aes(fill = status), color = NA, linewidth = 0) +
    scale_fill_manual(
      values = c(
        "Non-ag county"        = "#e8e8e8",
        "Ag - Not yet treated" = "#f6c453",
        "Ag - Treated"         = "#1a3a5c"
      ),
      name = NULL
    ) +
    labs(title = paste("Secure Communities Rollout, Farming-Dependent Counties:", yr)) +
    theme_void() +
    theme(
      plot.title    = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      legend.position  = "bottom",
      legend.text      = element_text(size = 11),
      plot.margin      = margin(10, 10, 10, 10)
    )

  out_path <- paste0(
    "/Users/kieran/Documents/GitHub/labor_and_automation/figs/primary/sc_rollout_ag_", yr, ".png"
  )

  ggsave(out_path, plot = p, width = 12, height = 7, dpi = 300)
  message("Saved: sc_rollout_ag_", yr, ".png")
}
