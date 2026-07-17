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

### define agricultural counties: union of (a) USDA ERS County Typology Codes (2015 edition)
### farming-dependent flag and (b) farmland acreage share >= 20% of county land area as of 2002
### (pre-SC baseline). (a) alone misses acreage-dominant-but-economically-diversified counties like the
### CA Central Valley (Fresno, Kern, Tulare, Merced, Stanislaus, San Joaquin...), where ag's absolute
### footprint is large but is dwarfed in dollar/job terms by other industries - see primary/main.r for
### the full construction and rationale.
landuse <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/landuse.csv")
ag_typology <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/ers_county_typology_2015.csv")

ers_ag_flag <- ag_typology |>
  filter(Farming_2015_Update == 1) |>
  transmute(
    state  = State,
    county = str_remove(County_name, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county)
  )

# farmland_share NA (not 0) when AG LAND - ACRES is unreported/suppressed for a county in 2002, so
# disclosure-suppressed counties aren't misclassified as having no farmland.
farmland_share_2002 <- landuse |>
  filter(year == 2002) |>
  mutate(
    county = str_to_title(str_remove(county_name, " County$| Parish$| Borough$| Census Area$| city$")),
    state  = state_alpha
  ) |>
  group_by(state, county) |>
  summarise(
    total_ag_acres  = if (all(is.na(value[short_desc == "AG LAND - ACRES"]))) NA_real_
                       else sum(value[short_desc == "AG LAND - ACRES"], na.rm = TRUE),
    land_area_acres = if (all(is.na(value[short_desc == "LAND AREA, INCL NON-AG - ACRES"]))) NA_real_
                       else sum(value[short_desc == "LAND AREA, INCL NON-AG - ACRES"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(farmland_share_2002 = total_ag_acres / land_area_acres) |>
  select(state, county, farmland_share_2002)

acreage_ag_flag <- farmland_share_2002 |>
  filter(farmland_share_2002 >= 0.20) |>
  select(state, county)

ag_counties <- bind_rows(ers_ag_flag, acreage_ag_flag) |>
  distinct(state, county) |>
  mutate(ag_county = TRUE)

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
