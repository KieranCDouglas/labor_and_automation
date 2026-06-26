####################################################################################################
## USDA NASS QuickStats API — AgCensus expenditures pull
## Docs: https://quickstats.nass.usda.gov/api
####################################################################################################

# install.packages(c("httr2", "jsonlite", "tidyverse"))
library(httr2)
library(jsonlite)
library(tidyverse)

API_KEY <- Sys.getenv("NASS_API_KEY")
BASE_URL <- "https://quickstats.nass.usda.gov/api/api_GET/"

# --- Core request ------------------------------------------------------------

nass_get <- function(params, api_key = API_KEY) {
  if (nchar(api_key) == 0) stop("Set NASS_API_KEY in your .Renviron file.")

  resp <- request(BASE_URL) |>
    req_url_query(!!!c(list(key = api_key, format = "JSON"), params)) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  if (resp_status(resp) != 200)
    stop("API error ", resp_status(resp), ": ", resp_body_string(resp))

  body <- resp_body_json(resp, simplifyVector = TRUE)
  if (!is.null(body$error)) stop("NASS error: ", body$error)

  as_tibble(body$data)
}

# --- Base params for county-level expenditure $ values ----------------------
# unit_desc = "$" and statisticcat_desc = "EXPENSE" filters to dollar rows only,
# dropping the parallel "OPERATIONS WITH EXPENSE" count rows (~half the data).

expenditure_params <- function(year, states = NULL) {
  p <- list(
    source_desc      = "CENSUS",
    sector_desc      = "ECONOMICS",
    group_desc       = "EXPENSES",
    agg_level_desc   = "COUNTY",
    unit_desc        = "$",
    statisticcat_desc = "EXPENSE",
    year             = as.character(year)
  )
  if (!is.null(states)) p$state_alpha <- paste(states, collapse = ",")
  p
}

# --- Per-state pull (API does not reliably accept comma-separated states) ----

ALL_STATES <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
                "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
                "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
                "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
                "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

pull_state_year <- function(state, year, delay = 1) {
  Sys.sleep(delay)
  raw <- nass_get(expenditure_params(year, state))
  if (nrow(raw) == 0) return(NULL)
  raw |>
    select(state_alpha, county_name, county_ansi, state_fips_code,
           year, commodity_desc, short_desc, Value) |>
    rename(value = Value) |>
    mutate(
      year  = as.integer(year),
      value = as.numeric(gsub(",", "", value))
    )
}

pull_year <- function(year, states = ALL_STATES) {
  message("Pulling ", year, " (", length(states), " states)...")
  results <- vector("list", length(states))
  for (i in seq_along(states)) {
    message("  [", i, "/", length(states), "] ", states[i])
    results[[i]] <- tryCatch(
      pull_state_year(states[i], year),
      error = \(e) { message("    ERROR: ", conditionMessage(e)); NULL }
    )
  }
  bind_rows(results)
}

# --- Full pull: all states, 2007 and 2012 ------------------------------------

pull_all_expenditures <- function(years = c(2007, 2012)) {
  map(years, pull_year) |> bind_rows()
}

# --- Crop area harvested params (all crops, county level) --------------------

crops_params <- function(year, states = NULL) {
  p <- list(
    source_desc       = "CENSUS",
    sector_desc       = "CROPS",
    agg_level_desc    = "COUNTY",
    unit_desc         = "ACRES",
    statisticcat_desc = "AREA HARVESTED",
    year              = as.character(year)
  )
  if (!is.null(states)) p$state_alpha <- paste(states, collapse = ",")
  p
}

pull_crops_state_year <- function(state, year, delay = 1) {
  Sys.sleep(delay)
  raw <- nass_get(crops_params(year, state))
  if (nrow(raw) == 0) return(NULL)
  raw |>
    select(state_alpha, county_name, county_ansi, state_fips_code,
           year, sector_desc, group_desc, commodity_desc, short_desc, Value) |>
    rename(value = Value) |>
    mutate(
      year  = as.integer(year),
      value = as.numeric(gsub(",", "", value))
    )
}

pull_crops_year <- function(year, states = ALL_STATES) {
  message("Pulling crops ", year, " (", length(states), " states)...")
  results <- vector("list", length(states))
  for (i in seq_along(states)) {
    message("  [", i, "/", length(states), "] ", states[i])
    results[[i]] <- tryCatch(
      pull_crops_state_year(states[i], year),
      error = \(e) { message("    ERROR: ", conditionMessage(e)); NULL }
    )
  }
  bind_rows(results)
}

pull_all_crops <- function(years = c(2007, 2012)) {
  map(years, pull_crops_year) |> bind_rows()
}

# --- Land use params (cropland, irrigated land, etc.) ------------------------

landuse_params <- function(year, states = NULL) {
  p <- list(
    source_desc       = "CENSUS",
    sector_desc       = "ECONOMICS",
    group_desc        = "FARMS & LAND & ASSETS",
    agg_level_desc    = "COUNTY",
    unit_desc         = "ACRES",
    statisticcat_desc = "AREA",
    year              = as.character(year)
  )
  if (!is.null(states)) p$state_alpha <- paste(states, collapse = ",")
  p
}

pull_landuse_state_year <- function(state, year, delay = 1) {
  Sys.sleep(delay)
  raw <- nass_get(landuse_params(year, state))
  if (nrow(raw) == 0) return(NULL)
  raw |>
    select(state_alpha, county_name, county_ansi, state_fips_code,
           year, short_desc, Value) |>
    rename(value = Value) |>
    mutate(
      year  = as.integer(year),
      value = as.numeric(gsub(",", "", value))
    )
}

pull_landuse_year <- function(year, states = ALL_STATES) {
  message("Pulling land use ", year, " (", length(states), " states)...")
  results <- vector("list", length(states))
  for (i in seq_along(states)) {
    message("  [", i, "/", length(states), "] ", states[i])
    results[[i]] <- tryCatch(
      pull_landuse_state_year(states[i], year),
      error = \(e) { message("    ERROR: ", conditionMessage(e)); NULL }
    )
  }
  bind_rows(results)
}

pull_all_landuse <- function(years = c(2007, 2012)) {
  map(years, pull_landuse_year) |> bind_rows()
}

# --- Run ---------------------------------------------------------------------

expenditures <- pull_all_expenditures()

# Deduplicate: for conflicting rows, prefer non-NA; if both non-NA keep the larger
# (duplicates arise because the API returns both a suppressed NA and a real value)
expenditures <- expenditures |>
  arrange(state_alpha, county_name, year, short_desc, desc(value)) |>
  distinct(state_alpha, county_name, county_ansi, state_fips_code, year, short_desc,
           .keep_all = TRUE)

# Long format — one row per county × year × expense category
write_csv(expenditures, "data/main/expenditures_all_states_long.csv")
message("Saved long format: ", nrow(expenditures), " rows")

# Wide format — one row per county × year, columns = expense categories
exp_wide <- expenditures |>
  pivot_wider(
    id_cols     = c(state_alpha, county_name, county_ansi, state_fips_code, year),
    names_from  = short_desc,
    values_from = value
  )

write_csv(exp_wide, "data/main/expenditures_all_states_wide.csv")
message("Saved wide format: ", nrow(exp_wide), " rows, ", ncol(exp_wide), " columns")

# crops: all area harvested by crop type, county x year
crops <- pull_all_crops()
crops <- crops |>
  arrange(state_alpha, county_name, year, short_desc, desc(value)) |>
  distinct(state_alpha, county_name, county_ansi, state_fips_code, year, short_desc,
           .keep_all = TRUE)
write_csv(crops, "data/main/crops_area_harvested.csv")
message("Saved crops: ", nrow(crops), " rows")

# land use: cropland, irrigated, etc., county x year
landuse <- pull_all_landuse()
landuse <- landuse |>
  arrange(state_alpha, county_name, year, short_desc, desc(value)) |>
  distinct(state_alpha, county_name, county_ansi, state_fips_code, year, short_desc,
           .keep_all = TRUE)
write_csv(landuse, "data/main/landuse.csv")
message("Saved land use: ", nrow(landuse), " rows")
