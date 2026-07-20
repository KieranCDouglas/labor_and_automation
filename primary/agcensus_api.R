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

pull_all_expenditures <- function(years = c(2007, 2012, 2017)) {
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

pull_all_crops <- function(years = c(2007, 2012, 2017)) {
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

pull_all_landuse <- function(years = c(2007, 2012, 2017)) {
  map(years, pull_landuse_year) |> bind_rows()
}

# --- Hired farm labor: workers and payroll (county level) --------------------
# domain_desc = "TOTAL" excludes the "LABOR: (HIRED WORKERS GE 150 DAYS & LT 150 DAYS)" breakdown
# rows that duplicate the same short_desc under domain_desc = "LABOR" - TOTAL gives one row per county.
# two different migrant-labor measures, with different year coverage:
#   - LABOR, MIGRANT - NUMBER OF WORKERS: actual headcount of migrant workers. Only exists for 2012/2017
#     (not tabulated at county level in the 2002/2007 censuses) - pulling it across all years is fine, it
#     will just come back empty pre-2012.
#   - LABOR, MIGRANT - OPERATIONS WITH WORKERS: count of farms reporting migrant labor (an extensive-
#     margin measure, not a headcount). Available at county level for all 4 years (2002/2007/2012/2017) -
#     this is what the published Census of Ag Table 7 "Migrant farm labor on farms with hired labor" row
#     shows.

LABOR_SHORT_DESC <- c(
  "LABOR, HIRED - NUMBER OF WORKERS",
  "LABOR, HIRED - EXPENSE, MEASURED IN $",
  "LABOR, MIGRANT - NUMBER OF WORKERS",
  "LABOR, MIGRANT - OPERATIONS WITH WORKERS"
)

labor_params <- function(year, states = NULL) {
  p <- list(
    source_desc    = "CENSUS",
    commodity_desc = "LABOR",
    agg_level_desc = "COUNTY",
    domain_desc    = "TOTAL",
    year           = as.character(year)
  )
  if (!is.null(states)) p$state_alpha <- paste(states, collapse = ",")
  p
}

pull_labor_state_year <- function(state, year, delay = 1) {
  Sys.sleep(delay)
  raw <- tryCatch(nass_get(labor_params(year, state)), error = \(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  raw |>
    filter(short_desc %in% LABOR_SHORT_DESC) |>
    select(state_alpha, county_name, county_ansi, state_fips_code,
           year, short_desc, Value) |>
    rename(value = Value) |>
    mutate(
      year  = as.integer(year),
      value = as.numeric(gsub(",", "", value))
    )
}

pull_labor_year <- function(year, states = ALL_STATES) {
  message("Pulling labor ", year, " (", length(states), " states)...")
  results <- vector("list", length(states))
  for (i in seq_along(states)) {
    message("  [", i, "/", length(states), "] ", states[i])
    results[[i]] <- tryCatch(
      pull_labor_state_year(states[i], year),
      error = \(e) { message("    ERROR: ", conditionMessage(e)); NULL }
    )
  }
  bind_rows(results)
}

pull_all_labor <- function(years = c(2002, 2007, 2012, 2017)) {
  map(years, pull_labor_year) |> bind_rows()
}

# --- Farms (count) and value of land & buildings (county level) --------------
# complements landuse.csv, which already has land-in-farms/land-use acreage; this adds the two pieces
# landuse.csv doesn't cover. domain_desc = "TOTAL" again excludes the AREA OPERATED/NAICS/TENURE
# breakdown rows so each short_desc collapses to one row per county.

FARMVALUE_SHORT_DESC <- c(
  "FARM OPERATIONS - NUMBER OF OPERATIONS",
  "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $"
)

farmvalue_params <- function(year, states = NULL) {
  p <- list(
    source_desc    = "CENSUS",
    agg_level_desc = "COUNTY",
    domain_desc    = "TOTAL",
    year           = as.character(year)
  )
  if (!is.null(states)) p$state_alpha <- paste(states, collapse = ",")
  p
}

pull_farmvalue_state_year <- function(state, year, delay = 1) {
  Sys.sleep(delay)
  results <- map(FARMVALUE_SHORT_DESC, \(sd) {
    raw <- tryCatch(
      nass_get(c(farmvalue_params(year, state), list(short_desc = sd))),
      error = \(e) NULL
    )
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    raw |>
      select(state_alpha, county_name, county_ansi, state_fips_code,
             year, short_desc, Value) |>
      rename(value = Value) |>
      mutate(
        year  = as.integer(year),
        value = as.numeric(gsub(",", "", value))
      )
  })
  bind_rows(results)
}

pull_farmvalue_year <- function(year, states = ALL_STATES) {
  message("Pulling farm count/value ", year, " (", length(states), " states)...")
  results <- vector("list", length(states))
  for (i in seq_along(states)) {
    message("  [", i, "/", length(states), "] ", states[i])
    results[[i]] <- tryCatch(
      pull_farmvalue_state_year(states[i], year),
      error = \(e) { message("    ERROR: ", conditionMessage(e)); NULL }
    )
  }
  bind_rows(results)
}

pull_all_farmvalue <- function(years = c(2002, 2007, 2012, 2017)) {
  map(years, pull_farmvalue_year) |> bind_rows()
}

# --- Harvested cropland by size of farm (county level) ------------------------
# domain_desc = "AREA OPERATED" breaks AG LAND, CROPLAND, HARVESTED - ACRES into 12 farm-size buckets
# (e.g. "AREA OPERATED: (1.0 TO 9.9 ACRES)" ... "AREA OPERATED: (2,000 OR MORE ACRES)") instead of the
# single county total already in crops_area_harvested.csv/landuse.csv.

harvested_by_size_params <- function(year, states = NULL) {
  p <- list(
    source_desc    = "CENSUS",
    agg_level_desc = "COUNTY",
    short_desc     = "AG LAND, CROPLAND, HARVESTED - ACRES",
    domain_desc    = "AREA OPERATED",
    year           = as.character(year)
  )
  if (!is.null(states)) p$state_alpha <- paste(states, collapse = ",")
  p
}

pull_harvested_by_size_state_year <- function(state, year, delay = 1) {
  Sys.sleep(delay)
  raw <- tryCatch(nass_get(harvested_by_size_params(year, state)), error = \(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  raw |>
    select(state_alpha, county_name, county_ansi, state_fips_code,
           year, short_desc, domaincat_desc, Value) |>
    rename(value = Value) |>
    mutate(
      year  = as.integer(year),
      value = as.numeric(gsub(",", "", value))
    )
}

pull_harvested_by_size_year <- function(year, states = ALL_STATES) {
  message("Pulling harvested cropland by farm size ", year, " (", length(states), " states)...")
  results <- vector("list", length(states))
  for (i in seq_along(states)) {
    message("  [", i, "/", length(states), "] ", states[i])
    results[[i]] <- tryCatch(
      pull_harvested_by_size_state_year(states[i], year),
      error = \(e) { message("    ERROR: ", conditionMessage(e)); NULL }
    )
  }
  bind_rows(results)
}

pull_all_harvested_by_size <- function(years = c(2002, 2007, 2012, 2017)) {
  map(years, pull_harvested_by_size_year) |> bind_rows()
}

# --- Run ---------------------------------------------------------------------
# To pull all data from scratch, uncomment and run the blocks below.
# To load functions only (e.g. for a targeted re-pull), source this file as-is.

# expenditures <- pull_all_expenditures()
# expenditures <- expenditures |>
#   arrange(state_alpha, county_name, year, short_desc, desc(value)) |>
#   distinct(state_alpha, county_name, county_ansi, state_fips_code, year, short_desc,
#            .keep_all = TRUE)
# write_csv(expenditures, "data/main/expenditures_all_states_long.csv")
# message("Saved long format: ", nrow(expenditures), " rows")
# exp_wide <- expenditures |>
#   pivot_wider(
#     id_cols     = c(state_alpha, county_name, county_ansi, state_fips_code, year),
#     names_from  = short_desc,
#     values_from = value
#   )
# write_csv(exp_wide, "data/main/expenditures_all_states_wide.csv")
# message("Saved wide format: ", nrow(exp_wide), " rows, ", ncol(exp_wide), " columns")

# crops <- pull_all_crops()
# crops <- crops |>
#   arrange(state_alpha, county_name, year, short_desc, desc(value)) |>
#   distinct(state_alpha, county_name, county_ansi, state_fips_code, year, short_desc,
#            .keep_all = TRUE)
# write_csv(crops, "data/main/crops_area_harvested.csv")
# message("Saved crops: ", nrow(crops), " rows")

# landuse <- pull_all_landuse()
# landuse <- landuse |>
#   arrange(state_alpha, county_name, year, short_desc, desc(value)) |>
#   distinct(state_alpha, county_name, county_ansi, state_fips_code, year, short_desc,
#            .keep_all = TRUE)
# write_csv(landuse, "data/main/landuse.csv")
# message("Saved land use: ", nrow(landuse), " rows")

# labor <- pull_all_labor()
# labor <- labor |>
#   arrange(state_alpha, county_name, year, short_desc, desc(value)) |>
#   distinct(state_alpha, county_name, county_ansi, state_fips_code, year, short_desc,
#            .keep_all = TRUE)
# write_csv(labor, "data/main/hired_labor.csv")
# message("Saved hired labor: ", nrow(labor), " rows")

# farmvalue <- pull_all_farmvalue()
# farmvalue <- farmvalue |>
#   arrange(state_alpha, county_name, year, short_desc, desc(value)) |>
#   distinct(state_alpha, county_name, county_ansi, state_fips_code, year, short_desc,
#            .keep_all = TRUE)
# write_csv(farmvalue, "data/main/farms_landvalue.csv")
# message("Saved farm count/land value: ", nrow(farmvalue), " rows")

# harvested_by_size <- pull_all_harvested_by_size()
# harvested_by_size <- harvested_by_size |>
#   arrange(state_alpha, county_name, year, domaincat_desc, desc(value)) |>
#   distinct(state_alpha, county_name, county_ansi, state_fips_code, year, domaincat_desc,
#            .keep_all = TRUE)
# write_csv(harvested_by_size, "data/main/harvested_cropland_by_farmsize.csv")
# message("Saved harvested cropland by farm size: ", nrow(harvested_by_size), " rows")
