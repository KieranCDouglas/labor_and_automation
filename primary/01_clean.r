####################################################################################################
## 01: data cleaning
## last edited: 10/22/2026
## by kieran
## does: loads raw data, cleans it, and builds every analysis dataset used downstream.
## reads:  dropbox kieran/data/raw/*, plus the TRAC, IPUMS, QCEW, dorn crosswalk and
##         generated_data files elsewhere in the dropbox
## writes: dropbox kieran/data/clean/*.rds (read by 02_models.r and 03_figs.r)
####################################################################################################

####################################################################################################
### prelude ###
####################################################################################################

pkgs <- c("tidyverse", "tidycensus", "haven")
install.packages(setdiff(pkgs, rownames(installed.packages())))

library(tidyverse)
library(tidycensus)
library(haven)

source("/Users/kieran/Documents/GitHub/labor_and_automation/primary/00_setup.r")

## exposure heterogeneity thresholds ##-------------------------------------------------------------
# cutoffs for the binary het_<m> indicators built in section 10.
het_config <- tribble(
  ~measure,    ~cut,    ~high_if, ~label,
  "noncit",    0.015,   ">=",     "Noncitizen Share",
  "undoc",     0.0091,  ">=",     "Undocumented Share (Proxy)",
  "early",     2011,    "<=",     "Early Activation",
  "jail_cap",  2.735,   ">=",     "Jail Capacity",
  "jail_flow", 12.565,  ">=",     "Jail Admissions"
)

# commuting-zone versions of the same splits (section 14). CZ's count as early if at least this share of its population activated by 2011.
het_cz_cuts <- c(noncit = NA, undoc = NA, jail_cap = NA, jail_flow = NA)
CZ_EARLY_SHARE <- 0.5

## county identifiers ##----------------------------------------------------------------------------
# one FIPS per (state, cleaned name). a handful of states have an independent city and a county that
# share a name once the suffix is removed (e.g. VA Fairfax); the county is preferred, matching how the
# ag census names counties.
fips_lookup <- fips_codes |>
  mutate(
    county_clean = clean_county(county),
    is_city      = str_detect(county, " city$")
  ) |>
  arrange(is_city) |>
  distinct(state, county_clean, .keep_all = TRUE)

name_to_fips <- fips_lookup |>
  transmute(state, county_key = county_key(county_clean), county_fips = paste0(state_code, county_code)) |>
  distinct(state, county_key, .keep_all = TRUE)

####################################################################################################
### load data ###
####################################################################################################

expenditures <- read.csv(file.path(RAW_DIR, "expenditures_all_states_wide.csv"))
sc_trac <- read.csv(file.path(RAW_DIR, "secure1904.csv"))
sc_ice <- read.csv(file.path(RAW_DIR, "sc_activation_dates.csv"))
hired_labor <- read.csv(file.path(RAW_DIR, "hired_labor.csv"))
farms_landvalue <- read.csv(file.path(RAW_DIR, "farms_landvalue.csv"))
jail_facilities_2006 <- read_dta(file.path(RAW_DIR, "jail_facilities_2006_icpsr26602.dta"))
zcta_county_crosswalk <- read.csv(file.path(RAW_DIR, "zcta_county_rel_10.csv"))

vera_county <- read_csv(
  file.path(RAW_DIR, "vera_incarceration_trends_county.csv"),
  col_select = c(year, county_fips, total_jail_pop, total_jail_admits, is_regional_jail),
  show_col_types = FALSE, progress = FALSE
)

####################################################################################################
### data cleaning ###
####################################################################################################

## 1. census data ##--------------------------------------------------------------------------------
# variables are selected/named plus mutations convert to workable format. mech_share_broad is the
# mechanization proxy: machinery-related expenses (fuel, repairs, utilities, machinery rental,
# depreciation) as a share of total operating expenses.
expenditures_clean <- expenditures |> 
  rename(
    state = state_alpha,
    county = county_name,
    customwork_exp = AG.SERVICES..CUSTOMWORK...EXPENSE..MEASURED.IN..,
    machinery_rent_exp = AG.SERVICES..MACHINERY.RENTAL...EXPENSE..MEASURED.IN..,
    other_services_exp = AG.SERVICES..OTHER...EXPENSE..MEASURED.IN..,
    utilities_exp = AG.SERVICES..UTILITIES...EXPENSE..MEASURED.IN..,
    depreciation_exp = DEPRECIATION...EXPENSE..MEASURED.IN..,
    chemical_total_exp = CHEMICAL.TOTALS...EXPENSE..MEASURED.IN..,
    fuel_total_exp = FUELS..INCL.LUBRICANTS...EXPENSE..MEASURED.IN..,
    labor_contract_exp = LABOR..CONTRACT...EXPENSE..MEASURED.IN..,
    labor_hired_exp = LABOR..HIRED...EXPENSE..MEASURED.IN..,
    repairs_exp = SUPPLIES...REPAIRS...EXCL.LUBRICANTS....EXPENSE..MEASURED.IN..,
    spacerent_total_exp = RENT..CASH..LAND...BUILDINGS...EXPENSE..MEASURED.IN..,
    total_exp = EXPENSE.TOTALS..OPERATING...EXPENSE..MEASURED.IN..
  ) |> 
  select(
    state, county, year, total_exp, customwork_exp, machinery_rent_exp, other_services_exp, utilities_exp, chemical_total_exp, depreciation_exp, fuel_total_exp, labor_contract_exp, labor_hired_exp, spacerent_total_exp, repairs_exp
  ) |> 
  mutate(
    county = str_to_title(county),
    mech_share_broad = (fuel_total_exp+repairs_exp+utilities_exp+machinery_rent_exp+depreciation_exp)/total_exp,
    # hired + contract labor as a share of operating expenses, the labor counterpart of mech_share_broad.
    # contract labor is suppressed in ~5% of county-years (where it is small) and counts as 0 there.
    labor_share = (labor_hired_exp + coalesce(as.numeric(labor_contract_exp), 0)) / total_exp,
    customwork_exp = as.numeric(customwork_exp),
    machinery_rent_exp = as.numeric(machinery_rent_exp),
    other_services_exp = as.numeric(other_services_exp),
    utilities_exp = as.numeric(utilities_exp),
    chemical_total_exp = as.numeric(chemical_total_exp),
    depreciation_exp = as.numeric(depreciation_exp),
    fuel_total_exp = as.numeric(fuel_total_exp),
    labor_contract_exp = as.numeric(labor_contract_exp),
    labor_hired_exp = as.numeric(labor_hired_exp),
    spacerent_total_exp = as.numeric(spacerent_total_exp),
    repairs_exp = as.numeric(repairs_exp)
    )

## 2. secure communities data from TRAC ##----------------------------------------------------------
# exposure-intensity measures generated, plus dating consistency and formatting 
sc_trac_clean <- sc_trac |>
  rename(
    sex = gender,
    age_rm = age_at_removal,
    entry_date = centry_date,
    detainer_date = detainer_prepare_date,
    deportation_type = current_deportation_type,
    apprehension_method = latest_apprehension_method
  ) |> 
  select(
    mscc_code, final_charge_section, county, citizenship_country, entry_status, case_category, apprehension_method, 
    removal_current_program, state, detainer_facility_state, detainer_facility_city, sex, age_rm, entry_date, 
    processing_disposition_code, final_charge_code, prior_removal, departed_date, detainer_date, deportation_type
  ) |>
  mutate(
    entry_date    = as.Date(entry_date,    format = "%m/%d/%Y"),
    departed_date = as.Date(departed_date, format = "%d%b%y"),
    detainer_date = as.Date(detainer_date, format = "%d%b%y"),
    age_rm = as.numeric(age_rm),
    sex = as.factor(sex),
    deportation_type = as.factor(deportation_type),
    entry_status = as.factor(entry_status),
    case_category = as.factor(case_category),
    apprehension_method = as.factor(apprehension_method),
    prior_removal = as.integer(prior_removal == "YES"),
    year   = year(coalesce(detainer_date, departed_date)),
    county = str_to_title(str_remove(county, " Borough$")),
    county = str_replace(county, "^Saint ", "St. "),
    county = str_replace(county, "^Sainte ", "Ste. "),
    county = if_else(county %in% c("Carson City", "Charles City", "James City"),
      county, str_remove(county, " City$")),
    community_channel = !apprehension_method %in% c(
      "CAP Federal Incarceration", "CAP State Incarceration", "Criminal Alien Program",
      "Patrol Border", "Patrol Interior", "Boat Patrol", "Anti-Smuggling",
      "Inspections", "Traffic Check", "Transportation Check Bus",
      "Transportation Check Freight Train", "Transportation Check Aircraft",
      "Transportation Check Passenger Train")
  )

## 3. population data ##----------------------------------------------------------------------------
# 2010 county population (denominator for the SC removal rates and jail measures), and baseline
# immigrant population shares for the exposure heterogeneity measures
county_pop <- get_decennial(
  geography = "county",
  variables = "P001001",
  year      = 2010
) |>
  separate(NAME, into = c("county", "state_name"), sep = ", ") |>
  mutate(
    county = clean_county(county),
    state  = state.abb[match(state_name, state.name)]
  ) |>
  select(state, county, population = value) |>
  group_by(state, county) |>
  summarise(population = sum(population), .groups = "drop")

# baseline foreign-born and noncitizen population shares from the 2005-2009 pooled ACS.
county_foreign_born <- get_acs(
  geography = "county",
  variables = c(
    total_pop    = "B05002_001",  
    foreign_born = "B05002_013",
    noncitizen   = "B05001_006",
    mexca_born   = "B05006_137"   
  ),
  year      = 2009,
  survey    = "acs5"
) |>
  separate(NAME, into = c("county", "state_name"), sep = ", ") |>
  mutate(
    county = clean_county(county),
    state  = state.abb[match(state_name, state.name)]
  ) |>
  select(state, county, variable, estimate) |>
  group_by(state, county, variable) |>
  summarise(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  mutate(
    foreign_born_share_2009 = foreign_born / total_pop,
    noncitizen_share_2009   = noncitizen / total_pop,
    foreign_born_count_2009 = foreign_born,
    noncitizen_count_2009 = noncitizen,
    mexca_share_2009      = mexca_born / total_pop
  ) |>
  select(state, county, foreign_born_share_2009, noncitizen_share_2009, foreign_born_count_2009, noncitizen_count_2009,
         mexca_share_2009)

# the same 2010 populations keyed by FIPS, for everything aggregated to commuting zones
county_pop_fips <- get_decennial(geography = "county", variables = "P001001", year = 2010) |>
  transmute(county_fips = GEOID, pop = value)

## 4. farm counts ##-------------------------------------------------------------------------------
# number of farm operations by county FIPS, the denominator for hired workers per farm. pulled as
# county totals (domain TOTAL); national sums match the published census (2.13m / 2.20m / 2.11m /
# 2.04m farms, 2002-2017). (per-acre labor measures were dropped: harvested cropland is tiny in
# urban places and the ratios explode there. the landuse.csv cropland item was also wrong in 2002 and
# 2012; see agcensus_api.R.)
farm_counts <- farms_landvalue |>
  filter(short_desc == "FARM OPERATIONS - NUMBER OF OPERATIONS", !is.na(county_ansi)) |>
  transmute(county_fips = sprintf("%02d%03d", as.integer(state_fips_code), as.integer(county_ansi)),
            year = as.integer(year), total_farms = value)
stopifnot(!anyDuplicated(farm_counts[c("county_fips", "year")]))

## 5. secure communities activation dates ##--------------------------------------------------------
# official county-level SC activation schedule from ICE
sc_activation_clean <- sc_ice |>
  mutate(
    county = clean_county(county),
    activation_date = as.Date(activation_date),
    first_detainer_year = year(activation_date)
  )

# one activation date per county FIPS, with the continuous treatment:
#   earliness_norm  (max activation date - date_j) / (max - min), day resolution, min/max over all
#                   activation dates, so 1 = earliest and 0 = latest activator
#   earliness_rank  1 - percentile rank of the activation date
#   exposure_years  years between the county's activation and the last activation (22 Jan 2013): the
#                   extra years of SC exposure an earlier county had. the same variable as
#                   earliness_norm in years (earliness_norm x 4.24), the treatment in the outcome models
# ICE names are matched to census county names with the independent-city suffix kept, so a city and
# the county sharing its name (VA Bedford, MD Baltimore, MO St. Louis) each keep their own date. a few
# places are listed under other names: New York City covers the five boroughs, the Alaska areas use
# pre-2008 boundaries (mapped to the boroughs carved from them), Honolulu HI is listed as Oahu, and
# Ste. Genevieve MO is spelled Saint Genevieve. counties ICE does not list (e.g. Oglala Lakota SD)
# have no date; the unmatched ICE rows are all territories.
act_key <- \(x) county_key(str_remove(x, " County$| Parish$| Borough$| Census Area$| Municipality$| City and Borough$"))

ice_aliases <- tribble(
  ~state, ~ice_county,                                   ~alias_fips,
  "NY",   "New York City",                               list(c("36005", "36047", "36061", "36081", "36085")),
  "AK",   "Prince of Wales-Outer Ketchikan Census Area", list("02198"),
  "AK",   "Skagway-Hoonah-Angoon Census Area",           list(c("02105", "02230")),
  "AK",   "Wrangell-Petersburg Census Area",             list(c("02195", "02275")),
  "MO",   "Saint Genevieve",                             list("29186"),
  "HI",   "Oahu",                                        list("15003")
) |>
  unnest(alias_fips) |>
  unnest(alias_fips)

fips_by_key <- fips_codes |>
  transmute(state, key = act_key(county), name_fips = paste0(state_code, county_code))
stopifnot(!anyDuplicated(fips_by_key[c("state", "key")]))

activation_matched <- sc_ice |>
  transmute(state, ice_county = county, key = act_key(county), activation_date = as.Date(activation_date)) |>
  left_join(ice_aliases, by = c("state", "ice_county"), relationship = "one-to-many") |>
  left_join(fips_by_key, by = c("state", "key")) |>
  mutate(county_fips = coalesce(alias_fips, name_fips))

message(sprintf("SC activation: %d of %d ICE rows matched to a county FIPS; unmatched: %s",
                sum(!is.na(activation_matched$county_fips)), nrow(activation_matched),
                paste(with(filter(activation_matched, is.na(county_fips)), paste(state, ice_county)), collapse = "; ")))

activation_fips <- activation_matched |>
  filter(!is.na(county_fips)) |>
  group_by(county_fips) |>
  summarise(activation_date = min(activation_date), .groups = "drop") |>
  mutate(earliness_norm = as.numeric(max(activation_date) - activation_date) /
                          as.numeric(max(activation_date) - min(activation_date)),
         earliness_rank = 1 - percent_rank(activation_date),
         exposure_years = as.numeric(max(activation_date) - activation_date) / 365.25)

## 6. analysis sample ##---------------------------------------------------------------------------
# no agricultural-county restriction: every county in the Census of Ag enters main, and every CZ that
# contains one enters the CZ panels. outcomes are shares (of working-age population, cropland or farm
# expenses), so places with little farming add small, noisy changes rather than bias, and how much a
# place relied on immigrant farm labor is reported as heterogeneity instead (section 19).
# (an earlier version kept ERS farming-dependent counties plus counties with >= 1% of land in farms in
# 2002; the farmland figures it used were understated ~4x, see agcensus_api.R.)

## 7. outcome varaibles ##---------------------------------------------------------------------------
# cleans hired farm labor data, giving each concept its own column per county-year.
# safe_sum -> NA (not a false 0) when every value in the group is missing.
safe_sum <- function(value, cond) {
  if (all(is.na(value[cond]))) NA_real_ else sum(value[cond], na.rm = TRUE)
}

hired_labor_controls <- hired_labor |>
  mutate(
    county = clean_county(county_name),
    state  = state_alpha
  ) |>
  group_by(state, county, year) |>
  summarise(
    hired_workers   = safe_sum(value, short_desc == "LABOR, HIRED - NUMBER OF WORKERS"),
    hired_labor_exp = safe_sum(value, short_desc == "LABOR, HIRED - EXPENSE, MEASURED IN $"),
    .groups = "drop"
  )

## 8. develop exposure vulnerability proxy ##---------------------------------------------------------
# counties with identical migrant share of working population may have heterogeneity in realized SC-related
# deportations. this is likely a function of local jail processing, enforcement strength, preexisting agreements etc.
# we want data that will act as both an exposure heterogeneity proxy and as a test for
# the extent to which those underlying baseline characteristics predict deportation intensity ex post.

# local jail capacity baseline, from BJS's Census of Jail Facilities, 2006 (ICPSR 26602) - the last full
# census of US jail jurisdictions before SC's 2008 rollout.
zcta_county_lookup <- zcta_county_crosswalk |>
  mutate(
    zip         = sprintf("%05d", ZCTA5),
    state_code  = sprintf("%02d", STATE),
    county_code = sprintf("%03d", COUNTY)
  ) |>
  group_by(zip) |>
  slice_max(POPPT, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(zip, state_code, county_code) |>
  left_join(fips_codes, by = c("state_code", "county_code")) |>
  mutate(county = clean_county(county)) |>
  select(zip, state, county)

# rated capacity is repeated per facility slot (up to 13 facilities under one reporting agency); -1 is
# BJS's documented missing code, not a real capacity of -1. safe_sum (defined above) -> NA when an
# agency reports no valid capacity at all, rather than a false 0.
capacity_vars <- c("V142","V168","V194","V220","V246","V272","V298","V324","V350","V376","V402","V428","V454")

jail_facility_capacity <- jail_facilities_2006 |>
  mutate(across(all_of(capacity_vars), ~na_if(.x, -1))) |>
  rowwise() |>
  mutate(rated_capacity_total = safe_sum(c_across(all_of(capacity_vars)), TRUE)) |>
  ungroup() |>
  transmute(agency_id = V1, agency_name = V2, state = V7, zip = V8, rated_capacity_total,
            persons_confined = na_if(V18, 0))

# ~1/3 of agencies report no rated capacity for any facility, but nearly all report persons confined
# (V18, agency level). the two track closely where both exist, so missing rated
# capacity is imputed as persons confined / the median confined-to-rated ratio, and flagged with a binary indicator for imputation. (CONFIRM APPROACH)
confined_to_rated <- with(jail_facility_capacity,
  median(persons_confined / rated_capacity_total, na.rm = TRUE))

jail_facility_capacity <- jail_facility_capacity |>
  mutate(
    capacity_imputed     = as.integer(is.na(rated_capacity_total) & !is.na(persons_confined)),
    rated_capacity_total = coalesce(rated_capacity_total, persons_confined / confined_to_rated)
  )

# primary match: zip -> county via ZCTA
jail_capacity_matched <- jail_facility_capacity |>
  left_join(zcta_county_lookup, by = c("zip", "state"))

# fallback: ~5% of agencies use a unique administrative zip with no residential population, so they never appear in
# the ZCTA crosswalk. 
jail_capacity_fallback <- jail_capacity_matched |>
  filter(is.na(county)) |>
  mutate(county_guess = str_to_title(str_trim(str_extract(agency_name, "^.*?(?=\\s+COUNTY|\\s+PARISH)")))) |>
  filter(!is.na(county_guess)) |>
  left_join(
    fips_codes |> mutate(county = clean_county(county)),
    by = c("state", "county_guess" = "county")
  ) |>
  filter(!is.na(county_code)) |>
  select(agency_id, county_from_name = county_guess)

jail_capacity_matched <- jail_capacity_matched |>
  left_join(jail_capacity_fallback, by = "agency_id") |>
  mutate(county = coalesce(county, county_from_name))

message(sprintf(
  "jail capacity: %d/%d agencies matched to a county (%d by zip, %d by name fallback), %d dropped (mostly small city jails/federal facilities without a county-identifiable zip or name)",
  sum(!is.na(jail_capacity_matched$county)), nrow(jail_capacity_matched),
  sum(!is.na(jail_capacity_matched$county)) - nrow(jail_capacity_fallback),
  nrow(jail_capacity_fallback),
  sum(is.na(jail_capacity_matched$county))
))

# aggregate to county, summing capacity across all reporting agencies within the same county (e.g. a
# county sheriff's jail plus a separate city lockup). NA means every matched agency in that county had
# missing capacity data, not that capacity is zero. jail_capacity_imputed = 1 if any agency in the
# county had its capacity imputed from persons confined.
# counties with no matched agency at all stay NA.
jail_capacity_controls <- jail_capacity_matched |>
  filter(!is.na(county)) |>
  group_by(state, county) |>
  summarise(
    jail_capacity_2006    = safe_sum(rated_capacity_total, TRUE),
    jail_capacity_imputed = max(capacity_imputed),
    .groups = "drop"
  )

message(sprintf(
  "jail capacity: %d counties, %d with any imputed agency (confined-to-rated ratio %.3f)",
  nrow(jail_capacity_controls), sum(jail_capacity_controls$jail_capacity_imputed), confined_to_rated
))

# baseline jail booking (flow) variable comes from, from Vera's Incarceration Trends county panel.
# jail_capacity_2006 above measures authorized beds (stock) but SC screening is a flow. everyone
# booked gets fingerprinted and run against the federal database regardless of how many beds exist.
# builds jail capacity flow variable averaged over 2004-2007.

jail_flow_0407 <- vera_county |>
  filter(year >= 2004, year <= 2007) |>
  group_by(county_fips) |>
  summarise(jail_admits_0407 = mean(total_jail_admits, na.rm = TRUE), .groups = "drop") |>
  mutate(
    jail_admits_0407 = ifelse(is.nan(jail_admits_0407), NA, jail_admits_0407),
    county_fips      = sprintf("%05d", as.integer(county_fips))
  )

# county-level flow measure (state, county). the high/low split is built in section 10
# (het_jail_flow) alongside the other exposure heterogeneity measures.
admits_split <- county_pop |>
  distinct(state, county, population) |>
  left_join(
    fips_lookup |> transmute(state, county = county_clean,
                             county_fips = paste0(state_code, county_code)),
    by = c("state", "county")
  ) |>
  left_join(jail_flow_0407, by = "county_fips") |>
  mutate(admits_per_1k = jail_admits_0407 / population * 1000) |>
  filter(!is.na(admits_per_1k), admits_per_1k > 0) |>
  select(state, county, admits_per_1k)

message(sprintf(
  "jail admissions: %d counties, median %.1f admits per 1k residents",
  nrow(admits_split), median(admits_split$admits_per_1k, na.rm = TRUE)
))


## 9. merge for main analysis df ##-------------------------------------------------------------------
# one row per (state, county). a few names map to two activation rows (a county and a same-named
# independent city, e.g. MD Baltimore); the earlier date is kept, as in het_early (section 10).
sc_county <- sc_activation_clean |>
  group_by(state, county) |>
  summarise(first_detainer_year = min(first_detainer_year), .groups = "drop") |>
  mutate(early_activator = as.integer(first_detainer_year <= 2011))

# create main df by merging on state and county, filtered to agricultural counties.
# year filter keeps all four ag census years (2002/2007/2012/2017); 2002 predates SC entirely so it's a
# second pre-treatment point for checking parallel trends against 2007.
main <- expenditures_clean |>
  left_join(sc_county, by = c("state", "county")) |>
  filter(year %in% c(2002, 2007, 2012, 2017), !is.na(early_activator)) |>
  # unique county identifier: county names repeat across states
  mutate(county_id = paste(state, county, sep = ", "),
         county_key = county_key(county)) |>
  left_join(name_to_fips,           by = c("state", "county_key")) |>
  select(-county_key) |>
  left_join(farm_counts,            by = c("county_fips", "year")) |>
  left_join(hired_labor_controls,   by = c("state", "county", "year")) |>
  left_join(county_foreign_born,    by = c("state", "county")) |>
  left_join(jail_capacity_controls, by = c("state", "county")) |>
  # admits_per_1k: baseline jail booking flow (binned as het_jail_flow in section 10).
  left_join(admits_split,           by = c("state", "county")) |>
  # hired workers per farm, the labor-intensity measure alongside the expense shares in section 1
  mutate(hired_workers_per_farm = hired_workers / na_if(total_farms, 0))
stopifnot(!anyDuplicated(main[c("county_id", "year")]))

## 10. exposure heterogeneity measures ##-----------------------------------------------------------
# collects baseline county characteristics that plausibly shift how hard SC hit a county. every measure shares
# one schema so they can be swapped into the same regression:
#   het_<m>_val  double   continuous pre-period value
#   het_<m>      integer  1 = high exposure side, 0 = low, NA when the value is missing
# measures:
#   noncit     noncitizen share of population, ACS 2005-09
#   undoc      PROXY: Mexico/Central America-born share of population, ACS 2005-09. 
#   jail_cap   rated jail capacity per 1k residents, BJS 2006 (stock)
#   jail_flow  jail admissions per 1k residents, Vera 2004-07 average (flow)
# thresholds are set in het_config in the prelude at the top of this script.

with_key <- \(df) df |> mutate(county_key = county_key(county)) |> select(-county)

# one row per county, continuous values only. universe is every county so the table can also be
# joined onto the SC removal-rate panels, not just main.
het_values <- county_pop |>
  mutate(county_key = county_key(county)) |>
  left_join(with_key(county_foreign_born), by = c("state", "county_key")) |>
  left_join(
    # min() because a few county names map to two activation rows (county + independent city)
    sc_county |> with_key() |> group_by(state, county_key) |>
      summarise(first_detainer_year = min(first_detainer_year), .groups = "drop"),
    by = c("state", "county_key")
  ) |>
  left_join(with_key(jail_capacity_controls), by = c("state", "county_key")) |>
  left_join(with_key(admits_split),           by = c("state", "county_key")) |>
  transmute(
    state, county, county_key,
    het_noncit_val    = noncitizen_share_2009,
    het_undoc_val     = mexca_share_2009,
    het_early_val     = as.numeric(first_detainer_year),
    het_jail_cap_val  = jail_capacity_2006 / population * 1000,
    het_jail_flow_val = admits_per_1k
  )
stopifnot(!anyDuplicated(het_values[c("state", "county_key")]))

# resolve NA cuts to the analysis-sample median
het_sample <- het_values |>
  semi_join(main |> distinct(state, county) |> mutate(county_key = county_key(county)), by = c("state", "county_key"))
het_config <- het_config |>
  mutate(cut = map2_dbl(measure, cut, \(m, c)
    if (is.na(c)) median(het_sample[[paste0("het_", m, "_val")]], na.rm = TRUE) else c))

het_measures <- het_values
for (i in seq_len(nrow(het_config))) {
  m   <- het_config$measure[i]
  val <- het_measures[[paste0("het_", m, "_val")]]
  het_measures[[paste0("het_", m)]] <- as.integer(
    if (het_config$high_if[i] == ">=") val >= het_config$cut[i] else val <= het_config$cut[i]
  )
}

main <- main |>
  mutate(county_key = county_key(county)) |>
  left_join(het_measures |> select(-county), by = c("state", "county_key")) |>
  select(-county_key)

# early_activator (matched on exact names) and het_early (matched on county_key, which the other
# panels use as their treatment) must agree
stopifnot(all(main$early_activator == main$het_early, na.rm = TRUE))

# balance check in the analysis sample: cuts used, then analysis-sample counties in each
# early_activator x het cell (the cells that identify the DDD term). tweak het_config to rebalance.
print(het_config)
main |>
  select(state, county, early_activator, all_of(paste0("het_", het_config$measure))) |>
  distinct() |>
  pivot_longer(starts_with("het_"), names_to = "het", values_to = "high") |>
  count(het, early_activator, high) |>
  pivot_wider(names_from = c(early_activator, high), values_from = n, names_glue = "early{early_activator}_high{high}") |>
  print()

## 11. SC removals ##-------------------------------------------------------------------------------
# community-channel SC removals (section 2) by county, for 02_vulnerability.r, which tests which
# baseline measure best predicts how hard SC hit a place. every county with an activation date, keyed
# by FIPS so they can be summed to CZs:
#   removals_county_year  removals per county and calendar year, 2008-2017 (0 where none)
#   removals_county       removals in the 12 and 24 months after the county's activation date
# a removal is dated by its detainer date, or its departure date where there is no detainer date (the
# same date that defines `year` in section 2). TRAC county names map to FIPS through county_key.
# SC was suspended in November 2014 (replaced by the Priority Enforcement Program), so the 24-month
# window of the last (January 2013) activators runs about two months past it.
sc_removals <- sc_trac_clean |>
  filter(community_channel) |>
  transmute(state, county_key = county_key(county), date = coalesce(detainer_date, departed_date)) |>
  filter(!is.na(date)) |>
  left_join(name_to_fips, by = c("state", "county_key"))

message(sprintf("SC removals: %s community-channel removals with a date, %.2f%% not matched to a county FIPS",
                format(nrow(sc_removals), big.mark = ","), 100 * mean(is.na(sc_removals$county_fips))))
sc_removals <- filter(sc_removals, !is.na(county_fips))

removals_county_year <- county_pop_fips |>
  cross_join(tibble(year = 2008:2017)) |>
  left_join(sc_removals |> count(county_fips, year = year(date), name = "removals"), by = c("county_fips", "year")) |>
  mutate(removals = replace_na(removals, 0L))

removals_county <- activation_fips |>
  select(county_fips, activation_date) |>
  inner_join(county_pop_fips, by = "county_fips") |>
  left_join(sc_removals |> select(county_fips, date), by = "county_fips", relationship = "one-to-many") |>
  group_by(county_fips, activation_date, pop) |>
  summarise(removals_12m = sum(date >= activation_date & date < activation_date + 365, na.rm = TRUE),
            removals_24m = sum(date >= activation_date & date < activation_date + 730, na.rm = TRUE),
            .groups = "drop")
stopifnot(!anyDuplicated(removals_county$county_fips))

## 12. ACS ag workforce by nativity (IPUMS) ##------------------------------------------------------
# yearly (2005-2022) county counts of employed working-age (16-64) ag workers, total and by nativity,
# from the IPUMS USA ACS 1-year extract. ag = IND1990 010 crops, 011 livestock, 030 ag services n.e.c.
# (excludes 012 veterinary, 020 landscaping, 031 forestry, 032 fishing). foreign-born = CITIZEN 2-5
# (naturalised, noncitizen, not reported); US-born = CITIZEN 0-1 (born in US/territories or abroad to
# US parents). noncitizen (CITIZEN 3-5) is kept as the closest available undocumented proxy.
# class of worker (CLASSWKRD) splits ag workers into wage/salary (20-28), the closest match to QCEW's
# UI-covered employment, and non-wage (10-14 self-employed, 29 unpaid family), which QCEW never sees.
# wage/salary ag workers living in non-institutional group quarters (GQ 4, incl. workers' dormitories)
# proxy for guestworkers (H-2A), who are mostly outside UI coverage. much H-2A housing is ordinary
# housing units, so this is a partial proxy. the 2005 ACS sampled households only (group quarters
# were added in 2006), so the group-quarters counts are missing in 2005 and the other counts exclude
# group-quarters residents that year. _hh counts are restricted to households (GQ 1, 2, 5) in every
# year, a series defined the same way in 2005 as after it.
#
# the microdata only identify PUMAs (100k+ residents), so each PUMA's totals are allocated to its
# counties by population share. rural counties sharing a PUMA therefore get the same ag share of
# population - this measure cannot distinguish a farm county from a town county within one PUMA.
# 2005-2011 samples use 2000 PUMAs, 2012-2021 use 2010 PUMAs and 2022 uses 2020 PUMAs; each gets its
# own crosswalk (puma_vintage() below).
# 2020 ACS was collected under COVID with experimental weights - treat with caution.
IPUMS_FILE   <- file.path(DROPBOX, "ipums_usa", "acs_2005_2022_sc_agriculture.dta")
AG_IND1990   <- c(10, 11, 30)
ACS_YEARS    <- 2005:2022
ACS_BASELINE <- 2006:2007
ACS_COUNTS   <- c("ag_total", "ag_fb", "ag_usb", "ag_noncit",
                  "ag_ws", "ag_nonws", "ag_usb_ws", "ag_usb_nonws", "ag_fb_ws", "ag_ws_gq", "ag_fb_ws_gq",
                  "ag_total_hh", "ag_fb_hh", "ag_usb_hh", "ag_noncit_hh")

# PUMA -> county crosswalks, built once and cached (downloads ~50 census files + 2010 tract populations)
xwalk_file <- file.path(RAW_DIR, "puma_county_crosswalks.rds")
if (!file.exists(xwalk_file)) {
  # 2000 PUMAs: Census 2000 PUMEQ5 equivalency files, summary level 781 = PUMA x county with 2000 pop.
  # file names vary by state (PUMEQ5-CA.TXT vs PUMEQ5-55.TXT), so each state folder's listing is read.
  pumeq_dir <- file.path(RAW_DIR, "puma2000_equivalency")
  dir.create(pumeq_dir, showWarnings = FALSE)
  for (d in str_replace_all(c(state.name, "District of Columbia"), " ", "_")) {
    dest <- file.path(pumeq_dir, paste0(d, ".TXT"))
    if (file.exists(dest) && file.size(dest) > 0) next
    url <- paste0("https://www2.census.gov/census_2000/datasets/PUMS/FivePercent/", d, "/")
    Sys.sleep(2)  # census server rate-limits rapid requests
    fname <- str_extract(paste(read_lines(url), collapse = " "), "PUMEQ5-[A-Z0-9]+\\.TXT")
    download.file(paste0(url, fname), dest, quiet = TRUE)
  }
  puma2000_county <- list.files(pumeq_dir, full.names = TRUE) |>
    map(read_lines) |>
    unlist() |>
    str_match("^\\s*781 (\\d{2}) \\d{5} (\\d{5}) (\\d{3}) .*?\\s(\\d+) [A-Za-z].*$") |>
    as_tibble(.name_repair = \(x) c("line", "statefip", "puma", "countyfp", "pop")) |>
    filter(!is.na(line)) |>
    transmute(statefip, puma, county_fips = paste0(statefip, countyfp), pop = as.numeric(pop))

  # 2010 PUMAs: Census tract-to-PUMA relationship file, weighted by 2010 tract population
  tract_puma <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt",
                         col_types = cols(.default = "c"))
  tract_pop <- map_dfr(c(state.abb, "DC"), \(s)
    get_decennial("tract", variables = "P001001", year = 2010, state = s) |> select(GEOID, pop = value))
  puma2010_county <- tract_puma |>
    filter(STATEFP != "72") |>
    mutate(GEOID = paste0(STATEFP, COUNTYFP, TRACTCE)) |>
    left_join(tract_pop, by = "GEOID") |>
    transmute(statefip = STATEFP, puma = PUMA5CE, county_fips = paste0(STATEFP, COUNTYFP), pop)

  saveRDS(list(puma2000 = puma2000_county, puma2010 = puma2010_county), xwalk_file)
}
puma_xwalks <- readRDS(xwalk_file)

# 2020 PUMAs: Census tract-to-PUMA relationship file, weighted by 2020 tract population (redistricting
# file). cached separately so the 2000/2010 build above is not repeated. counties created since 2010
# are mapped back to the county they came from, so the panel keeps the 2010 county set: AK
# Chugach (02063) and Copper River (02066) -> Valdez-Cordova (02261); SD Oglala Lakota (46102) ->
# Shannon (46113).
xwalk2020_file <- file.path(RAW_DIR, "puma2020_county_crosswalk.rds")
if (!file.exists(xwalk2020_file)) {
  tract_puma <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel2020/2020_Census_Tract_to_2020_PUMA.txt",
                         col_types = cols(.default = "c"))
  tract_pop <- map_dfr(c(state.abb, "DC"), \(s)
    get_decennial("tract", variables = "P1_001N", year = 2020, sumfile = "pl", state = s) |> select(GEOID, pop = value))
  tract_puma |>
    filter(STATEFP != "72") |>
    mutate(GEOID = paste0(STATEFP, COUNTYFP, TRACTCE)) |>
    left_join(tract_pop, by = "GEOID") |>
    transmute(statefip = STATEFP, puma = PUMA5CE, county_fips = paste0(STATEFP, COUNTYFP), pop) |>
    saveRDS(xwalk2020_file)
}
puma_xwalks$puma2020 <- readRDS(xwalk2020_file) |>
  mutate(county_fips = recode(county_fips, "02063" = "02261", "02066" = "02261", "46102" = "46113"))

# which crosswalk a sample year uses
puma_vintage <- \(year) case_when(year <= 2011 ~ "puma2000", year <= 2021 ~ "puma2010", TRUE ~ "puma2020")

# allocate PUMA x year totals to another geography. afactors is a list of allocation tables (one per
# vintage, named as puma_vintage()) with columns statefip, puma, <geo>, afactor.
allocate_pumas <- function(puma_year, afactors, geo) {
  imap(afactors, \(af, v) puma_year |> filter(puma_vintage(year) == v) |>
         inner_join(af, by = c("statefip", "puma"), relationship = "many-to-many")) |>
    bind_rows() |>
    group_by(across(all_of(c(geo, "year")))) |>
    summarise(across(c(wa_pop, all_of(ACS_COUNTS)), \(x) sum(x * afactor)), .groups = "drop")
}

# allocation factors: share of each PUMA's population living in each county (or other geo).
# IPUMS merges Louisiana PUMAs 01801, 01802, 01905 (New Orleans) into 77777 in the 2006-2011 samples
# after Katrina, so the 2000 crosswalk carries the three pooled as 77777 as well as separately (the
# 2005 sample, collected before the merge, uses the originals).
puma_afactors <- function(xw, geo = "county_fips") {
  xw |>
    group_by(across(all_of(c("statefip", "puma", geo)))) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") |>
    group_by(statefip, puma) |>
    mutate(afactor = pop / sum(pop)) |>
    ungroup() |>
    select(all_of(c("statefip", "puma", geo)), afactor)
}
afactor_2000 <- bind_rows(
  puma_xwalks$puma2000,
  puma_xwalks$puma2000 |> filter(statefip == "22", puma %in% c("01801", "01802", "01905")) |> mutate(puma = "77777")
) |>
  puma_afactors()
afactor_2010 <- puma_afactors(puma_xwalks$puma2010)
afactor_2020 <- puma_afactors(puma_xwalks$puma2020)

# PUMA x year totals from the microdata, read one year at a time (rows are stored in sample order).
# cached; years in ACS_YEARS missing from the cache are read and appended, and the whole cache is
# rebuilt if it predates any of ACS_COUNTS.
acs_puma_file   <- file.path(CLEAN_DIR, "acs_puma_year.rds")
acs_puma_cached <- if (file.exists(acs_puma_file)) readRDS(acs_puma_file)
if (!all(ACS_COUNTS %in% names(acs_puma_cached))) acs_puma_cached <- NULL
acs_years_missing <- setdiff(ACS_YEARS, acs_puma_cached$year)
if (length(acs_years_missing) > 0) {
  year_blocks <- read_dta(IPUMS_FILE, col_select = year) |>
    count(year = as.integer(year)) |>
    mutate(skip = lag(cumsum(n), default = 0)) |>
    filter(year %in% acs_years_missing)

  acs_puma_cached <- year_blocks |>
    pmap(\(year, n, skip) {
      d <- read_dta(IPUMS_FILE, col_select = c(year, statefip, puma, perwt, age, citizen, empstat, classwkrd, ind1990, gq),
                    skip = skip, n_max = n) |> zap_labels()
      stopifnot(all(d$year == year))
      d |>
        filter(age >= 16, age <= 64) |>
        mutate(ag    = empstat == 1 & ind1990 %in% AG_IND1990,
               ws    = classwkrd %in% 20:28,
               nonws = classwkrd %in% c(10:14, 29),
               ws_gq = ws & gq == 4,
               hh    = gq %in% c(1, 2, 5)) |>
        group_by(year, statefip = sprintf("%02d", statefip), puma = sprintf("%05d", puma)) |>
        summarise(
          wa_pop       = sum(perwt),
          ag_total     = sum(perwt * ag),
          ag_fb        = sum(perwt * (ag & citizen %in% 2:5)),
          ag_usb       = sum(perwt * (ag & citizen %in% 0:1)),
          ag_noncit    = sum(perwt * (ag & citizen %in% 3:5)),
          ag_ws        = sum(perwt * (ag & ws)),
          ag_nonws     = sum(perwt * (ag & nonws)),
          ag_usb_ws    = sum(perwt * (ag & ws & citizen %in% 0:1)),
          ag_usb_nonws = sum(perwt * (ag & nonws & citizen %in% 0:1)),
          ag_fb_ws     = sum(perwt * (ag & ws & citizen %in% 2:5)),
          ag_ws_gq     = sum(perwt * (ag & ws_gq)),
          ag_fb_ws_gq  = sum(perwt * (ag & ws_gq & citizen %in% 2:5)),
          ag_total_hh  = sum(perwt * (ag & hh)),
          ag_fb_hh     = sum(perwt * (ag & hh & citizen %in% 2:5)),
          ag_usb_hh    = sum(perwt * (ag & hh & citizen %in% 0:1)),
          ag_noncit_hh = sum(perwt * (ag & hh & citizen %in% 3:5)),
          .groups = "drop"
        )
    }) |>
    bind_rows(acs_puma_cached) |>
    arrange(year, statefip, puma)
  saveRDS(acs_puma_cached, acs_puma_file)
}
acs_puma_year <- acs_puma_cached |>
  filter(year %in% ACS_YEARS) |>
  mutate(across(c(ag_ws_gq, ag_fb_ws_gq), \(x) if_else(year == 2005, NA_real_, x)))

# allocate PUMA totals to counties
acs_county_year <- allocate_pumas(acs_puma_year,
                                  list(puma2000 = afactor_2000, puma2010 = afactor_2010, puma2020 = afactor_2020),
                                  "county_fips")

# shares of the county's baseline (2006-07 average) working-age population, plus the baseline shares
acs_ag_panel <- acs_county_year |>
  group_by(county_fips) |>
  mutate(
    wa_pop_base = mean(wa_pop[year %in% ACS_BASELINE]),
    across(all_of(ACS_COUNTS),
           list(share      = \(x) x / wa_pop_base,
                share_base = \(x) mean(x[year %in% ACS_BASELINE]) / wa_pop_base))
  ) |>
  ungroup() |>
  left_join(
    fips_codes |> transmute(state, county_fips = paste0(state_code, county_code),
                            is_city = str_detect(county, " city$"),
                            county = clean_county(county)),
    by = "county_fips"
  ) |>
  mutate(county_key = county_key(county)) |>
  relocate(state, county, county_fips, county_key, year)

message(sprintf(
  "ACS ag workforce: %d counties x %d years; national ag workers %s (%.0f%% foreign-born) in %d",
  n_distinct(acs_ag_panel$county_fips), n_distinct(acs_ag_panel$year),
  format(round(sum(acs_ag_panel$ag_total[acs_ag_panel$year == 2007])), big.mark = ","),
  100 * sum(acs_ag_panel$ag_fb[acs_ag_panel$year == 2007]) / sum(acs_ag_panel$ag_total[acs_ag_panel$year == 2007]),
  2007L
))

## 13. long panel for the ACS labor outcomes ##-----------------------------------------------------
# county x year (2005-2022) analysis panel: the same counties as main, with the ACS ag workforce
# outcomes, the early/late indicator and the het indicators attached, ready for the long-panel
# event study and DDD in 02_models.r. early_activator here is het_early (one value per county).
# where an independent city shares its name with a county (e.g. VA Fairfax), the county is kept,
# matching how the ag census names counties.
acs_keyed <- acs_ag_panel |>
  arrange(is_city) |>
  distinct(state, county_key, year, .keep_all = TRUE) |>
  select(-county, -is_city)

acs_panel <- main |>
  distinct(state, county, county_id) |>
  mutate(county_key = county_key(county)) |>
  inner_join(acs_keyed, by = c("state", "county_key")) |>
  left_join(het_measures |> select(-county), by = c("state", "county_key")) |>
  mutate(early_activator = het_early, first_detainer_year = het_early_val) |>
  select(-county_key)
stopifnot(!anyDuplicated(acs_panel[c("county_id", "year")]))

message(sprintf("ACS long panel: %d counties x %d years (%d of %d main counties)",
                n_distinct(acs_panel$county_id), n_distinct(acs_panel$year),
                n_distinct(acs_panel$county_id), n_distinct(main$county_id)))

## 14. commuting-zone version of the ACS long panel ##----------------------------------------------
# CZs (Autor-Dorn, 741 nationally) usually contain whole PUMAs, so CZ outcomes are close to direct
# survey estimates rather than population allocations, and the same fixed CZs are used under the 2000,
# 2010 and 2020 PUMA definitions. treatment and het measures are aggregated from counties.
cw_dir <- file.path(DROPBOX, "dorn_crosswalks")
cw_puma2000 <- read_dta(file.path(cw_dir, "cw_puma2000_czone.dta")) |> zap_labels()
cw_puma2010 <- read_dta(file.path(cw_dir, "cw_puma2010_czone.dta")) |> zap_labels()
cw_cty_cz   <- read_dta(file.path(cw_dir, "cw_cty_czone.dta")) |> zap_labels() |>
  transmute(county_fips = sprintf("%05d", as.integer(cty_fips)), czone = as.integer(czone))

# dorn's file uses 1990 county codes. counties recoded or created since then get the CZ of the 1990
# county they came from, so they are not dropped from CZ aggregates: Miami-Dade (12086, coded 12025
# before 1997), Broomfield CO (08014, from Boulder), Oglala Lakota SD (46102, formerly Shannon), and
# the Alaska boroughs split or renamed since 1990.
fips_since_1990 <- c("12086" = "12025", "08014" = "08013", "46102" = "46113",
                     "02158" = "02270", "02198" = "02201", "02195" = "02280", "02275" = "02280",
                     "02105" = "02231", "02230" = "02231", "02232" = "02231", "02282" = "02231",
                     "02068" = "02290", "02063" = "02261", "02066" = "02261")
stopifnot(all(fips_since_1990 %in% cw_cty_cz$county_fips), !any(names(fips_since_1990) %in% cw_cty_cz$county_fips))
cw_cty_cz <- bind_rows(
  cw_cty_cz,
  tibble(county_fips = names(fips_since_1990), czone = cw_cty_cz$czone[match(fips_since_1990, cw_cty_cz$county_fips)])
)

# PUMA -> CZ factors in the acs_puma_year coding (2-digit state + 5-digit PUMA).
# IPUMS's merged Louisiana PUMA 77777 (2006-2011) gets the population-weighted average of the
# factors of its three source PUMAs (01801, 01802, 01905).
la_merged <- c("01801", "01802", "01905")
la_pop <- puma_xwalks$puma2000 |>
  filter(statefip == "22", puma %in% la_merged) |>
  group_by(puma) |>
  summarise(pop = sum(pop), .groups = "drop")
cz_afactor_2000 <- cw_puma2000 |>
  transmute(statefip = sprintf("%02d", puma2000 %/% 10000), puma = sprintf("%05d", puma2000 %% 10000),
            czone = as.integer(czone), afactor)
cz_afactor_2000 <- bind_rows(
  cz_afactor_2000,
  cz_afactor_2000 |>
    filter(statefip == "22", puma %in% la_merged) |>
    left_join(la_pop, by = "puma") |>
    group_by(statefip, czone) |>
    summarise(afactor = sum(afactor * pop) / sum(la_pop$pop), .groups = "drop") |>
    mutate(puma = "77777")
)
cz_afactor_2010 <- cw_puma2010 |>
  transmute(statefip = sprintf("%02d", puma2010 %/% 100000), puma = sprintf("%05d", puma2010 %% 100000),
            czone = as.integer(czone), afactor)

# 2020 PUMAs: dorn has no 2020 file, so the factors are built the same way from the 2020 tract
# populations in section 12, with counties assigned to CZs by cw_cty_czone. (the constructed
# cw_puma2020_czone.dta in the same folder puts all of Connecticut in one CZ, so it is not used.)
cz_afactor_2020 <- puma_xwalks$puma2020 |>
  inner_join(cw_cty_cz, by = "county_fips") |>
  puma_afactors("czone")
stopifnot(all(abs(tapply(cz_afactor_2020$afactor, paste(cz_afactor_2020$statefip, cz_afactor_2020$puma), sum) - 1) < 1e-9))
message(sprintf("2020 PUMA -> CZ: %.3f%% of 2020 population is in counties missing from cw_cty_czone",
                100 * with(puma_xwalks$puma2020, sum(pop[!county_fips %in% cw_cty_cz$county_fips], na.rm = TRUE) / sum(pop, na.rm = TRUE))))

acs_cz_year <- allocate_pumas(acs_puma_year,
                              list(puma2000 = cz_afactor_2000, puma2010 = cz_afactor_2010, puma2020 = cz_afactor_2020),
                              "czone")

# every PUMA's workers should land in some CZ (dorn's 2000 factors are rounded, summing to 0.998-1.002
# per PUMA, so totals match to within rounding rather than exactly)
cz_total_gap <- sum(acs_cz_year$ag_total) / sum(acs_puma_year$ag_total) - 1
stopifnot(abs(cz_total_gap) < 0.002)

# county-level inputs keyed on FIPS. names map to FIPS through name_to_fips (county preferred over a
# same-named independent city), and 2010 populations come from county_pop_fips (section 3).

cz_county_inputs <- cw_cty_cz |>
  inner_join(county_pop_fips, by = "county_fips") |>
  left_join(activation_fips |> transmute(county_fips, first_detainer_year = year(activation_date)), by = "county_fips") |>
  left_join(
    het_measures |> inner_join(name_to_fips, by = c("state", "county_key")) |>
      select(county_fips, noncit_share = het_noncit_val, undoc_share = het_undoc_val),
    by = "county_fips"
  ) |>
  left_join(
    jail_capacity_controls |> transmute(state, county_key = county_key(county), jail_beds = jail_capacity_2006) |>
      inner_join(name_to_fips, by = c("state", "county_key")) |> select(county_fips, jail_beds),
    by = "county_fips"
  ) |>
  left_join(jail_flow_0407 |> select(county_fips, jail_admits = jail_admits_0407), by = "county_fips")

# CZ aggregates. shares are population-weighted over counties with data. jail beds and admissions are
# summed over the CZ: a county with no matched jail contributes 0 beds (its residents are booked in a
# neighbouring or regional jail, which is in the same CZ), while admissions use only Vera-covered counties.
cz_het <- cz_county_inputs |>
  group_by(czone) |>
  summarise(
    cz_pop          = sum(pop),
    early_share     = sum(pop[!is.na(first_detainer_year) & first_detainer_year <= 2011]) /
                      sum(pop[!is.na(first_detainer_year)]),
    het_noncit_val    = weighted.mean(noncit_share, pop, na.rm = TRUE),
    het_undoc_val     = weighted.mean(undoc_share,  pop, na.rm = TRUE),
    het_jail_cap_val  = sum(jail_beds, na.rm = TRUE) / sum(pop) * 1000,
    het_jail_flow_val = sum(jail_admits, na.rm = TRUE) / sum(pop[!is.na(jail_admits)]) * 1000,
    # CZs can cross state lines; each is assigned the state of its most populous county
    state           = fips_codes$state[match(substr(county_fips[which.max(pop)], 1, 2), fips_codes$state_code)],
    .groups = "drop"
  ) |>
  mutate(
    across(c(early_share, het_noncit_val, het_undoc_val, het_jail_flow_val), \(x) if_else(is.nan(x), NA_real_, x)),
    early_activator = as.integer(early_share >= CZ_EARLY_SHARE)
  )

# sample: CZs containing at least one county in main
cz_sample <- main |>
  distinct(state, county) |>
  mutate(county_key = county_key(county)) |>
  inner_join(name_to_fips, by = c("state", "county_key")) |>
  inner_join(cw_cty_cz, by = "county_fips") |>
  distinct(czone)

# high/low splits at the CZ level, using het_cz_cuts (NA -> median across sample CZs)
cz_het_sample <- cz_het |> semi_join(cz_sample, by = "czone")
for (m in names(het_cz_cuts)) {
  val <- cz_het[[paste0("het_", m, "_val")]]
  cut <- if (is.na(het_cz_cuts[[m]])) median(cz_het_sample[[paste0("het_", m, "_val")]], na.rm = TRUE) else het_cz_cuts[[m]]
  het_cz_cuts[[m]] <- cut
  cz_het[[paste0("het_", m)]] <- as.integer(val >= cut)
}

acs_cz_panel <- acs_cz_year |>
  semi_join(cz_sample, by = "czone") |>
  group_by(czone) |>
  mutate(
    wa_pop_base = mean(wa_pop[year %in% ACS_BASELINE]),
    across(all_of(ACS_COUNTS),
           list(share      = \(x) x / wa_pop_base,
                share_base = \(x) mean(x[year %in% ACS_BASELINE]) / wa_pop_base))
  ) |>
  ungroup() |>
  left_join(cz_het, by = "czone") |>
  mutate(cz_id = as.character(czone))
stopifnot(!anyDuplicated(acs_cz_panel[c("czone", "year")]))

message(sprintf("ACS CZ panel: %d CZs x %d years; %d early / %d late; CZ cuts: %s; allocation gap %.4f%%",
                n_distinct(acs_cz_panel$czone), n_distinct(acs_cz_panel$year),
                sum(cz_het_sample$early_share >= CZ_EARLY_SHARE, na.rm = TRUE),
                sum(cz_het_sample$early_share <  CZ_EARLY_SHARE, na.rm = TRUE),
                paste(names(het_cz_cuts), signif(het_cz_cuts, 3), sep = "=", collapse = ", "), 100 * cz_total_gap))

## 15. QCEW ag employment long panel ##------------------------------------------------------------
# yearly county annual-average employment from the BLS QCEW (employer UI filings), private ownership,
# for NAICS 11 (ag, forestry, fishing, hunting), 111 (crop production), 112 (animal production) and
# 115 (support activities, incl. farm labor contractors in 1151). 4-digit codes are too suppressed in
# ag counties to use. qcew_11_resid = 11 - 111 - 112 (forestry, fishing and support), defined only
# where all three are balanced.
# UI only covers ag employers above the FUTA threshold ($20k quarterly payroll or 10+ workers in 20
# weeks; some states cover more) and mostly exempts H-2A workers, so this is formal payroll
# employment at larger operations. counts are direct county figures, so county clustering is fine.
# suppressed cells (disclosure_code "N") are reported as 0 and are set to missing. an outcome is
# kept only for counties disclosed in every year of QCEW_YEARS (a balanced panel per industry), so
# suppression switching on and off cannot drive the estimates. 2000-2004 are kept as optional extra
# leads; they are not part of the balance check. outcomes are shares of the county's 2006-07
# working-age population (from the ACS allocation in section 12), the same units as the ACS outcomes.
QCEW_FILE     <- file.path(DROPBOX, "generated_data", "qcew_ag_county_annual_2005_2022.dta")
QCEW_NAICS    <- c(`11` = "qcew_11", `111` = "qcew_111", `112` = "qcew_112", `115` = "qcew_115")
QCEW_VARS     <- c(unname(QCEW_NAICS), "qcew_11_resid")
QCEW_YEARS    <- 2005:2022

# one row per county (incl. XX999 "unknown county") x industry x year, with establishment counts,
# which BLS publishes even where employment is suppressed (used for the CZ imputation in section 16)
qcew_cells <- read_dta(QCEW_FILE) |>
  zap_labels() |>
  filter(own_code == 5, agglvl_code %in% 74:76, industry_code %in% names(QCEW_NAICS)) |>
  transmute(
    county_fips   = area_fips,
    statefip      = substr(area_fips, 1, 2),
    year          = as.integer(year),
    industry_code,
    estabs        = as.numeric(annual_avg_estabs),
    emp           = if_else(disclosure_code %in% "N", NA_real_, as.numeric(annual_avg_emplvl))
  )

qcew_county_year <- qcew_cells |>
  transmute(county_fips, year, outcome = QCEW_NAICS[industry_code], emp) |>
  group_by(county_fips, outcome) |>
  mutate(balanced = sum(!is.na(emp[year %in% QCEW_YEARS])) == length(QCEW_YEARS),
         emp      = if_else(balanced, emp, NA_real_)) |>
  ungroup() |>
  select(-balanced) |>
  pivot_wider(names_from = outcome, values_from = emp) |>
  mutate(qcew_11_resid = qcew_11 - qcew_111 - qcew_112)

qcew_panel <- main |>
  distinct(state, county, county_id) |>
  mutate(county_key = county_key(county)) |>
  inner_join(name_to_fips, by = c("state", "county_key")) |>
  inner_join(qcew_county_year, by = "county_fips") |>
  left_join(acs_ag_panel |> distinct(county_fips, wa_pop_base), by = "county_fips") |>
  mutate(across(all_of(QCEW_VARS), \(x) x / wa_pop_base, .names = "{.col}_share")) |>
  left_join(het_measures |> select(-county), by = c("state", "county_key")) |>
  mutate(early_activator = het_early, first_detainer_year = het_early_val) |>
  select(-county_key)
stopifnot(!anyDuplicated(qcew_panel[c("county_id", "year")]))

message(sprintf(
  "QCEW panel: %d of %d main counties; balanced %s counties (NAICS 11 / 111 / 112 / 115 / 11 resid)",
  n_distinct(qcew_panel$county_id), n_distinct(main$county_id),
  paste(map_int(QCEW_VARS, \(v) n_distinct(qcew_panel$county_id[!is.na(qcew_panel[[paste0(v, "_share")]])])),
        collapse = " / ")
))

## 16. QCEW at the commuting-zone level, suppressed cells imputed ##--------------------------------
# BLS suppresses about 45% of county x NAICS 11 x year employment cells, and almost every CZ contains a
# suppressed county, so CZ totals need those cells filled in. establishment counts are published for
# nearly every suppressed cell and state totals for nearly every state, so each suppressed county x
# industry x year is imputed in two steps:
#   1. prior = the county's establishments x employment per establishment in the same state, industry
#      and year's disclosed counties (the national figure where the state has no disclosed county)
#   2. the state's hidden employment (state total - disclosed counties) is split across its suppressed
#      counties in proportion to their priors, so the counties add up to the published state total.
#      where the state total is itself suppressed, or the hidden total is not positive, the prior is
#      used as is.
# county XX999 rows ("unknown or undefined" county) take part in the split and are then dropped, since
# they belong to no CZ. a county absent from the file in a year had no covered establishments (0).
# state totals come from the BLS annual singlefiles in dropbox qcew/raw, read once and cached.
# the imputation is validated by hiding a random 20% of disclosed county cells, imputing them, and
# comparing with the published values. the county panel in section 15 keeps the disclosed-only,
# balanced design and serves as the robustness check.
QCEW_RAW_DIR <- file.path(DROPBOX, "qcew", "raw")

qcew_state_file <- file.path(RAW_DIR, "qcew_state_ag_annual.rds")
if (!file.exists(qcew_state_file)) {
  map_dfr(QCEW_YEARS, \(yr) {
    read_csv(file.path(QCEW_RAW_DIR, paste0(yr, "_annual_singlefile.zip")), col_types = cols(.default = "c"),
             col_select = c(area_fips, own_code, industry_code, agglvl_code, disclosure_code, annual_avg_emplvl)) |>
      filter(own_code == "5", agglvl_code %in% c("54", "55"), industry_code %in% names(QCEW_NAICS)) |>
      transmute(statefip = substr(area_fips, 1, 2), year = as.integer(yr), industry_code,
                state_emp = if_else(disclosure_code %in% "N", NA_real_, as.numeric(annual_avg_emplvl)))
  }) |>
    saveRDS(qcew_state_file)
}
qcew_state <- readRDS(qcew_state_file)

impute_qcew <- function(cells) {
  natl <- cells |>
    filter(!is.na(emp)) |>
    group_by(industry_code, year) |>
    summarise(natl_size = sum(emp) / sum(estabs), .groups = "drop")
  cells |>
    group_by(statefip, industry_code, year) |>
    mutate(state_size = sum(emp, na.rm = TRUE) / sum(estabs[!is.na(emp)]),
           disclosed  = sum(emp, na.rm = TRUE)) |>
    ungroup() |>
    left_join(natl, by = c("industry_code", "year")) |>
    left_join(qcew_state, by = c("statefip", "industry_code", "year")) |>
    mutate(prior = if_else(is.na(emp), estabs * if_else(is.finite(state_size) & state_size > 0, state_size, natl_size), NA_real_)) |>
    group_by(statefip, industry_code, year) |>
    mutate(hidden  = state_emp - disclosed,
           emp_imp = case_when(!is.na(emp)                                           ~ emp,
                               !is.na(hidden) & hidden > 0 & sum(prior, na.rm = TRUE) > 0 ~ hidden * prior / sum(prior, na.rm = TRUE),
                               TRUE                                                  ~ prior),
           imputed = is.na(emp)) |>
    ungroup() |>
    select(county_fips, statefip, year, industry_code, emp_imp, imputed)
}

# county cells -> CZ x year employment per industry, with the share of each total that is imputed
qcew_to_cz <- function(imputed) {
  imputed |>
    inner_join(cw_cty_cz, by = "county_fips") |>
    group_by(czone, year, industry_code) |>
    summarise(emp = sum(emp_imp, na.rm = TRUE), imp = sum(emp_imp[imputed], na.rm = TRUE), .groups = "drop")
}

qcew_cells_cz <- filter(qcew_cells, year %in% QCEW_YEARS)
qcew_imputed  <- impute_qcew(qcew_cells_cz)
qcew_cz_year  <- qcew_to_cz(qcew_imputed)

# validation: hide 20% of disclosed (non-999) cells, impute, compare with the published values, both
# cell by cell and as CZ totals
set.seed(20261022)
qcew_mask <- qcew_cells_cz |> mutate(hide = !is.na(emp) & !str_ends(county_fips, "999") & runif(n()) < 0.2)
qcew_valid_imp <- qcew_mask |> mutate(emp = if_else(hide, NA_real_, emp)) |> impute_qcew()
qcew_valid_cells <- qcew_mask |>
  filter(hide) |>
  select(county_fips, year, industry_code, emp) |>
  inner_join(qcew_valid_imp, by = c("county_fips", "year", "industry_code"))
qcew_valid_cz <- qcew_to_cz(qcew_valid_imp) |>
  inner_join(qcew_cz_year, by = c("czone", "year", "industry_code"), suffix = c("_valid", "")) |>
  semi_join(cz_sample, by = "czone") |>
  filter(emp > 0)
qcew_validation <- bind_rows(
  qcew_valid_cells |> group_by(industry_code) |>
    summarise(level = "county cell", cor = cor(emp, emp_imp), med_abs_pct_err = median(abs(emp_imp - emp) / pmax(emp, 1))),
  qcew_valid_cz |> group_by(industry_code) |>
    summarise(level = "CZ total", cor = cor(emp, emp_valid), med_abs_pct_err = median(abs(emp_valid - emp) / emp))
)
print(qcew_validation)

# CZ panel: sample CZs x QCEW_YEARS, absent industry-years = 0. outcomes are shares of the CZ's 2006-07
# working-age population (as for the ACS CZ panel); qcew_<naics>_imputed is the imputed share of the
# CZ's employment in that industry and year.
qcew_cz_panel <- expand_grid(czone = cz_sample$czone, year = QCEW_YEARS, industry_code = names(QCEW_NAICS)) |>
  left_join(qcew_cz_year, by = c("czone", "year", "industry_code")) |>
  mutate(emp      = replace_na(emp, 0),
         imputed  = if_else(emp > 0, replace_na(imp, 0) / emp, 0),
         outcome  = QCEW_NAICS[industry_code]) |>
  select(czone, year, outcome, emp, imputed) |>
  pivot_wider(names_from = outcome, values_from = c(emp, imputed), names_glue = "{outcome}_{.value}") |>
  rename_with(\(x) str_remove(x, "_emp$"), ends_with("_emp")) |>
  mutate(qcew_11_resid = qcew_11 - qcew_111 - qcew_112) |>
  left_join(acs_cz_panel |> distinct(czone, wa_pop_base), by = "czone") |>
  mutate(across(all_of(QCEW_VARS), \(x) x / wa_pop_base, .names = "{.col}_share")) |>
  left_join(cz_het, by = "czone") |>
  mutate(cz_id = as.character(czone))
stopifnot(!anyDuplicated(qcew_cz_panel[c("czone", "year")]))

message(sprintf(
  "QCEW CZ panel: %d CZs x %d years; imputed share of 2007 employment (NAICS 11 / 111 / 112 / 115): %s; %.2f%% of US county employment unmapped to a CZ",
  n_distinct(qcew_cz_panel$czone), n_distinct(qcew_cz_panel$year),
  paste(map_chr(names(QCEW_NAICS), \(i) sprintf("%.0f%%", 100 * with(filter(qcew_cz_year, year == 2007, industry_code == i), sum(imp) / sum(emp)))), collapse = " / "),
  100 * with(filter(qcew_imputed, !str_ends(county_fips, "999"), !statefip %in% c("72", "78")), sum(emp_imp[!county_fips %in% cw_cty_cz$county_fips], na.rm = TRUE) / sum(emp_imp, na.rm = TRUE))
))

## 17. crop mix: labor-intensive acreage ##---------------------------------------------------------
# census of ag county acreage for the two hand-harvest crop groups, pulled as single group totals
# (pull_all_cropmix() in agcensus_api.R): vegetables harvested in the open, and orchards bearing &
# non-bearing (census orchards include vineyards, citrus and tree nuts). outcomes are shares of the
# same year's harvested cropland, which also counts orchard land. berries (measure changes after 2002)
# and nursery (heavily suppressed) are left out.
# a county with no row for a group had no operations growing it (0 acres); a (D) cell is unknown (NA).
# as with the QCEW, each outcome keeps only units observed in every census year, so suppression
# switching on and off cannot drive the estimates. CZ acres are county sums. requiring every county in
# a CZ to be disclosed leaves only ~70 CZs, so for CZs (D) crop cells count as 0 (cropland must still
# be disclosed). checked against state totals, (D) cells hide only ~1-2% of national orchard acres and
# ~3-5% of vegetable acres, so CZ shares are slight lower bounds. counties keep the strict rule, since
# one (D) cell can be large relative to a single county's cropland.
CROPMIX_YEARS <- c(2002L, 2007L, 2012L, 2017L, 2022L)
CROPMIX_VARS  <- c("VEGETABLE TOTALS, IN THE OPEN - ACRES HARVESTED" = "veg_acres",
                   "ORCHARDS - ACRES BEARING & NON-BEARING"          = "orch_acres",
                   "AG LAND, CROPLAND, HARVESTED - ACRES"            = "cropland_acres")
CROPMIX_OUTCOMES <- c("li_share", "veg_share", "orch_share")

cropmix_raw <- read_csv(file.path(RAW_DIR, "crop_mix.csv"), show_col_types = FALSE,
                        col_types = cols(state_fips_code = "c", county_ansi = "c")) |>
  filter(!is.na(county_ansi)) |>   # drops the "other (combined) counties" rows
  transmute(county_fips = paste0(str_pad(state_fips_code, 2, pad = "0"), str_pad(county_ansi, 3, pad = "0")),
            year = as.integer(year), var = CROPMIX_VARS[short_desc], value)

# absent county-years become 0 acres; explicit (D) NAs are kept
cropmix_county_year <- cropmix_raw |>
  complete(county_fips = union(county_fips, cw_cty_cz$county_fips), year = CROPMIX_YEARS,
           var = unname(CROPMIX_VARS), fill = list(value = 0), explicit = FALSE) |>
  pivot_wider(names_from = var, values_from = value)

# shares from acres, then NA out every outcome that is not observed in all census years
add_crop_shares <- function(df, unit) {
  df |>
    mutate(
      li_acres   = veg_acres + orch_acres,
      li_share   = li_acres   / cropland_acres,
      veg_share  = veg_acres  / cropland_acres,
      orch_share = orch_acres / cropland_acres,
      across(all_of(CROPMIX_OUTCOMES), \(x) if_else(is.finite(x), x, NA_real_))
    ) |>
    group_by(pick(all_of(unit))) |>
    mutate(across(all_of(CROPMIX_OUTCOMES), \(x) if (all(!is.na(x[year %in% CROPMIX_YEARS]))) x else NA_real_)) |>
    ungroup()
}

crop_panel <- main |>
  distinct(state, county, county_id) |>
  mutate(county_key = county_key(county)) |>
  inner_join(name_to_fips, by = c("state", "county_key")) |>
  inner_join(cropmix_county_year, by = "county_fips") |>
  add_crop_shares("county_fips") |>
  left_join(het_measures |> select(-county), by = c("state", "county_key")) |>
  mutate(early_activator = het_early, first_detainer_year = het_early_val) |>
  select(-county_key)
stopifnot(!anyDuplicated(crop_panel[c("county_id", "year")]))

crop_cz_panel <- cropmix_county_year |>
  inner_join(cw_cty_cz, by = "county_fips") |>
  group_by(czone, year) |>
  summarise(across(c(veg_acres, orch_acres), \(x) sum(x, na.rm = TRUE)),
            cropland_acres = sum(cropland_acres), .groups = "drop") |>
  semi_join(cz_sample, by = "czone") |>
  add_crop_shares("czone") |>
  left_join(cz_het, by = "czone") |>
  mutate(cz_id = as.character(czone))
stopifnot(!anyDuplicated(crop_cz_panel[c("czone", "year")]))

n_balanced <- \(df, id) paste(map_int(CROPMIX_OUTCOMES, \(v) n_distinct(df[[id]][!is.na(df[[v]])])), collapse = " / ")
message(sprintf("crop mix: %d county / %d CZ units; balanced (li / veg / orch) counties %s, CZs %s",
                n_distinct(crop_panel$county_id), n_distinct(crop_cz_panel$czone),
                n_balanced(crop_panel, "county_id"), n_balanced(crop_cz_panel, "czone")))

## 18. census of ag outcomes at the commuting-zone level ##----------------------------------------
# the Census of Ag outcomes in main (section 9), rebuilt for CZs by summing numerators and
# denominators over every county in the CZ:
#   mech_share_broad          machinery-related expenses / total operating expenses
#   labor_share               hired + contract labor expenses / total operating expenses
#   hired_workers_per_farm    hired workers / farm operations
# suppressed (D) numerator cells (mostly machinery rental, ~8-11% of county-years; hired labor ~2%)
# count as 0, so CZ values are slight lower bounds, as for the CZ crop mix. mech_share_broad needs
# total expenses disclosed in every county of the CZ, and each outcome keeps only CZs observed in
# every census year.
AGC_YEARS    <- c(2002L, 2007L, 2012L, 2017L)
AGC_OUTCOMES <- c("mech_share_broad", "labor_share", "hired_workers_per_farm")

agc_county_parts <- expenditures_clean |>
  filter(year %in% AGC_YEARS) |>
  transmute(state, county_key = county_key(county), year, total_exp,
            mech_exp = rowSums(cbind(fuel_total_exp, repairs_exp, utilities_exp, machinery_rent_exp, depreciation_exp), na.rm = TRUE),
            labor_exp = rowSums(cbind(labor_hired_exp, labor_contract_exp), na.rm = TRUE)) |>
  left_join(hired_labor_controls |> transmute(state, county_key = county_key(county), year, hired_workers),
            by = c("state", "county_key", "year")) |>
  inner_join(name_to_fips, by = c("state", "county_key")) |>
  left_join(farm_counts, by = c("county_fips", "year")) |>
  inner_join(cw_cty_cz, by = "county_fips")
stopifnot(!anyDuplicated(agc_county_parts[c("county_fips", "year")]))

agc_cz_panel <- agc_county_parts |>
  group_by(czone, year) |>
  summarise(exp_complete = all(!is.na(total_exp)),
            across(c(total_exp, mech_exp, labor_exp, hired_workers, total_farms), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(mech_share_broad         = if_else(exp_complete & total_exp > 0, mech_exp / total_exp, NA_real_),
         labor_share              = if_else(exp_complete & total_exp > 0, labor_exp / total_exp, NA_real_),
         hired_workers_per_farm   = if_else(total_farms > 0, hired_workers / total_farms, NA_real_)) |>
  semi_join(cz_sample, by = "czone") |>
  group_by(czone) |>
  mutate(across(all_of(AGC_OUTCOMES), \(x) if (all(AGC_YEARS %in% year) && all(!is.na(x))) x else NA_real_)) |>
  ungroup() |>
  left_join(cz_het, by = "czone") |>
  mutate(cz_id = as.character(czone))
stopifnot(!anyDuplicated(agc_cz_panel[c("czone", "year")]))

message(sprintf("census of ag CZ panel: %d CZs; balanced (mech / labor share / hired workers per farm) %s",
                n_distinct(agc_cz_panel$czone),
                paste(map_int(AGC_OUTCOMES, \(v) n_distinct(agc_cz_panel$czone[!is.na(agc_cz_panel[[v]])])), collapse = " / ")))

## 19. treatment, controls and baseline characteristics ##----------------------------------------
# unit-level inputs for the event-study specification in 00_setup.r (the triple-interaction design
# in dropbox slides/sc_agriculture_slides.tex), attached to every panel:
#   earliness_norm, earliness_rank, exposure_years  built in section 5
#   border_dist_km  distance to the US-Mexico border
#   gr_severity     Great Recession severity, ln private emp 2009 - ln private emp 2007
#   het_hispnc_val  Hispanic noncitizens / working-age pop., 2000 Census (the vulnerability measure)
#   ag_share_base   ACS ag workers / working-age pop., 2006-07 average (section 12)
#   log_pop_2010    log 2010 population (an urbanization proxy)
#   rel_fb_ag       reliance on immigrant farm labor: foreign-born ag workers / working-age pop.,
#                   2006-07 (ACS, section 12)
#   rel_labor       reliance on hired farm labor: hired + contract labor / farm operating expenses,
#                   2002 and 2007 pooled (census of ag, section 18), NA unless expenses are disclosed
#                   in both years (and, for a CZ, in every county)
# border distance, recession severity and het_hispnc come from the dropbox generated_data files.
# CZs take the 2000 population-weighted mean of their counties' earliness and exposure years.
# ag_share_base and log_pop_2010 are baseline characteristics interacted with year in the outcome
# scripts' robustness check (did early places differ in ways that predict their trends?).
read_gen <- \(f) read_dta(file.path(GEN_DIR, f)) |> zap_labels()
fips5    <- \(x) sprintf("%05d", as.integer(x))

nhgis_2000 <- read_gen("county_nhgis_baseline.dta") |>
  transmute(county_fips = fips5(cty_fips), czone = as.integer(czone), totpop_2000,
            hisp_fb_noncit_2000, workingage_pop_2000)

cexp_county_fips <- activation_fips |>
  left_join(nhgis_2000 |> transmute(county_fips, het_hispnc_val = hisp_fb_noncit_2000 / workingage_pop_2000), by = "county_fips") |>
  left_join(read_gen("county_border_distance.dta") |> transmute(county_fips = fips5(cty_fips), border_dist_km), by = "county_fips") |>
  left_join(read_gen("county_gr_severity.dta")     |> transmute(county_fips = fips5(cty_fips), gr_severity),    by = "county_fips")

# pre-SC reliance on hired farm labor, from the census of ag parts built in section 18
rel_labor_parts <- agc_county_parts |> filter(year %in% c(2002L, 2007L))
rel_labor_of <- \(d) d |> summarise(rel_labor = if (n() == 2 * n_distinct(county_fips) && n_distinct(year) == 2 && all(!is.na(total_exp)))
                                                   sum(labor_exp) / sum(total_exp) else NA_real_, .groups = "drop")

cexp_county <- cexp_county_fips |>
  left_join(acs_ag_panel |> distinct(county_fips, ag_share_base = ag_total_share_base, rel_fb_ag = ag_fb_share_base), by = "county_fips") |>
  left_join(rel_labor_parts |> group_by(county_fips) |> rel_labor_of(), by = "county_fips") |>
  left_join(county_pop_fips |> transmute(county_fips, log_pop_2010 = log(pop)), by = "county_fips") |>
  select(county_fips, earliness_norm, earliness_rank, exposure_years, border_dist_km, gr_severity, het_hispnc_val,
         ag_share_base, log_pop_2010, rel_fb_ag, rel_labor)

cexp_cz <- nhgis_2000 |>
  left_join(cexp_county_fips |> select(county_fips, earliness_norm, earliness_rank, exposure_years), by = "county_fips") |>
  group_by(czone) |>
  summarise(across(c(earliness_norm, earliness_rank, exposure_years), \(x) weighted.mean(x, totpop_2000, na.rm = TRUE)),
            het_hispnc_val = sum(hisp_fb_noncit_2000, na.rm = TRUE) / sum(workingage_pop_2000, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(c(earliness_norm, earliness_rank, exposure_years), \(x) if_else(is.nan(x), NA_real_, x))) |>
  left_join(read_gen("czone_border_distance.dta") |> transmute(czone = as.integer(czone), border_dist_km), by = "czone") |>
  left_join(read_gen("czone_gr_severity.dta")     |> transmute(czone = as.integer(czone), gr_severity),    by = "czone") |>
  left_join(acs_cz_panel |> distinct(czone, ag_share_base = ag_total_share_base), by = "czone") |>
  left_join(cz_het |> transmute(czone, log_pop_2010 = log(cz_pop)), by = "czone") |>
  left_join(acs_cz_panel |> distinct(czone, rel_fb_ag = ag_fb_share_base), by = "czone") |>
  left_join(rel_labor_parts |> group_by(czone) |> rel_labor_of(), by = "czone")

add_cexp_county <- \(df) left_join(df, cexp_county, by = "county_fips")

main          <- add_cexp_county(main)
acs_panel     <- add_cexp_county(acs_panel)
qcew_panel    <- add_cexp_county(qcew_panel)
crop_panel    <- add_cexp_county(crop_panel)
acs_cz_panel  <- left_join(acs_cz_panel,  cexp_cz, by = "czone")
qcew_cz_panel <- left_join(qcew_cz_panel, cexp_cz, by = "czone")
crop_cz_panel <- left_join(crop_cz_panel, cexp_cz, by = "czone")
agc_cz_panel  <- left_join(agc_cz_panel,  cexp_cz, by = "czone")

####################################################################################################
### write outputs ###
####################################################################################################

saveRDS(main,                    file.path(CLEAN_DIR, "main.rds"))
saveRDS(sc_county,               file.path(CLEAN_DIR, "sc_county.rds"))
saveRDS(het_measures,            file.path(CLEAN_DIR, "het_measures.rds"))
saveRDS(het_config,              file.path(CLEAN_DIR, "het_config.rds"))
saveRDS(removals_county,         file.path(CLEAN_DIR, "removals_county.rds"))
saveRDS(removals_county_year,    file.path(CLEAN_DIR, "removals_county_year.rds"))
saveRDS(cw_cty_cz,               file.path(CLEAN_DIR, "cw_cty_cz.rds"))
saveRDS(acs_ag_panel,            file.path(CLEAN_DIR, "acs_ag_panel.rds"))
saveRDS(acs_panel,               file.path(CLEAN_DIR, "acs_panel.rds"))
saveRDS(acs_cz_panel,            file.path(CLEAN_DIR, "acs_cz_panel.rds"))
saveRDS(qcew_panel,              file.path(CLEAN_DIR, "qcew_panel.rds"))
saveRDS(qcew_cz_panel,           file.path(CLEAN_DIR, "qcew_cz_panel.rds"))
saveRDS(crop_panel,              file.path(CLEAN_DIR, "crop_panel.rds"))
saveRDS(crop_cz_panel,           file.path(CLEAN_DIR, "crop_cz_panel.rds"))
saveRDS(agc_cz_panel,            file.path(CLEAN_DIR, "agc_cz_panel.rds"))

# csv copy of main df for eyeballing - downstream scripts read main.rds
write_csv(main, file.path(CLEAN_DIR, "main_inspect.csv"), na = "")
