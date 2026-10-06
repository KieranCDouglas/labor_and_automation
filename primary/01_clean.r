####################################################################################################
## 01: data cleaning
## last edited: 10/01/2026
## by kieran
## does: loads raw data, cleans it, and builds every analysis dataset used downstream.
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

# data and outputs live in dropbox, code lives in the repo
DROPBOX   <- "/Users/kieran/Library/CloudStorage/Dropbox/secure_communities"
ROOT      <- file.path(DROPBOX, "kieran")
RAW_DIR   <- file.path(ROOT, "data", "raw")
CLEAN_DIR <- file.path(ROOT, "data", "clean")
dir.create(CLEAN_DIR, showWarnings = FALSE, recursive = TRUE)

## exposure heterogeneity thresholds ##-------------------------------------------------------------
# cutoffs for the binary het_<m> indicators built in section 11. 
het_config <- tribble(
  ~measure,    ~cut,    ~high_if, ~label,
  "noncit",    0.015,   ">=",     "Noncitizen Share",
  "undoc",     0.0091,  ">=",     "Undocumented Share (Proxy)",
  "early",     2011,    "<=",     "Early Activation",
  "jail_cap",  2.735,   ">=",     "Jail Capacity",
  "jail_flow", 12.565,  ">=",     "Jail Admissions"
)

# commuting-zone versions of the same splits (section 16). CZ's count as early if at least this share of its population activated by 2011.
het_cz_cuts <- c(noncit = NA, undoc = NA, jail_cap = NA, jail_flow = NA)
CZ_EARLY_SHARE <- 0.5

####################################################################################################
### load data ###
####################################################################################################

expenditures <- read.csv(file.path(RAW_DIR, "expenditures_all_states_wide.csv"))
sc_trac <- read.csv(file.path(RAW_DIR, "secure1904.csv"))
crops  <- read.csv(file.path(RAW_DIR, "crops_area_harvested.csv"))
landuse <- read.csv(file.path(RAW_DIR, "landuse.csv"))
sc_ice <- read.csv(file.path(RAW_DIR, "sc_activation_dates.csv"))
ag_typology <- read.csv(file.path(RAW_DIR, "ers_county_typology_2015.csv"))
hired_labor <- read.csv(file.path(RAW_DIR, "hired_labor.csv"))
farms_landvalue <- read.csv(file.path(RAW_DIR, "farms_landvalue.csv"))
harvested_by_farmsize <- read.csv(file.path(RAW_DIR, "harvested_cropland_by_farmsize.csv"))
all_removals_trac <- read.csv(file.path(DROPBOX, "TRAC", "all_deportations", "removals.csv"))
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
# variables are selected/named plus mutations convert to workable format. new variables generated as mechanization proxies.
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
    labor_share = (labor_hired_exp+labor_contract_exp)/total_exp,
    log_labor_share = log((labor_hired_exp+labor_contract_exp)/total_exp),
    mech_share_broad = (fuel_total_exp+repairs_exp+utilities_exp+machinery_rent_exp+depreciation_exp)/total_exp,
    mech_share_narrow = (machinery_rent_exp+fuel_total_exp+repairs_exp)/total_exp,
    mech_share_nofuel       = (machinery_rent_exp+repairs_exp)/total_exp,
    mech_share_nofuel_broad = (machinery_rent_exp+repairs_exp+depreciation_exp)/total_exp,
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

## 3. population rates data ##----------------------------------------------------------------------
# collects county-level population data for exposure intensity score
county_pop <- get_decennial(
  geography = "county",
  variables = "P001001",
  year      = 2010
) |>
  separate(NAME, into = c("county", "state_name"), sep = ", ") |>
  mutate(
    county = str_remove(county, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county),
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
    county = str_remove(county, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county),
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

# generate pooled exposure intensity scores using 2 and 3, treating midpoint population as fixed (2010).
county_exposure_pooled <- sc_trac_clean |>
  filter(year >= 2008, year <= 2013, community_channel) |>
  group_by(state, county) |>
  summarise(cases = n(), .groups = "drop") |>
  left_join(county_pop, by = c("state", "county")) |>
  mutate(exposure_pooled = cases / population * 10000) |>
  select(state, county, exposure_pooled)

# generate seperate yearly exposure intensity scores using 2 and 3
county_exposure_yr <- sc_trac_clean |>
  filter(!is.na(year), community_channel) |>
  group_by(state, county, year) |>
  summarise(cases = n(), .groups = "drop") |>
  left_join(county_pop, by = c("state", "county")) |>
  mutate(exposure_yr = cases / population * 10000) |>
  select(state, county, year, exposure_yr)

# add pooled and yearly exposure intensity variables to sc_trac_clean df
sc_trac_clean <- sc_trac_clean |>
  left_join(county_exposure_pooled, by = c("state", "county")) |>
  left_join(county_exposure_yr,     by = c("state", "county", "year"))

## 4. crop type and land use controls ##------------------------------------------------------------
# collect county-level crop type and land use data, cleans, and generates share vars
specialty_groups <- c("VEGETABLES", "FRUIT & TREE NUTS", "HORTICULTURE")

# crops
crop_controls <- crops |>
  mutate(
    county = str_to_title(county_name),
    state  = state_alpha
  ) |>
  group_by(state, county, year) |>
  summarise(
    specialty_acres  = sum(value[group_desc %in% specialty_groups], na.rm = TRUE),
    field_crop_acres = sum(value[group_desc == "FIELD CROPS"],      na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(specialty_share = specialty_acres / (specialty_acres + field_crop_acres))

# land use
landuse_controls <- landuse |>
  mutate(
    county = str_to_title(county_name),
    state  = state_alpha
  ) |>
  group_by(state, county, year) |>
  summarise(
    harvested_acres = sum(value[short_desc == "AG LAND, CROPLAND, HARVESTED - ACRES"], na.rm = TRUE),
    irrigated_acres = sum(value[short_desc == "AG LAND, IRRIGATED - ACRES"],           na.rm = TRUE),
    total_ag_acres  = sum(value[short_desc == "AG LAND - ACRES"],                      na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(irrigated_share = irrigated_acres / total_ag_acres)

## 5. secure communities activation dates ##--------------------------------------------------------
# official county-level SC activation schedule fro ICE
sc_activation_clean <- sc_ice |>
  mutate(
    county = str_remove(county, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county),
    activation_date = as.Date(activation_date),
    first_detainer_year = year(activation_date)
  )

## 6. define ag counties ##-------------------------------------------------------------------------
# provides indicator for agricultural counties, defined as the union of two criteria:
#   (a) ERS earnings/employment flag: ≥20% of labor earnings or ≥17% of jobs from ag
#   (b) farmland acreage share: ≥1% of county land area in farms as of 2002 (kinda arbitrary???)
ers_ag_flag <- ag_typology |>
  filter(Farming_2015_Update == 1) |>
  transmute(
    state  = State,
    county = str_remove(County_name, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county)
  )

# farmland_share NA when AG LAND ACRES is missing for a county in 2002, so
# counties aren't misclassified as having no farmland.
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
  filter(farmland_share_2002 >= 0.01) |>
  select(state, county)

ag_counties <- bind_rows(ers_ag_flag, acreage_ag_flag) |>
  distinct(state, county)

# combined flag table showing which criterion (or both) each county met
ag_counties_flagged <- full_join(
  ers_ag_flag     |> mutate(ers_flag = TRUE),
  acreage_ag_flag |> mutate(acreage_flag = TRUE),
  by = c("state", "county")
) |>
  mutate(
    ers_flag      = replace_na(ers_flag, FALSE),
    acreage_flag  = replace_na(acreage_flag, FALSE)
  )

message(sprintf(
  "ag county flags: %d ERS-only, %d acreage-only, %d overlap, %d total",
  sum(ag_counties_flagged$ers_flag & !ag_counties_flagged$acreage_flag),
  sum(!ag_counties_flagged$ers_flag & ag_counties_flagged$acreage_flag),
  sum(ag_counties_flagged$ers_flag & ag_counties_flagged$acreage_flag),
  nrow(ag_counties_flagged)
))

# write ag county csv 
write_csv(ag_counties_flagged, file.path(DROPBOX, "agcensus", "ag_counties_flagged.csv"))

## 7. outcome varaibles ##---------------------------------------------------------------------------
# cleans hired farm labor data, giving each concept its own column per county-year. 
safe_sum <- function(value, cond) {
  if (all(is.na(value[cond]))) NA_real_ else sum(value[cond], na.rm = TRUE)
}

hired_labor_controls <- hired_labor |>
  mutate(
    county = str_to_title(str_remove(county_name, " County$| Parish$| Borough$| Census Area$| city$")),
    state  = state_alpha
  ) |>
  group_by(state, county, year) |>
  summarise(
    hired_workers          = safe_sum(value, short_desc == "LABOR, HIRED - NUMBER OF WORKERS"),
    hired_labor_exp        = safe_sum(value, short_desc == "LABOR, HIRED - EXPENSE, MEASURED IN $"),
    migrant_workers        = safe_sum(value, short_desc == "LABOR, MIGRANT - NUMBER OF WORKERS"),
    migrant_farms_hired    = safe_sum(value, short_desc == "LABOR, MIGRANT - OPERATIONS WITH WORKERS" &
                                         domaincat_desc == "LABOR: (INCL HIRED WORKERS)"),
    migrant_farms_contract = safe_sum(value, short_desc == "LABOR, MIGRANT - OPERATIONS WITH WORKERS" &
                                         domaincat_desc == "LABOR: (ONLY CONTRACT)"),
    .groups = "drop"
  ) |>
  mutate(migrant_farms = migrant_farms_hired + migrant_farms_contract)

# cleans farm count and asset value, needed here for total_farms
# denominator for migrant_farm_share (migrant_farms is a count of farms, so it's normalized as a
# share of all farms, not per acre).
farms_landvalue_controls <- farms_landvalue |>
  mutate(
    county = str_to_title(str_remove(county_name, " County$| Parish$| Borough$| Census Area$| city$")),
    state  = state_alpha
  ) |>
  group_by(state, county, year) |>
  summarise(
    total_farms      = safe_sum(value, short_desc == "FARM OPERATIONS - NUMBER OF OPERATIONS"),
    land_asset_value = safe_sum(value, short_desc == "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $"),
    .groups = "drop"
  )

## 8. clean all removals data ##------------------------------------------------------------------
# here we want to both clean the all_removals_trac df and generate some variables that connect it to the sc-specific cases
all_removals_clean <- all_removals_trac |> 
  rename(
    departed_port = port_of_departure,
    sex = gender,
    age_rem = age_at_removal,
    state = depart_state,
    mscc_code = mscc_code2,
    offense = most_ser_crim_conv_mscc2,
    app_mechanism = apprehend_method,
  ) |> 
  mutate(
    departed_date = as.Date(departed_date, format = "%d%b%y")
  ) |> 
  select(
    departed_port, sex, age_rem, state, mscc_code, offense, app_mechanism, entry_date, departed_date
  ) |> 
  filter(
    year(departed_date) >= 2007, year(departed_date) <= 2017
  )

## 9. develop exposure vulnerability proxy ##---------------------------------------------------------
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
  mutate(county = str_to_title(str_remove(county, " County$| Parish$| Borough$| Census Area$| city$"))) |>
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
    fips_codes |> mutate(county = str_to_title(str_remove(county, " County$| Parish$| Borough$| Census Area$| city$"))),
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

# confirm preexisting ICE 287(g) agreements as of end of 2008 - counties that had already delegated some
# immigration enforcement authority to local law enforcement before SC's rollout. hand-built from MPI's
# "Delegation and Divergence" Appendix 2.
agreements_287g_raw <- tribble(
  ~state, ~agency_name,                              ~model,              ~date_signed, ~geography_level, ~county_guess,
  "AL", "Alabama Department of Public Safety",       "Task Force",        "09/10/2003", "state",  NA,
  "AL", "Etowah County Sheriff's Office",             "Jail Enforcement",  "07/08/2008", "county", "Etowah",
  "AZ", "Arizona Department of Corrections",         "Jail Enforcement",  "09/16/2005", "state",  NA,
  "AZ", "Arizona Department of Public Safety",       "Jail & Task Force", "04/15/2007", "state",  NA,
  "AZ", "City of Mesa Police Department",             "Jail & Task Force", "11/19/2009", "city",   "Maricopa",
  "AZ", "City of Phoenix Police Department",          "Jail & Task Force", "03/10/2008", "city",   "Maricopa",
  "AZ", "Florence Police Department",                "Task Force",        "10/21/2009", "city",   "Pinal",
  "AZ", "Maricopa County Sheriff's Office",           "Jail Enforcement",  "02/07/2007", "county", "Maricopa",
  "AZ", "Pima County Sheriff's Office",               "Jail & Task Force", "03/10/2008", "county", "Pima",
  "AZ", "Pinal County Sheriff's Office",              "Jail & Task Force", "03/10/2008", "county", "Pinal",
  "AZ", "Yavapai County Sheriff's Office",            "Jail & Task Force", "03/10/2008", "county", "Yavapai",
  "AR", "Benton County Sheriff's Office",             "Jail & Task Force", "09/26/2007", "county", "Benton",
  "AR", "City of Springdale Police Department",       "Task Force",        "09/26/2007", "city",   "Washington",
  "AR", "Rogers Police Department",                  "Task Force",        "09/25/2007", "city",   "Benton",
  "AR", "Washington County Sheriff's Office",         "Jail & Task Force", "09/26/2007", "county", "Washington",
  "CA", "Los Angeles County Sheriff's Office",        "Jail Enforcement",  "02/01/2005", "county", "Los Angeles",
  "CA", "Orange County Sheriff's Office",             "Jail Enforcement",  "11/02/2006", "county", "Orange",
  "CA", "Riverside County Sheriff's Office",          "Jail Enforcement",  "04/28/2006", "county", "Riverside",
  "CA", "San Bernardino County Sheriff's Office",     "Jail Enforcement",  "11/19/2005", "county", "San Bernardino",
  "CO", "Colorado Department of Public Safety",      "Task Force",        "03/29/2007", "state",  NA,
  "CO", "El Paso County Sheriff's Office",            "Jail Enforcement",  "05/17/2007", "county", "El Paso",
  "CT", "City of Danbury Police Department",         "Task Force",        "10/15/2009", "city",   "Fairfield",
  "DE", "Delaware Department of Corrections",        "Jail Enforcement",  "10/15/2009", "state",  NA,
  "FL", "Bay County Sheriff's Office",                "Task Force",        "06/15/2008", "county", "Bay",
  "FL", "Collier County Sheriff's Office",            "Jail & Task Force", "08/06/2007", "county", "Collier",
  "FL", "Florida Department of Law Enforcement",     "Task Force",        "07/02/2002", "state",  NA,
  "FL", "Jacksonville Sheriff's Office",              "Jail Enforcement",  "07/08/2008", "county", "Duval",
  "GA", "Cobb County Sheriff's Office",               "Jail Enforcement",  "02/13/2007", "county", "Cobb",
  "GA", "Georgia Department of Public Safety",       "Task Force",        "07/27/2007", "state",  NA,
  "GA", "Gwinnett County Sheriff's Office",           "Jail Enforcement",  "10/15/2009", "county", "Gwinnett",
  "GA", "Hall County Sheriff's Office",               "Jail & Task Force", "02/29/2008", "county", "Hall",
  "GA", "Whitfield County Sheriff's Office",          "Jail Enforcement",  "02/04/2008", "county", "Whitfield",
  "MD", "Frederick County Sheriff's Office",          "Jail & Task Force", "02/06/2008", "county", "Frederick",
  "MN", "Minnesota Department of Public Safety",     "Task Force",        "09/22/2008", "state",  NA,
  "MO", "Missouri State Highway Patrol",             "Task Force",        "06/25/2008", "state",  NA,
  "NV", "Las Vegas Metropolitan Police Department",  "Jail Enforcement",  "09/08/2008", "city",   "Clark",
  "NH", "Hudson City Police Department",             "Task Force",        "05/05/2007", "city",   "Hillsborough",
  "NJ", "Hudson County Department of Corrections",   "Jail & Task Force", "08/11/2008", "county", "Hudson",
  "NJ", "Monmouth County Sheriff's Office",           "Jail Enforcement",  "10/15/2009", "county", "Monmouth",
  "NM", "New Mexico Department of Corrections",      "Jail Enforcement",  "09/17/2007", "state",  NA,
  "NC", "Alamance County Sheriff's Office",           "Jail Enforcement",  "01/10/2007", "county", "Alamance",
  "NC", "Cabarrus County Sheriff's Office",           "Jail Enforcement",  "08/02/2007", "county", "Cabarrus",
  "NC", "Durham Police Department",                  "Task Force",        "02/01/2008", "county", "Durham",
  "NC", "Gaston County Sheriff's Office",             "Jail Enforcement",  "02/22/2007", "county", "Gaston",
  "NC", "Guilford County Sheriff's Office",           "Task Force",        "10/15/2009", "county", "Guilford",
  "NC", "Henderson County Sheriff's Office",          "Jail Enforcement",  "06/25/2008", "county", "Henderson",
  "NC", "Mecklenburg County Sheriff's Office",        "Jail Enforcement",  "02/27/2006", "county", "Mecklenburg",
  "NC", "Wake County Sheriff's Office",               "Jail Enforcement",  "06/25/2008", "county", "Wake",
  "OH", "Butler County Sheriff's Office",             "Jail & Task Force", "02/05/2008", "county", "Butler",
  "OK", "Tulsa County Sheriff's Office",              "Jail & Task Force", "08/06/2007", "county", "Tulsa",
  "RI", "Rhode Island State Police",                 "Task Force",        "10/15/2009", "state",  NA,
  "SC", "Beaufort County Sheriff's Office",           "Task Force",        "06/25/2008", "county", "Beaufort",
  "SC", "Charleston County Sheriff's Office",         "Jail Enforcement",  "11/09/2009", "county", "Charleston",
  "SC", "York County Sheriff's Office",               "Jail Enforcement",  "10/16/2007", "county", "York",
  "TN", "Davidson County Sheriff's Office",           "Jail Enforcement",  "02/21/2007", "county", "Davidson",
  "TN", "Tennessee Department of Safety",            "Task Force",        "06/25/2008", "state",  NA,
  "TX", "Carrollton Police Department",              "Jail Enforcement",  "08/12/2008", "city",   "Dallas",
  "TX", "Farmers Branch Police Department",          "Task Force",        "07/08/2008", "city",   "Dallas",
  "TX", "Harris County Sheriff's Office",             "Jail Enforcement",  "07/20/2008", "county", "Harris",
  "UT", "Washington County Sheriff's Office",         "Jail Enforcement",  "09/22/2008", "county", "Washington",
  "UT", "Weber County Sheriff's Office",              "Jail Enforcement",  "09/22/2008", "county", "Weber",
  "VA", "Herndon Police Department",                 "Task Force",        "03/21/2007", "city",   "Fairfax",
  "VA", "Loudoun County Sheriff's Office",            "Task Force",        "06/25/2008", "county", "Loudoun",
  "VA", "Manassas Park Police Department",            "Task Force",        "03/10/2008", "city",   "Manassas Park",
  "VA", "Manassas Police Department",                "Task Force",        "03/05/2008", "city",   "Manassas",
  "VA", "Prince William County Police Department",   "Task Force",        "02/26/2008", "county", "Prince William",
  "VA", "Prince William County Sheriff's Office",     "Task Force",        "02/26/2008", "county", "Prince William",
  "VA", "Prince William-Manassas Regional Jail",      "Jail Enforcement",  "07/09/2007", "county", "Prince William",
  "VA", "Rockingham County Sheriff's Office",         "Jail & Task Force", "04/25/2007", "county", "Rockingham",
  "VA", "Shenandoah County Sheriff's Office",         "Jail & Task Force", "05/10/2007", "county", "Shenandoah",
  "MA", "Massachusetts Department of Corrections",   "Jail Enforcement",  NA,           "state",  NA,
)

# a handful of states have both an independent city and a county that share a name after removing the 
# suffix. prefer the county/parish/borough match over the independent-city match on those collisions.
fips_lookup <- fips_codes |>
  mutate(
    county_clean = str_to_title(str_remove(county, " County$| Parish$| Borough$| Census Area$| city$")),
    is_city      = str_detect(county, " city$")
  ) |>
  arrange(is_city) |>
  distinct(state, county_clean, .keep_all = TRUE)

# statewide agencies (state police/corrections/public safety departments) don't identify a single
# county, so they're excluded from this county-matched binary.
agreements_287g_controls <- agreements_287g_raw |>
  filter(geography_level %in% c("county", "city")) |>
  left_join(fips_lookup, by = c("state" = "state", "county_guess" = "county_clean")) |>
  mutate(date_signed = as.Date(date_signed, format = "%m/%d/%Y")) |>
  filter(!is.na(date_signed), date_signed <= as.Date("2008-12-31")) |>
  distinct(state, county = county_guess) |>
  mutate(preexisting_287g_2008 = 1L)

# baseline jail booking (flow) variable comes from, from Vera's Incarceration Trends county panel.
# jail_capacity_2006 above measures authorized beds (stock) but SC screening is a flow. everyone
# booked gets fingerprinted and run against the federal database regardless of how many beds exist.
# builds jail capacity flow variable averaged over 2004-2007.

jail_flow_0407 <- vera_county |>
  filter(year >= 2004, year <= 2007) |>
  group_by(county_fips) |>
  summarise(
    jail_admits_0407 = mean(total_jail_admits, na.rm = TRUE),
    jail_pop_0407    = mean(total_jail_pop,    na.rm = TRUE),
    regional_jail    = max(is_regional_jail,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(c(jail_admits_0407, jail_pop_0407), ~ifelse(is.nan(.x), NA, .x)),
    regional_jail = ifelse(is.infinite(regional_jail), NA, regional_jail),
    county_fips   = sprintf("%05d", as.integer(county_fips))
  )

# county-level flow measures (state, county). the high/low split is built in section 11
# (het_jail_flow) alongside the other exposure heterogeneity measures.
admits_split <- county_pop |>
  distinct(state, county, population) |>
  left_join(
    fips_lookup |> transmute(state, county = county_clean,
                             county_fips = paste0(state_code, county_code)),
    by = c("state", "county")
  ) |>
  left_join(jail_flow_0407, by = "county_fips") |>
  mutate(
    admits_per_1k = jail_admits_0407 / population * 1000,
    turnover_0407 = jail_admits_0407 / jail_pop_0407
  ) |>
  filter(!is.na(admits_per_1k), admits_per_1k > 0) |>
  select(state, county, admits_per_1k, turnover_0407, regional_jail)

message(sprintf(
  "jail admissions: %d counties, median %.1f admits per 1k residents",
  nrow(admits_split), median(admits_split$admits_per_1k, na.rm = TRUE)
))


## 10. merge for main analysis df ##------------------------------------------------------------------
sc_county <- sc_activation_clean |>
  left_join(county_exposure_pooled, by = c("state", "county")) |>
  mutate(
    treated         = as.integer(first_detainer_year <2011),
    early_activator = as.integer(first_detainer_year <= 2011),
    late_activator  = as.integer(first_detainer_year > 2011)
  ) |>
  select(state, county, first_detainer_year, exposure_pooled, treated, early_activator, late_activator)

# create main df by merging on state and county, filtered to agricultural counties.
# exposure_yr joins on (state, county, year) so each census year picks up that same calendar year's SC
# case rate specifically.
# year filter keeps all four ag census years (2002/2007/2012/2017); 2002 predates SC entirely so it's a
# second pre-treatment point for checking parallel trends against 2007.
main <- expenditures_clean |>
  semi_join(ag_counties, by = c("state", "county")) |>
  left_join(sc_county,          by = c("state", "county")) |>
  left_join(county_exposure_yr, by = c("state", "county", "year")) |>
  filter(year %in% c(2002, 2007, 2012, 2017),
    !is.na(treated)) |>
  mutate(
    # unique county identifier: county names repeat across states 
    county_id = paste(state, county, sep = ", "),
    post = case_when(
      year == 2017          ~ 1L,
      year %in% c(2002, 2007) ~ 0L,
      TRUE                    ~ NA_integer_
    ),
    # exposure_pooled NA -> 0: county_exposure_pooled only contains counties with at least one
    # community-channel SC removal in 2008-2013 so NA is a structural zero.
    exposure_pooled = replace_na(exposure_pooled, 0)
  ) |>
  left_join(crop_controls,            by = c("state", "county", "year")) |>
  left_join(landuse_controls,         by = c("state", "county", "year")) |>
  left_join(hired_labor_controls,     by = c("state", "county", "year")) |>
  left_join(farms_landvalue_controls, by = c("state", "county", "year")) |>
  left_join(county_foreign_born,      by = c("state", "county")) |>
  left_join(jail_capacity_controls,   by = c("state", "county")) |>
  left_join(agreements_287g_controls, by = c("state", "county")) |>
  # admits_per_1k: baseline jail booking flow (binned as het_jail_flow in section 11).
  left_join(admits_split,             by = c("state", "county")) |>
  mutate(
    # NA here is a genuine structural zero: agreements_287g_controls is
    # built from a complete roster of every county/city-level 287(g) agreement active as
    # of 2008, so a county missing from the join really had none.
    preexisting_287g_2008 = replace_na(preexisting_287g_2008, 0L)
  ) |>
  # labor-reliance measures, proxying mechanization from the labor side: harvested_acres (not
  # total_ag_acres) is the denominator for the per-acre measures since hired labor is tied to actively-
  # cropped land. migrant_farms is a farm count, so it's
  # normalized as a share of all farms (migrant_farm_share) rather than per acre.
  # harvested_acres/total_farms == 0 -> NA before dividing
  mutate(
    harvested_acres_safe     = na_if(harvested_acres, 0),
    total_farms_safe         = na_if(total_farms, 0),
    hired_workers_per_acre   = hired_workers    / harvested_acres_safe,
    log_hired_workers_per_acre   = log(hired_workers    / harvested_acres_safe),
    hired_labor_exp_per_acre = hired_labor_exp  / harvested_acres_safe,
    log_hired_labor_exp_per_acre = log(hired_labor_exp  / harvested_acres_safe),
    migrant_farm_share       = migrant_farms    / total_farms_safe,
    mech_labor_share = mech_share_broad / hired_labor_exp
  ) |>
  select(-harvested_acres_safe, -total_farms_safe)

## 11. exposure heterogeneity measures ##-----------------------------------------------------------
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

# normalize county names
county_key <- function(county) {
  county |>
    str_to_lower() |>
    str_replace("^saint |^st\\.? ", "st ") |>
    str_replace("^sainte |^ste\\.? ", "ste ") |>
    str_remove_all("[^a-z]")
}
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
    het_jail_flow_val = admits_per_1k,
    jail_capacity_imputed
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
  left_join(het_measures |> select(-county, -jail_capacity_imputed), by = c("state", "county_key")) |>
  select(-county_key)

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

# noncitizen split used for the SC removal-rate panels 
noncit_split <- county_foreign_born |>
  transmute(state, county, noncit_bin = as.integer(noncitizen_share_2009 >= 0.01))

## 12. SC removal-rate panels ##--------------------------------------------------------------------
# state, county, year (2008-2017) panel of community-channel SC removals per 10,000 population.
# community-channel counting as county_exposure_pooled/county_exposure_yr above.
sc_detainer_rate_yearly <- county_pop |>
  distinct(state, county, population) |>
  semi_join(ag_counties, by = c("state", "county")) |>
  cross_join(tibble(year = 2008:2017)) |>
  left_join(
    sc_trac_clean |>
      filter(year >= 2008, year <= 2017, community_channel) |>
      group_by(state, county, year) |>
      summarise(cases = n(), .groups = "drop"),
    by = c("state", "county", "year")
  ) |>
  mutate(
    cases = replace_na(cases, 0),
    detainer_rate = cases / population * 10000
  ) |>
  select(state, county, year, detainer_rate)

## 13. rollout map inputs ##------------------------------------------------------------------------
# first activation year per county (all counties, not just ag) for the rollout maps
sc_rollout <- sc_activation_clean |>
  group_by(state, county) |>
  summarise(first_detainer_year = min(first_detainer_year, na.rm = TRUE), .groups = "drop")

## 14. ACS ag workforce by nativity (IPUMS) ##------------------------------------------------------
# yearly (2006-2020) county counts of employed working-age (16-64) ag workers, total and by nativity,
# from the IPUMS USA ACS 1-year extract. ag = IND1990 010 crops, 011 livestock, 030 ag services n.e.c.
# (excludes 012 veterinary, 020 landscaping, 031 forestry, 032 fishing). foreign-born = CITIZEN 2-5
# (naturalised, noncitizen, not reported); US-born = CITIZEN 0-1 (born in US/territories or abroad to
# US parents). noncitizen (CITIZEN 3-5) is kept as the closest available undocumented proxy.
#
# the microdata only identify PUMAs (100k+ residents), so each PUMA's totals are allocated to its
# counties by population share. rural counties sharing a PUMA therefore get the same ag share of
# population - this measure cannot distinguish a farm county from a town county within one PUMA.
# 2006-2011 samples use 2000 PUMAs, 2012-2020 use 2010 PUMAs; each gets its own crosswalk.
# 2020 ACS was collected under COVID with experimental weights - treat with caution.
IPUMS_FILE   <- file.path(DROPBOX, "ipums_usa", "acs_2005_2022_sc_agriculture.dta")
AG_IND1990   <- c(10, 11, 30)
ACS_YEARS    <- 2006:2020
ACS_BASELINE <- 2006:2007

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

# allocation factors: share of each PUMA's population living in each county.
# IPUMS merges Louisiana PUMAs 01801, 01802, 01905 (New Orleans) into 77777 in the 2006-2011 samples
# after Katrina, so those three are pooled under 77777 in the 2000 crosswalk.
puma_afactors <- function(xw) {
  xw |>
    group_by(statefip, puma, county_fips) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") |>
    group_by(statefip, puma) |>
    mutate(afactor = pop / sum(pop)) |>
    ungroup() |>
    select(statefip, puma, county_fips, afactor)
}
afactor_2000 <- puma_xwalks$puma2000 |>
  mutate(puma = if_else(statefip == "22" & puma %in% c("01801", "01802", "01905"), "77777", puma)) |>
  puma_afactors()
afactor_2010 <- puma_afactors(puma_xwalks$puma2010)

# PUMA x year totals from the microdata, read one year at a time (rows are stored in sample order).
acs_puma_file <- file.path(CLEAN_DIR, "acs_puma_year.rds")
if (!file.exists(acs_puma_file)) {
  year_blocks <- read_dta(IPUMS_FILE, col_select = year) |>
    count(year = as.integer(year)) |>
    mutate(skip = lag(cumsum(n), default = 0)) |>
    filter(year %in% ACS_YEARS)

  acs_puma_year <- year_blocks |>
    pmap(\(year, n, skip) {
      d <- read_dta(IPUMS_FILE, col_select = c(year, statefip, puma, perwt, age, citizen, empstat, ind1990),
                    skip = skip, n_max = n) |> zap_labels()
      stopifnot(all(d$year == year))
      d |>
        filter(age >= 16, age <= 64) |>
        mutate(ag = empstat == 1 & ind1990 %in% AG_IND1990) |>
        group_by(year, statefip = sprintf("%02d", statefip), puma = sprintf("%05d", puma)) |>
        summarise(
          wa_pop    = sum(perwt),
          ag_total  = sum(perwt * ag),
          ag_fb     = sum(perwt * (ag & citizen %in% 2:5)),
          ag_usb    = sum(perwt * (ag & citizen %in% 0:1)),
          ag_noncit = sum(perwt * (ag & citizen %in% 3:5)),
          .groups = "drop"
        )
    }) |>
    bind_rows()
  saveRDS(acs_puma_year, acs_puma_file)
}
acs_puma_year <- readRDS(acs_puma_file)

# allocate PUMA totals to counties
acs_county_year <- bind_rows(
  acs_puma_year |> filter(year <= 2011) |> inner_join(afactor_2000, by = c("statefip", "puma"), relationship = "many-to-many"),
  acs_puma_year |> filter(year >= 2012) |> inner_join(afactor_2010, by = c("statefip", "puma"), relationship = "many-to-many")
) |>
  group_by(county_fips, year) |>
  summarise(across(c(wa_pop, ag_total, ag_fb, ag_usb, ag_noncit), \(x) sum(x * afactor)), .groups = "drop")

# shares of the county's baseline (2006-07 average) working-age population, plus the baseline shares
acs_ag_panel <- acs_county_year |>
  group_by(county_fips) |>
  mutate(
    wa_pop_base = mean(wa_pop[year %in% ACS_BASELINE]),
    across(c(ag_total, ag_fb, ag_usb, ag_noncit),
           list(share      = \(x) x / wa_pop_base,
                share_base = \(x) mean(x[year %in% ACS_BASELINE]) / wa_pop_base))
  ) |>
  ungroup() |>
  left_join(
    fips_codes |> transmute(state, county_fips = paste0(state_code, county_code),
                            is_city = str_detect(county, " city$"),
                            county = str_to_title(str_remove(county, " County$| Parish$| Borough$| Census Area$| city$"))),
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

## 15. long panel for the ACS labor outcomes ##-----------------------------------------------------
# county x year (2006-2020) analysis panel: the same counties as main, with the ACS ag workforce
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
  left_join(het_measures |> select(-county, -jail_capacity_imputed), by = c("state", "county_key")) |>
  mutate(early_activator = het_early, first_detainer_year = het_early_val) |>
  select(-county_key)
stopifnot(!anyDuplicated(acs_panel[c("county_id", "year")]))

message(sprintf("ACS long panel: %d counties x %d years (%d of %d main counties)",
                n_distinct(acs_panel$county_id), n_distinct(acs_panel$year),
                n_distinct(acs_panel$county_id), n_distinct(main$county_id)))

## 16. commuting-zone version of the ACS long panel ##----------------------------------------------
# CZs (Autor-Dorn, 741 nationally) usually contain whole PUMAs, so CZ outcomes are close to direct
# survey estimates rather than population allocations, and the same fixed CZs are used under both the
# 2000 and 2010 PUMA definitions. treatment and het measures are aggregated from counties.
cw_dir <- file.path(DROPBOX, "dorn_crosswalks")
cw_puma2000 <- read_dta(file.path(cw_dir, "cw_puma2000_czone.dta")) |> zap_labels()
cw_puma2010 <- read_dta(file.path(cw_dir, "cw_puma2010_czone.dta")) |> zap_labels()
cw_cty_cz   <- read_dta(file.path(cw_dir, "cw_cty_czone.dta")) |> zap_labels() |>
  transmute(county_fips = sprintf("%05d", as.integer(cty_fips)), czone = as.integer(czone))

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

acs_cz_year <- bind_rows(
  acs_puma_year |> filter(year <= 2011) |> inner_join(cz_afactor_2000, by = c("statefip", "puma"), relationship = "many-to-many"),
  acs_puma_year |> filter(year >= 2012) |> inner_join(cz_afactor_2010, by = c("statefip", "puma"), relationship = "many-to-many")
) |>
  group_by(czone, year) |>
  summarise(across(c(wa_pop, ag_total, ag_fb, ag_usb, ag_noncit), \(x) sum(x * afactor)), .groups = "drop")

# every PUMA's workers should land in some CZ (dorn's 2000 factors are rounded, summing to 0.998-1.002
# per PUMA, so totals match to within rounding rather than exactly)
cz_total_gap <- sum(acs_cz_year$ag_total) / sum(acs_puma_year$ag_total) - 1
stopifnot(abs(cz_total_gap) < 0.002)

# county-level inputs keyed on FIPS. names map to FIPS through fips_lookup (county preferred over a
# same-named independent city), and 2010 populations come straight from the census by GEOID.
county_pop_fips <- get_decennial(geography = "county", variables = "P001001", year = 2010) |>
  transmute(county_fips = GEOID, pop = value)
name_to_fips <- fips_lookup |>
  transmute(state, county_key = county_key(county_clean), county_fips = paste0(state_code, county_code)) |>
  distinct(state, county_key, .keep_all = TRUE)

cz_county_inputs <- cw_cty_cz |>
  inner_join(county_pop_fips, by = "county_fips") |>
  left_join(
    sc_activation_clean |>
      transmute(state, county_key = county_key(county), first_detainer_year) |>
      group_by(state, county_key) |>
      summarise(first_detainer_year = min(first_detainer_year), .groups = "drop") |>
      inner_join(name_to_fips, by = c("state", "county_key")) |>
      select(county_fips, first_detainer_year),
    by = "county_fips"
  ) |>
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

# sample: CZs containing at least one ag-sample county
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
    across(c(ag_total, ag_fb, ag_usb, ag_noncit),
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

## 17. QCEW ag employment long panel ##------------------------------------------------------------
# yearly county annual-average employment from the BLS QCEW (employer UI filings), private ownership,
# for NAICS 11 (ag, forestry, fishing, hunting), 111 (crop production) and 112 (animal production).
# 4-digit codes (1151 incl. farm labor contractors) are too suppressed in ag counties to use.
# UI only covers ag employers above the FUTA threshold ($20k quarterly payroll or 10+ workers in 20
# weeks; some states cover more) and mostly exempts H-2A workers, so this is formal payroll
# employment at larger operations. counts are direct county figures, so county clustering is fine.
# suppressed cells (disclosure_code "N") are reported as 0 and are set to missing. an outcome is
# kept only for counties disclosed in every year of QCEW_YEARS (a balanced panel per industry), so
# suppression switching on and off cannot drive the estimates. 2000-2004 are kept as optional extra
# leads; they are not part of the balance check. outcomes are shares of the county's 2006-07
# working-age population (from the ACS allocation in section 14), the same units as the ACS outcomes.
QCEW_FILE     <- file.path(DROPBOX, "generated_data", "qcew_ag_county_annual_2005_2022.dta")
QCEW_NAICS    <- c(`11` = "qcew_11", `111` = "qcew_111", `112` = "qcew_112")
QCEW_YEARS    <- 2005:2022

qcew_county_year <- read_dta(QCEW_FILE) |>
  zap_labels() |>
  filter(own_code == 5, agglvl_code %in% 74:76, industry_code %in% names(QCEW_NAICS)) |>
  transmute(
    county_fips = area_fips,
    year        = as.integer(year),
    outcome     = QCEW_NAICS[industry_code],
    emp         = if_else(disclosure_code == "N", NA_real_, as.numeric(annual_avg_emplvl))
  ) |>
  group_by(county_fips, outcome) |>
  mutate(balanced = sum(!is.na(emp[year %in% QCEW_YEARS])) == length(QCEW_YEARS),
         emp      = if_else(balanced, emp, NA_real_)) |>
  ungroup() |>
  select(-balanced) |>
  pivot_wider(names_from = outcome, values_from = emp)

qcew_panel <- main |>
  distinct(state, county, county_id) |>
  mutate(county_key = county_key(county)) |>
  inner_join(name_to_fips, by = c("state", "county_key")) |>
  inner_join(qcew_county_year, by = "county_fips") |>
  left_join(acs_ag_panel |> distinct(county_fips, wa_pop_base), by = "county_fips") |>
  mutate(across(all_of(unname(QCEW_NAICS)), \(x) x / wa_pop_base, .names = "{.col}_share")) |>
  left_join(het_measures |> select(-county, -jail_capacity_imputed), by = c("state", "county_key")) |>
  mutate(early_activator = het_early, first_detainer_year = het_early_val) |>
  select(-county_key)
stopifnot(!anyDuplicated(qcew_panel[c("county_id", "year")]))

message(sprintf(
  "QCEW panel: %d of %d main counties; balanced %s counties (NAICS 11 / 111 / 112)",
  n_distinct(qcew_panel$county_id), n_distinct(main$county_id),
  paste(map_int(unname(QCEW_NAICS), \(v) n_distinct(qcew_panel$county_id[!is.na(qcew_panel[[paste0(v, "_share")]])])),
        collapse = " / ")
))

## 18. crop mix: labor-intensive acreage ##---------------------------------------------------------
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
  left_join(het_measures |> select(-county, -jail_capacity_imputed), by = c("state", "county_key")) |>
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

####################################################################################################
### write outputs ###
####################################################################################################

saveRDS(main,                    file.path(CLEAN_DIR, "main.rds"))
saveRDS(sc_county,               file.path(CLEAN_DIR, "sc_county.rds"))
saveRDS(noncit_split,            file.path(CLEAN_DIR, "noncit_split.rds"))
saveRDS(het_measures,            file.path(CLEAN_DIR, "het_measures.rds"))
saveRDS(het_config,              file.path(CLEAN_DIR, "het_config.rds"))
saveRDS(sc_detainer_rate_yearly, file.path(CLEAN_DIR, "sc_detainer_rate_yearly.rds"))
saveRDS(ag_counties,             file.path(CLEAN_DIR, "ag_counties.rds"))
saveRDS(sc_rollout,              file.path(CLEAN_DIR, "sc_rollout.rds"))
saveRDS(all_removals_clean,      file.path(CLEAN_DIR, "all_removals_clean.rds"))
saveRDS(acs_ag_panel,            file.path(CLEAN_DIR, "acs_ag_panel.rds"))
saveRDS(acs_panel,               file.path(CLEAN_DIR, "acs_panel.rds"))
saveRDS(acs_cz_panel,            file.path(CLEAN_DIR, "acs_cz_panel.rds"))
saveRDS(qcew_panel,              file.path(CLEAN_DIR, "qcew_panel.rds"))
saveRDS(crop_panel,              file.path(CLEAN_DIR, "crop_panel.rds"))
saveRDS(crop_cz_panel,           file.path(CLEAN_DIR, "crop_cz_panel.rds"))

# csv copy of main df for eyeballing - downstream scripts read main.rds
write_csv(main, file.path(CLEAN_DIR, "main_inspect.csv"), na = "")
