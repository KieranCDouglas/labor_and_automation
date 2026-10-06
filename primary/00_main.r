####################################################################################################
## main script for data cleaning and analysis
## last edited: 09/24/2026
## by kieran
####################################################################################################

####################################################################################################
### prelude ###
####################################################################################################

install.packages("tidyverse")
install.packages("tidycensus")
install.packages("fixest")
install.packages("broom")
install.packages("triplediff")
install.packages("haven")

library(tidyverse)
library(tidycensus)
library(fixest)
library(broom)
library(triplediff)
library(haven)

####################################################################################################
### load data ###
####################################################################################################

expenditures <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/expenditures_all_states_wide.csv")
sc_trac <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/secure1904.csv")
crops  <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/crops_area_harvested.csv")
landuse <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/landuse.csv")
sc_ice <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/sc_activation_dates.csv")
ag_typology <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/ers_county_typology_2015.csv")
hired_labor <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/hired_labor.csv")
farms_landvalue <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/farms_landvalue.csv")
harvested_by_farmsize <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/harvested_cropland_by_farmsize.csv")
all_removals_trac <- read.csv("/Users/kieran/Library/CloudStorage/Dropbox/secure_communities/TRAC/all_deportations/removals.csv")
jail_facilities_2006 <- read_dta("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/jail_facilities_2006_icpsr26602.dta")
zcta_county_crosswalk <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/zcta_county_rel_10.csv")

vera_county <- read_csv(
  "/Users/kieran/Documents/GitHub/labor_and_automation/data/main/vera_incarceration_trends_county.csv",
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
    noncitizen   = "B05001_006"   
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
    noncitizen_count_2009 = noncitizen
  ) |>
  select(state, county, foreign_born_share_2009, noncitizen_share_2009, foreign_born_count_2009, noncitizen_count_2009)

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
#   (b) farmland acreage share: ≥1% of county land area in farms as of 2002 (pre-SC baseline)
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
write_csv(ag_counties_flagged, "/Users/kieran/Library/CloudStorage/Dropbox/secure_communities/agcensus/ag_counties_flagged.csv")

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
# counties with identicl migrant share of working population may have heterogeneity in realized SC-related
# deportations. this is likely a function of local jail processing, enforcement strength, preexisting agreements etc.
# here we want to load and clean data that will act as both an exposure heterogeneity proxy and as a test for
# the extent to which those underlying baseline characteristics predict deportation intensity ex post.

# local jail capacity baseline, from BJS's Census of Jail Facilities, 2006 (ICPSR 26602) - the last full
# census of US jail jurisdictions before SC's 2008 rollout, so it's a clean pre-treatment measure.
# the file has no county/FIPS field (only zip and state), so county is assigned via the Census
# ZCTA-to-county relationship file: for each zip, take the county holding the largest share of that
# ZCTA's population (some ZCTAs straddle county lines).
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
  transmute(agency_id = V1, agency_name = V2, state = V7, zip = V8, rated_capacity_total)

# primary match: zip -> county via ZCTA
jail_capacity_matched <- jail_facility_capacity |>
  left_join(zcta_county_lookup, by = c("zip", "state"))

# fallback: ~5% of agencies (disproportionately large CA county jail systems, e.g. Orange, San Diego,
# Riverside) use a unique administrative zip with no residential population, so they never appear in
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
# missing capacity data, not that capacity is zero.
jail_capacity_controls <- jail_capacity_matched |>
  filter(!is.na(county)) |>
  group_by(state, county) |>
  summarise(jail_capacity_2006 = safe_sum(rated_capacity_total, TRUE), .groups = "drop")

# confirm preexisting ICE 287(g) agreements as of end of 2008 - counties that had already delegated some
# immigration enforcement authority to local law enforcement before SC's rollout. hand-built from MPI's
# "Delegation and Divergence" Appendix 2 (active MOAs, August 2010: state, agency, model, date signed),
# since that's the standard academic source and its table is small/clean enough to transcribe directly.
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

# a handful of states have both an independent city and a county that share a name after suffix-
# stripping (e.g. VA's Fairfax County vs. Fairfax city). prefer the county/parish/borough match over
# the independent-city match on those collisions.
fips_lookup <- fips_codes |>
  mutate(
    county_clean = str_to_title(str_remove(county, " County$| Parish$| Borough$| Census Area$| city$")),
    is_city      = str_detect(county, " city$")
  ) |>
  arrange(is_city) |>
  distinct(state, county_clean, .keep_all = TRUE)

# statewide agencies (state police/corrections/public safety departments) don't identify a single
# county, so they're excluded from this county-matched binary; kept in agreements_287g_raw above in
# case a state-level control is wanted later.
agreements_287g_controls <- agreements_287g_raw |>
  filter(geography_level %in% c("county", "city")) |>
  left_join(fips_lookup, by = c("state" = "state", "county_guess" = "county_clean")) |>
  mutate(date_signed = as.Date(date_signed, format = "%m/%d/%Y")) |>
  filter(!is.na(date_signed), date_signed <= as.Date("2008-12-31")) |>
  distinct(state, county = county_guess) |>
  mutate(preexisting_287g_2008 = 1L)

# baseline jail booking FLOW, from Vera's Incarceration Trends county panel.
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

# county-level flow measures keyed on (state, county), parallel to noncit_split below so it can be
# joined into the yearly-rate plots the same way. admits_bin splits at the median; change
# admits_bin_cut to rebalance the DDD cells.
admits_bin_cut <- NULL  # NULL -> median; set a number (admissions per 1k residents) to override

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
  mutate(
    admits_bin = as.integer(
      admits_per_1k >= (if (is.null(admits_bin_cut)) median(admits_per_1k, na.rm = TRUE) else admits_bin_cut)
    )
  ) |>
  select(state, county, admits_per_1k, turnover_0407, regional_jail, admits_bin)

message(sprintf(
  "jail admissions: %d counties, median %.1f admits per 1k residents, %d high / %d low",
  nrow(admits_split), median(admits_split$admits_per_1k, na.rm = TRUE),
  sum(admits_split$admits_bin == 1), sum(admits_split$admits_bin == 0)
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
    # unique county identifier: county names repeat across states (100 of the 635 names in the
    # analysis sample appear in >1 state), so FE or clustering on the bare name would pool
    # distinct counties. 
    county_id = paste(state, county, sep = ", "),
    post = case_when(
      year == 2017          ~ 1L,
      year %in% c(2002, 2007) ~ 0L,
      TRUE                    ~ NA_integer_
    ),
    # exposure_pooled NA -> 0: county_exposure_pooled only contains counties with at least one
    # community-channel SC removal in 2008-2013 (see its construction above), so NA is a structural zero.
    # safe to fill, unlike the ag-census-derived covariates below.
    exposure_pooled = replace_na(exposure_pooled, 0)
  ) |>
  left_join(crop_controls,            by = c("state", "county", "year")) |>
  left_join(landuse_controls,         by = c("state", "county", "year")) |>
  left_join(hired_labor_controls,     by = c("state", "county", "year")) |>
  left_join(farms_landvalue_controls, by = c("state", "county", "year")) |>
  left_join(county_foreign_born,      by = c("state", "county")) |>
  left_join(jail_capacity_controls,   by = c("state", "county")) |>
  left_join(agreements_287g_controls, by = c("state", "county")) |>
  # admits_bin / admits_per_1k: baseline jail booking flow, an alternative to noncit_bin as the third
  # DDD dimension. the two are close to orthogonal (r = 0.15), so this is not a relabelled noncit_bin.
  # NA stays NA - a county missing from Vera's panel has unknown throughput, not zero.
  left_join(admits_split,             by = c("state", "county")) |>
  mutate(
    # unlike jail_capacity_2006, NA here is a genuine structural zero: agreements_287g_controls is
    # built from a complete roster of every county/city-level 287(g) agreement active as
    # of 2008, so a county missing from the join really had none.
    preexisting_287g_2008 = replace_na(preexisting_287g_2008, 0L)
  ) |>
  # labor-reliance measures, proxying mechanization from the labor side: harvested_acres (not
  # total_ag_acres) is the denominator for the per-acre measures since hired labor is tied to actively-
  # cropped land, not pasture. migrant_farms is a farm count, not a worker count, so it's
  # normalized as a share of all farms (migrant_farm_share) rather than per acre.
  # harvested_acres/total_farms == 0 -> NA before dividing: landuse_controls/farms_landvalue_controls
  # don't distinguish "0 reported" from "unreported/suppressed" the way hired_labor_controls' safe_sum
  # does, so a 0 denominator here is a data gap.
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

####################################################################################################
### event study justification ###
####################################################################################################
## secure communities-related exposure intensity over time ##---------------------------------------
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

# lets visualize this
sc_detainer_rate_filtered <- sc_detainer_rate_yearly |>
  group_by(state, county) |>
  filter(sum(detainer_rate > 0) >= 2) |>
  ungroup()

# define quartiles as under count of hired workers per acre to differentiate high versus low labor dependence 
worker_quartiles <- main |>
  filter(year == 2007) |>
  distinct(state, county, hired_workers_per_acre)
worker_cutoffs <- quantile(worker_quartiles$hired_workers_per_acre, probs = c(.25, .5, .75), na.rm = TRUE)
worker_quartiles <- worker_quartiles |>
  mutate(worker_quartile = case_when(
    is.na(hired_workers_per_acre)         ~ NA_character_,
    hired_workers_per_acre <= worker_cutoffs[1] ~ "Q1 (lowest)",
    hired_workers_per_acre <= worker_cutoffs[2] ~ "Q2",
    hired_workers_per_acre <= worker_cutoffs[3] ~ "Q3",
    TRUE                                         ~ "Q4 (highest)"
  )) |>
  select(state, county, worker_quartile)

detainer_plot_data <- sc_detainer_rate_filtered |>
  left_join(worker_quartiles, by = c("state", "county")) |>
  filter(!is.na(worker_quartile))

county_gradient_palette <- colorRampPalette(c("#7CA982", "#E0EEC6", "#f4a259", "#243E36", "#bc4b51"))(
  n_distinct(detainer_plot_data$state)
)
# directory for figures
FIGS_DIR <- "/Users/kieran/Documents/GitHub/labor_and_automation/figs/primary"

# create figure showing heterogeneity of detainer rate by county between hired worker per acre quartiles
het_dose_fig <- ggplot(
  data = detainer_plot_data,
  aes(x = year, y = detainer_rate, color = state, group = interaction(state, county))) +
  geom_smooth(method = loess, weight = .5, linewidth = 0.4, se = FALSE) +
  facet_wrap(~worker_quartile) +
  scale_color_manual(values = county_gradient_palette) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Community-Channel SC Removal Rate Over Time Per County (By Hired-Worker-Per-Acre Quartile)",
      y = "Community-Channel SC Removals Per 10,000 Population", x = "Year") +
  ylim(0,18)
ggsave(file.path(FIGS_DIR, "het_dose_fig.png"), het_dose_fig,
       width = 10, height = 7, dpi = 300)

## now to visualize some very basic pretrends between early versus late activation ##---------------------------------------
# pretrends with mech share 
pretrend_fig <- main |>
  filter(!is.na(mech_share_broad)) |>
  ggplot(aes(
    x = year, y = mech_share_narrow,
    color = factor(early_activator, labels = c("Late activator (2011+)", "Early activator (<2011)"))
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
    scale_color_manual(values = c("#7CA982", "#243E36")) +
  theme_minimal() +
  labs(color = NULL, x = "Year", y = "Mechanization Share of Expenditures", title = "Mechanization Share of Expenditures Over Year by Activation Timing")
print(pretrend_fig)

# pretreds with hired_workers_per_acre
pretrend_labor <- main |>
  filter(!is.na(hired_workers_per_acre)) |>
  ggplot(aes(
    x = year, y = hired_workers_per_acre,
    color = factor(early_activator, labels = c("Late activator (2011+)", "Early activator (<2011)"))
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  scale_color_manual(values = c("#7CA982", "#243E36")) +
  theme_minimal() +
  labs(color = NULL, x = "Year", y = "Hired Workers Per Acre", title = "Hired Workers Per Acre Over Year by Activation Timing")
print(pretrend_labor)

# pretrends with hired_labor_exp_per_acre 
pretrend_hired <- main |>
  filter(!is.na(hired_labor_exp_per_acre)) |>
  ggplot(aes(
    x = year, y = hired_labor_exp_per_acre,
    color = factor(early_activator, labels = c("Late activator (2011+)", "Early activator (<2011)"))
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
    scale_color_manual(values = c("#7CA982", "#243E36")) +
  theme_minimal() +
  labs(color = NULL, x = "Year", y = "Hired Labor Expenditures Per Acre", title = "Hired Labor Expenditures Per Acre Over Year by Activation Timing")
print(pretrend_hired)

## removals over time per subgroup ##---------------------------------------
# time path of detentions broken down by early/late exposure and high/low noncit share
noncit_split <- county_foreign_born |>
  transmute(state, county, noncit_bin = as.integer(noncitizen_share_2009 >= 0.01))

ddd_pretrend_detainer <- sc_detainer_rate_yearly |>
  left_join(sc_county |> select(state, county, early_activator), by = c("state", "county")) |>
  left_join(noncit_split, by = c("state", "county")) |>
  filter(!is.na(early_activator), !is.na(noncit_bin), year <= 2015) |>
  ggplot(aes(
    x = year, y = detainer_rate,
    color = factor(early_activator, levels = c(0, 1),
                   labels = c("Late activator (2011+)", "Early activator (<2011)")),
    linetype = factor(noncit_bin, levels = c(1, 0),
                      labels = c("High noncitizen share", "Low noncitizen share"))
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("#4F8A5B", "#243E36")) +
  theme_minimal() +
  labs(
    color = NULL, linetype = NULL,
    x = "Year", y = "Community-Channel SC Removals Per 10,000 Population",
    title = "Community-Channel SC Removal Rate Over Time by Activation Timing and Noncitizen Share"
  )
print(ddd_pretrend_detainer)

# ES of detentions path broken down by early/late exposure and high/low noncit share 
# same four-way split in event time: removal rate relative to each county's own activation year
# we want to include error bars so the first step is to fit an es regression and pull its coefficients
es_group_data <- sc_detainer_rate_yearly |>
  left_join(sc_county |> select(state, county, early_activator, first_detainer_year),
            by = c("state", "county")) |>
  left_join(noncit_split, by = c("state", "county")) |>
  filter(!is.na(early_activator), !is.na(noncit_bin), year <= 2015) |>
  mutate(
    rel_year = year - first_detainer_year,
    county_id = paste(state, county, sep = ", ")
  ) |>
  filter(rel_year >= -4, rel_year <= 4)
# define groups of interest and fit individual models per group
group_defs <- list(
  "Late activator (2011+) / High noncitizen share (>1% pop.)" = list(early = 0, noncit = 1),
  "Late activator (2011+) / Low noncitizen share (≤1% pop.)"  = list(early = 0, noncit = 0),
  "Early activator (<2011) / High noncitizen share (>1% pop.)" = list(early = 1, noncit = 1),
  "Early activator (<2011) / Low noncitizen share (≤1% pop.)"  = list(early = 1, noncit = 0)
)
es_cellmeans <- group_defs |>
  map(\(g) es_group_data |> filter(early_activator == g$early, noncit_bin == g$noncit)) |>
  map(\(d) feols(detainer_rate ~ 0 + factor(rel_year), data = d, cluster = ~county_id)) |>
  map(\(mod) tidy(mod, conf.int = TRUE, conf.level = 0.95)) |>
  bind_rows(.id = "group") |>
  mutate(rel_year = as.integer(str_extract(term, "-?\\d+$"))) |>
  separate(group, into = c("activation_label", "noncit_label"), sep = " / ")
# plot those model outputs 
ggplot(es_cellmeans, aes(x = rel_year, y = estimate,
                          color = activation_label, linetype = noncit_label,
                          fill = activation_label,
                          group = interaction(activation_label, noncit_label))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_x_continuous(breaks = -4:4) +
  scale_color_manual(values = c("#4F8A5B", "#243E36")) +
  scale_fill_manual(values = c("#4F8A5B", "#243E36")) +
  theme_minimal() +
  labs(color = NULL, linetype = NULL, fill = NULL,
       x = "Years Relative to SC Activation", y = "SC Removals Per 10,000 Population",
       title = "SC Removals Schedule Relative to Activation Year")

####################################################################################################
### ES per period ###
####################################################################################################
# this runs an es-style did for each of the three periods: pre, interim, and post
# compares early to late activated counties to understand how program duration affects outcomes
# first difs are between pre and post period for early and late activations while second are difs
# between those two. for now, we ignore exposure intensity heterogeneity and endogeneity of regressors. 
# early is defined at pre 2012 and late is defined at post 2012 rollout. 

## starting off with log_hired_labor_exp_per_acre as the outcome variable
# first model looks at pre trends between early versus late rollout counties
pre_mod1 <- feols(
  hired_labor_exp_per_acre ~ early_activator * i(year, ref = 2002) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2002, 2007)),
  cluster = ~county_id
)
summary(pre_mod1)

# second model looks at interim trends between early versus late rollout counties
int_mod1 <- feols(
  hired_labor_exp_per_acre ~ early_activator * i(year, ref = 2007) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2007, 2012)),
  cluster = ~county_id
)
summary(int_mod1)

# third model looks at longer-run differences between early versus late rollout counties
lr_mod1 <- feols(
  hired_labor_exp_per_acre ~ early_activator * i(year, ref = 2012) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2012, 2017)),
  cluster = ~county_id
)
summary(lr_mod1)
# combine coefficients
etable(pre_mod1, int_mod1, lr_mod1)

## now looking at hired_workers_per_acre
# first model looks at pre trends between early versus late rollout counties
pre_mod2 <- feols(
  hired_workers_per_acre ~ early_activator * i(year, ref = 2002) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2002, 2007)),
  cluster = ~county_id
)
summary(pre_mod2)

# second model looks at interim trends between early versus late rollout counties
int_mod2 <- feols(
  hired_workers_per_acre ~ early_activator * i(year, ref = 2007) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2007, 2012)),
  cluster = ~county_id
)
summary(int_mod2)

# third model looks at longer-run differences between early versus late rollout counties
lr_mod2 <- feols(
  hired_workers_per_acre ~ early_activator * i(year, ref = 2012) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2012, 2017)),
  cluster = ~county_id
)
summary(lr_mod2)
# combine coefficients
etable(pre_mod2, int_mod2, lr_mod2)

## finally looking at mech_share_broad
# first model looks at pre trends between early versus late rollout counties
pre_mod3 <- feols(
  mech_share_broad ~ early_activator * i(year, ref = 2002) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2002, 2007)),
  cluster = ~county_id
)
summary(pre_mod3)

# second model looks at interim trends between early versus late rollout counties
int_mod3 <- feols(
  mech_share_broad ~ early_activator * i(year, ref = 2007) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2007, 2012)),
  cluster = ~county_id
)
summary(int_mod3)

# third model looks at longer-run differences between early versus late rollout counties
lr_mod3 <- feols(
  mech_share_broad ~ early_activator * i(year, ref = 2012) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2012, 2017)),
  cluster = ~county_id
)
summary(lr_mod3)

# fourth model looks at 2007 to 2017 differences between early versus late rollout counties
final_mod4 <- feols(
  mech_share_broad ~ early_activator * i(year, ref = 2007) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2007, 2017)),
  cluster = ~county_id
)
summary(final_mod4)

# combine coefficients
etable(pre_mod3, int_mod3, lr_mod3, final_mod4)

# all table
etable(pre_mod1, int_mod1, lr_mod1, pre_mod2, int_mod2, lr_mod2,pre_mod3, int_mod3, lr_mod3, final_mod4)

####################################################################################################
### full es: year-specific treatment effects ###
####################################################################################################
# instead of the pairwise two-period comparisons above, estimate all years at once:
# y_jt = sum_s beta_s * early_j * 1{t = s} + gamma_j + delta_{t,state(j)} + e_jt
# ref year is 2007 (last pre-activation census), so beta_2002 is a placebo/pretrend check
# and beta_2012, beta_2017 trace out the dynamic treatment effects. the early_j main effect
# is absorbed by county FE and the year main effects by state^year FE.

es_full1 <- feols(
  hired_labor_exp_per_acre ~ i(year, early_activator, ref = 2007) | county_id + state^year,
  data = main,
  cluster = ~county_id
)
summary(es_full1)

es_full2 <- feols(
  hired_workers_per_acre ~ i(year, early_activator, ref = 2007) | county_id + state^year,
  data = main,
  cluster = ~county_id
)
summary(es_full2)

es_full3 <- feols(
  mech_share_broad ~ i(year, early_activator, ref = 2007) | county_id + state^year,
  data = main,
  cluster = ~county_id
)
summary(es_full3)

# combine coeffs
etable(es_full1, es_full2, es_full3, tex = TRUE)

# plot point estimates and 95% CIs for the year-specific coefficients
es_plot_data <- list(
  "Hired Labor Exp Per Acre" = es_full1,
  "Hired Workers Per Acre"   = es_full2,
  "Mechanization Share" = es_full3
) |>
  map(\(mod) tidy(mod, conf.int = TRUE, conf.level = 0.95)) |>
  bind_rows(.id = "outcome") |>
  mutate(year = as.integer(str_extract(term, "\\d{4}"))) |>
  bind_rows(
    expand_grid(
      outcome = c("Hired Labor Exp Per Acre", "Hired Workers Per Acre",
                  "Mechanization Share"),
      year = 2007, estimate = 0, conf.low = 0, conf.high = 0
    )
  )

es_full_fig <- ggplot(es_plot_data, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.6, color = "#7CA982") +
  geom_point(size = 2, color = "#243E36") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  theme_minimal() +
  labs(
    x = "Census Year", y = "Estimate Relative to 2007",
    title = "Year-Specific Effects of Early SC Activation (95% CI)"
  )
print(es_full_fig)

####################################################################################################
### ddd justification ###
####################################################################################################
# the idea here is that we can measure the change in pre versus post sc activation differences in 
# mechanization and labor by some third characteristic like crop mix or baseline ACS-estimated migrant pop
# in counties that have vs have not activated. 
# this model comes in the following form: δ_DDD = δ_GST = [((y_111-y_101)-(y_011-y_001))-((y_110-y_100)-(y_010-y_000))]
# where G is the treatment/control group, S is the third dim partition, and T is the time window.

# starting with ptrends for DDD justification i will make a few plots
# define the third dif cohorts: what could plausibly involve differential treatment effects across early and late treated groups?
# check med value for noncitizen share so we know where to split
median(main$noncitizen_share_2009, na.rm = TRUE)
median(main$noncitizen_count_2009, na.rm = TRUE)
median(main$foreign_born_share_2009, na.rm = TRUE)
median(main$foreign_born_count_2009, na.rm = TRUE)

# create binary indicator for high versus low baseline third dif characteristic 
main <- main |>
  group_by(state, county) |>
  mutate(
    noncit_base = mean(noncitizen_share_2009, na.rm = TRUE),
    noncit_bin  = as.integer(noncit_base >= 0.015, na.rm = TRUE),
    noncit_count_base = mean(noncitizen_count_2009, na.rm = TRUE),
    noncit_count_bin = as.integer(noncit_count_base >= 100, na.rm = TRUE),
    foreign_born_share_base = mean(foreign_born_share_2009, na.rm = TRUE),
    foreign_born_share_bin  = as.integer(foreign_born_share_base >= 0.02, na.rm = TRUE),
    foreign_born_count_base = mean(foreign_born_count_2009, na.rm = TRUE),
    foreign_born_count_bin = as.integer(foreign_born_count_base >= 400, na.rm = TRUE)
  ) |>
  ungroup()

# the cells that actually identify the DDD term. tweak previous thresholds to balance groups.
main |> distinct(state, county, early_activator, noncit_bin) |>
  count(early_activator, noncit_bin)
main |> distinct(state, county, early_activator, noncit_count_bin) |>
  count(early_activator, noncit_count_bin)
main |> distinct(state, county, early_activator, foreign_born_count_bin) |>
  count(early_activator, foreign_born_count_bin)

## make some graphics to visualize the ptrends for the new partition for noncit_bin
# DDD pretrends with mech share 
ddd_pretrend_fig <- main |>
  filter(!is.na(mech_share_broad), !is.na(noncit_bin)) |>
  ggplot(aes(
    x = year, y = mech_share_broad,
    color = factor(early_activator, levels = c(0, 1),
                   labels = c("Late activator (2011+)", "Early activator (<2011)")),
    linetype = factor(noncit_bin, levels = c(1, 0),
                      labels = c("High noncitizen share", "Low noncitizen share"))
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "black") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  scale_color_manual(values = c("#4F8A5B", "#243E36")) +
  theme_minimal() +
  labs(
    color = NULL, linetype = NULL,
    x = "Year", y = "Mechanization Share of Expenditures",
    title = "Mechanization Share by Activation Timing and Baseline Noncitizen Share"
  )
print(ddd_pretrend_fig)


# DDD pretreds with hired_workers_per_acre
ddd_pretrend_labor <- main |>
  filter(!is.na(hired_workers_per_acre), !is.na(noncit_bin)) |>
  ggplot(aes(
    x = year, y = hired_workers_per_acre,
    color = factor(early_activator, levels = c(0, 1),
                   labels = c("Late activator (2011+)", "Early activator (<2011)")),
    linetype = factor(noncit_bin, levels = c(1, 0),
                      labels = c("High noncitizen share", "Low noncitizen share"))
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "black") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  scale_color_manual(values = c("#4F8A5B", "#243E36")) +
  theme_minimal() +
  labs(
    color = NULL, linetype = NULL,
    x = "Year", y = "Hired Workers Per Acre",
    title = "Hired Workers Per Acre Over Year by Activation Timing"
  )
print(ddd_pretrend_labor)

# DDD pretrends with log_hired_labor_exp_per_acre 
ddd_pretrend_hired <- main |>
  filter(!is.na(hired_labor_exp_per_acre), !is.na(noncit_bin)) |>
  ggplot(aes(
    x = year, y = hired_labor_exp_per_acre,
    color = factor(early_activator, levels = c(0, 1),
                   labels = c("Late activator (2011+)", "Early activator (<2011)")),
    linetype = factor(noncit_bin, levels = c(1, 0),
                      labels = c("High noncitizen share", "Low noncitizen share"))
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "black") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  scale_color_manual(values = c("#4F8A5B", "#243E36")) +
  theme_minimal() +
  labs(
    color = NULL, linetype = NULL,
    x = "Year", y = "Hired Labor Expenditures Per Acre",
    title = "Log Hired Labor Expenditures Per Acre Over Year by Activation Timing"
  )
print(ddd_pretrend_hired)

####################################################################################################
### DDD estimation ###
####################################################################################################
# regression-based DDD in the early/late duration framing:
# y_jt = sum_s beta_s (early_j x high_j x 1{t=s}) + early_j x year + high_j x year + gamma_j + delta_t + e_jt
# triple term is the extra early-vs-late effect in high-noncitizen counties, relative to 2007.

main <- main |>
  mutate(early_x_high = early_activator * noncit_bin)

ddd_mod1 <- feols(
  hired_labor_exp_per_acre ~ i(year, early_x_high, ref = 2007) +
    i(year, early_activator, ref = 2007) + i(year, noncit_bin, ref = 2007) | county_id + year,
  data = main,
  cluster = ~county_id
)
summary(ddd_mod1)

ddd_mod2 <- feols(
  hired_workers_per_acre ~ i(year, early_x_high, ref = 2007) +
    i(year, early_activator, ref = 2007) + i(year, noncit_bin, ref = 2007) | county_id + year,
  data = main,
  cluster = ~county_id
)
summary(ddd_mod2)

ddd_mod3 <- feols(
  mech_share_broad ~ i(year, early_x_high, ref = 2007) +
    i(year, early_activator, ref = 2007) + i(year, noncit_bin, ref = 2007) | county_id + year,
  data = main,
  cluster = ~county_id
)
summary(ddd_mod3)

# combine coefficients, county-clustered
etable(ddd_mod1, ddd_mod2, ddd_mod3, tex = TRUE)
# with state^year FE gone, activation timing varies mostly at the state level, so also show
# state-clustered SEs (35 clusters) as the conservative benchmark
etable(ddd_mod1, ddd_mod2, ddd_mod3, cluster = ~state)

# plot triple-interaction coefficients with 95% CIs
ddd_plot_data <- list(
  "Hired Labor Exp Per Acre" = ddd_mod1,
  "Hired Workers Per Acre"   = ddd_mod2,
  "Mechanization Share" = ddd_mod3
) |>
  map(\(mod) tidy(mod, conf.int = TRUE, conf.level = 0.95)) |>
  bind_rows(.id = "outcome") |>
  filter(str_detect(term, "early_x_high")) |>
  mutate(year = as.integer(str_extract(term, "\\d{4}"))) |>
  bind_rows(
    expand_grid(
      outcome = c("Hired Labor Exp Per Acre", "Hired Workers Per Acre",
                  "Mechanization Share"),
      year = 2007, estimate = 0, conf.low = 0, conf.high = 0
    )
  )

ddd_fig <- ggplot(ddd_plot_data, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.6, color = "#4F8A5B") +
  geom_point(size = 2, color = "#243E36") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  theme_minimal() +
  labs(
    x = "Census Year", y = "Triple-Difference Estimate Relative to 2007",
    title = "DDD: Early Activation x High Baseline Noncitizen Share (95% CI)"
  )
print(ddd_fig)

####################################################################################################
### DDD estimation: jail admissions as the third difference ###
####################################################################################################
# same specification as above, but partitioning on baseline jail booking flow instead of noncitizen
# share. the two partitions are close to orthogonal (r = 0.15), so this is a genuinely different cut
# of the data rather than a relabelling - see section 9 for how admits_bin is built and its caveats.
# admits_bin is NA for counties Vera doesn't cover, so this estimates on a slightly smaller sample.

main <- main |>
  mutate(early_x_high_admits = early_activator * admits_bin)

ddd_adm1 <- feols(
  hired_labor_exp_per_acre ~ i(year, early_x_high_admits, ref = 2007) +
    i(year, early_activator, ref = 2007) + i(year, admits_bin, ref = 2007) | county_id + year,
  data = main,
  cluster = ~county_id
)
summary(ddd_adm1)

ddd_adm2 <- feols(
  hired_workers_per_acre ~ i(year, early_x_high_admits, ref = 2007) +
    i(year, early_activator, ref = 2007) + i(year, admits_bin, ref = 2007) | county_id + year,
  data = main,
  cluster = ~county_id
)
summary(ddd_adm2)

ddd_adm3 <- feols(
  mech_share_broad ~ i(year, early_x_high_admits, ref = 2007) +
    i(year, early_activator, ref = 2007) + i(year, admits_bin, ref = 2007) | county_id + year,
  data = main,
  cluster = ~county_id
)
summary(ddd_adm3)

etable(ddd_adm1, ddd_adm2, ddd_adm3, tex = TRUE)
etable(ddd_adm1, ddd_adm2, ddd_adm3, cluster = ~state)

# plot triple-interaction coefficients with 95% CIs
ddd_admits_plot_data <- list(
  "Hired Labor Exp Per Acre" = ddd_adm1,
  "Hired Workers Per Acre"   = ddd_adm2,
  "Mechanization Share"      = ddd_adm3
) |>
  map(\(mod) tidy(mod, conf.int = TRUE, conf.level = 0.95)) |>
  bind_rows(.id = "outcome") |>
  filter(str_detect(term, "early_x_high_admits")) |>
  mutate(year = as.integer(str_extract(term, "\\d{4}"))) |>
  bind_rows(
    expand_grid(
      outcome = c("Hired Labor Exp Per Acre", "Hired Workers Per Acre",
                  "Mechanization Share"),
      year = 2007, estimate = 0, conf.low = 0, conf.high = 0
    )
  )

ddd_admits_fig <- ggplot(ddd_admits_plot_data, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.6, color = "#4F8A5B") +
  geom_point(size = 2, color = "#243E36") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  theme_minimal() +
  labs(
    x = "Census Year", y = "Triple-Difference Estimate Relative to 2007",
    title = "DDD: Early Activation x High Baseline Jail Admissions (95% CI)"
  )
print(ddd_admits_fig)





