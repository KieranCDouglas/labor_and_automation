####################################################################################################
## main script for data cleaning and analysis
## last edited: 07/02/2026
## by kieran
####################################################################################################

#### begin ####
### prelude ###
install.packages("tidyverse")
install.packages("tidycensus")
install.packages("fixest")

library(tidyverse)
library(tidycensus)
library(fixest)


### load data ###
expenditures <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/expenditures_all_states_wide.csv")
sc_trac <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/secure1904.csv")
crops  <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/crops_area_harvested.csv")
landuse <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/landuse.csv")
sc_ice <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/sc_activation_dates.csv")
ag_typology <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/ers_county_typology_2015.csv")
hired_labor <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/hired_labor.csv")
farms_landvalue <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/farms_landvalue.csv")
harvested_by_farmsize <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/harvested_cropland_by_farmsize.csv")


### clean and merge ###
## 1. census data ##
# data are renamed for clarity and consistency. necesarry variables are selected and mutations convert to workable format. new variables generated as mechanization proxies.
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
    mech_share_broad = (fuel_total_exp+repairs_exp+utilities_exp+machinery_rent_exp+depreciation_exp)/total_exp,
    mech_share_narrow = (machinery_rent_exp+fuel_total_exp+repairs_exp)/total_exp,
    # fuel-excluded mechanization measures: fuel is by far the most price-volatile component of
    # mech_share (tracks the oil price cycle almost exactly, per issues.txt-style investigation), so a
    # farm's mech_share can swing a lot purely from commodity price movements rather than any real change
    # in how much machinery/capital it's actually using. These strip fuel out entirely, leaving a measure
    # closer to real mechanization intensity.
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
    repairs_exp = as.numeric(repairs_exp),
  )

## 2. secure communities data from TRAC ##
# several variables are renamed for clarity and consistency. necesarry variables are selected, and mutations convert data into usable format. new variables are generated as exposure-intensity scores.
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
    county = str_remove(county, " Borough$")
  )

## 3. population rates data ##
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

# baseline (pre-SC) foreign-born population share, used as a control for the confound where exposure_pooled
# is mechanically higher in counties with a larger existing immigrant population (more people who can be
# flagged once SC's fingerprint-sharing exists), independent of any actual enforcement effect - see
# discussion in issues.txt-style notes on migrant_farm_share's dose-response models. 2005-2009 ACS 5-year
# estimates (year=2009, the earliest 5-year vintage tidycensus supports) predates SC's Oct 2008 launch
# almost entirely, giving a genuinely pre-treatment baseline like the ag-census baseline covariates.
county_foreign_born <- get_acs(
  geography = "county",
  variables = c(total_pop = "B05002_001", foreign_born = "B05002_013"),
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
  # sum before pivoting: independent cities sharing a cleaned name with an adjacent county (Baltimore
  # City/County, St. Louis City/County, several VA independent cities) collapse to the same (state,
  # county) after the suffix-stripping above - same collision county_pop already handles the same way.
  group_by(state, county, variable) |>
  summarise(estimate = sum(estimate, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  mutate(foreign_born_share_2009 = foreign_born / total_pop) |>
  select(state, county, foreign_born_share_2009)

# generate pooled exposure intensity scores using 2 and 3, treating midpoint population as fixed (2010).
# restricted to rows attributable to SC specifically: a genuine detainer, or a CAP Local Incarceration
# apprehension (SC's fingerprint screening still applies at local jail booking even when the match
# never rose to a formal detainer). Excludes CAP Federal/State Incarceration (screened independent of
# county SC activation), 287(g) (a legally distinct partnership program), and border/non-custodial/other
# pathways that never go through local jail booking (see issues.txt).
county_exposure_pooled <- sc_trac_clean |>
  filter(year >= 2008, year <= 2013,
         !is.na(detainer_date) | apprehension_method == "CAP Local Incarceration") |>
  group_by(state, county) |>
  summarise(cases = n(), .groups = "drop") |>
  left_join(county_pop, by = c("state", "county")) |>
  mutate(exposure_pooled = cases / population * 10000) |>
  select(state, county, exposure_pooled)

# generate seperate yearly exposure intensity scores using 2 and 3; same SC-attributable restriction.
county_exposure_yr <- sc_trac_clean |>
  filter(!is.na(year), (!is.na(detainer_date) | apprehension_method == "CAP Local Incarceration")) |>
  group_by(state, county, year) |>
  summarise(cases = n(), .groups = "drop") |>
  left_join(county_pop, by = c("state", "county")) |>
  mutate(exposure_yr = cases / population * 10000) |>
  select(state, county, year, exposure_yr)

# adds pooled and yearly exposure intensity variables to sc_trac_clean df
sc_trac_clean <- sc_trac_clean |>
  left_join(county_exposure_pooled, by = c("state", "county")) |>
  left_join(county_exposure_yr,     by = c("state", "county", "year"))

## 4. crop type and land use controls ##
# collects county-level crop type and land use data, cleans, and generates share vars
specialty_groups <- c("VEGETABLES", "FRUIT & TREE NUTS", "HORTICULTURE")

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

## 5. secure communities activation dates ##
# ICE's official county-level SC activation roster
sc_activation_clean <- sc_ice |>
  mutate(
    county = str_remove(county, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county),
    activation_date = as.Date(activation_date),
    first_detainer_year = year(activation_date)
  )

## 6. define ag counties ##
# provides indicator for agricultural counties, defined as the union of two criteria:
#   (a) ERS earnings/employment flag: ≥20% of labor earnings or ≥17% of jobs from ag
#   (b) farmland acreage share: ≥20% of county land area in farms as of 2002 (pre-SC baseline,
#       avoids the classification being contaminated by any treatment effect on farmland acreage)
# (a) alone misses acreage-dominant-but-economically-diversified counties like the CA Central Valley
# (Fresno, Kern, Tulare, Merced, Stanislaus, San Joaquin...) where ag's absolute footprint is large but
# is dwarfed in dollar/job terms by other industries, so it never crosses the earnings/employment bar.
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
  distinct(state, county)

## 7. outcome varaibles ##
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

# cleans farm count and land/building asset value, needed here for total_farms
# denominator for migrant_farm_share below (migrant_farms is a count of farms, so it's normalized as a
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


## 8. merge census_clean with sc_trac_clean for main analysis df ##
# treated = 1 if SC had activated in the county by 2011, one full year before the 2012 ag census,
# so post counties in 2012 aren't contaminated by activations occurring that same year.
# early_activator/late_activator split the treated group by activation timing, built purely from the
# activation roster rather than case-level dates - same reason we moved treated off detainer/departure
# dates: case dates are downstream/selected, and for the ~85% of rows without a detainer_date, dated by
# departed_date instead, which lags the true encounter and isn't SC-specific to begin with.
# early_activator = activated 2008-2010; late_activator = activated in 2011, the last pre-cutoff year.
sc_county <- sc_activation_clean |>
  left_join(county_exposure_pooled, by = c("state", "county")) |>
  mutate(
    treated         = as.integer(first_detainer_year <= 2011),
    early_activator = as.integer(first_detainer_year <= 2010),
    late_activator  = as.integer(first_detainer_year == 2011)
  ) |>
  select(state, county, first_detainer_year, exposure_pooled, treated, early_activator, late_activator)

# create main df by merging on state and county, filtered to agricultural counties.
# exposure_yr joins on (state, county, year) so each census year picks up that same calendar year's SC
# case rate specifically.
# year filter keeps all four ag census years (2002/2007/2012/2017); 2002 predates SC entirely so it's a
# second pre-treatment point for checking parallel trends against 2007, not just a single baseline
# snapshot. post is 0 for both pre-treatment years (2002, 2007), 1 for 2017 (fully post-treatment, well
# after SC wound down), and NA for 2012 - SC rollout was still actively ongoing in many counties around
# the 2012 census, so it's neither clean pre- nor post-treatment; leaving it NA (rather than 0, which
# would misclassify a contaminated period as untreated) means it stays in main for anything that doesn't
# key off post (balance tables, the year_f-based event-study models, etc.), while any :post interaction
# automatically drops those rows (NA propagates through the interaction, same as feols already does for
# other missing covariates) - explicit filter(!is.na(post)) added below anyway for clarity.
main <- expenditures_clean |>
  semi_join(ag_counties, by = c("state", "county")) |>
  left_join(sc_county,          by = c("state", "county")) |>
  left_join(county_exposure_yr, by = c("state", "county", "year")) |>
  filter(year %in% c(2002, 2007, 2012, 2017),
    !is.na(treated)) |>
  mutate(
    post = case_when(
      year == 2017          ~ 1L,
      year %in% c(2002, 2007) ~ 0L,
      TRUE                    ~ NA_integer_
    ),
    # exposure_pooled NA -> 0: county_exposure_pooled only contains counties with at least one
    # SC-attributable case in 2008-2013 (see its construction above), so NA here is a structural zero
    # (verified: zero qualifying cases), not missing data - safe to fill, unlike the ag-census-derived
    # covariates below, whose NAs mean "USDA didn't report this county-year" (genuinely unknown, not zero).
    exposure_pooled = replace_na(exposure_pooled, 0)
  ) |>
  left_join(crop_controls,            by = c("state", "county", "year")) |>
  left_join(landuse_controls,         by = c("state", "county", "year")) |>
  left_join(hired_labor_controls,     by = c("state", "county", "year")) |>
  left_join(farms_landvalue_controls, by = c("state", "county", "year")) |>
  left_join(county_foreign_born,      by = c("state", "county")) |>
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
    hired_labor_exp_per_acre = hired_labor_exp  / harvested_acres_safe,
    migrant_farm_share       = migrant_farms    / total_farms_safe
  ) |>
  select(-harvested_acres_safe, -total_farms_safe)


### balance  and assumption checks ###
# at baseline (2007) the counties in our would-be treatment group have slightly higher labor share of total expenditures (1.48pp dif)
# and slightly lower total expenditures. additionally, treated group experienced much higher exposure to sc than the untreated group
# which likely has to do with longevity of program post activation since it went offline in 2013. 
# treated group, again, means pre 2011 rollout of sc in a given county. 

## balance tables with binary treatment indicator ##
# baseline balance table for treated versus control groups 2002
main |>
  filter(year == 2002) |>
  group_by(treated) |>
  summarise(
    n              = n(),
    mech     = mean(mech_share_broad, na.rm = TRUE),
    labor          = mean(labor_share, na.rm = TRUE),
    expenditures    = mean(total_exp, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE),
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE),
    worker_acres = mean(hired_workers_per_acre, na.rm = TRUE),
    mig_share = mean(migrant_farm_share, na.rm = TRUE)
  )
# baseline balance table for treated versus control groups 2007
main |>
  filter(year == 2007) |>
  group_by(treated) |>
  summarise(
    n              = n(),
    mech     = mean(mech_share_broad, na.rm = TRUE),
    labor          = mean(labor_share, na.rm = TRUE),
    expenditures    = mean(total_exp, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE),
    worker_acres = mean(hired_workers_per_acre, na.rm = TRUE),
    mig_share = mean(migrant_farm_share, na.rm = TRUE)
  )

# mid term balance table for treated versus control groups
main |>
  filter(year == 2012) |>
  group_by(treated) |>
  summarise(
    n              = n(),
    mech     = mean(mech_share_broad, na.rm = TRUE),
    labor          = mean(labor_share, na.rm = TRUE),
    expenditures    = mean(total_exp, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE),
    worker_acres = mean(hired_workers_per_acre, na.rm = TRUE),
    mig_share = mean(migrant_farm_share, na.rm = TRUE)
  )

# post-treatment balance table for treated versus control groups
main |>
  filter(year == 2017) |>
  group_by(treated) |>
  summarise(
    n              = n(),
    mech     = mean(mech_share_broad, na.rm = TRUE),
    labor          = mean(labor_share, na.rm = TRUE),
    expenditures    = mean(total_exp, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE),
    worker_acres = mean(hired_workers_per_acre, na.rm = TRUE),
    mig_share = mean(migrant_farm_share, na.rm = TRUE)
  )

# categorical exposure intensity tiers, for balance comparisons beyond the binary treated/control split.
# built from exposure_pooled specifically (not exposure_yr), since exposure_pooled is time-invariant per
# county and populated consistently across all three periods - exposure_yr has no signal at all in 2017
# (see issues.txt), so tiers built from it would be meaningless in the post-treatment table.
# NA/0 -> Control: these counties have zero SC-attributable cases during the 2008-2013 rollout window, a
# structural zero (not missing data - see the exposure_pooled construction above), so it's safe to fold
# them into one tier rather than leave them NA. Low/Medium/High are terciles computed only among counties with
# exposure_pooled > 0, so the three nonzero bins split real variation rather than being swamped by the
# ~58% of counties sitting at zero. Note this is a stricter "control" than treated == 0: a county that
# activated after 2011 (so treated == 0) can still show up in Low/Medium/High if it accumulated real
# exposure by the time of a later census wave.
exposure_cutoffs <- main |>
  distinct(state, county, exposure_pooled) |>
  filter(exposure_pooled > 0) |>
  pull(exposure_pooled) |>
  quantile(probs = c(1/3, 2/3), na.rm = TRUE)

main <- main |>
  mutate(
    exposure_tier = case_when(
      is.na(exposure_pooled) | exposure_pooled == 0 ~ "Control",
      exposure_pooled <= exposure_cutoffs[1]         ~ "Low",
      exposure_pooled <= exposure_cutoffs[2]         ~ "Medium",
      TRUE                                            ~ "High"
    ),
    exposure_tier = factor(exposure_tier, levels = c("Control", "Low", "Medium", "High"))
  )

## balance tables with categorical treatment intensity groups ##
# baseline balance table 2002
# shows what we would expect: higher exposure = higher labor at baseline
main |>
  filter(year == 2002) |>
  group_by(exposure_tier) |>
  summarise(
    n              = n(),
    mech     = mean(mech_share_broad, na.rm = TRUE),
    labor          = mean(labor_share, na.rm = TRUE),
    expenditures    = mean(total_exp, na.rm = TRUE),
    exposure       = mean(exposure_pooled, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE),
    worker_acres = mean(hired_workers_per_acre, na.rm = TRUE),
    mig_share = mean(migrant_farm_share, na.rm = TRUE)
  )
# baseline balance table 2007
# higher exposure tiers have lower baseline mechanization and higher labor share
main |>
  filter(year == 2007) |>
  group_by(exposure_tier) |>
  summarise(
    n              = n(),
    mech     = mean(mech_share_broad, na.rm = TRUE),
    labor          = mean(labor_share, na.rm = TRUE),
    expenditures    = mean(total_exp, na.rm = TRUE),
    exposure       = mean(exposure_pooled, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE),
    worker_acres = mean(hired_workers_per_acre, na.rm = TRUE),
    mig_share = mean(migrant_farm_share, na.rm = TRUE)
  )
# mid term balance table
main |>
  filter(year == 2012) |>
  group_by(exposure_tier) |>
  summarise(
    n              = n(),
    mech     = mean(mech_share_broad, na.rm = TRUE),
    labor          = mean(labor_share, na.rm = TRUE),
    expenditures    = mean(total_exp, na.rm = TRUE),
    exposure       = mean(exposure_pooled, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE),
    worker_acres = mean(hired_workers_per_acre, na.rm = TRUE),
    mig_share = mean(migrant_farm_share, na.rm = TRUE)
  )
# post-treatment balance table
# mech share increases most for those high exposure counties
main |>
  filter(year == 2017) |>
  group_by(exposure_tier) |>
  summarise(
    n              = n(),
    mech     = mean(mech_share_broad, na.rm = TRUE),
    labor          = mean(labor_share, na.rm = TRUE),
    expenditures    = mean(total_exp, na.rm = TRUE),
    exposure       = mean(exposure_pooled, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE),
    worker_acres = mean(hired_workers_per_acre, na.rm = TRUE),
    mig_share = mean(migrant_farm_share, na.rm = TRUE)
  )

## checking pre trends ##
FIGS_DIR <- "/Users/kieran/Documents/GitHub/labor_and_automation/figs/primary"

# labor share binary
p_pretrend_labor_treated <- main |>
  group_by(year, treated) |>
  summarise(labor_share = mean(labor_share, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = labor_share*100, color = factor(treated))) +
  geom_line() +
  ylim(5, 12) +
  labs(title = "Pre-Trends: Labor Share by Treatment Status", color = "Treated") +
  theme_light()
ggsave(file.path(FIGS_DIR, "pretrend_labor_share_treated.png"), p_pretrend_labor_treated, width = 8, height = 6, dpi = 300)

# mech share binary
p_pretrend_mech_treated <- main |>
  group_by(year, treated) |>
  summarise(mech_share_broad = mean(mech_share_broad, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = mech_share_broad*100, color = factor(treated))) +
  geom_line() +
  ylim(25, 30) +
  labs(title = "Pre-Trends: Mechanization Share by Treatment Status", color = "Treated") +
  theme_light()
ggsave(file.path(FIGS_DIR, "pretrend_mech_share_treated.png"), p_pretrend_mech_treated, width = 8, height = 6, dpi = 300)

# hired workers per acre by exposure tier, Low/Medium/High collapsed into one "Exposed" line (recomputed
# directly on the pooled Low+Medium+High counties, not averaged from the three tier-level medians) versus
# Control - simplifies the 4-line plot down to a 2-line Control-vs-any-exposure comparison.
p_pretrend_workers_acre_tier <- main |>
  mutate(exposure_group = if_else(exposure_tier == "Control", "Control", "Exposed")) |>
  group_by(year, exposure_group) |>
  summarise(hired_workers_per_acre = median(hired_workers_per_acre, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = hired_workers_per_acre, color = factor(exposure_group))) +
  geom_line() +
  ylim(0, 0.02) +
  labs(title = "Pre-Trends: Hired Workers Per Acre, Control vs Exposed", color = "Exposure") +
  theme_light()
ggsave(file.path(FIGS_DIR, "p_pretrend_workers_acre_tier.png"), p_pretrend_workers_acre_tier, width = 8, height = 6, dpi = 300)

# migrant farm share by exposure tier, Low/Medium/High collapsed into one "Exposed" line (same as the
# hired workers per acre plot above) versus Control.
p_pretrend_migrant_share_tier <- main |>
  mutate(exposure_group = if_else(exposure_tier == "Control", "Control", "Exposed")) |>
  group_by(year, exposure_group) |>
  summarise(migrant_farm_share = median(migrant_farm_share, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = migrant_farm_share, color = factor(exposure_group))) +
  geom_line() +
  ylim(0, 0.07) +
  labs(title = "Pre-Trends: Migrant-Farm Share, Control vs Exposed", color = "Exposure") +
  theme_light()
ggsave(file.path(FIGS_DIR, "p_pretrend_migrant_share_tier.png"), p_pretrend_migrant_share_tier, width = 8, height = 6, dpi = 300)


### analysis ###
## prep ##
# pull 2007 baseline covariates and join to main as time-invariant trend controls
baseline_2007 <- main |>
  filter(year == 2007) |>
  select(state, county,
   labor_share_2007   = labor_share,
   mech_share_broad_2007 = mech_share_broad,
   mech_share_narrow_2007 = mech_share_narrow,
   mech_share_nofuel_2007 = mech_share_nofuel,
   total_exp_2007     = total_exp,
   specialty_share_2007 = specialty_share,
   irrigated_share_2007 = irrigated_share,
  hired_labor_exp_per_acre_2007 = hired_labor_exp_per_acre,
migrant_farm_share_2007 = migrant_farm_share,
hired_workers_per_acre_2007 = hired_workers_per_acre)

# (may remove) eliminate na values from main for simplicity
main <- main |>
  left_join(baseline_2007, by = c("state", "county"))

# year_f: year as a factor, used to interact exposure_pooled/exposure_tier/baseline controls with each
# census wave individually below.
main <- main |>
  mutate(year_f = factor(year))

####################################################################################################
## models: exposure-intensity dose-response  ##
####################################################################################################
model_labor_share_dr_es <- feols(labor_share ~ exposure_pooled:year_f +
 labor_share_2007:year_f + log(total_exp_2007):year_f +
 specialty_share_2007:year_f + irrigated_share_2007:year_f |
 county + state^year,
  data = main, vcov = ~county)

summary(model_labor_share_dr_es)

testmod <-  feols(migrant_farm_share ~ exposure_pooled:year_f +
  foreign_born_share_2009:year_f + migrant_farm_share_2007:year_f|
  county + state^year,
data = main |> filter(year != 2007) |> mutate(year_f = droplevels(year_f)), vcov = ~county)
summary(testmod)


model_workers_acre_dr_es <- feols(hired_workers_per_acre ~ exposure_pooled:year_f +
  foreign_born_share_2009:year_f + hired_workers_per_acre_2007:year_f |
  county + state^year,
  data = main |> filter(year != 2007) |> mutate(year_f = droplevels(year_f)), vcov = ~county)
summary(model_workers_acre_dr_es)




