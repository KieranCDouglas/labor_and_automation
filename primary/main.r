####################################################################################################
## main script for data cleaning and analysis
## last edited: 08/01/2026
## by kieran
####################################################################################################

####################################################################################################
### prelude ###
####################################################################################################

install.packages("tidyverse")
install.packages("tidycensus")
install.packages("fixest")
install.packages("broom")

library(tidyverse)
library(tidycensus)
library(fixest)
library(broom)

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

####################################################################################################
### clean and merge ###
####################################################################################################

## 1. census data ##--------------------------------------------------------------------------------
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
    mech_labor_share = (fuel_total_exp+repairs_exp+utilities_exp+machinery_rent_exp+depreciation_exp)/(labor_hired_exp+labor_contract_exp)
  )

## 2. secure communities data from TRAC ##----------------------------------------------------------
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

# baseline (pre-SC) foreign-born population share, used as a control for the confound where exposure_pooled
# is mechanically higher in counties with a larger existing immigrant population (more people who can be
# flagged once SC's fingerprint-sharing exists)
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

## 4. crop type and land use controls ##------------------------------------------------------------
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

## 5. secure communities activation dates ##--------------------------------------------------------
# ICE's official county-level SC activation roster
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
#   (b) farmland acreage share: ≥20% of county land area in farms as of 2002 (pre-SC baseline)
ers_ag_flag <- ag_typology |>
  filter(Farming_2015_Update == 1) |>
  transmute(
    state  = State,
    county = str_remove(County_name, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county)
  )

# farmland_share NA (not 0) when AG LAND ACRES is missing for a county in 2002, so
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


## 8. merge for main analysis df ##------------------------------------------------------------------
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
# second pre-treatment point for checking parallel trends against 2007.
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

####################################################################################################
### event study justification ###
####################################################################################################
## secure communities-related exposure intensity over time ##---------------------------------------
# state, county, year (2008-2017) panel of SC-attributable detainers per 10,000 population 
# SC-attributable restriction as county_exposure_pooled/county_exposure_yr above. 
sc_detainer_rate_yearly <- county_pop |>
  distinct(state, county, population) |>
  semi_join(ag_counties, by = c("state", "county")) |>
  cross_join(tibble(year = 2008:2017)) |>
  left_join(
    sc_trac_clean |>
      filter(year >= 2008, year <= 2017,
             !is.na(detainer_date) | apprehension_method == "CAP Local Incarceration") |>
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

FIGS_DIR <- "/Users/kieran/Documents/GitHub/labor_and_automation/figs/primary"

# create figure
het_dose_fig <- ggplot(
  data = detainer_plot_data,
  aes(x = year, y = detainer_rate, color = state, group = interaction(state, county))) +
  geom_smooth(method = loess, weight = .5, linewidth = 0.4, se = FALSE) +
  facet_wrap(~worker_quartile) +
  scale_color_manual(values = county_gradient_palette) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "SC Detainer Rate Over Time Per County (By Hired-Worker-Per-Acre Quartile)",
      y = "Detainers Issued Per 10,000 Population", x = "Year") +
  ylim(0,18)
ggsave(file.path(FIGS_DIR, "het_dose_fig.png"), het_dose_fig,
       width = 10, height = 7, dpi = 300)

####################################################################################################
### staggered event-study with time-varying treatment intensity (dose), county-year level ###
####################################################################################################
# specify outcome variable, controls, and event window to estimate both binned and full es
## specify config ##---------------------------------------------------------------------------------
# outcome variable to test 
OUTCOME_VAR <- "mech_labor_share"

# control variables e.g. c("specialty_share", "irrigated_acres"). Leave as character(0) for none. 
CONTROL_VARS <- character(0)

# baseline (2007) control variables, interacted with event_time/bin rather than added flat to
# control for cross-farm time-variant heterogeneity: a FIXED pre-treatment snapshot of each variable
BASELINE_CONTROL_VARS <- c( "specialty_share")

# event-study window (event_time values outside this range are dropped
EVENT_WINDOW <- c(-5, 5)

OUTPUT_DIR <- "output"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

## read data ##--------------------------------------------------------------------------------------
main_es <- main |>
  select(
    county, year, state, first_detainer_year, exposure_yr, labor_share, mech_share_broad, mech_share_narrow,
    specialty_acres, field_crop_acres, specialty_share, harvested_acres, irrigated_acres, hired_workers, hired_labor_exp,
    migrant_workers, migrant_farms_hired, migrant_farms, total_farms, land_asset_value, hired_workers_per_acre,
    hired_labor_exp_per_acre, total_exp, mech_labor_share
  ) |>
  mutate(
    county = paste(state, county),
    G      = first_detainer_year,
    dose   = replace_na(exposure_yr, 0),
    y      = .data[[OUTCOME_VAR]]
  )

# pulls each BASELINE_CONTROL_VARS column's 2007 value only, suffixed "_2007", and joins it back onto
# every year for that county (fixed, time-invariant snapshot).
baseline_vars_2007 <- paste0(BASELINE_CONTROL_VARS, "_2007")
baseline_2007_es <- main |>
  filter(year == 2007) |>
  mutate(county = paste(state, county)) |>
  select(county, all_of(BASELINE_CONTROL_VARS)) |>
  rename_with(~ paste0(., "_2007"), all_of(BASELINE_CONTROL_VARS))

main_es <- main_es |>
  left_join(baseline_2007_es, by = "county")

## validate required columns ##----------------------------------------------------------------------
required_cols <- c("county", "year", "G", "dose", "y", CONTROL_VARS, baseline_vars_2007)
missing_cols <- setdiff(required_cols, names(main_es))
if (length(missing_cols) > 0) {
  stop(
    "main_es is missing required column(s): ", paste(missing_cols, collapse = ", "),
    ". Expected columns: ", paste(required_cols, collapse = ", "),
    ". Add missing columns to the select() above, or check OUTCOME_VAR/CONTROL_VARS/",
    "BASELINE_CONTROL_VARS in CONFIGURATION."
  )
}

## check G (first treatment year) is constant within county ##---------------------------------------
# G should be a single fixed value per county (staggered adoption = one adoption year per unit). 
check_G_constant <- function(data) {
  offenders <- data |>
    group_by(county) |>
    summarise(n_distinct_G = n_distinct(G, na.rm = TRUE), .groups = "drop") |>
    filter(n_distinct_G > 1)
  if (nrow(offenders) > 0) {
    warning(
      nrow(offenders), " counties have more than one distinct value of G (first treatment year): ",
      paste(head(offenders$county, 10), collapse = ", "),
      if (nrow(offenders) > 10) ", ..." else "",
      ". Treatment timing must be constant within a county for this design - check your data."
    )
  }
  invisible(offenders)
}
check_G_constant(main_es)

## construct treatment timing variables ##-----------------------------------------------------------
# convention used throughout: G is NA for never-treated counties 
main_es <- main_es |>
  mutate(
    ever_treated = !is.na(G)
  )

## construct event time ##---------------------------------------------------------------------------
# event_time = calendar year minus the county's own adoption year. NA for never-treated counties (G is
# NA). identification comes from variation in when counties are treated and how their dose evolves
# relative to their own adoption, rather than a a treated-vs-never-treated contrast.
main_es <- main_es |>
  mutate(event_time = year - G)

## construct binned event time ##--------------------------------------------------------------------
# pre  : event_time < 0            (before adoption)
# early: 0 <= event_time <= 2      (first 3 post-adoption years)
# late : event_time >= 3           (further out post-adoption)
# NA event_time (never-treated)
main_es <- main_es |>
  mutate(bin = case_when(
    is.na(event_time)                          ~ NA_character_,
    event_time < 0                             ~ "pre",
    event_time >= 0 & event_time <= 2          ~ "early",
    event_time >= 3                            ~ "late"
  ))

## data-quality warnings ##--------------------------------------------------------------------------
# dose should be 0 or NA before adoption by construction 
n_pretreatment_nonzero_dose <- main_es |> filter(event_time < 0, !is.na(dose), dose != 0) |> nrow()
if (n_pretreatment_nonzero_dose > 0) {
  warning(
    n_pretreatment_nonzero_dose, " county-year observations have a nonzero dose in a pre-treatment ",
    "period (event_time < 0). Check whether `dose` is coded correctly relative to `G`."
  )
}

# counties with G <= the first year they're observed have no pre-treatment observations at all, and
# can't contribute to identifying early/negative event-time coefficients.
counties_always_treated <- main_es |>
  group_by(county) |>
  summarise(first_year_observed = min(year), G = first(G), .groups = "drop") |>
  filter(!is.na(G), G <= first_year_observed)
if (nrow(counties_always_treated) > 0) {
  warning(
    nrow(counties_always_treated), " counties are treated at or before the first year they're observed ",
    "(no pre-treatment data): ", paste(head(counties_always_treated$county, 10), collapse = ", "),
    if (nrow(counties_always_treated) > 10) ", ..." else ""
  )
}

# counties with zero rows where event_time < 0 can't help identify the pre-period at all
counties_no_pretrend_obs <- main_es |>
  filter(ever_treated) |>
  group_by(county) |>
  summarise(has_pre = any(event_time < 0, na.rm = TRUE), .groups = "drop") |>
  filter(!has_pre)
if (nrow(counties_no_pretrend_obs) > 0) {
  warning(
    nrow(counties_no_pretrend_obs), " treated counties have no pre-treatment (event_time < 0) ",
    "observations at all."
  )
}

## diagnostic summaries ##---------------------------------------------------------------------------
cat("\n==================== DIAGNOSTIC SUMMARY ====================\n")
cat("N counties:", n_distinct(main_es$county), "\n")
cat("N years:   ", n_distinct(main_es$year), "(", min(main_es$year, na.rm = TRUE), "-", max(main_es$year, na.rm = TRUE), ")\n")

cat("\nRollout year (G) distribution (excluding never-treated):\n")
print(main_es |> distinct(county, G) |> filter(!is.na(G)) |> count(G) |> arrange(G))

cat("\nN never-treated counties (G is NA):", n_distinct(main_es$county[is.na(main_es$G)]), "\n")

cat("\nEvent-time distribution (county-year observations):\n")
print(main_es |> filter(!is.na(event_time)) |> count(event_time) |> arrange(event_time))

cat("\nBinned event-time distribution:\n")
print(main_es |> filter(!is.na(bin)) |> count(bin))
cat("==============================================================\n\n")

# builds the "+ control1 + control2" formula fragment from CONTROL_VARS (empty string if none), plus the
# baseline-2007 controls interacted with event_time or bin 
control_terms <- if (length(CONTROL_VARS) > 0) paste0(" + ", paste(CONTROL_VARS, collapse = " + ")) else ""
baseline_terms_canonical <- if (length(baseline_vars_2007) > 0) {
  paste0(" + ", paste(paste0(baseline_vars_2007, ":factor(event_time)"), collapse = " + "))
} else ""
baseline_terms_binned <- if (length(baseline_vars_2007) > 0) {
  paste0(" + ", paste(paste0(baseline_vars_2007, ":bin"), collapse = " + "))
} else ""
na_check_cols <- c("y", "dose", "county", "year", CONTROL_VARS, baseline_vars_2007)

## estimate canonical event-study with event_time x dose ##-------------------------------------------
# Y_jt = alpha_j + lambda_t + sum_{e != -1} beta_e * dose_jt * 1(event_time = e) + [controls] + epsilon_jt
#
# beta_e is the average marginal effect of a one-unit increase in dose, at event time e, relative to
# event time -1 (the reference period: year right before adoption)
#
# restricted to the configured EVENT_WINDOW so the endpoints aren't estimated off a handful of counties
# observed very far from their own adoption year. 
main_es_window <- main_es |>
  filter(!is.na(event_time), event_time >= EVENT_WINDOW[1], event_time <= EVENT_WINDOW[2]) |>
  # drop rows missing anything actually used in estimation 
  filter(if_all(all_of(na_check_cols), ~ !is.na(.)))

cat("Canonical event-study estimation sample: ", nrow(main_es_window), " county-year observations, ",
    n_distinct(main_es_window$county), " counties, event_time in [", EVENT_WINDOW[1], ", ", EVENT_WINDOW[2],
    "]\n", sep = "")


# the cannonical reference model 
# includes county and state^year fixed effects 
canonical_formula <- as.formula(paste0(
  "y ~ i(event_time, dose, ref = -1)", control_terms, baseline_terms_canonical, " | county + state^year"
))
cat("Canonical model formula:", deparse(canonical_formula), "\n")
model_canonical <- feols(
  canonical_formula,
  data = main_es_window,
  cluster = ~county
)

print(summary(model_canonical))

## estimate binned event-study ##--------------------------------------------------------------------
# collapses event time into 3 bins (pre / early / late) instead of estimating
# one coefficient per exact event-year 
df_binned <- main_es |>
  filter(if_all(all_of(c(na_check_cols, "bin")), ~ !is.na(.)))

binned_formula <- as.formula(paste0(
  "y ~ i(bin, dose, ref = \"pre\")", control_terms, baseline_terms_binned, " | county + state^year"
))
cat("Binned model formula:", deparse(binned_formula), "\n")
model_binned <- feols(
  binned_formula,
  data = df_binned,
  cluster = ~county
)

print(summary(model_binned))

## checking pre trends ##---------------------------------------------------------------------------
# labor share by event time (year relative to each county's own SC adoption year) 
pretrend_event_data <- main_es |>
  filter(!is.na(event_time), !is.na(labor_share)) |>
  group_by(event_time) |>
  summarise(labor_share = mean(labor_share, na.rm = TRUE), n = n(), .groups = "drop")

p_pretrend_labor_treated <- pretrend_event_data |>
  ggplot(aes(x = event_time, y = labor_share * 100)) +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
  geom_line() +
  geom_point(aes(size = n)) +
  labs(
    title = "Labor Share by Event Time (Ever-Treated Counties)",
    subtitle = "Point size: N observations at that event time",
    x = "Event time (years since county's SC adoption)", y = "Labor share of expenditures (%)",
    size = "N obs"
  ) +
  theme_light()
ggsave(file.path(FIGS_DIR, "pretrend_labor_share_treated.png"), p_pretrend_labor_treated, width = 8, height = 6, dpi = 300)


# canonical event-study plot
canonical_coefs <- tidy(model_canonical, conf.int = TRUE) |>
  filter(str_detect(term, "^event_time::")) |>
  mutate(event_time = as.numeric(str_extract(term, "(?<=::)-?[0-9]+(?=:)")))

p_canonical <- ggplot(canonical_coefs, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "grey50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  geom_point(size = 2) +
  labs(
    title = "Event-Study: Marginal Effect of Dose by Event Time",
    subtitle = "Reference period: event_time = -1 (year before adoption)",
    x = "Event time (years since adoption)",
    y = "Estimated marginal effect of dose"
  ) +
  theme_minimal()

ggsave(file.path(OUTPUT_DIR, "canonical_event_study.png"), p_canonical, width = 9, height = 6, dpi = 300)

# binned event-study plot 
binned_coefs <- tidy(model_binned, conf.int = TRUE) |>
  filter(str_detect(term, "^bin::")) |>
  mutate(bin_label = str_extract(term, "(?<=::)[a-z]+(?=:)"),
         bin_label = factor(bin_label, levels = c("early", "late")))

p_binned <- ggplot(binned_coefs, aes(x = bin_label, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  geom_point(size = 3) +
  labs(
    title = "Binned Event-Study: Marginal Effect of Dose",
    subtitle = "Reference period: pre-adoption",
    x = "Period (relative to adoption)",
    y = "Estimated marginal effect of dose"
  ) +
  theme_minimal()

ggsave(file.path(OUTPUT_DIR, "binned_event_study.png"), p_binned, width = 7, height = 6, dpi = 300)

####################################################################################################
### dose-quartile robustness: same spec as above, but dose intensity split into quartiles ###
####################################################################################################
# same OUTCOME_VAR/CONTROL_VARS/BASELINE_CONTROL_VARS/EVENT_WINDOW as the continuous-dose models above.
# `dose` in i(bin, dose, ref="pre") / i(event_time, dose, ref=-1) is continuous, so those coefficients
# are a single linear marginal effect per unit of dose -- if a handful of high-dose counties are driving
# it (plausible, given how unstable some of the continuous-dose coefficients have been), a linear slope
# masks that. This splits counties into dose quartiles instead, using exposure_pooled (the FIXED
# 2008-2013 pooled SC-attributable rate per county) rather than the time-varying per-year `dose`, so a
# county's dose group doesn't shift across its own pre/early/late periods. is_q2_dose/is_q3_dose/
# is_q4_dose are binary dummies for quartile membership (quartile 1, lowest dose, is the implicit
# omitted group).
county_dose_quartile <- main |>
  mutate(county = paste(state, county)) |>
  distinct(county, exposure_pooled) |>
  mutate(dose_quartile = ntile(exposure_pooled, 4))

cat("\nDose quartile cutoffs (exposure_pooled = SC-attributable cases per 10,000 population, pooled 2008-2013):\n")
print(county_dose_quartile |>
  group_by(dose_quartile) |>
  summarise(n = n(), min = min(exposure_pooled), max = max(exposure_pooled), .groups = "drop"))

main_es_quartile <- main_es |>
  left_join(county_dose_quartile |> select(county, dose_quartile), by = "county") |>
  mutate(
    is_q2_dose = as.integer(dose_quartile == 2),
    is_q3_dose = as.integer(dose_quartile == 3),
    is_q4_dose = as.integer(dose_quartile == 4)
  )


## estimate binned event-study, dose quartiles ##------------------------------------------------------
df_binned_quartile <- main_es_quartile |>
  filter(if_all(all_of(c(na_check_cols, "bin")), ~ !is.na(.)))

binned_formula_quartile <- as.formula(paste0(
  "y ~ i(bin, ref = \"pre\") + i(bin, is_q2_dose, ref = \"pre\") + i(bin, is_q3_dose, ref = \"pre\") + ",
  "i(bin, is_q4_dose, ref = \"pre\")",
  control_terms, baseline_terms_binned, " | county + state^year"
))
cat("Binned (dose-quartile) model formula:", deparse(binned_formula_quartile), "\n")
model_binned_quartile <- feols(
  binned_formula_quartile,
  data = df_binned_quartile,
  cluster = ~county
)

print(summary(model_binned_quartile))

## estimate canonical event-study, dose quartiles ##---------------------------------------------------
main_es_window_quartile <- main_es_quartile |>
  filter(!is.na(event_time), event_time >= EVENT_WINDOW[1], event_time <= EVENT_WINDOW[2]) |>
  filter(if_all(all_of(na_check_cols), ~ !is.na(.)))

canonical_formula_quartile <- as.formula(paste0(
  "y ~ i(event_time, ref = -1) + i(event_time, is_q2_dose, ref = -1) + i(event_time, is_q3_dose, ref = -1) + ",
  "i(event_time, is_q4_dose, ref = -1)",
  control_terms, baseline_terms_canonical, " | county + state^year"
))
cat("Canonical (dose-quartile) model formula:", deparse(canonical_formula_quartile), "\n")
model_canonical_quartile <- feols(
  canonical_formula_quartile,
  data = main_es_window_quartile,
  cluster = ~county
)

print(summary(model_canonical_quartile))

