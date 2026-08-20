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
install.packages("triplediff")

library(tidyverse)
library(tidycensus)
library(fixest)
library(broom)
library(triplediff)

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
    log_labor_share = log((labor_hired_exp+labor_contract_exp)/total_exp),
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
    repairs_exp = as.numeric(repairs_exp)
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

# baseline (pre-SC) foreign-born and noncitizen population shares from the 2005-2009 pooled ACS.
county_foreign_born <- get_acs(
  geography = "county",
  variables = c(
    total_pop    = "B05002_001",   # place-of-birth universe (total population)
    foreign_born = "B05002_013",
    noncitizen   = "B05001_006"    # "Not a U.S. citizen"; universe matches B05002_001
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
    # distinct counties. use this for all FE/cluster specs; joins are fine as they key on both.
    county_id = paste(state, county, sep = ", "),
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

# pretreds with log_hired_workers_per_acre
pretrend_labor <- main |>
  filter(!is.na(log_hired_workers_per_acre)) |>
  ggplot(aes(
    x = year, y = log_hired_workers_per_acre,
    color = factor(early_activator, labels = c("Late activator (2011+)", "Early activator (<2011)"))
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  scale_color_manual(values = c("#7CA982", "#243E36")) +
  theme_minimal() +
  labs(color = NULL, x = "Year", y = "Log Hired Workers Per Acre", title = "Log Hired Workers Per Acre Over Year by Activation Timing")
print(pretrend_labor)

# pretrends with log_hired_labor_exp_per_acre 
pretrend_hired <- main |>
  filter(!is.na(log_hired_labor_exp_per_acre)) |>
  ggplot(aes(
    x = year, y = log_hired_labor_exp_per_acre,
    color = factor(early_activator, labels = c("Late activator (2011+)", "Early activator (<2011)"))
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
    scale_color_manual(values = c("#7CA982", "#243E36")) +
  theme_minimal() +
  labs(color = NULL, x = "Year", y = "Log Hired Labor Expenditures Per Acre", title = "Log Hired Labor Expenditures Per Acre Over Year by Activation Timing")
print(pretrend_hired)

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
  log_hired_labor_exp_per_acre ~ early_activator * i(year, ref = 2002) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2002, 2007)),
  cluster = ~county_id
)
summary(pre_mod1)

# second model looks at interim trends between early versus late rollout counties
int_mod1 <- feols(
  log_hired_labor_exp_per_acre ~ early_activator * i(year, ref = 2007) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2007, 2012)),
  cluster = ~county_id
)
summary(int_mod1)

# third model looks at longer-run differences between early versus late rollout counties
lr_mod1 <- feols(
  log_hired_labor_exp_per_acre ~ early_activator * i(year, ref = 2012) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2012, 2017)),
  cluster = ~county_id
)
summary(lr_mod1)
# combine coefficients
etable(pre_mod1, int_mod1, lr_mod1)

## now looking at log_hired_workers_per_acre
# first model looks at pre trends between early versus late rollout counties
pre_mod2 <- feols(
  log_hired_workers_per_acre ~ early_activator * i(year, ref = 2002) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2002, 2007)),
  cluster = ~county_id
)
summary(pre_mod2)

# second model looks at interim trends between early versus late rollout counties
int_mod2 <- feols(
  log_hired_workers_per_acre ~ early_activator * i(year, ref = 2007) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2007, 2012)),
  cluster = ~county_id
)
summary(int_mod2)

# third model looks at longer-run differences between early versus late rollout counties
lr_mod2 <- feols(
  log_hired_workers_per_acre ~ early_activator * i(year, ref = 2012) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2012, 2017)),
  cluster = ~county_id
)
summary(lr_mod2)
# combine coefficients
etable(pre_mod2, int_mod2, lr_mod2)

## finally looking at log(mech_share_narrow)
# first model looks at pre trends between early versus late rollout counties
pre_mod3 <- feols(
  mech_share_narrow ~ early_activator * i(year, ref = 2002) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2002, 2007)),
  cluster = ~county_id
)
summary(pre_mod3)

# second model looks at interim trends between early versus late rollout counties
int_mod3 <- feols(
  mech_share_narrow ~ early_activator * i(year, ref = 2007) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2007, 2012)),
  cluster = ~county_id
)
summary(int_mod3)

# third model looks at longer-run differences between early versus late rollout counties
lr_mod3 <- feols(
  mech_share_narrow ~ early_activator * i(year, ref = 2012) | county_id + state^year,
  data = main |> 
    filter(year %in% c(2012, 2017)),
  cluster = ~county_id
)
summary(lr_mod3)

# fourth model looks at 2007 to 2017 differences between early versus late rollout counties
final_mod4 <- feols(
  mech_share_narrow ~ early_activator * i(year, ref = 2007) | county_id + state^year,
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
  log_hired_labor_exp_per_acre ~ i(year, early_activator, ref = 2007) | county_id + state^year,
  data = main,
  cluster = ~county_id
)
summary(es_full1)

es_full2 <- feols(
  log_hired_workers_per_acre ~ i(year, early_activator, ref = 2007) | county_id + state^year,
  data = main,
  cluster = ~county_id
)
summary(es_full2)

es_full3 <- feols(
  mech_share_narrow ~ i(year, early_activator, ref = 2007) | county_id + state^year,
  data = main,
  cluster = ~county_id
)
summary(es_full3)

# combine coeffs
etable(es_full1, es_full2, es_full3)

# plot point estimates and 95% CIs for the year-specific coefficients
es_plot_data <- list(
  "Log Hired Labor Exp Per Acre" = es_full1,
  "Log Hired Workers Per Acre"   = es_full2,
  "Mechanization Share" = es_full3
) |>
  map(\(mod) tidy(mod, conf.int = TRUE, conf.level = 0.95)) |>
  bind_rows(.id = "outcome") |>
  mutate(year = as.integer(str_extract(term, "\\d{4}"))) |>
  bind_rows(
    expand_grid(
      outcome = c("Log Hired Labor Exp Per Acre", "Log Hired Workers Per Acre",
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
### triple differences ###
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
    noncit_bin  = as.integer(noncit_base >= 0.01, na.rm = TRUE),
    noncit_count_base = mean(noncitizen_count_2009, na.rm = TRUE),
    noncit_count_bin = as.integer(noncit_count_base >= 103, na.rm = TRUE),
    foreign_born_share_base = mean(foreign_born_share_2009, na.rm = TRUE),
    foreign_born_share_bin  = as.integer(foreign_born_share_base >= 0.011, na.rm = TRUE),
    foreign_born_count_base = mean(foreign_born_count_2009, na.rm = TRUE),
    foreign_born_count_bin = as.integer(foreign_born_count_base >= 169, na.rm = TRUE)
  ) |>
  ungroup()

# the cells that actually identify the DDD term
main |> distinct(state, county, early_activator, noncit_bin) |>
  count(early_activator, noncit_bin)
main |> distinct(state, county, early_activator, noncit_count_bin) |>
  count(early_activator, noncit_count_bin)
main |> distinct(state, county, early_activator, foreign_born_share_bin) |>
  count(early_activator, foreign_born_share_bin)

## make some graphics to visualize the ptrends for the new partition for noncit_bin
# DDD pretrends with mech share 
ddd_pretrend_fig <- main |>
  filter(!is.na(mech_share_narrow), !is.na(noncit_bin)) |>
  ggplot(aes(
    x = year, y = mech_share_narrow,
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


# DDD pretreds with log_hired_workers_per_acre
ddd_pretrend_labor <- main |>
  filter(!is.na(log_hired_workers_per_acre), !is.na(noncit_bin)) |>
  ggplot(aes(
    x = year, y = log_hired_workers_per_acre,
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
    x = "Year", y = "Log Hired Workers Per Acre",
    title = "Log Hired Workers Per Acre Over Year by Activation Timing"
  )
print(ddd_pretrend_labor)

# DDD pretrends with log_hired_labor_exp_per_acre 
ddd_pretrend_hired <- main |>
  filter(!is.na(log_hired_labor_exp_per_acre), !is.na(noncit_bin)) |>
  ggplot(aes(
    x = year, y = log_hired_labor_exp_per_acre,
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
    x = "Year", y = "Log Hired Labor Expenditures Per Acre",
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
  log_hired_labor_exp_per_acre ~ i(year, early_x_high, ref = 2007) +
    i(year, early_activator, ref = 2007) + i(year, noncit_bin, ref = 2007) | county_id + year,
  data = main,
  cluster = ~county_id
)
summary(ddd_mod1)

ddd_mod2 <- feols(
  log_hired_workers_per_acre ~ i(year, early_x_high, ref = 2007) +
    i(year, early_activator, ref = 2007) + i(year, noncit_bin, ref = 2007) | county_id + year,
  data = main,
  cluster = ~county_id
)
summary(ddd_mod2)

ddd_mod3 <- feols(
  mech_share_narrow ~ i(year, early_x_high, ref = 2007) +
    i(year, early_activator, ref = 2007) + i(year, noncit_bin, ref = 2007) | county_id + year,
  data = main,
  cluster = ~county_id
)
summary(ddd_mod3)

# combine coefficients, county-clustered
etable(ddd_mod1, ddd_mod2, ddd_mod3)
# with state^year FE gone, activation timing varies mostly at the state level, so also show
# state-clustered SEs (35 clusters) as the conservative benchmark
etable(ddd_mod1, ddd_mod2, ddd_mod3, cluster = ~state)

# plot triple-interaction coefficients with 95% CIs
ddd_plot_data <- list(
  "Log Hired Labor Exp Per Acre" = ddd_mod1,
  "Log Hired Workers Per Acre"   = ddd_mod2,
  "Mechanization Share (Narrow)" = ddd_mod3
) |>
  map(\(mod) tidy(mod, conf.int = TRUE, conf.level = 0.95)) |>
  bind_rows(.id = "outcome") |>
  filter(str_detect(term, "early_x_high")) |>
  mutate(year = as.integer(str_extract(term, "\\d{4}"))) |>
  bind_rows(
    expand_grid(
      outcome = c("Log Hired Labor Exp Per Acre", "Log Hired Workers Per Acre",
                  "Mechanization Share (Narrow)"),
      year = 2007, estimate = 0, conf.low = 0, conf.high = 0
    )
  )

ddd_fig <- ggplot(ddd_plot_data, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.6, color = "#243E36") +
  geom_point(size = 2, color = "#243E36") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  theme_minimal() +
  labs(
    x = "Census Year", y = "Triple-Difference Estimate Relative to 2007",
    title = "DDD: Early Activation x High Baseline Noncitizen Share (95% CI)"
  )
print(ddd_fig)





