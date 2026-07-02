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

# generates pooled exposure intensity scores using 2 and 3, treating midpoint population as fixed (2010).
# restricted to rows attributable to SC specifically: a genuine detainer, or a CAP Local Incarceration
# apprehension (SC's fingerprint screening still applies at local jail booking even when the match
# never rose to a formal detainer). Excludes CAP Federal/State Incarceration (screened independent of
# county SC activation), 287(g) (a legally distinct partnership program), and border/non-custodial/other
# pathways that never go through local jail booking (see issues.txt).
county_exposure_pooled <- sc_trac_clean |>
  filter(!is.na(detainer_date) | apprehension_method == "CAP Local Incarceration") |>
  group_by(state, county) |>
  summarise(cases = n(), .groups = "drop") |>
  left_join(county_pop, by = c("state", "county")) |>
  mutate(exposure_pooled = cases / population * 10000) |>
  select(state, county, exposure_pooled)

# generates seperate yearly exposure intensity scores using 2 and 3; same SC-attributable restriction.
# year is coalesce(detainer_date, departed_date) - detainer date preferred, departed date as a fallback
# timing signal for cases without one. Imprecise for any single case, but still informative of which
# counties carried more exposure over time.
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
# ICE's official county-level SC activation roster (FOIA'd "IDENT/IAFIS Interoperability" report),
# used instead of first detainer/departure date since those are downstream (removes potential anticipatory weirdness),
# selected-on-outcome case events (most rows lack a detainer date because they were never routed through SC at all,
# e.g. CAP/287(g)/border encounters) rather than a measure of when SC itself went live in a county.
sc_activation_clean <- sc_ice |>
  mutate(
    county = str_remove(county, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county),
    activation_date = as.Date(activation_date),
    first_detainer_year = year(activation_date)
  )

## 6. USDA ERS county typology ##
# provides indicator for farming-dependent counties (2015 edition) to restrict the analysis sample to agricultural counties
# defined as those in which ≥20% of labor earnings or ≥17% of # of jobs come from ag (16% of countiees total, conservative estimate)
ag_counties <- ag_typology |>
  filter(Farming_2015_Update == 1) |>
  transmute(
    state  = State,
    county = str_remove(County_name, " County$| Parish$| Borough$| Census Area$| city$"),
    county = str_to_title(county)
  )

## 7. merge census_clean with sc_trac_clean for main analysis df ##
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

# create main df by merging on state and county, filtered to agricultural counties (USDA ERS typology).
# exposure_yr joins on (state, county, year) so each census year picks up that same calendar year's SC
# case rate specifically (not cumulative) (lets heterogeneous-intensity regressions vary exposure by
# census wave rather than relying on the single all-years-pooled exposure_pooled figure).
# NAs left in for now (not replaced with 0/0L) to inspect match coverage before deciding how to handle them.
# year filter now spans all four ag census years on hand (2002/2007/2012/2017); 2002 predates SC entirely
# (launched Oct 2008) so it's a second pre-treatment point for checking parallel trends against 2007,
# not just a single baseline snapshot. post is 0 for both pre-treatment years (2002, 2007) and 1 for both
# post-treatment years (2012, 2017).
main <- expenditures_clean |>
  semi_join(ag_counties, by = c("state", "county")) |>
  left_join(sc_county,          by = c("state", "county")) |>
  left_join(county_exposure_yr, by = c("state", "county", "year")) |>
  filter(year %in% c(2002, 2007, 2012, 2017),
    !is.na(treated)) |>
  mutate(
    post = as.integer(year >= 2012)
  ) |>
  left_join(crop_controls,    by = c("state", "county", "year")) |>
  left_join(landuse_controls, by = c("state", "county", "year"))


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
    exposure       = mean(exposure_pooled, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE)
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
    exposure       = mean(exposure_pooled, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE)
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
    exposure       = mean(exposure_pooled, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE)
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
    exposure       = mean(exposure_pooled, na.rm = TRUE),
    specialty_share = mean(specialty_share, na.rm = TRUE)*100,
    total_ag_acres = mean(total_ag_acres, na.rm = TRUE),
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE)
  )

# categorical exposure intensity tiers, for balance comparisons beyond the binary treated/control split.
# built from exposure_pooled specifically (not exposure_yr), since exposure_pooled is time-invariant per
# county and populated consistently across all three periods - exposure_yr has no signal at all in 2017
# (see issues.txt), so tiers built from it would be meaningless in the post-treatment table.
# NA/0 -> Control: these counties have zero SC-attributable cases ever recorded, a structural zero (not
# missing data - see the exposure_pooled construction above), so it's safe to fold them into one tier
# rather than leave them NA. Low/Medium/High are terciles computed only among counties with
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
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE)
  )
# baseline balance table 2007
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
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE)
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
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE)
  )
# post-treatment balance table
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
    irrigated_acres = mean(irrigated_acres, na.rm = TRUE)
  )

## checking par trends ##
# labor share binary
main |>
  group_by(year, treated) |>
  summarise(labor_share = mean(labor_share, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = labor_share*100, color = factor(treated))) +
  geom_line() +
  ylim(0, 10) +
  theme_light()

# mech share binary
main |>
  group_by(year, treated) |>
  summarise(mech_share_broad = mean(mech_share_broad, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = mech_share_broad*100, color = factor(treated))) +
  geom_line() +
  ylim(20, 30) +
  theme_light()

# labor share by exposure tier
main |>
  group_by(year, exposure_tier) |>
  summarise(labor_share = mean(labor_share, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = labor_share*100, color = factor(exposure_tier))) +
  geom_line() +
  ylim(4, 10) +
  theme_light()

# mech share by exposure tier
main |>
  group_by(year, exposure_tier) |>
  summarise(mech_share_broad = mean(mech_share_broad, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = mech_share_broad*100, color = factor(exposure_tier))) +
  geom_line() +
  ylim(20,30) +
  theme_light()


### analysis ###
## prep ##
# pull 2007 baseline covariates and join to main as time-invariant trend controls
baseline_2007 <- main |>
  filter(year == 2007) |>
  select(state, county,
   labor_share_2007   = labor_share,
   total_exp_2007     = total_exp,
   specialty_share_2007 = specialty_share,
   irrigated_share_2007 = irrigated_share)

# (may remove) eliminate na values from main for simplicity
main <- main |>
  left_join(baseline_2007, by = c("state", "county"))

## binary treatment models ##
# coefficient now tells you how much labor/mechanization share changes for each additional detainer per 10,000 residents
# replaces binary treated:post with continuous exposure_pooled:post

# treatment:
# treated:post = avg change in labor share in counties that were treated rleative counties that were not
# exposure_pooled:post = for each additional deterner issued per 10000 residents how much does the outcome change relative to lower exposed counties?
# controls:
# post:labor_share_2008 = how much more outcome changes per unit of baseline labor share
# post:log(total_exp_2007) = differential trend per 1% increase in baseline farm size
# post:speciality_share_2007 = differential trend per unit of baseline specialty crop share
# post:irrigated_share_2007 = differential trend per unit of baseline irrigation share

# model effect of treatment on labor share with baseline x post controls to absorb differential pre-existing trends
model_labor_share <- feols(labor_share ~ treated:post +
                       labor_share_2007:post + log(total_exp_2007):post +
                       specialty_share_2007:post + irrigated_share_2007:post |
                       county + year,
                     data = main, vcov = ~county)

summary(model_labor_share)

# model effect of treatment on mech share with baseline x post controls to absorb differential pre-existing trends
model_mech_share <- feols(mech_share_narrow ~ treated:post +
                       labor_share_2007:post + log(total_exp_2007):post +
                       specialty_share_2007:post + irrigated_share_2007:post |
                       county + year,
                     data = main, vcov = ~county)

summary(model_mech_share)

## dose response models with pooled ##
model_labor_share_dr <- feols(labor_share ~ exposure_pooled:post +
                                labor_share_2007:post + log(total_exp_2007):post +
                                specialty_share_2007:post + irrigated_share_2007:post |
                                county + year,
                              data = main, vcov = ~county)

summary(model_labor_share_dr)

model_mech_share_dr <- feols(mech_share_narrow ~ exposure_pooled:post +
                               labor_share_2007:post + log(total_exp_2007):post +
                               specialty_share_2007:post + irrigated_share_2007:post |
                               county + year,
                             data = main, vcov = ~county)

summary(model_mech_share_dr)

## dose response models with activation timing variation ##
# tests whether counties activated earlier (2008-2010) respond differently than those activated
# later (2011, the last pre-cutoff year) - early_activator/late_activator built in section 7

model_labor_share_dr_yr <- feols(labor_share ~ early_activator:post + late_activator:post +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_labor_share_dr_yr)

model_mech_share_dr_yr <- feols(mech_share_narrow ~ early_activator:post + late_activator:post +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year,
  data = main, vcov = ~county)

summary(model_mech_share_dr_yr)

## heterogeneous treatment effects ##
# tests whether SC effect on labor share and mech share varies by baseline county characteristics
# three-way interactions: treated:post:baseline_var
# treated:post coefficient = ATE at zero baseline; interaction term = how ATE changes with baseline

# farm size heterogeneity
# does the sc labor share effect differ between small and large farm counties 
model_labor_het <- feols(labor_share ~ treated:post + treated:post:log(total_exp_2007) +
 labor_share_2007:post + log(total_exp_2007):post +
 specialty_share_2007:post + irrigated_share_2007:post |
 county + year, data = main, vcov = ~county)

summary(model_labor_het)

# labor intensity heterogeneity
# does the sc labor share effect differ between counties with high vs. low initial labor reliance
model_labor_het_li <- feols(labor_share ~ treated:post + treated:post:labor_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_labor_het_li)

# specialty crop heterogeneity
# does the sc labor share effect differ between specialty crop counties and field crop counties 
model_labor_het_sp <- feols(labor_share ~ treated:post + treated:post:specialty_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_labor_het_sp)

# irrigation heterogeneity
# does the sc labor share effect differ between more and less irrigated counties 
model_labor_het_irr <- feols(labor_share ~ treated:post + treated:post:irrigated_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_labor_het_irr)

# initial mechanization heterogeneity
# does the sc labor share effect differ for counties that were already highly mechanized in 2007
mech_baseline <- main |>
  filter(year == 2007) |>
  select(state, county, mech_share_narrow_2007 = mech_share_narrow)
main <- main |> left_join(mech_baseline, by = c("state", "county"))

model_labor_het_mech <- feols(labor_share ~ treated:post + treated:post:mech_share_narrow_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_labor_het_mech)

## mech share heterogeneity models ##

# farm size heterogeneity
# does the sc mechanization effect differ between small and large farm counties - capital constraints - 
model_mech_het_fs <- feols(mech_share_narrow ~ treated:post + treated:post:log(total_exp_2007) +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_mech_het_fs)

# labor intensity heterogeneity
# does the sc mechanization effect differ for counties more reliant on labor in 2007
model_mech_het_li <- feols(mech_share_narrow ~ treated:post + treated:post:labor_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_mech_het_li)

# specialty crop heterogeneity
# does the sc mechanization effect differ between specialty crop and field crop counties
model_mech_het_sp <- feols(mech_share_narrow ~ treated:post + treated:post:specialty_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_mech_het_sp)

# irrigation heterogeneity
# does the sc mechanization effect differ between more and less irrigated counties
model_mech_het_irr <- feols(mech_share_narrow ~ treated:post + treated:post:irrigated_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_mech_het_irr)

# initial mechanization heterogeneity
# do counties with more room to mechanize (low baseline mech) respond more to sc
model_mech_het_mech <- feols(mech_share_narrow ~ treated:post + treated:post:mech_share_narrow_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main, vcov = ~county)

summary(model_mech_het_mech)

# findings so far show disruption without mechanization. since mechanization plausibly happens more slowly than over a few-year period, we want to test longer run trends using 2017 agcensus.
# by 2017 SC had been fully rolled out and suspended nationwide, so binary treated/control is no longer meaningful.
# identification now comes from variation in enforcement intensity (exposure_pooled) across counties with similar 2007 baselines.

## medium-run dose-response analysis (2007 vs 2017) ##
main_2017 <- expenditures_clean |>
  filter(year %in% c(2007, 2017)) |>
  left_join(sc_county, by = c("state", "county")) |>
  mutate(
    exposure_pooled = replace_na(exposure_pooled, 0),
    post            = as.integer(year == 2017)
  ) |>
  left_join(crop_controls,    by = c("state", "county", "year")) |>
  left_join(landuse_controls, by = c("state", "county", "year"))

baseline_2007_mr <- main_2017 |>
  filter(year == 2007) |>
  select(state, county,
   labor_share_2007     = labor_share,
   total_exp_2007       = total_exp,
   specialty_share_2007 = specialty_share,
   irrigated_share_2007 = irrigated_share)

main_2017 <- main_2017 |>
  left_join(baseline_2007_mr, by = c("state", "county"))

# labor share: does higher enforcement intensity predict higher labor costs by 2017?
model_labor_2017 <- feols(labor_share ~ exposure_pooled:post +
  labor_share_2007:post + log(total_exp_2007):post +
  irrigated_share_2007:post |
  county + year,
  data = main_2017, vcov = ~county)

summary(model_labor_2017)

# mech share: does higher enforcement intensity predict more mechanization by 2017?
model_mech_2017 <- feols(mech_share_narrow ~ exposure_pooled:post +
 labor_share_2007:post + log(total_exp_2007):post +
 irrigated_share_2007:post |
 county + year,
 data = main_2017, vcov = ~county)

summary(model_mech_2017)

