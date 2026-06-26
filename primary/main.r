####################################################################################################
## AgCensus expenditures data cleaning
## Last Edited: 06/26/2026
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
census <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/expenditures_all_states_wide.csv")
sc     <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/secure1904.csv")
crops  <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/crops_area_harvested.csv")
landuse <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/landuse.csv")

### clean and merge ###
## 1. census data ##
# data are renamed for clarity and consistency. necesarry variables are selected and mutations convert to workable format. new variables generated as mechanization proxies.
census_clean <- census |> 
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

## 2. secure communities data ##
# several variables are renamed for clarity and consistency. necesarry variables are selected, and mutations convert data into usable format. new variables are generated as exposure-intensity scores.
sc_clean <- sc |> 
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
## 3. rates data ##
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

# filters to period of interest
sc_clean <- sc_clean |> filter(year < 2015)

# generates pooled exposure intensity score, treating midpoint population as fixed
county_exposure_pooled <- sc_clean |>
  group_by(state, county) |>
  summarise(cases = n(), .groups = "drop") |>
  left_join(county_pop, by = c("state", "county")) |>
  mutate(exposure_pooled = cases / population * 10000) |>
  select(state, county, exposure_pooled)

# generates seperate yearly exposure intensity scores using updated population estimates
county_exposure_yr <- sc_clean |>
  filter(!is.na(year)) |>
  group_by(state, county, year) |>
  summarise(cases = n(), .groups = "drop") |>
  left_join(county_pop, by = c("state", "county")) |>
  mutate(exposure_yr = cases / population * 10000) |>
  select(state, county, year, exposure_yr)

# adds pooled and yearly exposure intansity variables to sc_clean df
sc_clean <- sc_clean |>
  left_join(county_exposure_pooled, by = c("state", "county")) |>
  left_join(county_exposure_yr,     by = c("state", "county", "year"))

## 4. crop type and land use controls ##
# collects county-level crop type and land use data, cleans, and generates share vars
specialty_groups <- c("VEGETABLES", "FRUIT & TREE NUTS")

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

## 5. merge census_clean with sc_clean for main analysis df ##
# add treated indicator variable and filter for counties with a detainer date 
sc_county <- sc_clean |>
  filter(!is.na(detainer_date)) |>
  group_by(state, county) |>
  summarise(
    first_detainer_year = min(year(detainer_date), na.rm = TRUE),
    exposure_pooled     = first(exposure_pooled),
    .groups = "drop"
  ) |>
  mutate(treated = as.integer(first_detainer_year <= 2011))

# create main file by merging on state and county. 
main <- census_clean |>
  left_join(sc_county, by = c("state", "county"))

main_nona <- main |>
  mutate(
    treated         = replace_na(treated, 0L),
    exposure_pooled = replace_na(exposure_pooled, 0),
    post            = as.integer(year == 2012)
  ) |>
  left_join(crop_controls,    by = c("state", "county", "year")) |>
  left_join(landuse_controls, by = c("state", "county", "year"))

### balance check ###
# treated counties already had lower mechanization (higher labor reliance), nearly twice total expenditures (treated are larger), and have much higher exposure. 
main_nona |>
  filter(year == 2007) |>
  group_by(treated) |>
  summarise(
    n              = n(),
    mech_broad     = mean(mech_share_broad, na.rm = TRUE),
    labor          = mean(labor_share, na.rm = TRUE),
    total_exp      = mean(total_exp, na.rm = TRUE),
    exposure       = mean(exposure_pooled, na.rm = TRUE)
  )


### analysis ###
## prep ##
# pull 2007 baseline covariates and join to main_nona as time-invariant trend controls
baseline_2007 <- main_nona |>
  filter(year == 2007) |>
  select(state, county,
         labor_share_2007   = labor_share,
         total_exp_2007     = total_exp,
         specialty_share_2007 = specialty_share,
         irrigated_share_2007 = irrigated_share)

# (may remove) eliminate na values from main for simplicity 
main_nona <- main_nona |>
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
                     data = main_nona, vcov = ~county)

summary(model_labor_share)

# model effect of treatment on mech share with baseline x post controls to absorb differential pre-existing trends
model_mech_share <- feols(mech_share_narrow ~ treated:post +
                       labor_share_2007:post + log(total_exp_2007):post +
                       specialty_share_2007:post + irrigated_share_2007:post |
                       county + year,
                     data = main_nona, vcov = ~county)

summary(model_mech_share)

## dose response models ##
model_labor_share_dr <- feols(labor_share ~ exposure_pooled:post +
                                labor_share_2007:post + log(total_exp_2007):post +
                                specialty_share_2007:post + irrigated_share_2007:post |
                                county + year,
                              data = main_nona, vcov = ~county)

summary(model_labor_share_dr)

model_mech_share_dr <- feols(mech_share_narrow ~ exposure_pooled:post +
                               labor_share_2007:post + log(total_exp_2007):post +
                               specialty_share_2007:post + irrigated_share_2007:post |
                               county + year,
                             data = main_nona, vcov = ~county)

summary(model_mech_share_dr)

## heterogeneous treatment effects ##
# tests whether SC effect on labor share and mech share varies by baseline county characteristics
# three-way interactions: treated:post:baseline_var
# treated:post coefficient = ATE at zero baseline; interaction term = how ATE changes with baseline

model_labor_het <- feols(labor_share ~ treated:post +
                           treated:post:labor_share_2007 +
                           treated:post:irrigated_share_2007 +
                           treated:post:specialty_share_2007 +
                           labor_share_2007:post + log(total_exp_2007):post +
                           specialty_share_2007:post + irrigated_share_2007:post |
                           county + year,
                         data = main_nona, vcov = ~county)

summary(model_labor_het)

model_mech_het <- feols(mech_share_narrow ~ treated:post +
                          treated:post:labor_share_2007 +
                          treated:post:irrigated_share_2007 +
                          treated:post:specialty_share_2007 +
                          labor_share_2007:post + log(total_exp_2007):post +
                          specialty_share_2007:post + irrigated_share_2007:post |
                          county + year,
                        data = main_nona, vcov = ~county)

summary(model_mech_het)

## heterogeneous dose-response models ##
# tests whether the continuous exposure-response relationship varies by baseline county characteristics

model_labor_het_dr <- feols(labor_share ~ exposure_pooled:post +
                              exposure_pooled:post:labor_share_2007 +
                              exposure_pooled:post:irrigated_share_2007 +
                              exposure_pooled:post:specialty_share_2007 +
                              labor_share_2007:post + log(total_exp_2007):post +
                              specialty_share_2007:post + irrigated_share_2007:post |
                              county + year,
                            data = main_nona, vcov = ~county)

summary(model_labor_het_dr)

model_mech_het_dr <- feols(mech_share_narrow ~ exposure_pooled:post +
                             exposure_pooled:post:labor_share_2007 +
                             exposure_pooled:post:irrigated_share_2007 +
                             exposure_pooled:post:specialty_share_2007 +
                             labor_share_2007:post + log(total_exp_2007):post +
                             specialty_share_2007:post + irrigated_share_2007:post |
                             county + year,
                           data = main_nona, vcov = ~county)

summary(model_mech_het_dr)

# findings so far show disruption without mechanization. since mechanization plausibly happens more slowly than over a few-year period, we want to test longer run trends using 2017 agcensus.


