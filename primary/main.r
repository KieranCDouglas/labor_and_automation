####################################################################################################
## main script for data cleaning and analysis
## last edited: 06/27/2026
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
sc     <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/secure1904.csv")
crops  <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/crops_area_harvested.csv")
landuse <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/landuse.csv")

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
  ) |> 
  filter(
    year < 2015
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

# generates pooled exposure intensity scores using 2 and 3, treating midpoint population as fixed (2010)
county_exposure_pooled <- sc_clean |>
  group_by(state, county) |>
  summarise(cases = n(), .groups = "drop") |>
  left_join(county_pop, by = c("state", "county")) |>
  mutate(exposure_pooled = cases / population * 10000) |>
  select(state, county, exposure_pooled)

# generates seperate yearly exposure intensity scores using 2 and 3
county_exposure_yr <- sc_clean |>
  filter(!is.na(year)) |>
  group_by(state, county, year) |>
  summarise(cases = n(), .groups = "drop") |>
  left_join(county_pop, by = c("state", "county")) |>
  mutate(exposure_yr = cases / population * 10000) |>
  select(state, county, year, exposure_yr)

# adds pooled and yearly exposure intensity variables to sc_clean df
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
    exposure_yr = first(exposure_yr),
    .groups = "drop"
  ) |>
  mutate(treated = as.integer(first_detainer_year <= 2012))

# create main file by merging on state and county. 
main <- expenditures_clean |>
  left_join(sc_county, by = c("state", "county"))

main_nona <- main |>
  filter(year %in% c(2007, 2012)) |>
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

## dose response models with pooled ##
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

## dose response models with exposure timing variation ##
# tests whether early vs. late enforcement intensity has differential effects on outcomes
# exposure_early = avg annual detainer rate 2008-2010; exposure_late = avg annual rate 2011-2012
# joined as county-level covariates since main_nona is only 2007/2012

exposure_trajectory <- sc_clean |>
  filter(!is.na(year), year >= 2008, year <= 2012) |>
  left_join(county_pop, by = c("state", "county")) |>
  filter(!is.na(population)) |>
  group_by(state, county, year) |>
  summarise(yr_rate = n() / first(population) * 10000, .groups = "drop") |>
  group_by(state, county) |>
  summarise(
    exposure_early = mean(yr_rate[year <= 2010], na.rm = TRUE),
    exposure_late  = mean(yr_rate[year >= 2011], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    exposure_early = ifelse(is.nan(exposure_early), 0, exposure_early),
    exposure_late  = ifelse(is.nan(exposure_late),  0, exposure_late)
  )

main_nona <- main_nona |>
  left_join(exposure_trajectory, by = c("state", "county")) |>
  mutate(
    exposure_early = replace_na(exposure_early, 0),
    exposure_late  = replace_na(exposure_late,  0)
  )

model_labor_share_dr_yr <- feols(labor_share ~ exposure_early:post + exposure_late:post +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

summary(model_labor_share_dr_yr)

model_mech_share_dr_yr <- feols(mech_share_narrow ~ exposure_early:post + exposure_late:post +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year,
  data = main_nona, vcov = ~county)

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
 county + year, data = main_nona, vcov = ~county)

summary(model_labor_het)

# labor intensity heterogeneity
# does the sc labor share effect differ between counties with high vs. low initial labor reliance
model_labor_het_li <- feols(labor_share ~ treated:post + treated:post:labor_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

summary(model_labor_het_li)

# specialty crop heterogeneity
# does the sc labor share effect differ between specialty crop counties and field crop counties 
model_labor_het_sp <- feols(labor_share ~ treated:post + treated:post:specialty_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

summary(model_labor_het_sp)

# irrigation heterogeneity
# does the sc labor share effect differ between more and less irrigated counties 
model_labor_het_irr <- feols(labor_share ~ treated:post + treated:post:irrigated_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

summary(model_labor_het_irr)

# initial mechanization heterogeneity
# does the sc labor share effect differ for counties that were already highly mechanized in 2007
mech_baseline <- main_nona |>
  filter(year == 2007) |>
  select(state, county, mech_share_narrow_2007 = mech_share_narrow)
main_nona <- main_nona |> left_join(mech_baseline, by = c("state", "county"))

model_labor_het_mech <- feols(labor_share ~ treated:post + treated:post:mech_share_narrow_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

summary(model_labor_het_mech)

## mech share heterogeneity models ##

# farm size heterogeneity
# does the sc mechanization effect differ between small and large farm counties - capital constraints - 
model_mech_het_fs <- feols(mech_share_narrow ~ treated:post + treated:post:log(total_exp_2007) +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

summary(model_mech_het_fs)

# labor intensity heterogeneity
# does the sc mechanization effect differ for counties more reliant on labor in 2007
model_mech_het_li <- feols(mech_share_narrow ~ treated:post + treated:post:labor_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

summary(model_mech_het_li)

# specialty crop heterogeneity
# does the sc mechanization effect differ between specialty crop and field crop counties
model_mech_het_sp <- feols(mech_share_narrow ~ treated:post + treated:post:specialty_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

summary(model_mech_het_sp)

# irrigation heterogeneity
# does the sc mechanization effect differ between more and less irrigated counties
model_mech_het_irr <- feols(mech_share_narrow ~ treated:post + treated:post:irrigated_share_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

summary(model_mech_het_irr)

# initial mechanization heterogeneity
# do counties with more room to mechanize (low baseline mech) respond more to sc
model_mech_het_mech <- feols(mech_share_narrow ~ treated:post + treated:post:mech_share_narrow_2007 +
  labor_share_2007:post + log(total_exp_2007):post +
  specialty_share_2007:post + irrigated_share_2007:post |
  county + year, data = main_nona, vcov = ~county)

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

