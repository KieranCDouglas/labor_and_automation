####################################################################################################
## AgCensus expenditures data cleaning
## Last Edited: 06/10/2025
####################################################################################################

# begin #
# prelude #
install.packages("tidyverse")

library(tidyverse)

# load data #
census <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/main/expenditures_all_states_wide.csv")

# clean and merge #
# 1. census data #
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

# 2. secure communities data #

# 3. rates data #

