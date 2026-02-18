####################################################################################################
## This script Runs the primary analysis to examine the raltionship between SC and tech adoption ##
###################################################################################################
# install packages
install.packages("tidyverse")
install.packages("fixest")
install.packages("did")
library(fixest)
library(tidyverse)
library(did)

arizona <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/statefiles/arizona.csv")
arizona_02 <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/statefiles/arizona_2002.csv")
california <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/statefiles/california.csv")
california_02 <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/statefiles/california_2002.csv")
GA <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/statefiles/GA.csv")
GA_02 <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/statefiles/GA_2002.csv")
mas <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/statefiles/mas.csv")
mas_02 <- read.csv("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/statefiles/mas_2002.csv")
# rbind WITH state identifier
states <- rbind(
  mutate(arizona, state = "AZ"),
  mutate(GA, state = "GA"),
  mutate(california, state = "CA"),
  mutate(mas, state = "MA"),
  mutate(arizona_02, state = "AZ"),
  mutate(GA_02, state = "GA"),
  mutate(california_02, state = "CA"),
  mutate(mas_02, state = "MA")
)
states_clean <- states |> 
  filter(year %in% c(2002, 2007, 2012)) |>
  distinct(state, county, year, .keep_all = TRUE) |>
  rename(
    total_exp = "Total.farm.production.expenditures",
    avg_exp = "Average.per.farm",
    rep_exp = "Repairs.supplies.and.maintenance.cost",
    utilities = "Utilities",
    labor_hired = "Hired.farm.labor",
    labor_con = "Contract.labor",
    mech_rent = "Rent.and.lease.expenses.for.machinery.equipment.and.farm.share.of.vehicles",
    other = "All.other.production.expenses",
    fuel = "Gasoline.fuels.and.oils.purchased"
  ) |> 
  select(total_exp, avg_exp, rep_exp, utilities, labor_hired, labor_con, mech_rent, other, county, year, state, fuel) |> 
  mutate(
    labor_share = (labor_hired + labor_con) / total_exp,
    mech = (fuel+mech_rent+rep_exp)/total_exp,
    county_fips = paste(state, county, sep = "_"),  # Unique ID for FE
    post_2012 = (year == 2012),
    sc_treated = state %in% c("AZ", "CA"),
    treat_post = post_2012 * sc_treated,
    labor_mech_r = (labor_hired)/fuel
  ) |>
  filter(!is.na(labor_share), labor_share > 0 & labor_share < 1)  # Clean

summary(states_clean$labor_share)  # Check

# pretrends graphs
ggplot(states_clean, aes(year, labor_share, color = factor(sc_treated))) +
  stat_summary(fun=mean, geom="point", size=3) +
  stat_summary(fun=mean, geom="line") +
  labs(title="Pre-trends: Labor Share by SC Exposure", y = "Labor Share of Expenditures", color = "State") +
  theme_minimal()
ggplot(states_clean, aes(year, mech, color = factor(sc_treated))) +
  stat_summary(fun=mean, geom="point", size=3) +
  stat_summary(fun=mean, geom="line") +
  labs(title="Pre-trends: Mechanization", y = "Fuel/Maintenence Share of Expenditures", x = "Year", color = "State") +
  theme_minimal()
ggplot(states_clean, aes(year, labor_mech_r, color = factor(sc_treated))) +
  stat_summary(fun=mean, geom="point", size=3) +
  stat_summary(fun=mean, geom="line") +
  labs(title="Pre-trends: Labor-Mech Ratio", y = "Ratio of Labor to Mechanization Expenditures (Proxy)", x = "Year", color = "State") +
  theme_minimal()

# Pre-trends assumption check:
states_clean <- states_clean |> 
  mutate(rel_year = case_when(
    year == 2002 ~ -1,  # Single pre-period proxy
    year == 2007 ~ 0,   # Ref pre
    year == 2012 ~ 1    # Post
  ))

# Event study: leads (pre) vs post
es_labor <- feols(labor_share ~ i(rel_year, sc_treated, ref = 0) | county_fips + year, 
                  data = states_clean, cluster = ~state)
etable(es_labor)
# Repeat for mech, labor_mech_r
es_mech <- feols(mech ~ i(rel_year, sc_treated, ref = 0) | county_fips + year, 
                 data = states_clean, cluster = ~state)
iplot(es_labor, es_mech)  # Coeff plot


# some regressions
model1 <- feols(labor_share ~ post_2012 * sc_treated | county_fips + year, 
                data = states_clean, cluster = ~state)
etable(model1)  # Summary table

ggplot(states_clean, aes(year, labor_share, color = factor(sc_treated))) +
  stat_summary(fun=mean, geom="point", size=3) +
  stat_summary(fun=mean, geom="line") +
  labs(title="Pre-trends: Labor Share by SC Exposure")

# Stage 1: SC effect on mech
mech_model <- feols(mech ~ post_2012 * sc_treated | county_fips + year, 
                    data = states_clean, cluster = ~state)
etable(mech_model)
# Stage 2: Total effect (your model1)
labor_total <- feols(labor_share ~ post_2012 * sc_treated | county_fips + year, 
                     data = states_clean, cluster = ~state)

# Stage 2b: Direct effect (control for mech)
labor_mediated <- feols(labor_share ~ post_2012 * sc_treated + mech | county_fips + year, 
                        data = states_clean, cluster = ~state)

etable(list(Total = labor_total, Direct = labor_mediated))
### from this basic naive regression it seems like the story is told: in places who had experiencewd full SC activation by 2012, there was a meaningful decrease in labor share and increase in mechanization in treated states by 2012.
