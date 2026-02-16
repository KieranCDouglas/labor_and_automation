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

# rbind WITH state identifier
states <- rbind(
  mutate(arizona, state = "AZ"),
  mutate(bama, state = "AL"),
  mutate(california, state = "CA"),
  mutate(mas, state = "MA")
)

states_clean <- states |> 
  rename(
    total_exp = "Total.farm.production.expenditures",
    avg_exp = "Average.per.farm",
    rep_exp = "Repairs.supplies.and.maintenance.cost",
    utilities = "Utilities",
    labor_hired = "Hired.farm.labor",
    labor_con = "Contract.labor",
    mech_rent = "Rent.and.lease.expenses.for.machinery.equipment.and.farm.share.of.vehicles",
    other = "All.other.production.expenses"
  ) |> 
  select(total_exp, avg_exp, rep_exp, utilities, labor_hired, labor_con, mech_rent, other, county, year, state) |> 
  mutate(
    labor_share = (labor_hired + labor_con) / total_exp,
    mech = (mech_rent+rep_exp)/total_exp,
    county_fips = paste(state, county, sep = "_"),  # Unique ID for FE
    post_2012 = (year == 2012),
    sc_treated = state %in% c("AZ", "CA"),
    treat_post = post_2012 * sc_treated
  ) |>
  filter(!is.na(labor_share), labor_share > 0 & labor_share < 1)  # Clean

summary(states_clean$labor_share)  # Check


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
