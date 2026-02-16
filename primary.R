# Install/load once
install.packages(c("rnassqs", "tidyverse"))
library(rnassqs)
library(tidyverse)

# 1) Set your QuickStats API key (string)
nassqs_auth(key = "")  # key must be in quotes [web:6][web:11]

# 2) Pull Census of Ag data (example: 2007, Alabama, county level)
# NOTE: QuickStats uses lots of fields; start broad, then narrow.
al_2007 <- nassqs(
  source_desc     = "CENSUS",
  sector_desc     = "ECONOMICS",
  group_desc      = "EXPENSES",
  agg_level_desc  = "COUNTY",
  state_alpha     = "AL",
  year            = 2012
)

glimpse(al_2007)

# 3) Typical cleanup: keep key columns + convert Value to numeric
al_2007_clean <- al_2007 %>%
  transmute(
    county   = county_name,
    variable = short_desc,
    value    = suppressWarnings(readr::parse_number(Value)),
    unit     = unit_desc
  )

# 4) If you want wide format (one row per county)
al_2007_wide <- al_2007_clean %>%
  select(county, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

write.csv(al_2007_wide, "alabama_county_expenses_2007.csv", row.names = FALSE)
head(al_2007_wide)
