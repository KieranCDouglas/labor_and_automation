####################################################################################################
## 02: models
## last edited: 10/01/2026
## by kieran
## does: estimates event-study and DDD models on the cleaned data.
## reads:  dropbox kieran/data/clean/*.rds (from 01_clean.r)
## writes: dropbox kieran/output/coefs/*.csv (tidy coefficients, read by 03_figs.r)
##         dropbox kieran/output/tables/*.tex (regression tables)
####################################################################################################

####################################################################################################
### prelude ###
####################################################################################################

pkgs <- c("tidyverse", "fixest", "broom")
install.packages(setdiff(pkgs, rownames(installed.packages())))

library(tidyverse)
library(fixest)
library(broom)

# data and outputs live in dropbox, code lives in the repo
ROOT       <- "/Users/kieran/Library/CloudStorage/Dropbox/secure_communities/kieran"
CLEAN_DIR  <- file.path(ROOT, "data", "clean")
COEFS_DIR  <- file.path(ROOT, "output", "coefs")
TABLES_DIR <- file.path(ROOT, "output", "tables")
dir.create(COEFS_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)

# tidy a named list of models into one coefficient table with an `outcome` column
tidy_models <- function(mods) {
  mods |>
    map(\(mod) tidy(mod, conf.int = TRUE, conf.level = 0.95)) |>
    bind_rows(.id = "outcome")
}

####################################################################################################
### load data ###
####################################################################################################

main                    <- readRDS(file.path(CLEAN_DIR, "main.rds"))
sc_county               <- readRDS(file.path(CLEAN_DIR, "sc_county.rds"))
noncit_split            <- readRDS(file.path(CLEAN_DIR, "noncit_split.rds"))
sc_detainer_rate_yearly <- readRDS(file.path(CLEAN_DIR, "sc_detainer_rate_yearly.rds"))
het_config              <- readRDS(file.path(CLEAN_DIR, "het_config.rds"))

het_vars <- paste0("het_", het_config$measure)

# outcomes looped over in the heterogeneity models, named by their plot/table label
outcomes <- c(
  "Hired Labor Exp Per Acre" = "hired_labor_exp_per_acre",
  "Hired Workers Per Acre"   = "hired_workers_per_acre",
  "Mechanization Share"      = "mech_share_broad"
)

# fit one model per het measure x outcome from a formula template with %1$s = outcome, %2$s = het var.
# returns a tibble with het, outcome and the fitted model in a list column.
# defaults to the 4-period panel; pass data/outs for the ACS long panel.
fit_het_grid <- function(template, hets, data = main, outs = outcomes, cluster = ~county_id) {
  expand_grid(het = hets, outcome = names(outs)) |>
    mutate(mod = map2(het, outcome, \(h, o)
      feols(as.formula(sprintf(template, outs[[o]], h)), data = data, cluster = cluster)))
}

# write county- and state-clustered tex tables per het measure, and one tidy coefficient csv
save_het_grid <- function(grid, name) {
  for (h in unique(grid$het)) {
    mods <- grid |> filter(het == h) |> pull(mod)
    hdrs <- grid |> filter(het == h) |> pull(outcome)
    print(etable(mods, headers = hdrs))
    etable(mods, headers = hdrs, tex = TRUE,
           file = file.path(TABLES_DIR, paste0(name, "_", str_remove(h, "^het_"), ".tex")), replace = TRUE)
    etable(mods, headers = hdrs, cluster = ~state, tex = TRUE,
           file = file.path(TABLES_DIR, paste0(name, "_", str_remove(h, "^het_"), "_stateclust.tex")), replace = TRUE)
  }
  grid |>
    mutate(coefs = map(mod, \(m) tidy(m, conf.int = TRUE, conf.level = 0.95))) |>
    select(het, outcome, coefs) |>
    unnest(coefs) |>
    write_csv(file.path(COEFS_DIR, paste0(name, ".csv")))
}

####################################################################################################
### event study justification: SC removals by group ###
####################################################################################################
# removal rate relative to each county's own activation year, split four ways by early/late activation
# and high/low noncitizen share. one cell-means regression per group so each gets its own CIs.
es_group_data <- sc_detainer_rate_yearly |>
  left_join(sc_county |> select(state, county, early_activator, first_detainer_year),
            by = c("state", "county")) |>
  left_join(noncit_split, by = c("state", "county")) |>
  filter(!is.na(early_activator), !is.na(noncit_bin), year <= 2015) |>
  mutate(
    rel_year = year - first_detainer_year,
    county_id = paste(state, county, sep = ", ")
  ) |>
  filter(rel_year >= -4, rel_year <= 4)
# define groups of interest and fit individual models per group
group_defs <- list(
  "Late activator (2011+) / High noncitizen share (>1% pop.)" = list(early = 0, noncit = 1),
  "Late activator (2011+) / Low noncitizen share (≤1% pop.)"  = list(early = 0, noncit = 0),
  "Early activator (<2011) / High noncitizen share (>1% pop.)" = list(early = 1, noncit = 1),
  "Early activator (<2011) / Low noncitizen share (≤1% pop.)"  = list(early = 1, noncit = 0)
)
es_cellmeans <- group_defs |>
  map(\(g) es_group_data |> filter(early_activator == g$early, noncit_bin == g$noncit)) |>
  map(\(d) feols(detainer_rate ~ 0 + factor(rel_year), data = d, cluster = ~county_id)) |>
  map(\(mod) tidy(mod, conf.int = TRUE, conf.level = 0.95)) |>
  bind_rows(.id = "group") |>
  mutate(rel_year = as.integer(str_extract(term, "-?\\d+$"))) |>
  separate(group, into = c("activation_label", "noncit_label"), sep = " / ")

write_csv(es_cellmeans, file.path(COEFS_DIR, "es_detainer_groups.csv"))

####################################################################################################
### ES per period ###
####################################################################################################
# this runs an es-style did for each of the three periods: pre, interim, and post
# compares early to late activated counties to understand how program duration affects outcomes
# first difs are between pre and post period for early and late activations while second are difs
# between those two. for now, we ignore exposure intensity heterogeneity and endogeneity of regressors.
# early is defined at pre 2012 and late is defined at post 2012 rollout.

# fits early_activator x year on a two-census window, with the first year as reference
period_mod <- function(outcome, years) {
  feols(
    as.formula(paste(outcome, "~ early_activator * i(year, ref =", years[1], ") | county_id + state^year")),
    data = main |> filter(year %in% years),
    cluster = ~county_id
  )
}

## starting off with hired_labor_exp_per_acre as the outcome variable
pre_mod1 <- period_mod("hired_labor_exp_per_acre", c(2002, 2007))  # pre trends
int_mod1 <- period_mod("hired_labor_exp_per_acre", c(2007, 2012))  # interim
lr_mod1  <- period_mod("hired_labor_exp_per_acre", c(2012, 2017))  # longer run
etable(pre_mod1, int_mod1, lr_mod1)

## now looking at hired_workers_per_acre
pre_mod2 <- period_mod("hired_workers_per_acre", c(2002, 2007))
int_mod2 <- period_mod("hired_workers_per_acre", c(2007, 2012))
lr_mod2  <- period_mod("hired_workers_per_acre", c(2012, 2017))
etable(pre_mod2, int_mod2, lr_mod2)

## finally looking at mech_share_broad
pre_mod3   <- period_mod("mech_share_broad", c(2002, 2007))
int_mod3   <- period_mod("mech_share_broad", c(2007, 2012))
lr_mod3    <- period_mod("mech_share_broad", c(2012, 2017))
final_mod4 <- period_mod("mech_share_broad", c(2007, 2017))  # 2007 to 2017
etable(pre_mod3, int_mod3, lr_mod3, final_mod4)

# all table
etable(pre_mod1, int_mod1, lr_mod1, pre_mod2, int_mod2, lr_mod2, pre_mod3, int_mod3, lr_mod3, final_mod4,
       tex = TRUE, file = file.path(TABLES_DIR, "es_periods.tex"), replace = TRUE)

####################################################################################################
### full es: year-specific treatment effects ###
####################################################################################################
# instead of the pairwise two-period comparisons above, estimate all years at once:
# y_jt = sum_s beta_s * early_j * 1{t = s} + gamma_j + delta_{t,state(j)} + e_jt
# ref year is 2007 (last pre-activation census), so beta_2002 is a placebo/pretrend check
# and beta_2012, beta_2017 trace out the dynamic treatment effects. the early_j main effect
# is absorbed by county FE and the year main effects by state^year FE.

es_full1 <- feols(
  hired_labor_exp_per_acre ~ i(year, early_activator, ref = 2007) | county_id + state^year,
  data = main,
  cluster = ~county_id
)

es_full2 <- feols(
  hired_workers_per_acre ~ i(year, early_activator, ref = 2007) | county_id + state^year,
  data = main,
  cluster = ~county_id
)

es_full3 <- feols(
  mech_share_broad ~ i(year, early_activator, ref = 2007) | county_id + state^year,
  data = main,
  cluster = ~county_id
)

etable(es_full1, es_full2, es_full3)
etable(es_full1, es_full2, es_full3,
       tex = TRUE, file = file.path(TABLES_DIR, "es_full.tex"), replace = TRUE)

list(
  "Hired Labor Exp Per Acre" = es_full1,
  "Hired Workers Per Acre"   = es_full2,
  "Mechanization Share"      = es_full3
) |>
  tidy_models() |>
  write_csv(file.path(COEFS_DIR, "es_full.csv"))

####################################################################################################
### es by exposure heterogeneity measure ###
####################################################################################################
# same year-specific spec as the full es above, with early_activator swapped for each het_<m> in turn:
# y_jt = sum_s beta_s * het_j * 1{t = s} + gamma_j + delta_{t,state(j)} + e_jt
# het_early reproduces the full es (it is the same early/late split).

es_het <- fit_het_grid("%1$s ~ i(year, %2$s, ref = 2007) | county_id + state^year", het_vars)
save_het_grid(es_het, "es_het")

####################################################################################################
### DDD estimation by exposure heterogeneity measure ###
####################################################################################################
# regression-based DDD in the early/late duration framing:
# y_jt = sum_s beta_s (early_j x high_j x 1{t=s}) + early_j x year + high_j x year + gamma_j + delta_t + e_jt
# triple term is the extra early-vs-late effect in high-exposure counties, relative to 2007.
# δ_DDD = [((y_111-y_101)-(y_011-y_001))-((y_110-y_100)-(y_010-y_000))]
# where G is the treatment/control group, S is the third dim partition, and T is the time window.
# het_early is skipped: it is the treatment itself, so there is no third difference.
# with state^year FE gone, activation timing varies mostly at the state level, so state-clustered
# tables are also written as the conservative benchmark.

ddd_hets <- setdiff(het_vars, "het_early")

main <- main |>
  mutate(across(all_of(ddd_hets), \(x) early_activator * x, .names = "early_x_{.col}"))

ddd_het <- fit_het_grid(
  "%1$s ~ i(year, early_x_%2$s, ref = 2007) + i(year, early_activator, ref = 2007) + i(year, %2$s, ref = 2007) | county_id + year",
  ddd_hets
)
save_het_grid(ddd_het, "ddd_het")

####################################################################################################
### long panel (ACS 2006-2020): labor outcomes ###
####################################################################################################
# same event study and DDD as above, on the yearly ACS ag workforce panel instead of the 4 census
# years. outcomes are ag workers (16-64, employed) as a share of the county's 2006-07 working-age
# population. ref year 2007 is the last full pre-SC year, so 2006 is a placebo lead.
# the ACS values are allocated from PUMAs, so counties in the same PUMA move together: county
# clustering understates uncertainty, and the state-clustered tables are the safer benchmark.

acs_panel <- readRDS(file.path(CLEAN_DIR, "acs_panel.rds")) |>
  mutate(across(all_of(ddd_hets), \(x) early_activator * x, .names = "early_x_{.col}"))

acs_outcomes <- c(
  "Total Ag Workforce"        = "ag_total_share",
  "Foreign-Born Ag Workforce" = "ag_fb_share",
  "US-Born Ag Workforce"      = "ag_usb_share",
  "Noncitizen Ag Workforce"   = "ag_noncit_share"
)

# event study: early activation x year
es_acs <- fit_het_grid("%1$s ~ i(year, %2$s, ref = 2007) | county_id + state^year", "early_activator",
                       data = acs_panel, outs = acs_outcomes)
save_het_grid(es_acs, "es_acs")

# DDD: early activation x high exposure x year, for each het measure
ddd_acs <- fit_het_grid(
  "%1$s ~ i(year, early_x_%2$s, ref = 2007) + i(year, early_activator, ref = 2007) + i(year, %2$s, ref = 2007) | county_id + year",
  ddd_hets, data = acs_panel, outs = acs_outcomes
)
save_het_grid(ddd_acs, "ddd_acs")

####################################################################################################
### long panel at the commuting-zone level (ACS 2006-2020) ###
####################################################################################################
# same specifications on the CZ version of the ACS panel. CZs mostly contain whole PUMAs, so outcomes
# are close to direct survey estimates and errors clustered by CZ are not inflated by allocation.
# early_activator = 1 if at least CZ_EARLY_SHARE of the CZ's population activated by 2011; het
# splits use the CZ cuts from 01_clean.r. CZs are assigned the state of their most populous county
# for the state x year fixed effects and the state-clustered tables.

acs_cz_panel <- readRDS(file.path(CLEAN_DIR, "acs_cz_panel.rds")) |>
  mutate(across(all_of(ddd_hets), \(x) early_activator * x, .names = "early_x_{.col}"))

es_acs_cz <- fit_het_grid("%1$s ~ i(year, %2$s, ref = 2007) | cz_id + state^year", "early_activator",
                          data = acs_cz_panel, outs = acs_outcomes, cluster = ~cz_id)
save_het_grid(es_acs_cz, "es_acs_cz")

ddd_acs_cz <- fit_het_grid(
  "%1$s ~ i(year, early_x_%2$s, ref = 2007) + i(year, early_activator, ref = 2007) + i(year, %2$s, ref = 2007) | cz_id + year",
  ddd_hets, data = acs_cz_panel, outs = acs_outcomes, cluster = ~cz_id
)
save_het_grid(ddd_acs_cz, "ddd_acs_cz")
