####################################################################################################
## 03: labor (Q1)
## last edited: 10/22/2026
## by kieran
## does: how SC activation timing affected ag employment of US-born versus foreign-born workers, and
##       how that differs with ex ante vulnerability. ACS ag workforce by nativity, commuting zones,
##       2005-2022, using the shared event-study specification in 00_setup.r.
## reads:  dropbox kieran/data/clean/{acs_cz_panel, acs_panel}.rds (from 01_clean.r),
##         kieran/output/vulnerability_choice.csv (from 02_vulnerability.r)
## writes: dropbox kieran/output/coefs/labor_*.csv, output/tables/labor_*.tex, figures/labor/*.png
####################################################################################################

####################################################################################################
### prelude ###
####################################################################################################

pkgs <- c("tidyverse", "fixest")
install.packages(setdiff(pkgs, rownames(installed.packages())))

library(tidyverse)
library(fixest)

setFixest_notes(FALSE)

source("/Users/kieran/Documents/GitHub/labor_and_automation/primary/00_setup.r")

VULN_CHOICE <- read_csv(file.path(ROOT, "output", "vulnerability_choice.csv"), show_col_types = FALSE)
VULN        <- VULN_CHOICE$measure
VULN_LABEL  <- str_remove(VULN_CHOICE$label, ", \\d{4}$")
VULN_ALT   <- "het_noncit_val"   # robustness: the runner-up measure from 02_vulnerability.r

# ag workers (employed, 16-64) as a share of the unit's 2006-07 working-age population
LABOR_OUTCOMES <- c(
  "Total Ag Workforce"        = "ag_total_share",
  "US-Born Ag Workforce"      = "ag_usb_share",
  "Foreign-Born Ag Workforce" = "ag_fb_share",
  "Noncitizen Ag Workforce"   = "ag_noncit_share"
)

####################################################################################################
### load data ###
####################################################################################################

# both geographies are restricted to units with the vulnerability measure, so the main effect and the
# vulnerability interaction are estimated on the same units
acs_cz     <- readRDS(file.path(CLEAN_DIR, "acs_cz_panel.rds")) |> filter(!is.na(.data[[VULN]]))
acs_county <- readRDS(file.path(CLEAN_DIR, "acs_panel.rds"))    |> filter(!is.na(.data[[VULN]]))

####################################################################################################
### main specification: commuting zones ###
####################################################################################################

labor_coefs <- es_main_coefs(acs_cz, LABOR_OUTCOMES, "cz_id", VULN)
write_csv(labor_coefs, file.path(COEFS_DIR, "labor_coefs.csv"))

####################################################################################################
### robustness ###
####################################################################################################
# the standard set from 00_setup.r, plus "Households only": counts that exclude group quarters in
# every year, so 2005 (households-only by design) is measured the same way as the later years.
specs <- es_spec_set(VULN, VULN_ALT) |>
  add_row(spec = "Households only", data = "cz", unit = "cz_id", time_fe = "state^year", cluster = "cz_id",
          vuln = VULN, suffix = "_hh", baseline = FALSE, .before = 6)

labor_summary <- es_robustness(specs, list(cz = acs_cz, county = acs_county), LABOR_OUTCOMES)
write_csv(labor_summary, file.path(COEFS_DIR, "labor_summary.csv"))

# average post-2007 effect per year of exposure, in pp of baseline working-age pop. (2005 and 2006
# are the pre-period years; 2005 excludes group quarters)
es_main_table(labor_summary, file.path(TABLES_DIR, "labor_main.tex"), scale = 100)

####################################################################################################
### heterogeneity ###
####################################################################################################
# the per-year effect in places 1 SD below and above the mean of each dimension: vulnerability, and
# pre-SC reliance on immigrant and on hired farm labor (ES_RELIANCE). main specification, CZs.
HET_DIMS <- c(set_names(paste0("Vulnerability (", VULN_CHOICE$label, ")"), VULN), ES_RELIANCE)

labor_het <- es_heterogeneity(acs_cz, LABOR_OUTCOMES, "cz_id", HET_DIMS)
write_csv(labor_het, file.path(COEFS_DIR, "labor_heterogeneity.csv"))
labor_terc <- es_terciles(acs_cz, LABOR_OUTCOMES, "cz_id", HET_DIMS)
write_csv(labor_terc, file.path(COEFS_DIR, "labor_terciles.csv"))

####################################################################################################
### figures ###
####################################################################################################

Y_LAB <- "Estimate Relative to 2007 (share of baseline working-age pop.)"

## 1. main effect by nativity ##---------------------------------------------------------------------
p <- labor_coefs |>
  filter(series == "Exposure") |>
  es_plot("Effect of One More Year of SC Exposure on the Ag Workforce, ACS Commuting Zones 2005-2022 (95% CI)", Y_LAB)
ggsave(fig_path("labor", "labor_es.png"), p, width = 10, height = 7, dpi = 300)

## 2. by vulnerability ##----------------------------------------------------------------------------
# the per-year exposure effect in a CZ 1 SD below and 1 SD above mean vulnerability
p <- labor_coefs |>
  filter(series %in% c("Low vulnerability (-1 SD)", "High vulnerability (+1 SD)")) |>
  mutate(series = factor(series, levels = c("Low vulnerability (-1 SD)", "High vulnerability (+1 SD)"))) |>
  es_plot(paste0("Effect of One More Year of SC Exposure by Baseline ", VULN_LABEL, " (95% CI)"),
          Y_LAB, color = "series", palette = COL_LOWHIGH)
ggsave(fig_path("labor", "labor_es_vulnerability.png"), p, width = 10, height = 7, dpi = 300)

## 3. robustness ##---------------------------------------------------------------------------------
p <- es_robustness_plot(labor_summary, "Ag Workforce Effects Across Specifications (95% CI)",
                        "Average post-2007 estimate (share of baseline working-age pop.)")
ggsave(fig_path("labor", "labor_robustness.png"), p, width = 14, height = 6.5, dpi = 300)

## 4. heterogeneity ##------------------------------------------------------------------------------
p <- es_heterogeneity_plot(labor_het, "Ag Workforce Effects by Vulnerability and Reliance on Farm Labor (95% CI)",
                           "Average post-2007 effect per year of exposure (share of baseline working-age pop.)")
ggsave(fig_path("labor", "labor_heterogeneity.png"), p, width = 15, height = 4.5, dpi = 300)

p <- es_tercile_plot(labor_terc, "Ag Workforce Effects by Tercile of Vulnerability and Reliance on Farm Labor (95% CI)",
                     "Average post-2007 effect per year of exposure (share of baseline working-age pop.)")
ggsave(fig_path("labor", "labor_terciles.png"), p, width = 15, height = 5, dpi = 300)
