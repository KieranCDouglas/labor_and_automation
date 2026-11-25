####################################################################################################
## 05: informality (Q3)
## last edited: 11/18/2026
## by kieran
## does: whether SC exposure shifted ag work away from formal payroll employment, and whether that
##       differs with vulnerability. formal jobs (QCEW: employer unemployment-insurance filings) are
##       compared with the ACS count of people working in ag (survey, by residence), each as a count
##       estimated by PPML (proportional changes), since the two cover different workers and places;
##       the gap is ln ACS wage workers minus ln QCEW jobs. also non-wage ag work (ACS) and the
##       group-quarters guestworker proxy (ACS). commuting zones, 2005-2022, using the shared
##       event-study specification in 00_setup.r.
## reads:  dropbox kieran/data/clean/{acs_cz_panel, qcew_cz_panel, acs_panel, qcew_panel}.rds (from
##         01_clean.r), kieran/output/vulnerability_choice.csv (from 02_vulnerability.r)
## writes: dropbox kieran/output/coefs/informality_*.csv, output/tables/informality_*.tex,
##         figures/informality/*.png
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
VULN_ALT    <- "het_noncit_val"   # robustness: the runner-up measure from 02_vulnerability.r

# counts are estimated by PPML (log points). the gap is ln ACS wage workers minus ln QCEW NAICS 11 jobs
# in the same unit and year (OLS), for units with both positive in every year: positive means ag work
# grew faster than the formal payroll jobs employers report.
# QCEW robustness: CZs where imputed cells (01_clean.r section 15) are under this share of 2005-07
# NAICS 11 employment
MAX_IMPUTED  <- 0.25

INFORMALITY_COUNTS <- c(
  "Formal Ag Jobs, QCEW (NAICS 11)"                   = "qcew_11",
  "Ag Wage Workers, ACS"                              = "ag_ws",
  "Farm Labor Contractors & Support, QCEW (NAICS 115)" = "qcew_115",
  "Self-Employed & Unpaid Family Ag Workers, ACS"     = "ag_nonws",
  "Ag Wage Workers in Group Quarters, ACS"            = "ag_ws_gq"
)
INFORMALITY_GAP <- c("Gap: ln ACS Wage Workers - ln QCEW Jobs" = "gap_ln")
OUTCOME_LEVELS  <- c(names(INFORMALITY_COUNTS)[1:2], names(INFORMALITY_GAP), names(INFORMALITY_COUNTS)[3:5])

####################################################################################################
### load data ###
####################################################################################################

# the ACS panel (treatment, controls, vulnerability and reliance measures) with the QCEW job counts
# joined by unit and year. restricted to units with the vulnerability measure, so the main effect and
# the vulnerability interaction are estimated on the same units. the gap is defined only for units
# with positive ACS wage workers and QCEW jobs in every year, so its sample does not change over time.
informality_panel <- function(acs, qcew, unit) {
  acs |>
    filter(!is.na(.data[[VULN]])) |>
    left_join(qcew |> select(all_of(c(unit, "year")), qcew_11, qcew_115, any_of("qcew_11_imputed")), by = c(unit, "year")) |>
    group_by(.data[[unit]]) |>
    mutate(gap_ln = if (all(!is.na(ag_ws) & ag_ws > 0 & !is.na(qcew_11) & qcew_11 > 0)) log(ag_ws) - log(qcew_11) else NA_real_) |>
    ungroup()
}

inf_cz     <- informality_panel(readRDS(file.path(CLEAN_DIR, "acs_cz_panel.rds")), readRDS(file.path(CLEAN_DIR, "qcew_cz_panel.rds")), "cz_id")
inf_county <- informality_panel(readRDS(file.path(CLEAN_DIR, "acs_panel.rds")),    readRDS(file.path(CLEAN_DIR, "qcew_panel.rds")),    "county_id")

# CZs whose QCEW NAICS 11 employment is mostly published (imputed share of 2005-07 jobs below MAX_IMPUTED)
low_imputed <- inf_cz |>
  filter(year %in% 2005:2007) |>
  group_by(cz_id) |>
  summarise(imp = sum(qcew_11 * qcew_11_imputed) / sum(qcew_11), .groups = "drop") |>
  filter(!is.na(imp), imp < MAX_IMPUTED)
inf_panels <- list(cz = inf_cz, county = inf_county, cz_lowimp = semi_join(inf_cz, low_imputed, by = "cz_id"))

message(sprintf("informality: %d CZs (%d with the gap; %d with < %.0f%% of QCEW jobs imputed), %d counties",
                n_distinct(inf_cz$cz_id), n_distinct(inf_cz$cz_id[!is.na(inf_cz$gap_ln)]), nrow(low_imputed),
                100 * MAX_IMPUTED, n_distinct(inf_county$county_id)))

####################################################################################################
### main specification: commuting zones ###
####################################################################################################

inf_coefs <- bind_rows(es_main_coefs(inf_cz, INFORMALITY_COUNTS, "cz_id", VULN, family = "pois"),
                       es_main_coefs(inf_cz, INFORMALITY_GAP, "cz_id", VULN)) |>
  mutate(outcome = factor(outcome, levels = OUTCOME_LEVELS))
write_csv(inf_coefs, file.path(COEFS_DIR, "informality_coefs.csv"))

####################################################################################################
### robustness ###
####################################################################################################
# the standard set from 00_setup.r, plus CZs where most QCEW employment is published rather than
# imputed. at county level the QCEW outcomes use the disclosed-only balanced counties (no imputation)
# and the ACS outcomes are allocated from PUMAs.
specs <- es_spec_set(VULN, VULN_ALT) |>
  add_row(spec = paste0("QCEW mostly published (< ", 100 * MAX_IMPUTED, "% imputed)"), data = "cz_lowimp", unit = "cz_id",
          time_fe = "state^year", cluster = "cz_id", vuln = VULN, suffix = "", baseline = FALSE, .before = 7)

inf_summary <- bind_rows(es_robustness(specs, inf_panels, INFORMALITY_COUNTS, family = "pois"),
                         es_robustness(specs, inf_panels, INFORMALITY_GAP)) |>
  mutate(outcome = factor(outcome, levels = OUTCOME_LEVELS))
write_csv(inf_summary, file.path(COEFS_DIR, "informality_summary.csv"))

# average post-2007 effect per year of exposure, in log points x 100 (2005-06 are the pre-period years)
es_main_table(inf_summary, file.path(TABLES_DIR, "informality_main.tex"), scale = 100)

inf_summary |>
  transmute(spec, outcome, term = if_else(term == "exposure", "per year", "x vuln"),
            cell = sprintf("%7.2f (%.2f) pre p %.2f", 100 * post_avg, 100 * post_se, pre_p)) |>
  pivot_wider(names_from = term, values_from = cell) |>
  print(n = Inf, width = Inf)

####################################################################################################
### heterogeneity ###
####################################################################################################
# the per-year effect in places 1 SD below and above the mean of each dimension, and by tercile:
# vulnerability, and pre-SC reliance on immigrant and on hired farm labor (ES_RELIANCE).
HET_DIMS <- c(set_names(paste0("Vulnerability (", VULN_CHOICE$label, ")"), VULN), ES_RELIANCE)

inf_het <- bind_rows(es_heterogeneity(inf_cz, INFORMALITY_COUNTS, "cz_id", HET_DIMS, family = "pois"),
                     es_heterogeneity(inf_cz, INFORMALITY_GAP, "cz_id", HET_DIMS)) |>
  mutate(outcome = factor(outcome, levels = OUTCOME_LEVELS))
write_csv(inf_het, file.path(COEFS_DIR, "informality_heterogeneity.csv"))
inf_terc <- bind_rows(es_terciles(inf_cz, INFORMALITY_COUNTS, "cz_id", HET_DIMS, family = "pois"),
                      es_terciles(inf_cz, INFORMALITY_GAP, "cz_id", HET_DIMS)) |>
  mutate(outcome = factor(outcome, levels = OUTCOME_LEVELS))
write_csv(inf_terc, file.path(COEFS_DIR, "informality_terciles.csv"))

####################################################################################################
### figures ###
####################################################################################################

Y_LAB  <- "Estimate Relative to 2007 (log points; PPML except the gap)"
FACETS <- facet_wrap(~outcome, scales = "free_y", ncol = 3)
LABELS <- scales::label_percent(accuracy = 1)

## 1. main effect ##---------------------------------------------------------------------------------
p <- inf_coefs |>
  filter(series == "Exposure") |>
  es_plot("Effect of One More Year of SC Exposure on Formal and Informal Ag Work, Commuting Zones 2005-2022 (95% CI)",
          Y_LAB, facet = FACETS, labels = LABELS)
ggsave(fig_path("informality", "informality_es.png"), p, width = 13, height = 7.5, dpi = 300)

## 2. by vulnerability ##----------------------------------------------------------------------------
p <- inf_coefs |>
  filter(series %in% c("Low vulnerability (-1 SD)", "High vulnerability (+1 SD)")) |>
  mutate(series = factor(series, levels = c("Low vulnerability (-1 SD)", "High vulnerability (+1 SD)"))) |>
  es_plot(paste0("Effect of One More Year of SC Exposure by Baseline ", VULN_LABEL, " (95% CI)"),
          Y_LAB, color = "series", palette = COL_LOWHIGH, facet = FACETS, labels = LABELS)
ggsave(fig_path("informality", "informality_es_vulnerability.png"), p, width = 13, height = 8, dpi = 300)

## 3. robustness ##---------------------------------------------------------------------------------
p <- es_robustness_plot(inf_summary, "Formal and Informal Ag Work Across Specifications (95% CI)",
                        "Average post-2007 estimate per year of exposure (units in column title)", labels = LABELS) +
  theme(strip.text.x = element_text(size = 7))
ggsave(fig_path("informality", "informality_robustness.png"), p, width = 18, height = 7, dpi = 300)

## 4. heterogeneity ##------------------------------------------------------------------------------
p <- es_heterogeneity_plot(inf_het, "Formal and Informal Ag Work by Vulnerability and Reliance on Farm Labor (95% CI)",
                           "Average post-2007 effect per year of exposure (units in column title)", labels = LABELS) +
  theme(strip.text.x = element_text(size = 7))
ggsave(fig_path("informality", "informality_heterogeneity.png"), p, width = 18, height = 4.5, dpi = 300)

p <- es_tercile_plot(inf_terc, "Formal and Informal Ag Work by Tercile of Vulnerability and Reliance on Farm Labor (95% CI)",
                     "Average post-2007 effect per year of exposure (units in column title)", labels = LABELS) +
  theme(strip.text.x = element_text(size = 7))
ggsave(fig_path("informality", "informality_terciles.png"), p, width = 18, height = 5, dpi = 300)
