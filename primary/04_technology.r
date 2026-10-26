####################################################################################################
## 04: technology (Q2)
## last edited: 10/26/2026
## by kieran
## does: whether SC exposure moved farms toward labor-replacing production, on two margins:
##       technology within crops (mechanization and labor shares of expenses, hired workers per
##       farm; census of ag 2002-2017) and crop choice (cropland in hand-harvested vegetables and orchards; census of
##       ag 2002-2022). commuting zones, using the shared event-study specification in 00_setup.r.
## reads:  dropbox kieran/data/clean/{agc_cz_panel, main, crop_cz_panel, crop_panel}.rds (from 01_clean.r),
##         kieran/output/vulnerability_choice.csv (from 02_vulnerability.r)
## writes: dropbox kieran/output/coefs/technology_*.csv, output/tables/technology_*.tex,
##         figures/technology/*.png
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

# outcomes in reporting units (the unit is in each label). census years only: 2002 is the one
# pre-period year, 2012 / 2017 (and 2022 for crop mix) are post.
TECH_OUTCOMES <- c(
  "Mechanization Share of Expenses (pp)" = "mech_pp",
  "Labor Share of Expenses (pp)"         = "labor_pp",
  "Hired Workers (per farm)"             = "hired_workers_per_farm"
)
CROP_OUTCOMES <- c(
  "Vegetables + Orchards (pp of cropland)" = "li_pp",
  "Vegetables (pp of cropland)"            = "veg_pp",
  "Orchards (pp of cropland)"              = "orch_pp"
)

####################################################################################################
### load data ###
####################################################################################################

# both geographies are restricted to units with the vulnerability measure, so the main effect and the
# vulnerability interaction are estimated on the same units
tech_units <- \(d) d |>
  filter(!is.na(.data[[VULN]])) |>
  mutate(mech_pp = 100 * mech_share_broad, labor_pp = 100 * labor_share)
crop_units <- \(d) d |>
  filter(!is.na(.data[[VULN]])) |>
  mutate(li_pp = 100 * li_share, veg_pp = 100 * veg_share, orch_pp = 100 * orch_share)

tech_panels <- list(cz = tech_units(readRDS(file.path(CLEAN_DIR, "agc_cz_panel.rds"))),
                    county = tech_units(readRDS(file.path(CLEAN_DIR, "main.rds"))))
crop_panels <- list(cz = crop_units(readRDS(file.path(CLEAN_DIR, "crop_cz_panel.rds"))),
                    county = crop_units(readRDS(file.path(CLEAN_DIR, "crop_panel.rds"))))

OUTCOME_LEVELS <- c(names(TECH_OUTCOMES), names(CROP_OUTCOMES))

####################################################################################################
### main specification: commuting zones ###
####################################################################################################

tech_coefs <- bind_rows(es_main_coefs(tech_panels$cz, TECH_OUTCOMES, "cz_id", VULN),
                        es_main_coefs(crop_panels$cz, CROP_OUTCOMES, "cz_id", VULN)) |>
  mutate(outcome = factor(outcome, levels = OUTCOME_LEVELS))
write_csv(tech_coefs, file.path(COEFS_DIR, "technology_coefs.csv"))

####################################################################################################
### robustness ###
####################################################################################################

specs <- es_spec_set(VULN, VULN_ALT)
tech_summary <- bind_rows(es_robustness(specs, tech_panels, TECH_OUTCOMES),
                          es_robustness(specs, crop_panels, CROP_OUTCOMES)) |>
  mutate(outcome = factor(outcome, levels = OUTCOME_LEVELS))
write_csv(tech_summary, file.path(COEFS_DIR, "technology_summary.csv"))

# average post-2007 effect per year of exposure, in each outcome's units (2002 is the pre-period year)
es_main_table(tech_summary, file.path(TABLES_DIR, "technology_main.tex"), scale = 1)

####################################################################################################
### heterogeneity ###
####################################################################################################
# the per-year effect in places 1 SD below and above the mean of each dimension: vulnerability, and
# pre-SC reliance on immigrant and on hired farm labor (ES_RELIANCE). main specification, CZs.
HET_DIMS <- c(set_names(paste0("Vulnerability (", VULN_CHOICE$label, ")"), VULN), ES_RELIANCE)

tech_het <- bind_rows(es_heterogeneity(tech_panels$cz, TECH_OUTCOMES, "cz_id", HET_DIMS),
                      es_heterogeneity(crop_panels$cz, CROP_OUTCOMES, "cz_id", HET_DIMS)) |>
  mutate(outcome = factor(outcome, levels = OUTCOME_LEVELS))
write_csv(tech_het, file.path(COEFS_DIR, "technology_heterogeneity.csv"))
tech_terc <- bind_rows(es_terciles(tech_panels$cz, TECH_OUTCOMES, "cz_id", HET_DIMS),
                       es_terciles(crop_panels$cz, CROP_OUTCOMES, "cz_id", HET_DIMS)) |>
  mutate(outcome = factor(outcome, levels = OUTCOME_LEVELS))
write_csv(tech_terc, file.path(COEFS_DIR, "technology_terciles.csv"))

# the crop-mix terciles under stricter comparisons: the further baseline characteristics x year
# (ES_BASELINE), and population size band x year fixed effects (places compared only within the same
# third of the population distribution, with no straight-line assumption about size)
CROP_TERC_OUTCOMES <- CROP_OUTCOMES[c("Vegetables + Orchards (pp of cropland)", "Orchards (pp of cropland)")]
crop_banded <- add_size_band(crop_panels$cz, "cz_id")
crop_terc_rob <- bind_rows(
  es_terciles(crop_panels$cz, CROP_TERC_OUTCOMES, "cz_id", HET_DIMS) |> mutate(variant = "Main specification"),
  es_terciles(crop_panels$cz, CROP_TERC_OUTCOMES, "cz_id", HET_DIMS, baseline = TRUE) |> mutate(variant = "+ baseline characteristics x year"),
  es_terciles(crop_banded, CROP_TERC_OUTCOMES, "cz_id", HET_DIMS, time_fe = "state^year + size_band^year") |> mutate(variant = "+ size band x year FE")
) |>
  mutate(variant = factor(variant, levels = c("Main specification", "+ baseline characteristics x year", "+ size band x year FE")))
write_csv(crop_terc_rob, file.path(COEFS_DIR, "technology_cropmix_terciles_robustness.csv"))

tech_summary |>
  transmute(spec, outcome, term = if_else(term == "exposure", "per year", "x vuln"),
            cell = sprintf("%7.3f (%.3f) pre p %.2f", post_avg, post_se, pre_p)) |>
  pivot_wider(names_from = term, values_from = cell) |>
  print(n = Inf, width = Inf)

####################################################################################################
### figures ###
####################################################################################################

Y_LAB  <- "Estimate Relative to 2007 (units in panel title)"
BREAKS <- c(2002, 2007, 2012, 2017, 2022)
FACETS <- facet_wrap(~outcome, scales = "free_y", ncol = 3)

## 1. main effect ##---------------------------------------------------------------------------------
p <- tech_coefs |>
  filter(series == "Exposure") |>
  es_plot("Effect of One More Year of SC Exposure on Technology and Crop Mix, Census of Ag Commuting Zones (95% CI)",
          Y_LAB, breaks = BREAKS, facet = FACETS, labels = scales::label_number(accuracy = 0.01), err_width = 1.2)
ggsave(fig_path("technology", "technology_es.png"), p, width = 12, height = 7, dpi = 300)

## 2. by vulnerability ##----------------------------------------------------------------------------
p <- tech_coefs |>
  filter(series %in% c("Low vulnerability (-1 SD)", "High vulnerability (+1 SD)")) |>
  mutate(series = factor(series, levels = c("Low vulnerability (-1 SD)", "High vulnerability (+1 SD)"))) |>
  es_plot(paste0("Effect of One More Year of SC Exposure by Baseline ", VULN_LABEL, " (95% CI)"),
          Y_LAB, color = "series", palette = COL_LOWHIGH, breaks = BREAKS, facet = FACETS,
          labels = scales::label_number(accuracy = 0.01))
ggsave(fig_path("technology", "technology_es_vulnerability.png"), p, width = 12, height = 7.5, dpi = 300)

## 3. robustness ##---------------------------------------------------------------------------------
p <- es_robustness_plot(tech_summary, "Technology and Crop Mix Effects Across Specifications (95% CI)",
                        "Average post-2007 estimate per year of exposure (units in column title)",
                        labels = scales::label_number(accuracy = 0.01)) +
  theme(strip.text.x = element_text(size = 8))
ggsave(fig_path("technology", "technology_robustness.png"), p, width = 17, height = 6.5, dpi = 300)

## 4. heterogeneity ##------------------------------------------------------------------------------
p <- es_heterogeneity_plot(tech_het, "Technology and Crop Mix Effects by Vulnerability and Reliance on Farm Labor (95% CI)",
                           "Average post-2007 effect per year of exposure (units in column title)",
                           labels = scales::label_number(accuracy = 0.01)) +
  theme(strip.text.x = element_text(size = 8))
ggsave(fig_path("technology", "technology_heterogeneity.png"), p, width = 18, height = 4.5, dpi = 300)

p <- es_tercile_plot(tech_terc, "Technology and Crop Mix Effects by Tercile of Vulnerability and Reliance on Farm Labor (95% CI)",
                     "Average post-2007 effect per year of exposure (units in column title)",
                     labels = scales::label_number(accuracy = 0.01)) +
  theme(strip.text.x = element_text(size = 8))
ggsave(fig_path("technology", "technology_terciles.png"), p, width = 18, height = 5, dpi = 300)

## 5. crop mix by tercile, stricter comparisons ##----------------------------------------------------
p <- crop_terc_rob |>
  mutate(outcome = factor(paste0(str_remove(outcome, " \\(pp of cropland\\)"), "\n", variant),
                          levels = as.vector(outer(str_remove(names(CROP_TERC_OUTCOMES), " \\(pp of cropland\\)"), levels(variant), paste, sep = "\n")))) |>
  es_tercile_plot("Crop Mix by Tercile of Vulnerability and Reliance on Farm Labor, Stricter Comparisons (95% CI)",
                  "Average post-2007 effect per year of exposure (pp of cropland)", labels = scales::label_number(accuracy = 0.1)) +
  theme(strip.text.x = element_text(size = 8))
ggsave(fig_path("technology", "cropmix_terciles_robustness.png"), p, width = 18, height = 5, dpi = 300)
