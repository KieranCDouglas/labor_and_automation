####################################################################################################
## 03: labor (Q1)
## last edited: 10/26/2026
## by kieran
## does: how SC activation timing affected ag employment of US-born versus foreign-born workers, and
##       how that differs with ex ante vulnerability. ACS ag workforce by nativity, and QCEW formal ag
##       payroll jobs by industry, both relative to the unit's working-age population. commuting zones,
##       2005-2022, using the shared event-study specification in 00_setup.r.
## reads:  dropbox kieran/data/clean/{acs_cz_panel, acs_panel, qcew_cz_panel, qcew_panel}.rds (from
##         01_clean.r), kieran/output/vulnerability_choice.csv (from 02_vulnerability.r)
## writes: dropbox kieran/output/coefs/labor_*.csv, output/tables/labor_*.tex, figures/labor/*.png,
##         figures/diagnostics/acs_qcew_*.png
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

# ag workers (employed, 16-64, ACS, by residence) and formal ag payroll jobs (QCEW, by workplace), each
# as a share of the unit's 2006-07 working-age population. QCEW covers employers above the UI
# threshold and mostly exempts H-2A workers. county QCEW keeps only counties disclosed in every year;
# CZ QCEW fills suppressed county cells by imputation (01_clean.r sections 15-16).
LABOR_OUTCOMES <- c(
  "Total Ag Workforce, ACS"                      = "ag_total_share",
  "US-Born Ag Workforce, ACS"                    = "ag_usb_share",
  "Foreign-Born Ag Workforce, ACS"               = "ag_fb_share",
  "Noncitizen Ag Workforce, ACS"                 = "ag_noncit_share",
  "Formal Ag Jobs, QCEW (NAICS 11)"              = "qcew_11_share",
  "Crop Production Jobs, QCEW (NAICS 111)"       = "qcew_111_share",
  "Animal Production Jobs, QCEW (NAICS 112)"     = "qcew_112_share",
  "Support & Labor Contractor Jobs, QCEW (NAICS 115)" = "qcew_115_share"
)
QCEW_SHARES <- str_subset(LABOR_OUTCOMES, "^qcew_")

####################################################################################################
### load data ###
####################################################################################################

# the ACS panel (treatment, controls, vulnerability and reliance measures) with the QCEW job shares
# joined by unit and year. both geographies are restricted to units with the vulnerability measure, so
# the main effect and the vulnerability interaction are estimated on the same units.
labor_panel <- function(acs, qcew, unit) {
  acs |>
    filter(!is.na(.data[[VULN]])) |>
    left_join(qcew |> select(all_of(c(unit, "year", QCEW_SHARES))), by = c(unit, "year"))
}
acs_cz     <- labor_panel(readRDS(file.path(CLEAN_DIR, "acs_cz_panel.rds")), readRDS(file.path(CLEAN_DIR, "qcew_cz_panel.rds")), "cz_id")
acs_county <- labor_panel(readRDS(file.path(CLEAN_DIR, "acs_panel.rds")),    readRDS(file.path(CLEAN_DIR, "qcew_panel.rds")),    "county_id")

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

## by vulnerability quartile, with and without the population control, counties and CZs ##----------
# the year-by-year exposure effect within each quartile of vulnerability (es_group_fit), so the ACS
# workforce and QCEW job series can be read side by side for the most vulnerable places. the effects
# are concentrated in the top quartile and in the most agricultural counties, which the linear
# interaction and CZ terciles average away.
# each model is fit pooled and by quartile, with the main controls ("after") and without log
# population x year ("before"). early activators were larger places with different ag-employment
# trends: without the control the ACS workforce rises and QCEW jobs fall with exposure in every
# quartile; with it the pooled effects go to about zero and the changes are confined to the top
# quartile. both models use the same units (those with log population).
N_QUART    <- 4
POP_MODELS <- list(before = setdiff(ES_CONTROLS, "log_pop_2010"), after = ES_CONTROLS)
quart_panels <- list(county = list(d = filter(acs_county, !is.na(log_pop_2010)), unit = "county_id"),
                     cz     = list(d = filter(acs_cz,     !is.na(log_pop_2010)), unit = "cz_id"))

quart_mods <- expand_grid(level = names(quart_panels), outcome = names(LABOR_OUTCOMES), model = names(POP_MODELS)) |>
  mutate(pooled = pmap(list(level, outcome, model), \(l, o, m)
                       fit_es(quart_panels[[l]]$d, LABOR_OUTCOMES[[o]], quart_panels[[l]]$unit, controls = POP_MODELS[[m]])),
         quart  = pmap(list(level, outcome, model), \(l, o, m)
                       es_group_fit(quart_panels[[l]]$d, LABOR_OUTCOMES[[o]], quart_panels[[l]]$unit, VULN, N_QUART, controls = POP_MODELS[[m]])))

# year-by-year coefficients: quartile 1-4 from the quartile model, quartile NA from the pooled model
labor_quart_coefs <- quart_mods |>
  mutate(c = map2(pooled, quart, \(mp, mq) bind_rows(
    es_coefs(mp),
    map(seq_len(N_QUART), \(g) es_coefs(mq, paste0("x_g", g)) |> mutate(quartile = g)) |> list_rbind()))) |>
  select(level, outcome, model, c) |>
  unnest(c)
write_csv(labor_quart_coefs, file.path(COEFS_DIR, "labor_quartile_coefs.csv"))

labor_quart_summary <- quart_mods |>
  mutate(s = map2(pooled, quart, \(mp, mq) bind_rows(
    es_summary(mp) |> mutate(group = "All units"),
    map(seq_len(N_QUART), \(g) es_summary(mq, paste0("x_g", g)) |> mutate(group = paste0("Q", g))),
    es_group_gap(mq, N_QUART) |> mutate(group = paste0("Q", N_QUART, " - Q1"))))) |>
  select(level, outcome, model, s) |>
  unnest(s)
write_csv(labor_quart_summary, file.path(COEFS_DIR, "labor_quartile_summary.csv"))

####################################################################################################
### figures ###
####################################################################################################

Y_LAB  <- "Estimate Relative to 2007 (share of baseline working-age pop.)"
FACETS <- facet_wrap(~outcome, scales = "free_y", ncol = 4)
STRIPS <- theme(strip.text.x = element_text(size = 7))

## 1. main effect by nativity and industry ##--------------------------------------------------------
p <- labor_coefs |>
  filter(series == "Exposure") |>
  es_plot("Effect of One More Year of SC Exposure on the Ag Workforce (ACS) and Formal Ag Jobs (QCEW), Commuting Zones 2005-2022 (95% CI)",
          Y_LAB, facet = FACETS)
ggsave(fig_path("labor", "labor_es.png"), p, width = 15, height = 7.5, dpi = 300)

## 2. by vulnerability ##----------------------------------------------------------------------------
# the per-year exposure effect in a CZ 1 SD below and 1 SD above mean vulnerability
p <- labor_coefs |>
  filter(series %in% c("Low vulnerability (-1 SD)", "High vulnerability (+1 SD)")) |>
  mutate(series = factor(series, levels = c("Low vulnerability (-1 SD)", "High vulnerability (+1 SD)"))) |>
  es_plot(paste0("Effect of One More Year of SC Exposure by Baseline ", VULN_LABEL, " (95% CI)"),
          Y_LAB, color = "series", palette = COL_LOWHIGH, facet = FACETS)
ggsave(fig_path("labor", "labor_es_vulnerability.png"), p, width = 15, height = 8, dpi = 300)

## 3. robustness ##---------------------------------------------------------------------------------
p <- es_robustness_plot(labor_summary, "Ag Workforce and Formal Ag Job Effects Across Specifications (95% CI)",
                        "Average post-2007 estimate (share of baseline working-age pop.)") + STRIPS
ggsave(fig_path("labor", "labor_robustness.png"), p, width = 22, height = 6.5, dpi = 300)

## 4. heterogeneity ##------------------------------------------------------------------------------
p <- es_heterogeneity_plot(labor_het, "Ag Workforce and Formal Ag Job Effects by Vulnerability and Reliance on Farm Labor (95% CI)",
                           "Average post-2007 effect per year of exposure (share of baseline working-age pop.)") + STRIPS
ggsave(fig_path("labor", "labor_heterogeneity.png"), p, width = 22, height = 4.5, dpi = 300)

p <- es_tercile_plot(labor_terc, "Ag Workforce and Formal Ag Job Effects by Tercile of Vulnerability and Reliance on Farm Labor (95% CI)",
                     "Average post-2007 effect per year of exposure (share of baseline working-age pop.)") + STRIPS
ggsave(fig_path("labor", "labor_terciles.png"), p, width = 22, height = 5, dpi = 300)

## 5. ACS and QCEW side by side, by vulnerability quartile ##----------------------------------------
# left column ACS workforce, right column QCEW jobs: interleaving the outcomes pairs them by row
ACS_LABELS  <- str_subset(names(LABOR_OUTCOMES), "ACS$")
QCEW_LABELS <- str_subset(names(LABOR_OUTCOMES), "QCEW")
PAIRED      <- facet_wrap(~outcome, scales = "free_y", ncol = 2)
COL_QUART   <- c(COL_LIGHT, COL_BAR, COL_MID, COL_POINT)
LEVEL_LABEL <- c(county = "Counties", cz = "Commuting Zones")
MODEL_LABEL <- c(before = "Before: border distance + recession x year", after = "After: + log 2010 population x year")
quart_plot_data <- labor_quart_coefs |>
  mutate(outcome = factor(outcome, levels = c(rbind(ACS_LABELS, QCEW_LABELS))))

walk(names(LEVEL_LABEL), \(l) {
  p <- quart_plot_data |>
    filter(level == l, model == "after", !is.na(quartile)) |>
    mutate(quartile = factor(quartile, levels = 1:4, labels = c("Q1 (least vulnerable)", "Q2", "Q3", "Q4 (most vulnerable)"))) |>
    es_plot(sprintf("Effect of One More Year of SC Exposure by Quartile of Baseline %s, %s (95%% CI)\nLeft: ACS ag workforce, by residence. Right: QCEW formal ag payroll jobs, by workplace.",
                    VULN_LABEL, LEVEL_LABEL[[l]]),
            Y_LAB, color = "quartile", palette = COL_QUART, facet = PAIRED)
  ggsave(fig_path("labor", sprintf("labor_quartiles_%s.png", l)), p, width = 13, height = 13, dpi = 300)
})

## 6. diagnostics: with and without the population control ##--------------------------------------
# pooled: both models overlaid. by quartile: one figure per model, with each outcome's y range shared
# between the two so they compare directly.
walk(names(LEVEL_LABEL), \(l) {
  p <- quart_plot_data |>
    filter(level == l, is.na(quartile)) |>
    mutate(model = factor(MODEL_LABEL[model], levels = MODEL_LABEL)) |>
    es_plot(sprintf("Effect of One More Year of SC Exposure, Before and After the Population Control: %s (95%% CI)\nLeft: ACS ag workforce, by residence. Right: QCEW formal ag payroll jobs, by workplace.",
                    LEVEL_LABEL[[l]]),
            Y_LAB, color = "model", palette = c(COL_BAR, COL_POINT), facet = PAIRED)
  ggsave(fig_path("diagnostics", sprintf("acs_qcew_pooled_%s.png", l)), p, width = 13, height = 13, dpi = 300)

  y_range <- quart_plot_data |>
    filter(level == l, !is.na(quartile)) |>
    group_by(outcome) |>
    summarise(y = list(range(c(conf.low, conf.high))), .groups = "drop") |>
    unnest(y) |>
    mutate(year = ES_REF)
  walk(names(MODEL_LABEL), \(m) {
    p <- quart_plot_data |>
      filter(level == l, model == m, !is.na(quartile)) |>
      mutate(quartile = factor(quartile, levels = 1:4, labels = c("Q1 (least vulnerable)", "Q2", "Q3", "Q4 (most vulnerable)"))) |>
      es_plot(sprintf("Effect of One More Year of SC Exposure by Quartile of Baseline %s, %s (95%% CI)\n%s",
                      VULN_LABEL, LEVEL_LABEL[[l]], MODEL_LABEL[[m]]),
              Y_LAB, color = "quartile", palette = COL_QUART, facet = PAIRED) +
      geom_blank(data = y_range, aes(x = year, y = y), inherit.aes = FALSE) +
      labs(caption = "Left: ACS ag workforce, by residence. Right: QCEW formal ag payroll jobs, by workplace. y-axes match across the before and after figures.")
    ggsave(fig_path("diagnostics", sprintf("acs_qcew_quartiles_%s_%s.png", l, m)), p, width = 13, height = 13, dpi = 300)
  })
})
