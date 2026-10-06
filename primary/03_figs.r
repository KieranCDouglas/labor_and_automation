####################################################################################################
## 03: figures
## last edited: 10/01/2026
## by kieran
## does: generates all figures. no estimation happens here - model output comes from 02_models.r.
## reads:  dropbox kieran/data/clean/*.rds (from 01_clean.r), kieran/output/coefs/*.csv (from 02_models.r)
## writes: dropbox kieran/figures/*.png
####################################################################################################

####################################################################################################
### prelude ###
####################################################################################################

pkgs <- c("tidyverse", "tigris", "sf")
install.packages(setdiff(pkgs, rownames(installed.packages())))

library(tidyverse)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

# data and outputs live in dropbox, code lives in the repo
ROOT      <- "/Users/kieran/Library/CloudStorage/Dropbox/secure_communities/kieran"
CLEAN_DIR <- file.path(ROOT, "data", "clean")
COEFS_DIR <- file.path(ROOT, "output", "coefs")
FIGS_DIR  <- file.path(ROOT, "figures")
dir.create(FIGS_DIR, showWarnings = FALSE, recursive = TRUE)

####################################################################################################
### load data ###
####################################################################################################

main                    <- readRDS(file.path(CLEAN_DIR, "main.rds"))
ag_counties            <- readRDS(file.path(CLEAN_DIR, "ag_counties.rds"))
sc_rollout              <- readRDS(file.path(CLEAN_DIR, "sc_rollout.rds"))
het_config              <- readRDS(file.path(CLEAN_DIR, "het_config.rds"))

het_labels <- set_names(het_config$label, paste0("het_", het_config$measure))

es_cellmeans <- read_csv(file.path(COEFS_DIR, "es_detainer_groups.csv"), show_col_types = FALSE)
es_agc       <- read_csv(file.path(COEFS_DIR, "es_agc_county.csv"),    show_col_types = FALSE)
ddd_agc      <- read_csv(file.path(COEFS_DIR, "ddd_agc_county.csv"),   show_col_types = FALSE)
es_acs       <- read_csv(file.path(COEFS_DIR, "es_acs_county.csv"),    show_col_types = FALSE)
ddd_acs      <- read_csv(file.path(COEFS_DIR, "ddd_acs_county.csv"),   show_col_types = FALSE)
es_acs_cz    <- read_csv(file.path(COEFS_DIR, "es_acs_cz.csv"),        show_col_types = FALSE)
ddd_acs_cz   <- read_csv(file.path(COEFS_DIR, "ddd_acs_cz.csv"),       show_col_types = FALSE)
es_qcew      <- read_csv(file.path(COEFS_DIR, "es_qcew_county.csv"),   show_col_types = FALSE)
ddd_qcew     <- read_csv(file.path(COEFS_DIR, "ddd_qcew_county.csv"),  show_col_types = FALSE)
es_crop      <- read_csv(file.path(COEFS_DIR, "es_crop_county.csv"),   show_col_types = FALSE)
ddd_crop     <- read_csv(file.path(COEFS_DIR, "ddd_crop_county.csv"),  show_col_types = FALSE)
es_crop_cz   <- read_csv(file.path(COEFS_DIR, "es_crop_cz.csv"),       show_col_types = FALSE)
ddd_crop_cz  <- read_csv(file.path(COEFS_DIR, "ddd_crop_cz.csv"),      show_col_types = FALSE)

# coefficient plots put a zero at the 2007 reference year for each outcome (and het measure, if present)
add_ref_year <- function(coefs) {
  coefs |>
    mutate(year = as.integer(str_extract(term, "\\d{4}"))) |>
    bind_rows(
      coefs |>
        distinct(pick(any_of(c("het", "outcome")))) |>
        mutate(year = 2007L, estimate = 0, conf.low = 0, conf.high = 0)
    )
}

####################################################################################################
### secure communities rollout maps ###
####################################################################################################

### get county shapefiles ###
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2010) |>
  shift_geometry() |>
  left_join(
    fips_codes |> distinct(state_code, state),
    by = c("STATEFP" = "state_code")
  ) |>
  mutate(
    county = str_remove(NAME, " County$| Parish$| Borough$| Census Area$| city$| Municipality$"),
    county = str_to_title(county)
  ) |>
  filter(state %in% state.abb)

### join rollout data to county geometries ###
counties_rollout <- counties_sf |>
  left_join(sc_rollout, by = c("state", "county"))

### generate one map per year of SC activity ###
for (yr in 2008:2013) {

  map_yr <- counties_rollout |>
    mutate(status = if_else(
      !is.na(first_detainer_year) & first_detainer_year <= yr,
      "Treated", "Not yet treated"
    ))

  p <- ggplot(map_yr) +
    geom_sf(aes(fill = status), color = NA, linewidth = 0) +
    scale_fill_manual(
      values = c("Not yet treated" = "#e8e8e8", "Treated" = "#1a3a5c"),
      name   = NULL
    ) +
    labs(title = paste("Secure Communities Rollout:", yr)) +
    theme_void() +
    theme(
      plot.title    = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      legend.position  = "bottom",
      legend.text      = element_text(size = 11),
      plot.margin      = margin(10, 10, 10, 10)
    )

  ggsave(file.path(FIGS_DIR, paste0("sc_rollout_", yr, ".png")), plot = p, width = 12, height = 7, dpi = 300, bg = "white")
  message("Saved: sc_rollout_", yr, ".png")
}

### agricultural counties only ###
# ag counties are defined in 01_clean.r (section 6) so the maps match the analysis sample
counties_rollout_ag <- counties_rollout |>
  left_join(ag_counties |> mutate(ag_county = TRUE), by = c("state", "county")) |>
  mutate(ag_county = replace_na(ag_county, FALSE))

for (yr in 2008:2013) {

  map_yr <- counties_rollout_ag |>
    mutate(status = case_when(
      !ag_county                                             ~ "Non-ag county",
      !is.na(first_detainer_year) & first_detainer_year <= yr ~ "Ag - Treated",
      TRUE                                                    ~ "Ag - Not yet treated"
    ))

  p <- ggplot(map_yr) +
    geom_sf(aes(fill = status), color = NA, linewidth = 0) +
    scale_fill_manual(
      values = c(
        "Non-ag county"        = "#e8e8e8",
        "Ag - Not yet treated" = "#f6c453",
        "Ag - Treated"         = "#1a3a5c"
      ),
      name = NULL
    ) +
    labs(title = paste("Secure Communities Rollout, Farming-Dependent Counties:", yr)) +
    theme_void() +
    theme(
      plot.title    = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      legend.position  = "bottom",
      legend.text      = element_text(size = 11),
      plot.margin      = margin(10, 10, 10, 10)
    )

  ggsave(file.path(FIGS_DIR, paste0("sc_rollout_ag_", yr, ".png")), plot = p, width = 12, height = 7, dpi = 300, bg = "white")
  message("Saved: sc_rollout_ag_", yr, ".png")
}

####################################################################################################
### event study justification ###
####################################################################################################
## removals in event time per subgroup ##----------------------------------------------------------
# four-way split by early/late activation and high/low noncit share, with CIs from the per-group
# models in 02_models.r
es_detainer_fig <- ggplot(es_cellmeans, aes(x = rel_year, y = estimate,
                          color = activation_label, linetype = noncit_label,
                          fill = activation_label,
                          group = interaction(activation_label, noncit_label))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_x_continuous(breaks = -4:4) +
  scale_color_manual(values = c("#4F8A5B", "#243E36")) +
  scale_fill_manual(values = c("#4F8A5B", "#243E36")) +
  theme_minimal() +
  labs(color = NULL, linetype = NULL, fill = NULL,
       x = "Years Relative to SC Activation", y = "SC Removals Per 10,000 Population",
       title = "SC Removals Schedule Relative to Activation Year")
ggsave(file.path(FIGS_DIR, "es_detainer_groups.png"), es_detainer_fig, width = 10, height = 6, dpi = 300)

####################################################################################################
### full es: year-specific treatment effects ###
####################################################################################################

es_agc_fig <- ggplot(add_ref_year(es_agc), aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.6, color = "#7CA982") +
  geom_point(size = 2, color = "#243E36") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
  theme_minimal() +
  labs(
    x = "Census Year", y = "Estimate Relative to 2007",
    title = "Year-Specific Effects of Early SC Activation (95% CI)"
  )
ggsave(file.path(FIGS_DIR, "es_agc_county.png"), es_agc_fig, width = 10, height = 5, dpi = 300)

####################################################################################################
### ddd triple-interaction coefficients ###
####################################################################################################
# one figure per het measure: early activation x high exposure

for (h in unique(ddd_agc$het)) {
  p <- ddd_agc |>
    filter(het == h, str_detect(term, "early_x_")) |>
    add_ref_year() |>
    ggplot(aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.6, color = "#4F8A5B") +
    geom_point(size = 2, color = "#243E36") +
    facet_wrap(~outcome, scales = "free_y") +
    scale_x_continuous(breaks = c(2002, 2007, 2012, 2017)) +
    theme_minimal() +
    labs(
      x = "Census Year", y = "Triple-Difference Estimate Relative to 2007",
      title = paste0("DDD: Early Activation x High Baseline ", het_labels[[h]], " (95% CI)")
    )
  ggsave(file.path(FIGS_DIR, paste0("ddd_agc_county_", str_remove(h, "^het_"), ".png")), p, width = 10, height = 5, dpi = 300)
}

####################################################################################################
### long panel (ACS 2006-2020): labor outcomes ###
####################################################################################################
# yearly coefficients relative to 2007; outcomes are ag workers as a share of 2006-07 working-age pop

# y_lab and breaks default to the ACS/QCEW yearly panels; the census-year crop panels override them
acs_coef_plot <- function(coefs, title, y_lab = "Estimate Relative to 2007 (share of baseline working-age pop.)",
                          breaks = scales::breaks_width(2), err_width = 0.4) {
  coefs |>
    add_ref_year() |>
    mutate(outcome = factor(outcome, levels = unique(coefs$outcome))) |>
    ggplot(aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = err_width, color = "#7CA982") +
    geom_point(size = 2, color = "#243E36") +
    facet_wrap(~outcome, scales = "free_y") +
    scale_x_continuous(breaks = breaks) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 0.01)) +
    theme_minimal() +
    labs(x = "Year", y = y_lab, title = title)
}

es_acs_fig <- acs_coef_plot(es_acs, "Early SC Activation and the Ag Workforce, ACS 2006-2020 (95% CI)")
ggsave(file.path(FIGS_DIR, "es_acs_county.png"), es_acs_fig, width = 10, height = 7, dpi = 300)

es_acs_cz_fig <- acs_coef_plot(es_acs_cz, "Early SC Activation and the Ag Workforce, Commuting Zones, ACS 2006-2020 (95% CI)")
ggsave(file.path(FIGS_DIR, "es_acs_cz.png"), es_acs_cz_fig, width = 10, height = 7, dpi = 300)

for (h in unique(ddd_acs_cz$het)) {
  p <- ddd_acs_cz |>
    filter(het == h, str_detect(term, "early_x_")) |>
    acs_coef_plot(paste0("DDD: Early Activation x High Baseline ", het_labels[[h]], ", Commuting Zones (95% CI)"))
  ggsave(file.path(FIGS_DIR, paste0("ddd_acs_cz_", str_remove(h, "^het_"), ".png")), p, width = 10, height = 7, dpi = 300)
}

for (h in unique(ddd_acs$het)) {
  p <- ddd_acs |>
    filter(het == h, str_detect(term, "early_x_")) |>
    acs_coef_plot(paste0("DDD: Early Activation x High Baseline ", het_labels[[h]], ", ACS 2006-2020 (95% CI)"))
  ggsave(file.path(FIGS_DIR, paste0("ddd_acs_county_", str_remove(h, "^het_"), ".png")), p, width = 10, height = 7, dpi = 300)
}

####################################################################################################
### long panel (QCEW 2005-2022): formal ag employment ###
####################################################################################################
# same layout and units as the ACS figures; each outcome is its own balanced county sample

es_qcew_fig <- acs_coef_plot(es_qcew, "Early SC Activation and Formal Ag Employment, QCEW 2005-2022 (95% CI)")
ggsave(file.path(FIGS_DIR, "es_qcew_county.png"), es_qcew_fig, width = 10, height = 5, dpi = 300)

for (h in unique(ddd_qcew$het)) {
  p <- ddd_qcew |>
    filter(het == h, str_detect(term, "early_x_")) |>
    acs_coef_plot(paste0("DDD: Early Activation x High Baseline ", het_labels[[h]], ", QCEW 2005-2022 (95% CI)"))
  ggsave(file.path(FIGS_DIR, paste0("ddd_qcew_county_", str_remove(h, "^het_"), ".png")), p, width = 10, height = 5, dpi = 300)
}

####################################################################################################
### crop mix (census of ag 2002-2022): labor-intensive acreage ###
####################################################################################################
# census years only; outcomes are shares of harvested cropland in vegetables and/or orchards

crop_plot <- \(coefs, title) acs_coef_plot(coefs, title, y_lab = "Estimate Relative to 2007 (share of harvested cropland)",
                                            breaks = c(2002, 2007, 2012, 2017, 2022), err_width = 1.2)

for (geo in c("county", "cz")) {
  geo_lab <- if (geo == "cz") "Commuting Zones" else "Counties"
  es  <- if (geo == "cz") es_crop_cz  else es_crop
  ddd <- if (geo == "cz") ddd_crop_cz else ddd_crop

  p <- crop_plot(es, paste0("Early SC Activation and Labor-Intensive Crop Mix, ", geo_lab, ", Census of Ag 2002-2022 (95% CI)"))
  ggsave(file.path(FIGS_DIR, paste0("es_crop_", geo, ".png")), p, width = 10, height = 5, dpi = 300)

  for (h in unique(ddd$het)) {
    p <- ddd |>
      filter(het == h, str_detect(term, "early_x_")) |>
      crop_plot(paste0("DDD: Early Activation x High Baseline ", het_labels[[h]], ", Crop Mix, ", geo_lab, " (95% CI)"))
    ggsave(file.path(FIGS_DIR, paste0("ddd_crop_", geo, "_", str_remove(h, "^het_"), ".png")), p, width = 10, height = 5, dpi = 300)
  }
}

####################################################################################################
### exposure heterogeneity maps by activation timing ###
####################################################################################################
# one figure per het measure: early activators | late activators. counties in the panel's timing
# group are shaded by quintile of the baseline value; quintile breaks come from the whole analysis
# sample, so a shade means the same thing in both panels. other counties are a flat background.

county_key <- \(x) x |> str_to_lower() |> str_replace("^saint |^st\\.? ", "st ") |>
  str_replace("^sainte |^ste\\.? ", "ste ") |> str_remove_all("[^a-z]")

het_map_counties <- main |>
  distinct(state, county, early_activator = het_early,
           pick(all_of(paste0("het_", setdiff(het_config$measure, "early"), "_val")))) |>
  mutate(county_key = county_key(county)) |>
  select(-county)

het_map_sf <- counties_sf |>
  mutate(county_key = county_key(county)) |>
  left_join(het_map_counties, by = c("state", "county_key"))

states_sf <- states(cb = TRUE, resolution = "20m", year = 2010) |>
  shift_geometry() |>
  filter(STATE %in% fips_codes$state_code[fips_codes$state %in% state.abb])

# how each measure's values print in the legend
het_value_fmt <- list(
  noncit    = scales::label_percent(accuracy = 0.1),
  undoc     = scales::label_percent(accuracy = 0.1),
  jail_cap  = scales::label_number(accuracy = 0.1),
  jail_flow = scales::label_number(accuracy = 0.1)
)
het_value_unit <- c(
  noncit    = "Share of population",
  undoc     = "Share of population",
  jail_cap  = "Rated beds per 1k residents",
  jail_flow = "Admissions per 1k residents"
)

HET_RAMP      <- colorRampPalette(c("#CFE3B4", "#7CA982", "#243E36"))(5)
HET_NODATA    <- "#b8b8b8"
HET_OTHER     <- "#f7f7f7"

for (m in setdiff(het_config$measure, "early")) {
  val_col <- paste0("het_", m, "_val")
  fmt     <- het_value_fmt[[m]]
  vals    <- het_map_counties[[val_col]]
  brks    <- unique(quantile(vals, probs = seq(0, 1, 0.2), na.rm = TRUE))
  # top bin is open-ended so a few extreme counties don't stretch its label
  bin_lab <- c(paste0(fmt(head(brks, -2)), " – ", fmt(brks[2:(length(brks) - 1)])),
               paste0("≥ ", fmt(brks[length(brks) - 1])))
  levels_all <- c(bin_lab, "No data", "Other counties")

  map_data <- map_dfr(c(1, 0), \(e) het_map_sf |>
    mutate(
      panel = if (e == 1) "Early activators (2011 or earlier)" else "Late activators (2012 or later)",
      fill  = case_when(
        is.na(early_activator) | early_activator != e ~ "Other counties",
        is.na(.data[[val_col]])                       ~ "No data",
        TRUE ~ as.character(cut(.data[[val_col]], brks, labels = bin_lab, include.lowest = TRUE))
      ),
      fill = factor(fill, levels = levels_all)
    ))

  cut_used <- het_config$cut[het_config$measure == m]
  p <- ggplot(map_data) +
    geom_sf(aes(fill = fill), color = NA) +
    geom_sf(data = states_sf, fill = NA, color = "white", linewidth = 0.25) +
    facet_wrap(~panel, ncol = 2) +
    scale_fill_manual(values = c(set_names(HET_RAMP[seq_along(bin_lab)], bin_lab),
                                 "No data" = HET_NODATA, "Other counties" = HET_OTHER),
                      drop = TRUE, name = het_value_unit[[m]]) +  # "No data" shows only if present
    labs(
      title    = paste0("Baseline ", het_labels[[paste0("het_", m)]], " by SC Activation Timing"),
      subtitle = paste0("Quintiles across ag-sample counties. Regression split: high if ≥ ", fmt(cut_used), ".")
    ) +
    theme_void() +
    theme(
      plot.title      = element_text(hjust = 0.5, size = 15, face = "bold"),
      plot.subtitle   = element_text(hjust = 0.5, size = 10, color = "gray30", margin = margin(t = 4, b = 8)),
      strip.text      = element_text(size = 12, margin = margin(b = 6)),
      legend.position = "bottom",
      legend.text     = element_text(size = 9),
      plot.margin     = margin(10, 10, 10, 10)
    ) +
    guides(fill = guide_legend(nrow = 1, title.position = "left", title.vjust = 0.8))
  ggsave(file.path(FIGS_DIR, paste0("map_het_", m, ".png")), p, width = 14, height = 5.5, dpi = 300, bg = "white")
}

####################################################################################################
### exploratory: dose-response DDD by exposure quantile ###
####################################################################################################
# reads the *_quant.csv outputs of the matching section in 02_models.r. for each run, one event-time
# figure per het measure (each upper bin's early x bin path, relative to the bottom bin).
# self-contained: delete this section and the matching one in 02_models.r to remove it.

quant_fig_runs <- list(
  ddd_agc_county  = list(lab = "Census of Ag, Counties", breaks = c(2002, 2007, 2012, 2017), y = "Estimate Relative to 2007", pct = FALSE),
  ddd_acs_county  = list(lab = "ACS, Counties",          breaks = seq(2006, 2020, 2),       y = "Estimate Relative to 2007 (share of baseline working-age pop.)", pct = TRUE),
  ddd_acs_cz      = list(lab = "ACS, Commuting Zones",   breaks = seq(2006, 2020, 2),       y = "Estimate Relative to 2007 (share of baseline working-age pop.)", pct = TRUE),
  ddd_qcew_county = list(lab = "QCEW, Counties",         breaks = seq(2005, 2021, 4),       y = "Estimate Relative to 2007 (share of baseline working-age pop.)", pct = TRUE),
  ddd_crop_county = list(lab = "Crop Mix, Counties",     breaks = c(2002, 2007, 2012, 2017, 2022), y = "Estimate Relative to 2007 (share of harvested cropland)", pct = TRUE),
  ddd_crop_cz     = list(lab = "Crop Mix, Commuting Zones", breaks = c(2002, 2007, 2012, 2017, 2022), y = "Estimate Relative to 2007 (share of harvested cropland)", pct = TRUE)
)

QUANT_COLORS <- c("#B5D3A8", "#7CA982", "#243E36")  # light -> dark = low -> high upper bin

# Q for quartile measures, T for tercile measures, so labels show which binning a measure used
bin_label <- \(bin, n_bins) paste0(if_else(n_bins == 3, "T", "Q"), bin)

iwalk(quant_fig_runs, \(r, name) {
  coefs <- read_csv(file.path(COEFS_DIR, paste0(name, "_quant.csv")), show_col_types = FALSE)
  y_scale <- if (r$pct) scale_y_continuous(labels = scales::label_percent(accuracy = 0.01)) else scale_y_continuous()
  out_levels <- unique(coefs$outcome)

  for (m in unique(coefs$het)) {
    d <- coefs |> filter(het == m)
    n_bins <- max(d$bin)
    d <- bind_rows(d, distinct(d, outcome, bin) |> mutate(year = 2007L, estimate = 0, conf.low = 0, conf.high = 0)) |>
      mutate(bin = factor(paste(bin_label(bin, n_bins), "vs", bin_label(1, n_bins)), levels = paste(bin_label(2:n_bins, n_bins), "vs", bin_label(1, n_bins))),
             outcome = factor(outcome, levels = out_levels))
    p <- ggplot(d, aes(x = year, y = estimate, color = bin)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, position = position_dodge(width = 0.8), alpha = 0.8) +
      geom_point(size = 1.8, position = position_dodge(width = 0.8)) +
      facet_wrap(~outcome, scales = "free_y") +
      scale_color_manual(values = tail(QUANT_COLORS, n_bins - 1), name = NULL) +
      scale_x_continuous(breaks = r$breaks) +
      y_scale +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(x = "Year", y = r$y,
           title = paste0("DDD by Baseline ", het_labels[[paste0("het_", m)]], " ", if (n_bins == 3) "Tercile" else "Quartile",
                          ": Early Activation x Bin, ", r$lab, " (95% CI)"))
    ggsave(file.path(FIGS_DIR, paste0(name, "_", m, "_quant.png")), p, width = 11, height = if (length(out_levels) > 3) 7 else 5, dpi = 300)
  }
})
