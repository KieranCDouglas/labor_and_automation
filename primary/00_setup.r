####################################################################################################
## 00: setup
## last edited: 10/22/2026
## by kieran
## does: paths and helpers shared by every script in primary/. each sources this file after loading
##       tidyverse.
####################################################################################################

# data and outputs live in dropbox, code lives in the repo
DROPBOX    <- "/Users/kieran/Library/CloudStorage/Dropbox/secure_communities"
ROOT       <- file.path(DROPBOX, "kieran")
GEN_DIR    <- file.path(DROPBOX, "generated_data")   # built inputs (controls, 2000 census baseline)
RAW_DIR    <- file.path(ROOT, "data", "raw")
CLEAN_DIR  <- file.path(ROOT, "data", "clean")
COEFS_DIR  <- file.path(ROOT, "output", "coefs")
TABLES_DIR <- file.path(ROOT, "output", "tables")
FIGS_DIR   <- file.path(ROOT, "figures")
for (d in c(CLEAN_DIR, COEFS_DIR, TABLES_DIR, FIGS_DIR)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# county names: drop the geography suffix and title-case, so sources that spell names differently
# ("Adams County", "ADAMS") agree
clean_county <- function(county) {
  str_to_title(str_remove(county, " County$| Parish$| City and Borough$| Borough$| Census Area$| Municipality$| city$"))
}

# looser join key for matching names across sources: lower case, saint/sainte normalized, letters only
county_key <- function(county) {
  county |>
    str_to_lower() |>
    str_replace("^saint |^st\\.? ", "st ") |>
    str_replace("^sainte |^ste\\.? ", "ste ") |>
    str_remove_all("[^a-z]")
}

# figure path under FIGS_DIR, creating the folder
fig_path <- function(...) {
  path <- file.path(FIGS_DIR, ...)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  path
}

# figure styling: the project's green palette. a single series is dark-green points with sage error
# bars; ordered groups run light -> dark green; the accent is a muted gold.
COL_POINT  <- "#243E36"
COL_BAR    <- "#7CA982"
COL_MID    <- "#4F8A5B"
COL_LIGHT  <- "#B5D3A8"
COL_ACCENT <- "#B08D57"
COL_GRAY   <- "#9A9A9A"
COL_TERCILE <- c("#B5D3A8", "#7CA982", "#243E36")                              # low -> high
COL_LOWHIGH <- c("#B5D3A8", "#243E36")                                         # low, high
col_ramp    <- \(n) colorRampPalette(c("#CFE3B4", "#7CA982", "#243E36"))(n)  # light -> dark

theme_paper <- function() {
  theme_minimal() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white", color = NA))
}

####################################################################################################
### event-study estimator shared by the outcome scripts ###
####################################################################################################
# every outcome question uses the same specification:
#   y_ut = sum_s b_s (exposure_u x 1{t=s})
#        [+ sum_s d_s (v_u x 1{t=s}) + sum_s g_s (exposure_u x v_u x 1{t=s})]   with vulnerability
#        + sum_s X_u' theta_s 1{t=s} + alpha_u + lambda_{state(u), t} + e_ut
# exposure_u is the extra years of SC a unit had relative to the last activator (0 to 4.24), so b_s is
# the effect of one more year of exposure in year s, relative to ES_REF. v_u is the chosen
# vulnerability measure standardized across the sample's units (per 1 SD), so g_s is how much the
# per-year effect grows per SD of vulnerability. X_u are border distance, Great Recession severity
# and log 2010 population: SC reached border and large metro jurisdictions first, and city size also
# predicts different farm-employment trends, so the comparison is between places of similar size.
# errors are clustered by unit.
ES_REF      <- 2007
ES_CONTROLS <- c("border_dist_km", "gr_severity", "log_pop_2010")

# robustness: further baseline characteristics x year, added to the controls. they absorb trends tied
# to how agricultural and how immigrant a place was before SC, both of which also tracked the
# activation order. (with a vulnerability interaction, the vulnerability term already carries its own
# year interaction.)
ES_BASELINE <- c("ag_share_base", "het_hispnc_val")

# estimation sample for one outcome: rows with the outcome, exposure, controls and (if given) the
# vulnerability measure, with vz = the measure standardized across the sample's units
es_sample <- function(data, outcome, unit, vuln = NULL, controls = ES_CONTROLS) {
  d <- data |>
    mutate(exposure = exposure_years) |>
    filter(if_all(all_of(c(outcome, "exposure", controls, vuln)), \(x) !is.na(x)))
  if (!is.null(vuln)) {
    u <- distinct(d, pick(all_of(c(unit, vuln))))[[vuln]]
    d <- mutate(d, vz = (.data[[vuln]] - mean(u)) / sd(u), xv = exposure * vz)
  }
  d
}

# baseline: FALSE, TRUE (all of ES_BASELINE) or a character vector of baseline variables to add x year
fit_es <- function(data, outcome, unit, vuln = NULL, time_fe = "state^year", cluster = unit, baseline = FALSE) {
  extra    <- if (isTRUE(baseline)) ES_BASELINE else if (is.character(baseline)) baseline else NULL
  controls <- c(ES_CONTROLS, setdiff(extra, vuln))
  d   <- es_sample(data, outcome, unit, vuln, controls)
  rhs <- c(sprintf("i(year, exposure, ref = %d)", ES_REF),
           if (!is.null(vuln)) sprintf("i(year, %s, ref = %d)", c("vz", "xv"), ES_REF),
           sprintf("i(year, %s, ref = %d)", controls, ES_REF))
  feols(as.formula(sprintf("%s ~ %s | %s + %s", outcome, paste(rhs, collapse = " + "), unit, time_fe)),
        data = d, cluster = as.formula(paste0("~", cluster)))
}

# year-by-year coefficients of one term ("exposure", "vz" or "xv"), plus a zero at ES_REF
es_coefs <- function(mod, term = "exposure") {
  b  <- coef(mod)
  ci <- confint(mod)
  k  <- str_detect(names(b), paste0("^year::\\d{4}:", term, "$"))
  tibble(year = as.integer(str_extract(names(b)[k], "\\d{4}")), estimate = b[k], std.error = se(mod)[k],
         conf.low = ci[k, 1], conf.high = ci[k, 2]) |>
    bind_rows(tibble(year = ES_REF, estimate = 0, std.error = 0, conf.low = 0, conf.high = 0)) |>
    arrange(year)
}

# the per-year exposure effect at a given level of vulnerability (in SD): b_s + g_s * at, with its CI
es_effect_at <- function(mod, at) {
  b <- coef(mod)
  V <- vcov(mod)
  map(sort(unique(as.integer(str_extract(names(b)[str_detect(names(b), ":exposure$")], "\\d{4}")))), \(yr) {
    w   <- set_names(c(1, at), sprintf("year::%d:%s", yr, c("exposure", "xv")))
    est <- sum(w * b[names(w)])
    se  <- sqrt(drop(t(w) %*% V[names(w), names(w)] %*% w))
    tibble(year = yr, estimate = est, std.error = se, conf.low = est - qnorm(0.975) * se, conf.high = est + qnorm(0.975) * se)
  }) |>
    bind_rows() |>
    bind_rows(tibble(year = ES_REF, estimate = 0, std.error = 0, conf.low = 0, conf.high = 0)) |>
    arrange(year)
}

# summary of one term: joint p-value of the years before ES_REF (pre-trend) and the average of the
# years after it, with its SE
es_summary <- function(mod, term = "exposure") {
  b    <- coef(mod)
  yrs  <- as.integer(str_match(names(b), paste0("^year::(\\d{4}):", term, "$"))[, 2])
  pre  <- names(b)[!is.na(yrs) & yrs < ES_REF]
  post <- names(b)[!is.na(yrs) & yrs > ES_REF]
  w    <- set_names(rep(1 / length(post), length(post)), post)
  tibble(term = term, n_obs = nobs(mod), n_units = unname(mod$fixef_sizes[1]),
         pre_p = tryCatch(wald(mod, keep = paste0("^\\Q", pre, "\\E$", collapse = "|"), print = FALSE)$p, error = \(e) NA_real_),
         post_avg = sum(w * b[post]), post_se = sqrt(drop(t(w) %*% vcov(mod)[post, post] %*% w)))
}

# event-study figure in the project style: dashed zero line, dashed line at the first SC year,
# error bars and points, one facet per outcome. with `color`, groups are dodged and colored.
es_plot <- function(d, title, y_lab, color = NULL, palette = NULL, breaks = scales::breaks_width(2),
                    facet = facet_wrap(~outcome, scales = "free_y"), labels = scales::label_percent(accuracy = 0.01),
                    err_width = 0.4) {
  dodge <- position_dodge(width = 0.6)
  p <- ggplot(d, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 2008, linetype = "dashed", color = "black")
  p <- if (is.null(color)) {
    p + geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = err_width, color = COL_BAR) +
      geom_point(size = 2, color = COL_POINT)
  } else {
    p + geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = .data[[color]]), width = 0, position = dodge) +
      geom_point(aes(color = .data[[color]]), size = 2, position = dodge) +
      scale_color_manual(values = palette, name = NULL)
  }
  p + facet +
    scale_x_continuous(breaks = breaks) +
    scale_y_continuous(labels = labels) +
    theme_paper() +
    labs(x = "Year", y = y_lab, title = title)
}

####################################################################################################
### a question's standard results ###
####################################################################################################
# every outcome question reports the same things: the main-specification event studies, and the
# average post-ES_REF effect under a fixed robustness set. outcomes are named c(label = variable);
# `panels` is list(cz = ..., county = ...), both restricted to units with the vulnerability measure.

# main specification: per-year exposure effect, its interaction with vulnerability, and the exposure
# effect at -1 and +1 SD of vulnerability
es_main_coefs <- function(data, outcomes, unit, vuln) {
  main_mods <- map(outcomes, \(o) fit_es(data, o, unit))
  het_mods  <- map(outcomes, \(o) fit_es(data, o, unit, vuln = vuln))
  c(
    imap(main_mods, \(m, o) es_coefs(m, "exposure") |> mutate(outcome = o, series = "Exposure")),
    imap(het_mods,  \(m, o) es_coefs(m, "xv")       |> mutate(outcome = o, series = "Exposure x vulnerability")),
    imap(het_mods,  \(m, o) es_effect_at(m, -1)      |> mutate(outcome = o, series = "Low vulnerability (-1 SD)")),
    imap(het_mods,  \(m, o) es_effect_at(m,  1)      |> mutate(outcome = o, series = "High vulnerability (+1 SD)"))
  ) |>
    unname() |>
    list_rbind() |>
    mutate(outcome = factor(outcome, levels = names(outcomes)))
}

# the robustness set: each row changes one thing from the main specification. the alternative
# vulnerability measure only changes the interaction, so it has no main-effect row. `suffix` swaps in
# a variant of the outcome (outcome_share -> outcome<suffix>_share); `baseline` adds ES_BASELINE x year.
es_spec_set <- function(vuln, vuln_alt) {
  tribble(
    ~spec,                                     ~data,    ~unit,       ~time_fe,     ~cluster,    ~vuln,    ~suffix, ~baseline,
    "Main (CZ, state x year FE)",              "cz",     "cz_id",     "state^year", "cz_id",     vuln,     "",      FALSE,
    "Baseline characteristics x year",         "cz",     "cz_id",     "state^year", "cz_id",     vuln,     "",      TRUE,
    "Year FE only",                            "cz",     "cz_id",     "year",       "cz_id",     vuln,     "",      FALSE,
    "SEs clustered by state",                  "cz",     "cz_id",     "state^year", "state",     vuln,     "",      FALSE,
    "County level",                            "county", "county_id", "state^year", "county_id", vuln,     "",      FALSE,
    "Vulnerability: noncitizen share 2005-09", "cz",     "cz_id",     "state^year", "cz_id",     vuln_alt, "",      FALSE
  )
}

es_robustness <- function(specs, panels, outcomes) {
  specs |>
    mutate(spec = factor(spec, levels = spec)) |>
    expand_grid(outcome = names(outcomes)) |>
    mutate(res = pmap(list(data, unit, time_fe, cluster, vuln, outcome, spec, suffix, baseline), \(dt, u, fe, cl, v, o, s, sfx, bl) {
      y   <- if (sfx == "") outcomes[[o]] else str_replace(outcomes[[o]], "_share$", paste0(sfx, "_share"))
      out <- es_summary(fit_es(panels[[dt]], y, u, vuln = v, time_fe = fe, cluster = cl, baseline = bl), "xv")
      if (!str_starts(s, "Vulnerability")) {
        out <- bind_rows(es_summary(fit_es(panels[[dt]], y, u, time_fe = fe, cluster = cl, baseline = bl), "exposure"), out)
      }
      out
    })) |>
    select(spec, outcome, res) |>
    unnest(res) |>
    mutate(outcome = factor(outcome, levels = names(outcomes)),
           term_label = if_else(term == "exposure", "Per year of exposure", "x Vulnerability (per SD)"))
}

# main-specification table: average post-ES_REF effect (SE) with stars, and the joint p-value of the
# pre-ES_REF coefficients. `scale` converts to reporting units (100 for shares -> pp).
es_main_table <- function(summary, file, scale = 1, digits = 3) {
  fmt <- \(b, se) sprintf(paste0("%.", digits, "f (%.", digits, "f)%s"), scale * b, scale * se,
                          case_when(abs(b / se) > 2.576 ~ "***", abs(b / se) > 1.96 ~ "**", abs(b / se) > 1.645 ~ "*", TRUE ~ ""))
  summary |>
    filter(spec == "Main (CZ, state x year FE)") |>
    transmute(Outcome = as.character(outcome), term = if_else(term == "exposure", "Per year", "x Vulnerability"),
              `post avg.` = fmt(post_avg, post_se), `pre p` = sprintf("%.2f", pre_p)) |>
    pivot_wider(names_from = term, values_from = c(`post avg.`, `pre p`), names_glue = "{term}: {.value}") |>
    select(Outcome, `Per year: post avg.`, `Per year: pre p`, `x Vulnerability: post avg.`, `x Vulnerability: pre p`) |>
    write_tex_table(file)
}

# average post-ES_REF effect under each robustness specification, one column per outcome
es_robustness_plot <- function(summary, title, x_lab, labels = scales::label_percent(accuracy = 0.1)) {
  summary |>
    mutate(lo = post_avg - qnorm(0.975) * post_se, hi = post_avg + qnorm(0.975) * post_se, spec = fct_rev(spec)) |>
    ggplot(aes(x = post_avg, y = spec)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.3, color = COL_BAR, orientation = "y") +
    geom_point(size = 2, color = COL_POINT) +
    facet_grid(term_label ~ outcome, scales = "free", space = "free_y") +
    scale_x_continuous(labels = labels, n.breaks = 4) +
    theme_paper() +
    theme(strip.text.y = element_text(angle = 0, hjust = 0), panel.spacing.x = unit(1.2, "lines")) +
    labs(x = x_lab, y = NULL, title = title)
}

# heterogeneity: for each dimension (a named vector c(variable = label)), the average post-ES_REF
# per-year exposure effect in places 1 SD below and above the dimension's mean, and the interaction
# itself (xv: how much the per-year effect changes per SD). main specification, unit = cz_id or county_id.
# the reliance measures are built in 01_clean.r section 19.
ES_RELIANCE <- c(rel_fb_ag = "Reliance on immigrant farm labor (foreign-born ag workers, 2006-07)",
                 rel_labor = "Reliance on hired farm labor (labor share of farm expenses, 2002-07)")

# average over post-ES_REF years of a weighted sum of terms, e.g. c(exposure = 1, xv = 1) for the
# exposure effect at +1 SD, with its SE
es_post_combo <- function(mod, weights) {
  b    <- coef(mod)
  yrs  <- as.integer(str_match(names(b), "^year::(\\d{4}):exposure$")[, 2])
  post <- sort(yrs[!is.na(yrs) & yrs > ES_REF])
  w    <- imap(weights, \(wt, term) set_names(rep(wt / length(post), length(post)), sprintf("year::%d:%s", post, term))) |>
    unname() |> unlist()
  tibble(post_avg = sum(w * b[names(w)]), post_se = sqrt(drop(t(w) %*% vcov(mod)[names(w), names(w)] %*% w)))
}
es_post_at <- \(mod, at) es_post_combo(mod, c(exposure = 1, xv = at))

es_heterogeneity <- function(data, outcomes, unit, dims) {
  expand_grid(dim = names(dims), outcome = names(outcomes)) |>
    mutate(res = map2(dim, outcome, \(v, o) {
      m <- fit_es(data, outcomes[[o]], unit, vuln = v)
      bind_rows(es_post_at(m, -1) |> mutate(group = "Low (-1 SD)"),
                es_post_at(m,  1) |> mutate(group = "High (+1 SD)"),
                es_summary(m, "xv") |> select(post_avg, post_se, pre_p, n_units) |> mutate(group = "Interaction (per SD)"))
    })) |>
    unnest(res) |>
    mutate(dim_label = factor(dims[dim], levels = dims), outcome = factor(outcome, levels = names(outcomes)))
}

# low vs high effect for each dimension, one column per outcome
es_heterogeneity_plot <- function(het, title, x_lab, labels = scales::label_percent(accuracy = 0.1)) {
  het |>
    filter(group != "Interaction (per SD)") |>
    mutate(group = factor(group, levels = c("Low (-1 SD)", "High (+1 SD)")),
           dim_label = fct_rev(factor(str_wrap(as.character(dim_label), 34), levels = str_wrap(levels(dim_label), 34))),
           lo = post_avg - qnorm(0.975) * post_se, hi = post_avg + qnorm(0.975) * post_se) |>
    ggplot(aes(x = post_avg, y = dim_label, color = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", position = position_dodge(width = 0.5)) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    facet_wrap(~outcome, nrow = 1, scales = "free_x") +
    scale_color_manual(values = set_names(COL_LOWHIGH, c("Low (-1 SD)", "High (+1 SD)")), name = NULL) +
    scale_x_continuous(labels = labels, n.breaks = 4) +
    theme_paper() +
    theme(panel.spacing.x = unit(1.2, "lines")) +
    labs(x = x_lab, y = NULL, title = title)
}

# heterogeneity by tercile: units are split into thirds of each dimension (by rank, so ties and skew
# do not matter), and one model gives the per-year exposure effect within each third:
#   y = sum_s [b_s exposure + c2_s exposure x T2 + c3_s exposure x T3 + d2_s T2 + d3_s T3] 1{t=s} + controls + FE
# the effect in tercile k is b + c_k (b in the bottom tercile). no linearity across the dimension.
# time_fe and baseline as in fit_es; "+ size_band^year" in time_fe compares within population terciles
# (add the column with add_size_band())
es_tercile_fit <- function(data, outcome, unit, v, time_fe = "state^year", baseline = FALSE) {
  extra    <- if (isTRUE(baseline)) ES_BASELINE else if (is.character(baseline)) baseline else NULL
  controls <- c(ES_CONTROLS, setdiff(extra, v))
  d    <- es_sample(data, outcome, unit, vuln = v, controls = controls)
  terc <- d[c(unit, v)] |> distinct()
  terc$tercile <- ntile(terc[[v]], 3)
  d <- d |>
    left_join(terc[c(unit, "tercile")], by = unit) |>
    mutate(t2 = as.integer(tercile == 2), t3 = as.integer(tercile == 3), xt2 = exposure * t2, xt3 = exposure * t3)
  rhs <- sprintf("i(year, %s, ref = %d)", c("exposure", "xt2", "xt3", "t2", "t3", controls), ES_REF)
  m <- feols(as.formula(sprintf("%s ~ %s | %s + %s", outcome, paste(rhs, collapse = " + "), unit, time_fe)),
             data = d, cluster = as.formula(paste0("~", unit)))
  bind_rows(es_post_combo(m, c(exposure = 1)),
            es_post_combo(m, c(exposure = 1, xt2 = 1)),
            es_post_combo(m, c(exposure = 1, xt3 = 1))) |>
    mutate(group = paste("Tercile", 1:3), n_units = n_distinct(d[[unit]]))
}

es_terciles <- function(data, outcomes, unit, dims, time_fe = "state^year", baseline = FALSE) {
  grid <- expand_grid(dim = names(dims), outcome = names(outcomes))
  grid$res <- map2(grid$dim, grid$outcome, \(v, o) es_tercile_fit(data, outcomes[[o]], unit, v, time_fe, baseline))
  grid |>
    unnest(res) |>
    mutate(dim_label = factor(dims[dim], levels = dims), outcome = factor(outcome, levels = names(outcomes)))
}

# population size band: tercile of log 2010 population across the panel's units
add_size_band <- function(data, unit) {
  bands <- data[c(unit, "log_pop_2010")] |> distinct()
  bands$size_band <- ntile(bands$log_pop_2010, 3)
  left_join(data, bands[c(unit, "size_band")], by = unit)
}

es_tercile_plot <- function(terc, title, x_lab, labels = scales::label_percent(accuracy = 0.1)) {
  terc |>
    mutate(group = factor(group, levels = paste("Tercile", 1:3)),
           dim_label = fct_rev(factor(str_wrap(as.character(dim_label), 34), levels = str_wrap(levels(dim_label), 34))),
           lo = post_avg - qnorm(0.975) * post_se, hi = post_avg + qnorm(0.975) * post_se) |>
    ggplot(aes(x = post_avg, y = dim_label, color = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", position = position_dodge(width = 0.6)) +
    geom_point(size = 2, position = position_dodge(width = 0.6)) +
    facet_wrap(~outcome, nrow = 1, scales = "free_x") +
    scale_color_manual(values = set_names(COL_TERCILE, paste("Tercile", 1:3)), name = "Lowest to highest third:") +
    scale_x_continuous(labels = labels, n.breaks = 4) +
    theme_paper() +
    theme(panel.spacing.x = unit(1.2, "lines")) +
    labs(x = x_lab, y = NULL, title = title)
}

# minimal booktabs table from a data frame (columns already formatted as text)
write_tex_table <- function(df, file, align = paste0("l", strrep("c", ncol(df) - 1))) {
  esc  <- \(x) str_replace_all(x, c("%" = "\\\\%", "&" = "\\\\&", "_" = "\\\\_"))
  rows <- apply(df, 1, \(r) paste(esc(r), collapse = " & "))
  writeLines(c(sprintf("\\begin{tabular}{%s}", align), "\\toprule",
               paste(esc(names(df)), collapse = " & ") |> paste("\\\\"), "\\midrule",
               paste(rows, "\\\\"), "\\bottomrule", "\\end{tabular}"), file)
}
