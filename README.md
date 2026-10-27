# Labor and Automation

This project examines how interior immigration enforcement affected US agriculture. It uses the staggered county rollout of Secure Communities (SC, 2008–2013) to ask whether enforcement reduced the immigrant farm workforce, and how farms adjusted: through mechanization, a shift away from labor-intensive crops, or guestworkers. Outcomes come from the USDA Census of Agriculture, the ACS (IPUMS), the BLS QCEW, and Census of Agriculture crop acreage. They are analysed at the county and commuting-zone level.

Data are not included in this repository. Inputs and outputs live in the project Dropbox (`secure_communities/kieran/`), and the paths are set in `primary/00_setup.r`.

## Pipeline

Run in order:

| Script | Does | Writes (Dropbox `kieran/`) |
|---|---|---|
| `primary/00_setup.r` | Paths, figure palette, and the shared event-study estimator and robustness set | — |
| `primary/01_clean.r` | Cleans raw data and builds every analysis panel (county and commuting zone) | `data/clean/*.rds` |
| `primary/02_vulnerability.r` | Picks the vulnerability measure: the baseline characteristic that best predicts SC removals | `output/vulnerability_choice.csv`, `figures/vulnerability/` |
| `primary/03_labor.r` | Q1: US-born vs foreign-born ag employment (ACS) and formal ag payroll jobs by industry (QCEW), by vulnerability quartile, with and without the population control | `figures/labor/`, `figures/diagnostics/`, `output/coefs/labor_*` |
| `primary/04_technology.r` | Q2: mechanization, hired labor and crop mix (Census of Agriculture) | `figures/technology/`, `output/coefs/technology_*` |
| `primary/05_informality.r` | Q3: formal (QCEW) vs informal (ACS) ag work, self-employment, guestworker proxy | `figures/informality/`, `output/coefs/informality_*` |

Every outcome script uses the same specification: an event study in years of SC exposure (years between a place's activation and the last activation), interacted with year, with unit and state × year fixed effects, border-distance, Great Recession and log-population controls (each × year), and an interaction with vulnerability (2000 Hispanic noncitizen share, per SD). Each also reports the same robustness set.

`primary/agcensus_api.R` holds the USDA NASS QuickStats pulls that produced the raw Census of Agriculture files. It is run by hand, not as part of the pipeline.

## File Structure

```
labor_and_automation/
├── primary/
│   ├── 00_setup.r          # paths, palette, shared estimator
│   ├── 01_clean.r          # data cleaning and panel construction
│   ├── 02_vulnerability.r  # choice of vulnerability measure
│   ├── 03_labor.r          # Q1: labor by nativity
│   ├── 04_technology.r     # Q2: technology and crop mix
│   ├── 05_informality.r    # Q3: formal vs informal ag work
│   ├── agcensus_api.R      # USDA Census of Agriculture API pull
│   └── writing/
│       └── main.tex        # manuscript (synced with Overleaf)
```
