# Labor and Automation

This project examines the relationship between interior immigration enforcement and agricultural mechanization in the United States. Using USDA Agricultural Census expenditure data merged with ICE Secure Communities removal records, the analysis investigates whether intensified enforcement — measured at the county level — is associated with shifts in farm labor composition and increased investment in labor-saving machinery. Data are not included in this repository and must be obtained separately.

## File Structure

```
labor_and_automation/
├── primary/
│   ├── main.r                  # primary data cleaning and analysis
│   ├── agcensus_api.R          # USDA Agricultural Census API pull
│   └── writing/
│       └── main.tex            # manuscript (synced with Overleaf)
├── preliminary/
│   ├── main.R                  # exploratory analysis
│   └── metadata_extraction.py  # PDF metadata extraction
├── figs/
│   ├── preliminary/            # exploratory figures
│   └── primary/                # primary analysis figures
└── data/                       # local only, not tracked by git
```
