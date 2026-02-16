####################################################################################################
# This script reads PDF data files on Ag Census farm expenditures and outputs per-state CSV files #
###################################################################################################
# Before running, set pathnames and clear DEBUG folder to avoid overlap
import re
from pathlib import Path
import pandas as pd
import camelot.io as camelot

# PDF path
PDF_PATH = "/Users/kieran/Documents/GitHub/labor_and_automation/data/states/pdfs/bama.pdf"

# Base project dir
OUT_DIR = Path("/Users/kieran/Documents/GitHub/labor_and_automation")
OUT_DIR.mkdir(parents=True, exist_ok=True)

# Debug tables directory
DEBUG_DIR = Path("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/debug")
DEBUG_DIR.mkdir(parents=True, exist_ok=True)

# Final statefile CSV directory
STATEFILES_DIR = Path("/Users/kieran/Documents/GitHub/labor_and_automation/data/states/statefiles")
STATEFILES_DIR.mkdir(parents=True, exist_ok=True)

# Camelot parameters
FLAVOR = "stream"
EDGE_TOL = 500
ROW_TOL = 10
STRIP_TEXT = "\n"
SPLIT_TEXT = True

# Canonical categories
CATEGORIES_CANON = [
    "Total farm production expenditures",
    "Average per farm",
    "Fertilizer lime and soil conditioners purchased",
    "Chemicals purchased",
    "Seeds plants vines and trees purchased",
    "Livestock and poultry purchased or leased",
    "Breeding livestock purchased or leased",
    "Other livestock and poultry purchased or leased",
    "Feed purchased",
    "Gasoline fuels and oils purchased",
    "Utilities",
    "Repairs supplies and maintenance cost",
    "Hired farm labor",
    "Contract labor",
    "Customwork and custom hauling",
    "Cash rent for land buildings and grazing fees",
    "Rent and lease expenses for machinery equipment and farm share of vehicles",
    "Interest expense",
    "Property taxes paid",
    "All other production expenses",
    "Depreciation expenses claimed",
]
CANON_SET = set(CATEGORIES_CANON)


def normalize(s):
    s = "" if s is None else str(s)
    s = s.replace("\u00a0", " ")
    s = s.replace("\u2014", "-")
    s = re.sub(r"\s+", " ", s).strip()
    return s


def scrub(s):
    s = normalize(s)
    s = re.sub(r"\.{2,}", " ", s)
    s = re.sub(r"[.*?]", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def norm_key(s):
    s = scrub(s).lower()
    s = re.sub(r"[^a-z0-9 ]+", "", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


CATEGORY_PATTERNS = [
    (re.compile(r"\btotal farm pr", re.I), "Total farm production expenditures"),
    (re.compile(r"\baverage per f", re.I), "Average per farm"),
    (re.compile(r"\bfertilizer", re.I), "Fertilizer lime and soil conditioners purchased"),
    (re.compile(r"\bchemicals purch", re.I), "Chemicals purchased"),
    (re.compile(r"\bseeds", re.I), "Seeds plants vines and trees purchased"),
    (re.compile(r"\blivestock and poultry purchased", re.I), "Livestock and poultry purchased or leased"),
    (re.compile(r"\bbreeding livestock purchased", re.I), "Breeding livestock purchased or leased"),
    (re.compile(r"\bother livestock and poultry purchased", re.I), "Other livestock and poultry purchased or leased"),
    (re.compile(r"\bfeed purch", re.I), "Feed purchased"),
    (re.compile(r"\bgasoline", re.I), "Gasoline fuels and oils purchased"),
    (re.compile(r"\butilities", re.I), "Utilities"),
    (re.compile(r"\brepairs", re.I), "Repairs supplies and maintenance cost"),
    (re.compile(r"\bhired farm labor", re.I), "Hired farm labor"),
    (re.compile(r"\bcontract labor", re.I), "Contract labor"),
    (re.compile(r"\bcustomwork", re.I), "Customwork and custom hauling"),
    (re.compile(r"\bcash rent for land", re.I), "Cash rent for land buildings and grazing fees"),
    (re.compile(r"\brent and leas", re.I), "Rent and lease expenses for machinery equipment and farm share of vehicles"),
    (re.compile(r"\binterest expense", re.I), "Interest expense"),
    (re.compile(r"\bproperty taxes paid", re.I), "Property taxes paid"),
    (re.compile(r"\ball other production expenses", re.I), "All other production expenses"),
    (re.compile(r"\bdepreciation expenses claimed", re.I), "Depreciation expenses claimed"),
]


def canonical_from_any_text(text):
    k = norm_key(text)
    if not k:
        return None
    k = k.replace("d epreciation", "depreciation")
    for pat, canon in CATEGORY_PATTERNS:
        if pat.search(k):
            return canon
    return None


def split_two_numbers(cell):
    cell = normalize(cell)
    if cell in ("", "-", "—", "NA", "N/A"):
        return (None, None)
    nums = re.findall(r"-?\d[\d,]*", cell)
    nums = [n.replace(",", "") for n in nums if n.strip() != ""]
    v1 = int(nums[0]) if len(nums) >= 1 else None
    v2 = int(nums[1]) if len(nums) >= 2 else None
    return (v1, v2)


def to_int(x):
    x = normalize(x)
    if x in ("", "-", "—", "NA", "N/A", "(D)"):
        return None
    x = x.replace(",", "")
    m = re.search(r"-?\d+", x)
    return int(m.group()) if m else None


def read_tables_and_write_debug():
    tables = camelot.read_pdf(
        PDF_PATH,
        pages="all",
        flavor=FLAVOR,
        split_text=SPLIT_TEXT,
        edge_tol=EDGE_TOL,
        row_tol=ROW_TOL,
        strip_text=STRIP_TEXT,
    )
    for k, t in enumerate(tables):
        table_id = f"p{int(t.page):02d}t{k:02d}"
        t.df.to_csv(DEBUG_DIR / f"DEBUG_{table_id}_{FLAVOR}.csv", index=False, header=False)
    return tables


def parse_debug_csv_money_only(csv_path):
    raw = pd.read_csv(csv_path, header=None, dtype=str).fillna("")
    raw = raw.map(normalize)

    header_i = None
    for i in range(min(30, len(raw))):
        row = raw.iloc[i].tolist()
        txt = " ".join(row).lower()
        if "item" in txt and sum(1 for c in row if re.search(r"[A-Za-z]", c)) >= 4:
            header_i = i
            break
    if header_i is None:
        return []

    counties = [scrub(x) for x in raw.iloc[header_i, 1:].tolist()]
    while counties and counties[-1] == "":
        counties.pop()
    n = len(counties)
    if n == 0:
        return []

    def get_row(i):
        return raw.iloc[i, :1 + n + 4].tolist()

    records = []
    current_cat = None
    i = header_i + 1

    while i < len(raw):
        row = get_row(i)
        full = " ".join(scrub(x) for x in row)
        low = full.lower()

        cand = canonical_from_any_text(full)
        if cand in CANON_SET:
            current_cat = cand

        if current_cat and ("$1,000" in low) and ("2012" in low) and ("2007" in low):
            for j, county in enumerate(counties, start=1):
                v2012, v2007 = split_two_numbers(row[j])
                if v2012 is not None:
                    records.append({"county": county, "year": "2012", "category": current_cat, "value": v2012})
                if v2007 is not None:
                    records.append({"county": county, "year": "2007", "category": current_cat, "value": v2007})
            i += 1
            continue

        if current_cat and ("$1,000" in low) and ("2012" in low) and ("2007" not in low):
            for j, county in enumerate(counties, start=1):
                v = to_int(row[j])
                if v is not None:
                    records.append({"county": county, "year": "2012", "category": current_cat, "value": v})

            if i + 1 < len(raw):
                nxt = get_row(i + 1)
                nxt0 = scrub(nxt[0]).lower()
                nxt_full = " ".join(scrub(x) for x in nxt).lower()

                if nxt0 == "2007":
                    for j, county in enumerate(counties, start=1):
                        v = to_int(nxt[j])
                        if v is not None:
                            records.append({"county": county, "year": "2007", "category": current_cat, "value": v})
                elif "2007" in nxt_full and "$1,000" not in nxt_full:
                    for j, county in enumerate(counties, start=1):
                        v1, v2 = split_two_numbers(nxt[j])
                        if v2 is not None:
                            records.append({"county": county, "year": "2007", "category": current_cat, "value": v2})

            i += 1
            continue

        if current_cat == "Average per farm" and ("dollars" in low) and ("2012" in low):
            if "2007" in low:
                for j, county in enumerate(counties, start=1):
                    v2012, v2007 = split_two_numbers(row[j])
                    if v2012 is not None:
                        records.append({"county": county, "year": "2012", "category": current_cat, "value": v2012})
                    if v2007 is not None:
                        records.append({"county": county, "year": "2007", "category": current_cat, "value": v2007})
                current_cat = None
                i += 1
                continue

            if i + 1 < len(raw):
                r2012 = get_row(i + 1)
                for j, county in enumerate(counties, start=1):
                    v = to_int(r2012[j])
                    if v is not None:
                        records.append({"county": county, "year": "2012", "category": current_cat, "value": v})

                if i + 2 < len(raw) and scrub(raw.iloc[i + 2, 0]).lower() == "2007" and i + 3 < len(raw):
                    r2007 = get_row(i + 3)
                    for j, county in enumerate(counties, start=1):
                        v = to_int(r2007[j])
                        if v is not None:
                            records.append({"county": county, "year": "2007", "category": current_cat, "value": v})

            current_cat = None
            i += 1
            continue

        i += 1

    return records


def main():
    read_tables_and_write_debug()
    debug_files = sorted(DEBUG_DIR.glob(f"DEBUG_*_{FLAVOR}.csv"))

    all_records = []
    for f in debug_files:
        all_records.extend(parse_debug_csv_money_only(f))

    df = pd.DataFrame(all_records)
    if df.empty:
        raise RuntimeError("No money records parsed from debug CSVs.")

    final = (
        df.pivot_table(index=["county", "year"], columns="category", values="value", aggfunc="first")
        .reset_index()
    )

    for c in CATEGORIES_CANON:
        if c not in final.columns:
            final[c] = pd.NA
    final = final[["county", "year"] + CATEGORIES_CANON]

    out_path = STATEFILES_DIR / "bama.csv"
    final.to_csv(out_path, index=False)

    print("Saved:", out_path)
    print("Non-missing money counts:")
    print(final[CATEGORIES_CANON].notna().sum().sort_values(ascending=False))


if __name__ == "__main__":
    main()
