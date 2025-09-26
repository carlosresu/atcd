# atcd

### Scrape Anatomical–Therapeutic–Chemical (ATC) classes from the WHO Collaborating Centre for Drug Statistics Methodology

**Codename:** `atcd`

This mini-toolset scrapes the WHO ATC website (https://www.whocc.no/atc_ddd_index/) and produces clean CSVs you can use in downstream pipelines. It:

- Crawls the ATC hierarchy **recursively** from each top-level root (A, B, C, …, V)
- Extracts **all levels (1–5)**, including Level‑5 DDD/UoM/Adm.R when present
- Exports a combined raw CSV snapshot (dated), plus a **Level‑5 only** CSV
- Optionally splits Level‑5 into **molecules** vs **excluded placeholders**

The scraper is **parallelized**, **memoized**, and uses a **hardened HTTP layer** with retries/timeouts.

---

## Quick start

```r
# From the repository root (or this subfolder)
# 1) Scrape the WHO ATC site (writes a dated raw CSV to ./output)
source("atcd.R")

# 2) Export Level‑5 only to ./output/who_atc_<YYYY-MM-DD>.csv
source("export.R")

# 3) (Optional) Split molecules vs excluded placeholders
source("filter.R")
```

Outputs are written under `./output/`:

- `WHO ATC-DDD <YYYY-MM-DD>.csv` – full crawl all levels (raw snapshot)
- `who_atc_<YYYY-MM-DD>.csv` – **Level‑5 only**, columns: `atc_name, atc_code`
- `who_atc_<YYYY-MM-DD>_molecules.csv` – Level‑5 molecules kept
- `who_atc_<YYYY-MM-DD>_excluded.csv` – Level‑5 placeholders removed

> Dates are inferred from the run day; re-run to refresh.

---

## How it works (high level)

```mermaid
flowchart TD
    A[Start] --> B[Scrape roots A,B,C,...,V]
    B --> C{Level < 5?}
    C -- yes --> D[Parse sublists → recurse]
    C -- no (Level 5) --> E[Parse table\nATC/Name/DDD/UoM/Adm.R/Note]
    D --> B
    E --> F[Write per-root RDS]
    F --> G[Bind all RDS → one tibble]
    G --> H[Write raw CSV: WHO ATC-DDD <date>.csv]
    H --> I[export.R]
    I --> J[who_atc_<date>.csv (Level‑5 only)]
    J --> K[filter_molecules.R]
    K --> L[molecules.csv / excluded.csv]
```

---

## Scripts & key behaviors

### `atcd.R`

- **Parallel scraping**: detects cores and uses `future::multisession` with `furrr::future_walk()` to process all ATC roots concurrently.
- **Hardened fetch** (`fetch_html`):
  - `httr2` request with custom User‑Agent and 30s timeout
  - Up to **3 retries** with exponential backoff (1s, 2s, 4s)
  - **In‑process memoization** via `memoise` to avoid duplicate fetches within a run
- **Resilient parsing**:
  - For Levels **1–4**, parses the subcode list and recurses
  - For **Level 5**, parses the details table into columns:
    `atc_code, atc_name, ddd, uom, adm_r, note`
  - Fills blank `atc_code/atc_name` on multi‑row DDD entries
- **Caching** with `wrapRDS()/getRDS()`
  - Each root (e.g., `who_atc_A`) is cached under `./output/rds/`
  - Final combined tibble is written as `WHO ATC-DDD <YYYY-MM-DD>.csv` in `./output/`

**Packages used**: `rvest`, `xml2`, `httr2`, `memoise`, `dplyr`, `readr`, `tibble`, `purrr`, `future`, `furrr`.

### `export.R`

- Scans `./output/` for the latest `WHO ATC-DDD <YYYY-MM-DD>.csv` by filename date
- Keeps **Level‑5** only (7‑char ATC codes)
- Exports **two columns** in sorted order: `atc_name, atc_code`
- Writes `./output/who_atc_<YYYY-MM-DD>.csv`

### `filter.R`

- Takes the latest `who_atc_<YYYY-MM-DD>.csv`
- Keeps Level‑5 rows (7‑char codes)
- Tags **pure placeholders** (e.g., `various`, `miscellaneous`, `unspecified`, `general`, `other/others`, `combination(s)`, `agents`, `products`)
- Writes two files:
  - `who_atc_<date>_molecules.csv` – molecules retained (even if name contains “combinations”)
  - `who_atc_<date>_excluded.csv` – placeholders only

---

## File schema

### Raw snapshot (`WHO ATC-DDD <YYYY-MM-DD>.csv`)

Columns may include:

- `atc_code` (chr) – ATC code (Level 1–5)
- `atc_name` (chr) – class or substance name
- `ddd` (dbl/chr, optional) – Defined Daily Dose (Level‑5 only; may be missing)
- `uom` (chr, optional) – unit of measure (Level‑5 only)
- `adm_r` (chr, optional) – route of administration (Level‑5 only)
- `note` (chr, optional) – additional notes

### Level‑5 export (`who_atc_<date>.csv`)

- `atc_name`, `atc_code` (7‑char codes only)

---

## Requirements

R ≥ 4.1 and the following packages:

```r
pacman::p_load(rvest, dplyr, readr, xml2, purrr, future, furrr, memoise, httr2, tibble, stringr)
```

If `pacman` isn’t installed:

```r
install.packages("pacman")
```

---

## Troubleshooting

- **Empty outputs**: WHO site may change layout or be rate‑limited. Re‑run later or reduce workers.
- **SSL / network errors**: Check connectivity; the fetcher retries (1s, 2s, 4s) before failing.
- **Stale data**: Delete `./output/rds/` to force a fresh scrape of a root.
- **Locale issues**: Ensure UTF‑8 encoding in your R session.

---

## Notes & licensing

- Data is scraped from the **WHO Collaborating Centre for Drug Statistics Methodology** and is subject to their terms. See their official site for the latest copyright/disclaimer.
- Original concept by [Fabrício Kury](https://github.com/fabkury) under **CC BY‑NC‑SA 4.0**; this fork retains the same license.
- This tool is for research and public‑interest use; please respect WHO server load—avoid excessive re‑scraping.
