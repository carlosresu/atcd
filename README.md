# atcd

### Scrape Anatomical-Therapeutic-Chemical (ATC) classes from the WHO Collaborating Centre for Drug Statistics Methodology website

###### codename: atcd

This script scrapes the World Health Organization's ATC website (https://www.whocc.no/atc_ddd_index/).  
It reads ATC classes and their information, and writes them to one flat CSV file.

The code runs recursively down the hierarchy from an input ATC code. For example, if provided with `C10`, it will download `C10` and all subcodes under `C10`.

- **ATC levels 1–4:** the codes and names of all classes.
- **ATC level 5:** some but not all classes also have Administration Route, Defined Daily Dose (DDD), and Note; these are scraped if present.

---

## 📊 Scraping results (reference snapshot)

As of **May 7th, 2020** there were **6,331** unique ATC codes and 5,517 unique names on the WHO website.

| ATC level | Codes | Names |
| :-------- | ----: | ----: |
| Level 1   |    14 |    14 |
| Level 2   |    94 |    94 |
| Level 3   |   267 |   262 |
| Level 4   |   889 |   819 |
| Level 5   |  5067 |  4363 |

Examples:

- The substance **miconazole** appears under multiple codes:  
  [A01AB09](https://www.whocc.no/atc_ddd_index/?code=A01AB09&showdescription=no),  
  [A07AC01](https://www.whocc.no/atc_ddd_index/?code=A07AC01&showdescription=no),  
  [D01AC02](https://www.whocc.no/atc_ddd_index/?code=D01AC02&showdescription=no),  
  [G01AF04](https://www.whocc.no/atc_ddd_index/?code=G01AF04&showdescription=no),  
  [J02AB01](https://www.whocc.no/atc_ddd_index/?code=J02AB01&showdescription=no),  
  [S02AA13](https://www.whocc.no/atc_ddd_index/?code=S02AA13&showdescription=no).
- The name with the most codes is **"combinations"** (39 codes).
- The ATC codes with the most DDD–UoM–Adm. route combinations include [G03CA03](https://www.whocc.no/atc_ddd_index/?code=G03CA03&showdescription=no) (10).

👉 **If you just need all ATC classes and data in one big table, download the CSV produced by this script.**  
Note that the WHO website is updated over time, so re-scraping ensures up-to-date data.

---

## 🚀 What’s new in this fork (`carlosresu/atcd`)

This repository is a fork of [fabkury/atcd](https://github.com/fabkury/atcd).  
Enhancements have been made for **speed, reliability, maintainability, and downstream filtering**:

### ✨ Added

- **Parallel execution**: Scraping ATC roots in parallel with [`future`](https://cran.r-project.org/package=future) + [`furrr`](https://cran.r-project.org/package=furrr).
- **Robust HTTP layer** via [`httr2`](https://cran.r-project.org/package=httr2):
  - Custom User-Agent
  - Connection timeouts
  - 3 retries with exponential backoff (1s, 2s, 4s)
  - In-process **memoization** with `memoise` (no duplicate fetches within a run)
- **Export helper (`export.R`)**: automatically finds the newest scrape in `./output/` and produces a filtered file with only **Level-5 ATC codes** (`who_atc_<DATE>.csv`).
- **Filtering helper (`filter_molecules.R`)**:
  - Splits the latest Level-5 file into **molecules.csv** (true molecules, including molecule + “combinations” rows) and **excluded.csv** (placeholders like “various”, “combinations”, “other agents”, etc.).
  - Comprehensive blacklist of generic descriptors built in (e.g. “various”, “miscellaneous”, “unspecified”, “agents”, “vitamins”, “vaccines”).
  - Retains molecules even if their names contain “combinations” (e.g. “ibuprofen, combinations”).

### 🔄 Changed

- **`read_html()` → `fetch_html()`**: hardened, memoized fetcher with retry and timeout.
- **Looping model**: sequential `for` loop replaced by parallel `future_map()` across ATC root letters.
- **Assignment semantics**: inside parallel workers, RDS is written but in-process assignment is skipped (`assign_val = FALSE`).
- **dplyr API modernized**: `mutate_all()` → `mutate(across(...))`.
- **Type checks**: `class(sdt) == 'xml_missing'` → `inherits(sdt, "xml_missing")`.
- **Explicit namespacing** of functions (`rvest::`, `dplyr::`, `readr::`, `tibble::`) for worker safety.

### ⚡ Performance

- **Faster wall-clock time** on first scrape (multi-core parallel).
- **Reduced redundant calls** with memoization.
- **Resilient to slow servers**: automatic retries and timeouts.

### 🔒 Reliability

- Clearer progress feedback (`.progress = TRUE` with furrr).
- Parallel plan resets back to sequential after scraping for safety.
- If rate-limited by WHO, easy to throttle with a sleep or rate-limiter.

### 🧩 Compatibility

- Output CSV schema unchanged for the main scrape.
- RDS caching behavior unchanged (per-root RDS).
- ATC root list identical to original.

---

## 📥 Installation & Usage

1. Clone this repo:

   ```bash
   git clone https://github.com/carlosresu/atcd
   cd atcd
   ```

2. Run the scraper in R:

   ```r
   source("atcd.R")
   ```

   This will:

   - Scrape all ATC roots (A, B, C, D, G, H, J, L, M, N, P, R, S, V)
   - Write cached `.rds` files in `output/rds/`
   - Write one combined CSV named like `WHO ATC-DDD 2025-09-25.csv` in `output/`

3. Export only **Level-5 ATC codes**:

   ```r
   source("export.R")
   ```

   Produces `who_atc_<DATE>.csv` in `output/`.

4. Split into **molecules vs. excluded**:

   ```r
   source("filter_molecules.R")
   ```

   Produces:

   - `who_atc_<DATE>_molecules.csv`
   - `who_atc_<DATE>_excluded.csv`

---

## ⚖️ License & Copyright

- Original code and README by [Fabrício Kury](https://github.com/fabkury), released under **CC BY-NC-SA 4.0**.
- This fork (`carlosresu/atcd`) maintains the same license.
- The ATC-DDD data itself remains property of the [WHO Collaborating Centre for Drug Statistics Methodology](https://www.whocc.no). See their [copyright disclaimer](https://www.whocc.no/copyright_disclaimer/).

---

## 🔍 Search keywords

ATC download complete ATC with DDD ATC hierarchy ATC database all ATC classes with defined daily dose atc code list excel all atc codes csv download atc codes free download atc classification of drugs WHOCC scraping molecules only exclude combinations
