# filter_molecules.R
# --------------------------------------------------------
# Splits the latest who_atc_YYYY-MM-DD.csv into:
#   molecules.csv (real substances, even if "... combinations")
#   excluded.csv  (generic placeholders only)
# --------------------------------------------------------

pacman::p_load(readr, dplyr, stringr)

output_dir <- "output"

# Find the latest who_atc_<YYYY-MM-DD>.csv
files <- list.files(
  path = output_dir,
  pattern = "^who_atc_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
)
if (length(files) == 0) stop("No 'who_atc_YYYY-MM-DD.csv' found in ./output. Run export.R first.")

date_strs <- sub("^who_atc_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", basename(files))
dates <- as.Date(date_strs, "%Y-%m-%d")
latest_idx <- which.max(dates)

in_file  <- files[latest_idx]
date_str <- date_strs[latest_idx]

out_file_molecules <- file.path(output_dir, sprintf("who_atc_%s_molecules.csv", date_str))
out_file_excluded  <- file.path(output_dir, sprintf("who_atc_%s_excluded.csv", date_str))

# cat("Using latest input file:", basename(in_file), "\n")

# Read file
atc <- readr::read_csv(in_file, show_col_types = FALSE)

# Level 5 filter: 7-character ATC codes only
atc <- atc %>% filter(nchar(atc_code) == 7)

# Terms that indicate pure placeholders
pure_bad_terms <- c(
  "various", "miscellaneous", "unspecified", "general",
  "other", "others", "combination", "combinations",
  "agents", "products"
)

# Classify rows
classified <- atc %>%
  mutate(
    name_trim   = str_squish(str_to_lower(atc_name)),
    is_excluded = name_trim %in% pure_bad_terms
  )

molecules <- classified %>%
  filter(!is_excluded) %>%
  select(atc_name, atc_code) %>%
  arrange(atc_code)

excluded <- classified %>%
  filter(is_excluded) %>%
  select(atc_name, atc_code) %>%
  arrange(atc_code)

# Write outputs
readr::write_csv(molecules, out_file_molecules)
readr::write_csv(excluded, out_file_excluded)

# cat("Filtering complete.\n",
#     "Input rows:", nrow(atc), "\n",
#     "Molecules kept:", nrow(molecules), "->", basename(out_file_molecules), "\n",
#     "Excluded rows:", nrow(excluded), "->", basename(out_file_excluded), "\n")
