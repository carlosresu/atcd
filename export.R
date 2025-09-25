# export.R
# ---------------------------------------------
# Find the latest "WHO ATC-DDD YYYY-MM-DD.csv" inside ./output,
# keep ONLY Level 5 ATC codes (7-char codes), and export two columns:
#   atc_name, atc_code
# Output file: ./output/who_atc_<YYYY-MM-DD>.csv
# ---------------------------------------------

pacman::p_load(readr, dplyr)

output_dir <- "output"

# List candidate files
files <- list.files(
  path = output_dir,
  pattern = "^WHO ATC-DDD \\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No 'WHO ATC-DDD YYYY-MM-DD.csv' files found in the 'output' folder.")
}

# Extract date string from each filename, then convert to Date
# Example basename: "WHO ATC-DDD 2025-09-25.csv"
date_strs <- sub("^WHO ATC-DDD (\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", basename(files))
dates <- as.Date(date_strs, format = "%Y-%m-%d")

# Pick the newest by the date embedded in the filename
latest_idx <- which.max(dates)
in_file   <- files[latest_idx]
date_str  <- date_strs[latest_idx]

cat("Using latest input file:", basename(in_file), "\n")

# Read data
atc <- readr::read_csv(in_file, show_col_types = FALSE)

# Validate required columns
required_cols <- c("atc_code", "atc_name")
missing <- setdiff(required_cols, names(atc))
if (length(missing) > 0) {
  stop("Input file is missing required columns: ", paste(missing, collapse = ", "))
}

# Keep only Level 5 (7-char) ATC codes and just the required columns
atc_level5 <- atc %>%
  filter(nchar(atc_code) == 7) %>%
  distinct(atc_name, atc_code) %>%
  arrange(atc_code)

# Output path carries the same date
out_file <- file.path(output_dir, sprintf("who_atc_%s.csv", date_str))

# Write CSV with two columns
readr::write_csv(atc_level5, out_file)

cat("Export complete:", basename(out_file), "\n",
    "Rows written:", nrow(atc_level5), "\n", sep = " ")
