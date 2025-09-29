# export.R
# ---------------------------------------------
# Find the latest "WHO ATC-DDD YYYY-MM-DD.csv" inside ./output,
# keep ONLY Level 5 ATC codes (7-char codes) with ALL original columns,
# and export them.
# Canonical output: ./output/who_atc_<YYYY-MM-DD>.csv
# Legacy compatibility copy: ./output/who_atc_level5_<YYYY-MM-DD>.csv
# ---------------------------------------------

pacman::p_load(readr, dplyr)

#' Return the directory where the script resides, regardless of invocation context.
#'
#' @return Normalized path pointing to the script's directory.
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args)
  if (length(file_arg) > 0) {
    script_path <- sub("--file=", "", args[file_arg[1]])
    return(dirname(normalizePath(script_path, mustWork = FALSE)))
  }
  frame_files <- unlist(lapply(sys.frames(), function(env) {
    if (!is.null(env$ofile)) env$ofile else NULL
  }))
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[length(frame_files)], mustWork = FALSE)))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

script_dir <- get_script_dir()
output_dir <- file.path(script_dir, "output")

if (!dir.exists(output_dir)) {
  stop(sprintf("Output directory not found: %s", output_dir))
}

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
in_file <- files[latest_idx]
date_str <- date_strs[latest_idx]

# cat("Using latest input file:", basename(in_file), "\n")

# Read data
atc <- readr::read_csv(in_file, show_col_types = FALSE)

# Validate required columns
required_cols <- c("atc_code", "atc_name")
missing <- setdiff(required_cols, names(atc))
if (length(missing) > 0) {
  stop("Input file is missing required columns: ", paste(missing, collapse = ", "))
}

# Keep only Level 5 (7-char) ATC codes, retaining all original columns
## Filter to leaf-level ATC entries because earlier stages rely on molecules only.
atc_level5 <- atc %>%
  filter(nchar(atc_code) == 7) %>%
  arrange(atc_code)

# Canonical output used by the downstream Python pipeline
out_file_canonical <- file.path(output_dir, sprintf("who_atc_%s.csv", date_str))
readr::write_csv(atc_level5, out_file_canonical)

# Legacy filename retained for backwards compatibility with older workflows
out_file_legacy <- file.path(output_dir, sprintf("who_atc_level5_%s.csv", date_str))
if (!identical(out_file_legacy, out_file_canonical)) {
  readr::write_csv(atc_level5, out_file_legacy)
}

# cat("Export complete:", basename(out_file_canonical), "\n",
#     "Rows written:", nrow(atc_level5), "\n", sep = " ")
