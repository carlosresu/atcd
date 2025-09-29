# filter.R
# --------------------------------------------------------
# Splits the latest who_atc_level5_YYYY-MM-DD.csv into:
#   molecules.csv (real substances, even if "... combinations")
#   excluded.csv  (generic placeholders only)
# This version keeps ALL original columns in the output files.
# --------------------------------------------------------

pacman::p_load(readr, dplyr, stringr, purrr)

#' Return the directory where the script lives.
#'
#' @return Normalized path to the script directory, even when sourced via Rscript.
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

# Find the latest canonical who_atc_<YYYY-MM-DD>.csv; fall back to legacy naming if needed
files_canonical <- list.files(
  path = output_dir,
  pattern = "^who_atc_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
)

files_legacy <- list.files(
  path = output_dir,
  pattern = "^who_atc_level5_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
)

files <- files_canonical
name_prefix <- "who_atc"
if (length(files) == 0) {
  files <- files_legacy
  name_prefix <- "who_atc_level5"
}

if (length(files) == 0) {
  stop("No who_atc CSV exports found in ./output. Run export.R first.")
}

date_pattern <- sprintf("^%s_(\\d{4}-\\d{2}-\\d{2})\\.csv$", name_prefix)
date_strs <- sub(date_pattern, "\\1", basename(files))
dates <- as.Date(date_strs, "%Y-%m-%d")
latest_idx <- which.max(dates)

in_file <- files[latest_idx]
date_str <- date_strs[latest_idx]

out_file_molecules_canonical <- file.path(output_dir, sprintf("who_atc_%s_molecules.csv", date_str))
out_file_excluded_canonical <- file.path(output_dir, sprintf("who_atc_%s_excluded.csv", date_str))

out_file_molecules_legacy <- file.path(output_dir, sprintf("who_atc_level5_%s_molecules.csv", date_str))
out_file_excluded_legacy <- file.path(output_dir, sprintf("who_atc_level5_%s_excluded.csv", date_str))

# cat("Using latest input file:", basename(in_file), "\n")

# Read file
atc <- readr::read_csv(in_file, show_col_types = FALSE)

# Terms that indicate pure placeholders
placeholder_tokens <- c(
  "various", "miscellaneous", "unspecified", "general",
  "other", "others", "combination", "combinations",
  "agents", "products"
)

# Classify rows, adding temporary columns for filtering
classified <- atc %>%
  mutate(
    name_trim = str_squish(str_to_lower(atc_name)),
    name_tokens = str_split(str_replace_all(name_trim, "[^a-z]+", " "), "\\s+"),
    is_excluded = map_lgl(name_tokens, function(tokens) {
      tokens <- tokens[tokens != ""]
      length(tokens) > 0 && all(tokens %in% placeholder_tokens)
    })
  )

# Filter for molecules, removing temporary columns and keeping all original ones
molecules <- classified %>%
  filter(!is_excluded) %>%
  select(-name_trim, -name_tokens, -is_excluded) %>%
  arrange(atc_code)

# Filter for excluded placeholders, removing temporary columns and keeping all original ones
excluded <- classified %>%
  filter(is_excluded) %>%
  select(-name_trim, -name_tokens, -is_excluded) %>%
  arrange(atc_code)

# Write outputs
readr::write_csv(molecules, out_file_molecules_canonical)
readr::write_csv(excluded, out_file_excluded_canonical)

if (!identical(out_file_molecules_canonical, out_file_molecules_legacy)) {
  readr::write_csv(molecules, out_file_molecules_legacy)
  readr::write_csv(excluded, out_file_excluded_legacy)
}

# cat("Filtering complete.\n",
#     "Input rows:", nrow(atc), "\n",
#     "Molecules kept:", nrow(molecules), "->", basename(out_file_molecules_canonical), "\n",
#     "Excluded rows:", nrow(excluded), "->", basename(out_file_excluded_canonical), "\n")
