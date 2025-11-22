# filter.R
# --------------------------------------------------------
# Splits the latest who_atc_YYYY-MM-DD.parquet into:
#   molecules.parquet (real substances, even if "... combinations")
#   excluded.parquet  (generic placeholders only)
# This version keeps ALL original columns in the output files and also emits CSVs for debugging.
# --------------------------------------------------------

pacman::p_load(polars)

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

# Mirror outputs into the superproject when available.
paths_equal <- function(path_a, path_b) {
  if (is.null(path_a) || is.null(path_b)) {
    return(FALSE)
  }
  a_norm <- tryCatch(
    normalizePath(path_a, winslash = "/", mustWork = FALSE),
    error = function(...) NA_character_
  )
  b_norm <- tryCatch(
    normalizePath(path_b, winslash = "/", mustWork = FALSE),
    error = function(...) NA_character_
  )
  !is.na(a_norm) && !is.na(b_norm) && identical(a_norm, b_norm)
}

safe_copy <- function(src, dest) {
  tryCatch({
    if (!file.exists(src) || paths_equal(src, dest)) {
      return(FALSE)
    }
    dest_dir <- dirname(dest)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!dir.exists(dest_dir)) {
      return(FALSE)
    }
    file.copy(src, dest, overwrite = TRUE, copy.mode = TRUE)
  }, error = function(...) FALSE)
}

copy_outputs_to_superproject <- function(src_file) {
  repo_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  dependencies_dir <- file.path(repo_root, "dependencies")
  if (!dir.exists(dependencies_dir)) {
    return(invisible(FALSE))
  }

  super_output_dir <- file.path(repo_root, "dependencies", "atcd", "output")
  safe_copy(src_file, file.path(super_output_dir, basename(src_file)))

  if (grepl("_molecules\\.(csv|parquet)$", basename(src_file), perl = TRUE)) {
    inputs_dir <- file.path(repo_root, "inputs", "drugs")
    base_no_suffix <- sub("_molecules", "", basename(src_file))
    safe_copy(src_file, file.path(inputs_dir, base_no_suffix))
  }
}

write_csv_and_parquet <- function(df, parquet_path) {
  df$write_parquet(parquet_path)
  csv_path <- sub("\\.parquet$", ".csv", parquet_path)
  df$write_csv(csv_path)
  c(csv = csv_path, parquet = parquet_path)
}

# Find the latest canonical who_atc_<YYYY-MM-DD>.parquet
files <- list.files(
  path = output_dir,
  pattern = "^who_atc_\\d{4}-\\d{2}-\\d{2}\\.parquet$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No who_atc Parquet exports found in ./output. Run export.R first.")
}

date_pattern <- "^who_atc_(\\d{4}-\\d{2}-\\d{2})\\.parquet$"
date_strs <- sub(date_pattern, "\\1", basename(files))
dates <- as.Date(date_strs, "%Y-%m-%d")
latest_idx <- which.max(dates)

in_file <- files[latest_idx]
date_str <- date_strs[latest_idx]

out_file_molecules_canonical <- file.path(output_dir, sprintf("who_atc_%s_molecules.parquet", date_str))
out_file_excluded_canonical <- file.path(output_dir, sprintf("who_atc_%s_excluded.parquet", date_str))

# cat("Using latest input file:", basename(in_file), "\n")

# Read file
atc <- polars::pl$scan_parquet(in_file)

# Terms that indicate pure placeholders
placeholder_tokens <- c(
  "various", "miscellaneous", "unspecified", "general",
  "other", "others", "combination", "combinations",
  "agents", "products"
)

# Classify rows, adding temporary columns for filtering
## Flag rows that are placeholders versus true molecule entries.
classified <- atc |>
    polars::pl$with_columns(
      name_trim = polars::pl$col("atc_name")$str$to_lowercase()$str$replace_all("[^a-z]+", " ")$str$strip(),
      name_tokens = polars::pl$col("atc_name")$str$to_lowercase()$str$replace_all("[^a-z]+", " ")$str$strip()$str$split_regex("\\s+")
    ) |>
    polars::pl$with_columns(
      is_excluded = polars::pl$when(polars::pl$col("name_tokens")$list$lengths() > 0)$then(
        polars::pl$col("name_tokens")$list$all(polars::pl$element()$is_in(placeholder_tokens))
      )$otherwise(FALSE)
    )

# Filter for molecules, removing temporary columns and keeping all original ones
molecules <- classified |>
  polars::pl$filter(polars::pl$col("is_excluded")$not()) |>
  polars::pl$drop("name_trim", "name_tokens", "is_excluded") |>
  polars::pl$arrange("atc_code") |>
  polars::pl$collect()

# Filter for excluded placeholders, removing temporary columns and keeping all original ones
excluded <- classified |>
  polars::pl$filter(polars::pl$col("is_excluded")) |>
  polars::pl$drop("name_trim", "name_tokens", "is_excluded") |>
  polars::pl$arrange("atc_code") |>
  polars::pl$collect()

# Write outputs
invisible(lapply(write_csv_and_parquet(molecules, out_file_molecules_canonical), copy_outputs_to_superproject))
invisible(lapply(write_csv_and_parquet(excluded, out_file_excluded_canonical), copy_outputs_to_superproject))

# cat("Filtering complete.\n",
#     "Input rows:", nrow(atc), "\n",
#     "Molecules kept:", nrow(molecules), "->", basename(out_file_molecules_canonical), "\n",
#     "Excluded rows:", nrow(excluded), "->", basename(out_file_excluded_canonical), "\n")
