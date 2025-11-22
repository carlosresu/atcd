# export.R
# ---------------------------------------------
# Find the latest "WHO ATC-DDD YYYY-MM-DD.parquet" inside ./output,
# keep ONLY Level 5 ATC codes (7-char codes) with ALL original columns,
# and export them as ./output/who_atc_<YYYY-MM-DD>.parquet (plus CSV for debugging)
# ---------------------------------------------

pacman::p_load(polars)

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

# Mirror outputs into the superproject when running inside ~/esoa.
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

# List candidate files
files <- list.files(
  path = output_dir,
  pattern = "^WHO ATC-DDD \\d{4}-\\d{2}-\\d{2}\\.parquet$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No 'WHO ATC-DDD YYYY-MM-DD.parquet' files found in the 'output' folder.")
}

# Extract date string from each filename, then convert to Date
# Example basename: "WHO ATC-DDD 2025-09-25.parquet"
date_strs <- sub("^WHO ATC-DDD (\\d{4}-\\d{2}-\\d{2})\\.parquet$", "\\1", basename(files))
dates <- as.Date(date_strs, format = "%Y-%m-%d")

# Pick the newest by the date embedded in the filename
latest_idx <- which.max(dates)
in_file <- files[latest_idx]
date_str <- date_strs[latest_idx]

# cat("Using latest input file:", basename(in_file), "\n")

# Read data
atc <- polars::pl$scan_parquet(in_file)

# Validate required columns
required_cols <- c("atc_code", "atc_name")
missing <- setdiff(required_cols, atc$columns)
if (length(missing) > 0) {
  stop("Input file is missing required columns: ", paste(missing, collapse = ", "))
}

# Keep only Level 5 (7-char) ATC codes, retaining all original columns.
# Filter to leaf-level ATC entries because earlier stages rely on molecules only.
atc_level5 <- atc$filter(polars::pl$col("atc_code")$str$len_bytes() == 7)$sort("atc_code")$collect()

out_file_canonical <- file.path(output_dir, sprintf("who_atc_%s.parquet", date_str))
# Matches dependencies/atcd/README.md note that Python loaders expect this filename pattern.
invisible(lapply(write_csv_and_parquet(atc_level5, out_file_canonical), copy_outputs_to_superproject))

# cat("Export complete:", basename(out_file_canonical), "\n",
#     "Rows written:", nrow(atc_level5), "\n", sep = " ")
