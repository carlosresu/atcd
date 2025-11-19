# filter.R
# --------------------------------------------------------
# Splits the latest who_atc_YYYY-MM-DD.csv into:
#   molecules.csv (real substances, even if "... combinations")
#   excluded.csv  (generic placeholders only)
# This version keeps ALL original columns in the output files.
# --------------------------------------------------------

pacman::p_load(readr, dplyr, stringr, purrr, future, furrr)

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

  inputs_dir <- file.path(repo_root, "inputs")
  safe_copy(src_file, file.path(inputs_dir, basename(src_file)))
}

.resolve_worker_count <- function() {
  env_workers <- Sys.getenv("ESOA_ATCD_FILTER_WORKERS", unset = NA_character_)
  if (!is.na(env_workers)) {
    parsed <- suppressWarnings(as.integer(env_workers))
    if (!is.na(parsed) && parsed > 0) {
      return(parsed)
    }
  }
  cores <- tryCatch(future::availableCores(), error = function(...) NA_integer_)
  if (is.na(cores) || cores < 1) {
    cores <- tryCatch(parallel::detectCores(), error = function(...) NA_integer_)
  }
  if (is.na(cores) || cores <= 1) {
    return(1L)
  }
  max(1L, cores - 1L)
}

.configure_future_plan <- function(workers = NULL) {
  if (is.null(workers) || !is.numeric(workers) || workers < 1) {
    workers <- .resolve_worker_count()
  }
  workers <- max(1L, as.integer(workers))
  if (.Platform$OS.type == "windows") {
    future::plan(future::multisession, workers = workers)
  } else if (future::supportsMulticore()) {
    future::plan(future::multicore, workers = workers)
  } else {
    future::plan(future::multisession, workers = workers)
  }
  workers
}

# Find the latest canonical who_atc_<YYYY-MM-DD>.csv
files <- list.files(
  path = output_dir,
  pattern = "^who_atc_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No who_atc CSV exports found in ./output. Run export.R first.")
}

date_pattern <- "^who_atc_(\\d{4}-\\d{2}-\\d{2})\\.csv$"
date_strs <- sub(date_pattern, "\\1", basename(files))
dates <- as.Date(date_strs, "%Y-%m-%d")
latest_idx <- which.max(dates)

in_file <- files[latest_idx]
date_str <- date_strs[latest_idx]

out_file_molecules_canonical <- file.path(output_dir, sprintf("who_atc_%s_molecules.csv", date_str))
out_file_excluded_canonical <- file.path(output_dir, sprintf("who_atc_%s_excluded.csv", date_str))

# cat("Using latest input file:", basename(in_file), "\n")

# Read file
atc <- readr::read_csv(in_file, show_col_types = FALSE)

# Terms that indicate pure placeholders
placeholder_tokens <- c(
  "various", "miscellaneous", "unspecified", "general",
  "other", "others", "combination", "combinations",
  "agents", "products"
)

workers <- .configure_future_plan()
on.exit(try(future::plan(future::sequential), silent = TRUE), add = TRUE)

# Classify rows, adding temporary columns for filtering
## Flag rows that are placeholders versus true molecule entries.
classified <- atc %>%
  mutate(
    name_trim = str_squish(str_to_lower(atc_name)),
    name_tokens = str_split(str_replace_all(name_trim, "[^a-z]+", " "), "\\s+"),
    is_excluded = furrr::future_map_lgl(name_tokens, function(tokens) {
      tokens <- tokens[tokens != ""]
      length(tokens) > 0 && all(tokens %in% placeholder_tokens)
    })
  )
try(future::plan(future::sequential), silent = TRUE)

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
copy_outputs_to_superproject(out_file_molecules_canonical)
copy_outputs_to_superproject(out_file_excluded_canonical)

# cat("Filtering complete.\n",
#     "Input rows:", nrow(atc), "\n",
#     "Molecules kept:", nrow(molecules), "->", basename(out_file_molecules_canonical), "\n",
#     "Excluded rows:", nrow(excluded), "->", basename(out_file_excluded_canonical), "\n")
