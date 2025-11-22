# Header ----------------------------------------------------------------------------------------------------------
#' atcd.R
#' ---
#' Scrapes the ATC data from https://www.whocc.no/atc_ddd_index/.
#'
#' Original by Fabrício Kury; parallelized and hardened HTTP by ChatGPT
#' File started on 2020/3/20 5:08.  Margin column at 120 characters.
#'
##
# Globals ---------------------------------------------------------------------------------------------------------
pacman::p_load(rvest)
pacman::p_load(xml2)
pacman::p_load(future)
pacman::p_load(furrr)
pacman::p_load(memoise)
pacman::p_load(httr2)
pacman::p_load(polars)

# --- MODIFICATION START ---
# The following section makes the script's file paths robust and independent of the
# current working directory, ensuring it works when called from anywhere.

get_script_dir <- function() {
  #' Returns the directory of the currently running script.
  #' This makes the script location-independent.
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args)
  if (length(file_arg) > 0) {
    # Executed when running the script with Rscript
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

this_dir <- get_script_dir()

#' Ensure a directory exists relative to the script, creating parents as needed.
#'
#' @param ... Path components passed to file.path.
#' @return Normalized absolute directory path.
ensure_directory <- function(...) {
  # Uses file.path() and normalizePath() for robust, OS-agnostic path creation.
  dir_path <- normalizePath(do.call(file.path, list(...)), mustWork = FALSE)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  dir_path
}

# Paths are now relative to the script's own location.
out_dir <- ensure_directory(this_dir, "output")
rds_dir <- ensure_directory(out_dir, "rds")

# --- MODIFICATION END ---

.resolve_worker_count <- function() {
  env_workers <- Sys.getenv("ESOA_ATCD_WORKERS", unset = NA_character_)
  if (!is.na(env_workers)) {
    parsed <- suppressWarnings(as.integer(env_workers))
    if (!is.na(parsed) && parsed > 0) {
      return(parsed)
    }
  }
  cores <- tryCatch(parallel::detectCores(), error = function(...) NA_integer_)
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

options(expressions = 100000) # Allow deep recursion.

# Standardized column order for ATC data frames.
atc_columns <- c("atc_code", "atc_name", "ddd", "uom", "adm_r", "note")

# Ensure consistent columns and rownames for downstream binding.
normalize_atc_df <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  if (inherits(df, "polars_object")) {
    df <- as.data.frame(df)
  }
  if (!is.data.frame(df)) {
    stop("normalize_atc_df() expects a data.frame or polars data frame.")
  }
  if (nrow(df) == 0) {
    return(NULL)
  }
  missing_cols <- setdiff(atc_columns, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  df <- df[atc_columns]
  df$atc_code <- as.character(df$atc_code)
  df$atc_name <- as.character(df$atc_name)
  df$ddd <- suppressWarnings(as.numeric(df$ddd))
  df$uom <- as.character(df$uom)
  df$adm_r <- as.character(df$adm_r)
  df$note <- as.character(df$note)
  rownames(df) <- NULL
  df
}

# Convert a list of ATC frames into a single polars data frame.
bind_rows_pl <- function(xs) {
  xs <- Filter(Negate(is.null), xs)
  if (length(xs) == 0) {
    return(NULL)
  }
  xs <- lapply(xs, normalize_atc_df)
  xs <- Filter(Negate(is.null), xs)
  if (length(xs) == 0) {
    return(NULL)
  }
  pl_list <- lapply(xs, polars::as_polars_df)
  do.call(polars::pl$concat, pl_list)
}

wrapRDS <- function(var, exprs, by_name = FALSE, pass_val = FALSE, assign_val = TRUE) {
  #' This is a handy function to store variables between runs of the code and skip recreating them.
  #' It checks if an RDS file for var already exists in rds_dir. If it does, read it from there. If
  #' it does not, evaluates exprs and saves it to such RDS file.
  #' var: The object itself, unquoted, or a character vector containing its name.
  #' exprs: Expression to be evaluated if the RDS file doesn't already exist.
  #' by_name: If true, var is interpreted as a character vector with the object name.
  #' pass_val: If true, will return the object at the end.
  #' assign_val: If true, will assign the value of the object to its name in the calling envirmt.
  if (by_name) {
    varname <- var
  } else {
    varname <- deparse(substitute(var))
  }

  if (exists(varname, envir = parent.frame(n = 1))) {
    # message("Object '", varname, "' already exists.")
  } else {
    # MODIFIED: Use file.path for robust path construction.
    rds_file <- file.path(rds_dir, paste0(varname, ".rds"))
    if (file.exists(rds_file)) {
      # message("Reading '", varname, "' from file '", rds_file, "'... ")
      var_val <- readRDS(rds_file)
    } else {
      # Evaluate the expression in a temporary environment, akin to a function call.
      # message('Building ', varname, '.')
      var_val <- eval(substitute(exprs),
        envir = new.env(parent = parent.frame(n = 1))
      )
      # message(varname, " completed. Saving to file '", rds_file, "'... ")
      if (!dir.exists(rds_dir)) {
        dir.create(rds_dir, recursive = TRUE)
      }
      # Cache the newly computed object for future runs.
      saveRDS(var_val, rds_file)
    }
    if (assign_val) {
      assign(varname, var_val, envir = parent.frame(n = 1))
    }
  }

  if (pass_val || !assign_val) {
    var_val
  }
}

getRDS <- function(var, by_name = FALSE, pass_val = FALSE, assign_val = TRUE) {
  #' In connection to wrapRDS, this function only loads the RDS file, or raises an error if unable to.
  #' var: The object itself, unquoted, or a character vector containing its name.
  #' by_name: If true, var is interpreted as a character vector with the object name.
  #' pass_val: If true, will return the object at the end.
  #' assign_val: If true, will assign the value of the object to its name in the calling envirmt.
  if (by_name) {
    varname <- var
  } else {
    varname <- deparse(substitute(var))
  }

  if (exists(varname, envir = parent.frame(n = 1))) {
    # message("Object '", varname, "' already exists.")
    var_val <- get(varname, envir = parent.frame(n = 1))
  } else {
    # MODIFIED: Use file.path for robust path construction.
    rds_file <- file.path(rds_dir, paste0(varname, ".rds"))
    if (file.exists(rds_file)) {
      # message("Reading '", varname, "' from file '", rds_file, "'... ")
      var_val <- readRDS(rds_file)
    } else {
      stop(paste0("Unable to find file ", rds_file, "."))
    }

    if (assign_val) {
      assign(varname, var_val, envir = parent.frame(n = 1))
    }
  }

  if (pass_val || !assign_val) {
    var_val
  }
}

# Polite, memoised HTML fetcher with retries ----------------------------------------------------------------------
fetch_html <- local({
  agent <- sprintf("R/%s (WHO ATC scraper; contact: you@example.com)", getRversion())
  f <- function(url) {
    req <- httr2::request(url) |>
      httr2::req_user_agent(agent) |>
      httr2::req_timeout(30)
    # Try up to 3 times with exponential backoff on failure
    # Mirrors README summary: hardened layer with retries/timeouts + memoization.
    for (i in 0:2) {
      resp <- try(httr2::req_perform(req), silent = TRUE)
      if (!inherits(resp, "try-error")) {
        status <- httr2::resp_status(resp)
        if (status >= 200 && status < 300) {
          return(xml2::read_html(httr2::resp_body_string(resp)))
        }
      }
      Sys.sleep(2^i) # 1s, 2s, 4s
    }
    stop("Failed to fetch: ", url)
  }
  memoise::memoise(f) # in-process memoization
})

# The script will retrieve all ATC roots in atc_roots. Remember that for each root all subcodes will be retrieved.
# A   Alimentary tract and metabolism
# B   Blood and blood forming organs
# C   Cardiovascular system
# D   Dermatologicals
# G   Genito-urinary system and sex hormones
# H   Systemic hormonal preparations, excluding sex hormones and insulins
# J   Antiinfectives for systemic use
# L   Antineoplastic and immunomodulating agents
# M   Musculo-skeletal system
# N   Nervous system
# P   Antiparasitic products, insecticides and repellents
# R   Respiratory system
# S   Sensory organs
# V   Various
atc_roots <- c("A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R", "S", "V")

# Scrape data -----------------------------------------------------------------------------------------------------
#' Scrape WHO ATC data for a root code and all descendants.
#'
#' @param root_atc_code One- to five-character ATC code indicating the subtree to scrape.
#' @return Data frame containing ATC codes, names, DDDs, and related metadata.
scrape_who_atc <- function(root_atc_code) {
  # This function scrapes and returns a data frame with all data available from https://www.whocc.no/atc_ddd_index/ for the
  # given ATC code and all its subcodes.
  if (length(root_atc_code) != 1) {
    stop("scrape_who_atc() only accepts single objects, not vectors. Please provide a single valid ATC code as input.")
  }

  web_address <- paste0("https://www.whocc.no/atc_ddd_index/?code=", root_atc_code, "&showdescription=no")
  # message('Scraping ', web_address, '.')
  atc_code_length <- nchar(root_atc_code)

  # Hardened + memoised HTTP fetch (replaces read_html)
  html_data <- fetch_html(web_address)

  if (atc_code_length < 5) {
    scraped_strings <- html_data |>
      rvest::html_node(css = "#content > p:nth-of-type(2n)") |>
      rvest::html_text() |>
      strsplit("\n")
    scraped_strings <- scraped_strings[[1]]
    scraped_strings <- Filter(f = nchar, scraped_strings)

    if (length(scraped_strings) == 0) {
      return(NULL)
    }

    tval <- bind_rows_pl(lapply(scraped_strings, function(scraped_string) {
      atc_codes <- sub("^([A-Z]\\S*) (.*)", "\\1", scraped_string)
      atc_names <- sub("^([A-Z]\\S*) (.*)", "\\2", scraped_string)
      t1 <- data.frame(
        atc_code = atc_codes,
        atc_name = atc_names,
        ddd = NA_real_,
        uom = NA_character_,
        adm_r = NA_character_,
        note = NA_character_,
        stringsAsFactors = FALSE
      )
      t2 <- bind_rows_pl(lapply(atc_codes, scrape_who_atc))
      bind_rows_pl(list(t1, t2))
    }))

    # Add the root node if needed.
    if (atc_code_length == 1) {
      root_atc_code_name <- html_data |>
        rvest::html_nodes(css = "#content a")
      root_atc_code_name <- root_atc_code_name[3]
      root_atc_code_name <- rvest::html_text(root_atc_code_name)

      root_row <- data.frame(
        atc_code = root_atc_code,
        atc_name = root_atc_code_name,
        ddd = NA_real_,
        uom = NA_character_,
        adm_r = NA_character_,
        note = NA_character_,
        stringsAsFactors = FALSE
      )

      return(bind_rows_pl(list(root_row, tval)))
    } else {
      return(tval)
    }
  } else {
    proc_sdt <- function(sdt) {
      if (inherits(sdt, "xml_missing")) {
        return(NULL)
      }

      raw_tbl <- sdt |>
        rvest::html_table(header = TRUE)

      if (is.null(raw_tbl) || length(raw_tbl) == 0) {
        return(NULL)
      }

      retval <- as.data.frame(raw_tbl, stringsAsFactors = FALSE)
      names(retval) <- c("atc_code", "atc_name", "ddd", "uom", "adm_r", "note")

      retval[] <- lapply(retval, function(col) {
        if (is.character(col)) {
          col[col == ""] <- NA_character_
        }
        col
      })

      # The table on the website does not repeat atc_code and atc_name in subsequent rows when that ATC code has more
      # than one ddd/uom/adm_r. Let's fill-in the blanks when that is the case.
      if (nrow(retval) > 1) {
        for (i in 2:nrow(retval)) {
          if (is.na(retval$atc_code[i])) {
            # Inherit the previous non-missing code/name so each row is fully populated.
            retval$atc_code[i] <- retval$atc_code[i - 1]
            retval$atc_name[i] <- retval$atc_name[i - 1]
          }
        }
      }

      return(normalize_atc_df(retval))
    }

    retval <- rvest::html_node(html_data, xpath = "//ul/table") |>
      proc_sdt()

    return(retval)
  }
}

# Build each root tree in parallel (safe on Windows/macOS/Linux) ---------------------------------------------------
workers <- .configure_future_plan()
on.exit(try(future::plan(future::sequential), silent = TRUE), add = TRUE)

# Create/refresh per-root RDS files in parallel.
# Use future_walk since we only care about the side effect (writing RDS files)
# and do not need to collect the results, which prevents printing them.
  furrr::future_walk(
    atc_roots,
    ~ wrapRDS(
      var        = paste0("who_atc_", .x),
      exprs      = scrape_who_atc(.x),
      by_name    = TRUE,
      assign_val = FALSE, # assignment inside worker not needed; we rely on RDS output
      pass_val   = FALSE
    ),
    .progress = TRUE,
    .options = furrr::furrr_options(
      packages = c("xml2", "rvest", "httr2", "memoise")
    )
  )

# Optional: return to sequential plan
try(future::plan(future::sequential), silent = TRUE)

# Write results to storage ----------------------------------------------------------------------------------------
# Read the files produced by scrape_who_atc().
who_atc <- paste0("who_atc_", atc_roots) |>
  lapply(getRDS, by_name = TRUE, assign_val = FALSE, pass_val = TRUE) |>
  bind_rows_pl()

# Write Parquet as the primary output and CSV as a debugging aid.
out_parquet_name <- file.path(out_dir, paste0("WHO ATC-DDD ", format(Sys.Date(), "%Y-%m-%d"), ".parquet"))
out_csv_name <- sub("\\.parquet$", ".csv", out_parquet_name)
# Ensure we have a proper Polars DataFrame for output.
who_atc_pl <- if (inherits(who_atc, "polars_object")) who_atc else polars::as_polars_df(who_atc)
if (file.exists(out_parquet_name)) {
  # message('Warning: file already exists. Will be overwritten.')
}
who_atc_pl$write_parquet(out_parquet_name)
who_atc_pl$write_csv(out_csv_name)

# Finish execution ------------------------------------------------------------------------------------------------
# message('Script execution completed.')
