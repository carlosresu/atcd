# AGENT INSTRUCTIONS (atcd submodule)

These apply whenever you edit the `dependencies/atcd` submodule.

1. **Polars-first everywhere.** Keep all tabular work in r-polars (no new dplyr/data.table/tibble/readr). When you touch any script, prefer moving remaining base fallbacks into polars using the latest API (e.g., list/str expressions) to shrink non-polars code over time.
2. **Track r-polars updates.** When polars releases new features that let us replace base glue (string/list handling, concat, sorting), adopt them in this submodule and refresh the README to reflect the new behavior.
3. **Run the trio after changes.** After edits, run `Rscript atcd.R`, `Rscript export.R`, and `Rscript filter.R` to ensure the scrape → export → split pipeline still works and writes to `./output/`.
4. **Document the migration.** If you move logic into polars or drop legacy deps, update docs/comments so future agents continue the polars migration.
5. **Respect submodule workflow.** Commit and push changes inside `dependencies/atcd` before updating the superproject pointer. Keep outputs under this submodule’s `output/` directory.***
