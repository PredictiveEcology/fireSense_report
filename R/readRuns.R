## Reading the `runs` table: one row per arm.

## columns every `runs` row must have; `ignitionLedgerYE` is optional (the ledger whose year effects hold the prior sd)
runsColumns <- c("arm", "role", "spreadLedger", "ignitionLedger", "scoreDir", "scoreDirYE")
runsRoles <- c("main", "appendix")

#' Read and check the `runs` table
#'
#' @param runs A csv path or a `data.frame`, one row per arm: `arm` (label), `role` (`"main"` or `"appendix"`),
#'   `spreadLedger` and `ignitionLedger` (national ledgers, one row per ELF), `scoreDir` (a fireSimulator_validation score
#'   folder, year effects drawn) and `scoreDirYE` (the same with fitted year effects; may be `NA`). `scoreDir` and
#'   `scoreDirYE` may hold several folders separated by `";"`. The optional column `ignitionLedgerYE` is an ignition
#'   ledger of the same arm that has `modelList$yearEffects`; the year-effect prior sd is read from it when given.
#' @return A `data.frame` with the columns above (`ignitionLedgerYE` added as `NA` when absent).
readRuns <- function(runs) {
  if (is.character(runs) && length(runs) == 1L) {
    if (!file.exists(runs)) stop("runs: file not found: ", runs)
    runs <- utils::read.csv(runs, stringsAsFactors = FALSE)
  }
  runs <- as.data.frame(runs, stringsAsFactors = FALSE)
  lacking <- setdiff(runsColumns, names(runs))
  if (length(lacking)) stop("runs lacks the column(s): ", paste(lacking, collapse = ", "))
  if (!"ignitionLedgerYE" %in% names(runs)) runs$ignitionLedgerYE <- NA_character_
  if (anyDuplicated(runs$arm)) stop("runs: arm labels must be unique")
  if (!all(runs$role %in% runsRoles)) stop("runs: role must be one of ", paste(runsRoles, collapse = ", "))
  runs
}

#' The folders of a `scoreDir` entry
#'
#' @param x One string, folders separated by `";"`, or `NA`.
#' @return A character vector of folders (none for `NA` or `""`).
splitPaths <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(character(0))
  trimws(strsplit(x, ";", fixed = TRUE)[[1]])
}

#' A string safe as a figure file name
#'
#' `Plots()` takes the text after the last dot of a file name as its extension, so dots become underscores.
#'
#' @param ... Strings pasted with `"_"`.
#' @return One string of letters, digits and underscores.
fileStem <- function(...) gsub("[^A-Za-z0-9]", "_", paste(..., sep = "_"))
