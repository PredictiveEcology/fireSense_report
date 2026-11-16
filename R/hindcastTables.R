## Tables 3 and 4: hindcast skill and bias from fireSimulator_validation score folders.

## the first fireSimulator_validation version whose scores have the bias rows (test "B"; PredictiveEcology/fireSimulator_validation#23, after 0.0.0.9018)
biasMinVersion <- "0.0.0.9019"

#' Read score folders
#'
#' A score folder (fireSimulator_validation's `outputPath`) has `wave1_results.csv`: skill rows and, from the version
#' of [biasMinVersion], the bias rows (test `"B"`). Several folders, for a score that covers a few ELFs each, are
#' combined; the pooled rows (`ELF` `"all"`) of one folder do not describe the others and are then dropped.
#'
#' @param scoreDir Folders separated by `";"`, as in [readRuns()]; `NA` or `""` for none.
#' @return The rows of the `wave1_results.csv` of the folders, or `NULL` when there is none. A folder without the file is
#'   skipped with a message.
readScoreDirs <- function(scoreDir) {
  dirs <- splitPaths(scoreDir)
  res <- list()
  for (d in dirs) {
    f <- file.path(d, "wave1_results.csv")
    if (!file.exists(f)) { message("no wave1_results.csv in ", d); next }
    res[[d]] <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = c(ELF = "character"))
  }
  results <- if (length(res)) do.call(rbind, unname(res))
  if (length(res) > 1L) results <- results[results$ELF != "all", , drop = FALSE]
  results
}

#' Table 3 or 4: hindcast skill and bias per ELF
#'
#' Rows: the skill rows of `wave1_results.csv` (`skill:<test>:<null>`) and its bias rows, test `"B"`, one per quantity
#' (`null`) and measure (`ratio`: simulated median over observed; `log2ratio`), as `B:<quantity>:<measure>`; `lo` and `hi`
#' are the 2.5-97.5% range of single runs (ELF rows) or over ELFs (the pooled row). A score without bias rows (made by
#' an older fireSimulator_validation) gets one `B` row per ELF with `NA` values and a `note`; the bias is not derived
#' from the gates.
#'
#' @param scores [readScoreDirs()].
#' @param ELFs ELFs to keep; empty for all (the pooled `"all"` rows are kept with them).
#' @return A `data.frame`: `ELF`, `metric`, `value`, `lo`, `hi`, `source` (`"wave1_results.csv"`), `note`; no rows
#'   without a score.
hindcastTable <- function(scores, ELFs = character(0)) {
  r <- scores
  if (is.null(r)) return(data.frame(ELF = character(), metric = character(), value = numeric(), lo = numeric(), hi = numeric(),
                                    source = character(), note = character()))
  rows <- function(d, metric, note = "") data.frame(ELF = d$ELF, metric = metric, value = d$value, lo = d$lo, hi = d$hi,
                                                    source = "wave1_results.csv", note = note)
  sk <- r[r$measure == "skill", , drop = FALSE]
  b <- r[r$test == "B", , drop = FALSE]
  bias <- if (nrow(b)) rows(b, paste("B", b$null, b$measure, sep = ":")) else {
    e <- unique(r$ELF[r$ELF != "all"])
    data.frame(ELF = e, metric = "B", value = NA_real_, lo = NA_real_, hi = NA_real_, source = "wave1_results.csv",
               note = paste("no bias rows: rescore with fireSimulator_validation >=", biasMinVersion))
  }
  tab <- rbind(rows(sk, paste("skill", sk$test, sk$null, sep = ":")), bias)
  if (length(ELFs)) tab <- tab[tab$ELF %in% c(ELFs, "all"), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}
