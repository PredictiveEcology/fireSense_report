## Tables 1 and 2 from the fit ledgers: parameter sets and in-sample (out-of-fold, for ignition and escape) fit.

#' Median and 2.5-97.5% range of a vector
#'
#' @param x Numeric.
#' @return A one-row `data.frame`: `median`, `lo` (2.5%), `hi` (97.5%), `n` (non-missing values); `NA` when none.
rangeOf <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(data.frame(median = NA_real_, lo = NA_real_, hi = NA_real_, n = 0L))
  q <- stats::quantile(x, c(0.025, 0.5, 0.975), names = FALSE)
  data.frame(median = q[2], lo = q[1], hi = q[3], n = length(x))
}

#' Read a national ledger
#'
#' @param path A ledger `.rds`: a `data.frame` with one row per ELF (`polygonID`).
#' @param ELFs ELFs to keep; empty for all.
#' @return The ledger rows.
readLedgerRows <- function(path, ELFs = character(0)) {
  if (!file.exists(path)) stop("ledger not found: ", path)
  x <- readRDS(path)
  x <- as.data.frame(x)[, setdiff(names(x), "geometry"), drop = FALSE]
  x$polygonID <- as.character(x$polygonID)
  if (anyDuplicated(x$polygonID)) stop(basename(path), " has more than one row for an ELF")
  if (length(ELFs)) x <- x[x$polygonID %in% ELFs, , drop = FALSE]
  x
}

#' Table 1, spread part: each coefficient over the kept parameter sets
#'
#' @param ledger A spread ledger from [readLedgerRows()]. Its `params` column holds a `data.table` per ELF, one row
#'   per kept parameter set; `objFunVal` a vector of the sets' objective values; scalar columns ending in `_spread`
#'   (power-law and asymptote settings) and `spreadLink` are the fit's link.
#' @return A `data.frame`: `ELF`, `source` (`"spread"`), `term`, `median`, `lo`, `hi` (2.5-97.5% over the sets), `n`
#'   (sets), `link` (the spread link). The ledger's scalar settings have one value, so `lo` and `hi` equal it.
spreadParameterTable <- function(ledger) {
  scalars <- names(ledger)[vapply(ledger, function(v) is.numeric(v) && !is.list(v), TRUE) & grepl("_spread$", names(ledger))]
  rows <- lapply(seq_len(nrow(ledger)), function(i) {
    p <- as.data.frame(ledger$params[[i]])
    p <- p[, vapply(p, is.numeric, TRUE), drop = FALSE]
    vals <- c(as.list(p), list(objFunVal = as.numeric(unlist(ledger$objFunVal[[i]]))),
              stats::setNames(lapply(scalars, function(s) ledger[[s]][i]), scalars))
    do.call(rbind, Map(function(term, v) cbind(term = term, rangeOf(v)), names(vals), vals))
  })
  out <- do.call(rbind, Map(function(r, elf, link) cbind(ELF = elf, source = "spread", r, link = link),
                            rows, ledger$polygonID, if ("spreadLink" %in% names(ledger)) ledger$spreadLink else NA_character_))
  rownames(out) <- NULL
  out
}

## the fitted model of one process (`fireSenseUtils::ignitionFitAdditionalColNamesTxt`: ignition, escape) of ledger row i
processModelList <- function(ledger, i, process) {
  col <- stats::setNames(fireSenseUtils::ignitionFitAdditionalColNamesTxt, c("ignition", "escape"))[[process]]
  ledger[[col]][[i]]$modelList
}

#' The year-effect prior sd of ignitions and escapes
#'
#' @param ledgerFile An ignition ledger with fitted year effects (`modelList$yearEffects`), read with
#'   `fireSenseUtils::readYearEffects()`.
#' @return A `data.frame`: `ELF`, `process`, `priorSD` (constant over the years of an ELF); `NULL` when the ledger has no
#'   year effects (an older fit), with a message.
priorSDTable <- function(ledgerFile) {
  ye <- tryCatch(fireSenseUtils::readYearEffects(ledgerFile),
                 error = function(e) { message(basename(ledgerFile), ": no year effects (", conditionMessage(e), ")"); NULL })
  if (is.null(ye)) return(NULL)
  ye <- ye[!duplicated(ye[c("ELF", "process")]), ]
  data.frame(ELF = ye$ELF, process = ye$process, priorSD = ye$priorSD)
}

#' Table 1, ignition part: the sd of the year effect of ignitions and escapes
#'
#' @param ledger An ignition ledger from [readLedgerRows()].
#' @param priorSD [priorSDTable()] or `NULL` (no year effects: the prior sd is `NA`).
#' @return A `data.frame`: `ELF`, `source` (`"ignition"`, `"escape"`), `term` (`"yearSD"`: out-of-year sd of the
#'   year effect that a hindcast draws; `"priorSD"`: the sd of the prior of the fitted year effects), `median`
#'   (the value; `NA` when the ledger does not have it), `lo`, `hi` (`NA`), `n` (1 or 0), `link` (`NA`).
ignitionParameterTable <- function(ledger, priorSD = NULL) {
  value <- function(x) if (length(x) && is.finite(x[1])) as.numeric(x[1]) else NA_real_
  rows <- lapply(seq_len(nrow(ledger)), function(i) do.call(rbind, lapply(c("ignition", "escape"), function(p) {
    yearSD <- value(processModelList(ledger, i, p)$yearSD)
    prior <- value(priorSD$priorSD[priorSD$ELF == ledger$polygonID[i] & priorSD$process == p])
    data.frame(ELF = ledger$polygonID[i], source = p, term = c("yearSD", "priorSD"), median = c(yearSD, prior),
               lo = NA_real_, hi = NA_real_, n = as.integer(is.finite(c(yearSD, prior))), link = NA_character_)
  })))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Observed and predicted totals per year of ignitions or escapes
#'
#' `fireSense_ignitionFit` keeps them in `modelList$model$yearTotals`: one row per year with `year` (as fitted, so
#' standardised), `observed` and `predicted` counts. Each row's prediction is out of fold: from a model that did not train
#' on it, and out of year when whole years can be held out (`yearBlockedFolds()`), so they are not in-sample
#' predictions in the strict sense.
#'
#' @param ledger An ignition ledger from [readLedgerRows()].
#' @return A `data.frame`: `ELF`, `process`, `year`, `observed`, `predicted`; no rows for an ELF without the table.
yearTotalsTable <- function(ledger) {
  rows <- lapply(seq_len(nrow(ledger)), function(i) do.call(rbind, lapply(c("ignition", "escape"), function(p) {
    yt <- processModelList(ledger, i, p)$model$yearTotals
    if (is.null(yt) || !nrow(yt)) return(NULL)
    data.frame(ELF = ledger$polygonID[i], process = p, year = yt$year, observed = yt$observed, predicted = yt$predicted)
  })))
  out <- do.call(rbind, rows)
  if (is.null(out)) out <- data.frame(ELF = character(), process = character(), year = numeric(), observed = numeric(), predicted = numeric())
  rownames(out) <- NULL
  out
}

#' Table 2, ignition and escape: the fit of the yearly totals
#'
#' No existing helper scores yearly counts (the skill score of fireSimulator_validation is an internal, ensemble CRPS
#' one), so the two numbers are computed here.
#'
#' @param totals [yearTotalsTable()].
#' @return A `data.frame`: `ELF`, `stage` (`"ignition"`, `"escape"`), `metric` (`totalRatio`: predicted over observed
#'   total, the bias; `spearman`: rank correlation of the years' observed and predicted counts; `nYears`), `value`, `note`.
totalsFitTable <- function(totals) {
  one <- function(d) {
    ok <- is.finite(d$observed) & is.finite(d$predicted)
    d <- d[ok, ]
    data.frame(ELF = d$ELF[1], stage = d$process[1], metric = c("totalRatio", "spearman", "nYears"),
               value = c(if (sum(d$observed) > 0) sum(d$predicted) / sum(d$observed) else NA_real_,
                         if (nrow(d) > 2L && stats::sd(d$observed) > 0 && stats::sd(d$predicted) > 0) stats::cor(d$observed, d$predicted, method = "spearman") else NA_real_,
                         nrow(d)),
               note = "out-of-fold totals (out of year when whole years can be held out)")
  }
  pieces <- split(totals, list(totals$ELF, totals$process), drop = TRUE)
  out <- do.call(rbind, unname(lapply(pieces, one)))
  if (is.null(out)) out <- data.frame(ELF = character(), stage = character(), metric = character(), value = numeric(), note = character())
  rownames(out) <- NULL
  out
}

#' The in-sample fire-size tables of a spread ledger
#'
#' @param ledger A spread ledger from [readLedgerRows()].
#' @return A named list (names are ELFs): `list(sizes =` the one-row `fireSenseUtils::scoreFireSizes()` table, `byYear =`
#'   the per-year table`)`, or `NULL` for an ELF whose ledger row has none (or a ledger without the column).
inSampleSizesOf <- function(ledger) {
  col <- fireSenseUtils::spreadFitInSampleSizesTxt
  stats::setNames(lapply(seq_len(nrow(ledger)), function(i) if (col %in% names(ledger)) ledger[[col]][[i]]),
                  ledger$polygonID)
}

#' Table 2, spread: the fit of the fitted fires' sizes and annual area
#'
#' @param inSample [inSampleSizesOf()].
#' @return A `data.frame`: `ELF`, `stage` (`"spread"`), `metric` (every single-valued column of the one-row
#'   `scoreFireSizes()` table, except the size quantiles), `value`, `note`. An ELF without the table gets one row with
#'   `NA` metric and value and a note.
spreadFitTable <- function(inSample) {
  do.call(rbind, unname(Map(function(x, elf) {
    if (is.null(x) || is.null(x$sizes))
      return(data.frame(ELF = elf, stage = "spread", metric = NA_character_, value = NA_real_,
                        note = paste0("no ", fireSenseUtils::spreadFitInSampleSizesTxt, " in the spread ledger")))
    s <- as.data.frame(x$sizes)
    s <- s[, !grepl("^q(Obs|Sim)_", names(s)), drop = FALSE]
    data.frame(ELF = elf, stage = "spread", metric = names(s), value = as.numeric(unlist(s[1, ])), note = "")
  }, inSample, names(inSample))))
}
