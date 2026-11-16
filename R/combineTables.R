## Putting the arms' tables together, and the summaries over ELFs.

#' Stack one table over the arms
#'
#' @param runs [readRuns()].
#' @param make `function(row)` returning the table of the arm in the row of `runs` (a `data.frame` with `ELF`), or `NULL`.
#' @return The tables with `arm` and `role` in front.
stackArms <- function(runs, make) {
  pieces <- lapply(seq_len(nrow(runs)), function(i) {
    t <- make(runs[i, ])
    if (is.null(t) || !nrow(t)) return(NULL)
    cbind(arm = runs$arm[i], role = runs$role[i], t, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

#' National summary: median and 2.5-97.5% range over ELFs
#'
#' Never a mean. The pooled rows (`ELF` `"all"`) are left out: they are not an ELF.
#'
#' @param tab A table with `arm`, `role`, `ELF` and the columns in `by`.
#' @param valueCol The column summarised.
#' @param by Columns that define a metric, e.g. `c("stage", "metric")`.
#' @return A `data.frame`: `arm`, `role`, the `by` columns, `nELF`, `median`, `lo`, `hi` (of the ELFs' values).
nationalSummary <- function(tab, valueCol = "value", by = "metric") {
  if (is.null(tab) || !nrow(tab)) return(NULL)
  tab <- tab[tab$ELF != "all" & is.finite(tab[[valueCol]]), , drop = FALSE]
  if (!nrow(tab)) return(NULL)
  key <- interaction(tab[c("arm", "role", by)], drop = TRUE, lex.order = TRUE)
  out <- do.call(rbind, lapply(split(seq_len(nrow(tab)), key), function(i) {
    r <- rangeOf(tab[[valueCol]][i])
    cbind(tab[i[1], c("arm", "role", by), drop = FALSE], nELF = nrow(tab[i, ]) , median = r$median, lo = r$lo, hi = r$hi)
  }))
  rownames(out) <- NULL
  out
}

#' The arms side by side
#'
#' @param tab A table with `arm`, `ELF` and the columns in `by`.
#' @param valueCol The column laid out, one column per arm.
#' @param by Columns that identify a row, e.g. `c("ELF", "stage", "metric")`.
#' @return A `data.frame` with the `by` columns and one column per arm.
sideBySide <- function(tab, valueCol = "value", by = c("ELF", "metric")) {
  if (is.null(tab) || !nrow(tab)) return(NULL)
  arms <- unique(tab$arm)
  key <- do.call(paste, c(tab[by], sep = "\r"))
  rows <- tab[!duplicated(key), by, drop = FALSE]
  for (a in arms) {
    t <- tab[tab$arm == a, ]
    rows[[a]] <- t[[valueCol]][match(do.call(paste, c(rows[by], sep = "\r")), do.call(paste, c(t[by], sep = "\r")))]
  }
  rownames(rows) <- NULL
  rows
}
