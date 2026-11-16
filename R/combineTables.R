## Putting the arms' tables together, and the summaries over ELFs.

#' National summary: median and 2.5-97.5% range over ELFs
#'
#' Never a mean. The pooled rows (`ELF` `"all"`) are left out: they are not an ELF. `nELF` counts every ELF of the arm
#' (any ELF in `tab`), and `nNA` those with no finite value for the metric (an NA row, or no row at all), so a missing
#' ELF is visible; the median and range are over the others.
#'
#' @param tab A table with `arm`, `role`, `ELF` and the columns in `by`.
#' @param valueCol The column summarised.
#' @param by Columns that define a metric, e.g. `c("table", "metric")`.
#' @return A `data.frame`: `arm`, `role`, the `by` columns, `nELF`, `nNA`, `median`, `lo`, `hi`.
nationalSummary <- function(tab, valueCol = "value", by = "metric") {
  if (is.null(tab) || !nrow(tab)) return(NULL)
  tab <- tab[tab$ELF != "all", , drop = FALSE]
  if (!nrow(tab)) return(NULL)
  elfsOfArm <- tapply(tab$ELF, tab$arm, function(e) length(unique(e)))
  key <- interaction(tab[c("arm", "role", by)], drop = TRUE, lex.order = TRUE)
  out <- do.call(rbind, lapply(split(seq_len(nrow(tab)), key), function(i) {
    v <- tab[[valueCol]][i]
    r <- rangeOf(v)
    n <- elfsOfArm[[tab$arm[i[1]]]]
    cbind(tab[i[1], c("arm", "role", by), drop = FALSE], nELF = n, nNA = n - length(unique(tab$ELF[i][is.finite(v)])),
          median = r$median, lo = r$lo, hi = r$hi)
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
