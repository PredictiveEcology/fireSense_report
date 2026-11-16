## Tables 3 and 4: hindcast skill and bias from fireSimulator_validation score folders.

#' Read score folders
#'
#' A score folder (fireSimulator_validation's `outputPath`) has `wave1_results.csv` (skill rows) and `wave1.rds`
#' (`$gates`, with the single-run ranges `runLo` and `runHi`). Several folders, for a score that covers a few ELFs
#' each, are combined; the pooled rows (`ELF` `"all"`) of one folder do not describe the others and are then dropped.
#'
#' @param scoreDir Folders separated by `";"`, as in [readRuns()]; `NA` or `""` for none.
#' @return A list: `results` (the rows of `wave1_results.csv`) and `gates` (the gates tables, `NULL` when no folder has
#'   `wave1.rds`); both `NULL` when there is no folder. A folder without the files is skipped with a message.
readScoreDirs <- function(scoreDir) {
  dirs <- splitPaths(scoreDir)
  res <- gat <- list()
  for (d in dirs) {
    f <- file.path(d, "wave1_results.csv")
    if (!file.exists(f)) { message("no wave1_results.csv in ", d); next }
    res[[d]] <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = c(ELF = "character"))
    g <- file.path(d, "wave1.rds")
    if (file.exists(g)) gat[[d]] <- as.data.frame(readRDS(g)$gates)
  }
  results <- if (length(res)) do.call(rbind, unname(res))
  if (length(res) > 1L) results <- results[results$ELF != "all", , drop = FALSE]
  list(results = results, gates = if (length(gat)) do.call(rbind, unname(gat)))
}

## the three links of gate G6 in its `detail`: "<link> <ratio> (median-run 95%: lo-hi; single runs 95%: lo-hi)"
g6Links <- c("ignitions", "escapeRate", "areaPerEscape")

#' The bias ratios of the gates, with the range of single runs
#'
#' Simulated over observed: G2 (area burned), G4 (median of the 10 largest fires) and G6's three links (ignitions,
#' escape rate, area per escape). `fireSimulator_validation::summariseGates()` does the same parsing but is internal to that
#' module, so the G6 links are read here.
#'
#' @param gates A `gates` table of fireSimulator_validation (`ELF`, `gate`, `pass`, `value`, `detail`, `runLo`, `runHi`).
#' @return A `data.frame`: `ELF`, `metric` (`ratio <what> (G<n>)`), `value`, `lo`, `hi` (the single runs' 2.5-97.5%
#'   range), `pass` (gates G2, G4; `NA` for the links).
gateRatios <- function(gates) {
  num <- "(?:NA|NaN|Inf|[0-9.]+(?:e[-+][0-9]+)?)"
  link <- function(detail, k) {
    m <- regmatches(detail, regexec(paste0(k, " (", num, ") \\(median-run 95%: ", num, "-", num, "; single runs 95%: (", num, ")-(", num, ")\\)"),
                                    detail, perl = TRUE))[[1]]
    if (length(m)) suppressWarnings(as.numeric(m[c(2, 3, 4)])) else rep(NA_real_, 3)
  }
  do.call(rbind, lapply(split(gates, factor(gates$ELF, unique(gates$ELF))), function(g) {
    one <- function(id, metric) { r <- g[g$gate == id, ]
      data.frame(ELF = g$ELF[1], metric = metric, value = r$value, lo = r$runLo, hi = r$runHi, pass = r$pass) }
    d6 <- g$detail[g$gate == "G6"]
    links <- do.call(rbind, lapply(g6Links, function(k) { v <- link(d6, k)
      data.frame(ELF = g$ELF[1], metric = paste0("ratio ", k, " (G6)"), value = v[1], lo = v[2], hi = v[3], pass = NA) }))
    rbind(one("G2", "ratio area (G2)"), one("G4", "ratio top10 median (G4)"), links)
  }))
}

#' Table 3 or 4: hindcast skill and bias per ELF
#'
#' Rows: the skill rows of `wave1_results.csv` (`skill:<test>:<null>`), the bias rows of test `"B"` when the score has
#' them (`B:<null>:<measure>`), else the bias ratios derived from the gates ([gateRatios()], labelled in `source`).
#'
#' @param scores [readScoreDirs()].
#' @param ELFs ELFs to keep; empty for all (the pooled `"all"` rows are kept with them).
#' @return A `data.frame`: `ELF`, `metric`, `value`, `lo`, `hi`, `source` (`"wave1_results.csv"` or
#'   `"derived from gates G2, G4, G6 (lo, hi: 2.5-97.5% of single runs)"`), `pass`; no rows without a score.
hindcastTable <- function(scores, ELFs = character(0)) {
  r <- scores$results
  out <- list()
  if (!is.null(r)) {
    sk <- r[r$measure == "skill", , drop = FALSE]
    out$skill <- data.frame(ELF = sk$ELF, metric = paste("skill", sk$test, sk$null, sep = ":"), value = sk$value, lo = sk$lo, hi = sk$hi,
                            source = "wave1_results.csv", pass = NA)
    b <- r[r$test == "B", , drop = FALSE]
    if (nrow(b))
      out$bias <- data.frame(ELF = b$ELF, metric = paste("B", b$null, b$measure, sep = ":"), value = b$value, lo = b$lo, hi = b$hi,
                             source = "wave1_results.csv", pass = NA)
  }
  if (is.null(out$bias) && !is.null(scores$gates))
    out$bias <- cbind(gateRatios(scores$gates), source = "derived from gates G2, G4, G6 (lo, hi: 2.5-97.5% of single runs)")[, c("ELF", "metric", "value", "lo", "hi", "source", "pass")]
  tab <- do.call(rbind, unname(out))
  if (is.null(tab)) return(data.frame(ELF = character(), metric = character(), value = numeric(), lo = numeric(), hi = numeric(),
                                      source = character(), pass = logical()))
  if (length(ELFs)) tab <- tab[tab$ELF %in% c(ELFs, "all"), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}
