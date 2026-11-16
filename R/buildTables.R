## The report's tables, from the `runs` table.

## the tables of one arm: the four numbered tables, and the data the figures draw
armTables <- function(row, ELFs = character(0)) {
  spread <- readLedgerRows(row$spreadLedger, ELFs)
  ign <- readLedgerRows(row$ignitionLedger, ELFs)
  ignYE <- if (!is.na(row$ignitionLedgerYE)) readLedgerRows(row$ignitionLedgerYE, ELFs)
  totals <- yearTotalsTable(ign)
  inSample <- inSampleSizesOf(spread)
  list(parameterSets = rbind(spreadParameterTable(spread), ignitionParameterTable(ign, ignYE)),
       inSample = rbind(totalsFitTable(totals), spreadFitTable(inSample)),
       hindcast = hindcastTable(readScoreDirs(row$scoreDir), ELFs),
       hindcastYE = hindcastTable(readScoreDirs(row$scoreDirYE), ELFs),
       yearTotals = totals, spreadSizes = inSample)
}

#' All the report's tables
#'
#' @param runs [readRuns()]. @param ELFs ELFs to keep; empty for all.
#' @return A list: `runs`; `parameterSets` (table 1: `arm`, `role`, `ELF`, `source`, `term`, `median`, `lo`, `hi`, `n`, `link`);
#'   `inSample` (table 2: `arm`, `role`, `ELF`, `stage`, `metric`, `value`, `note`); `hindcast` and `hindcastYE` (tables 3 and 4:
#'   `arm`, `role`, `ELF`, `metric`, `value`, `lo`, `hi`, `source`, `pass`); `nationalSummary` (`arm`, `role`, `table`, `metric`,
#'   `nELF`, `median`, `lo`, `hi`: the median over ELFs and their 2.5-97.5% range); `yearTotals` (per year totals, for the
#'   figures) and `spreadSizes` (per arm, per ELF, the in-sample fire-size tables).
buildReportTables <- function(runs, ELFs = character(0)) {
  per <- stats::setNames(lapply(seq_len(nrow(runs)), function(i) armTables(runs[i, ], ELFs)), runs$arm)
  bind <- function(name) {
    out <- do.call(rbind, lapply(names(per), function(a) {
      t <- per[[a]][[name]]
      if (is.null(t) || !nrow(t)) return(NULL)
      cbind(arm = a, role = runs$role[runs$arm == a], t, stringsAsFactors = FALSE)
    }))
    rownames(out) <- NULL
    out
  }
  tables <- stats::setNames(lapply(c("parameterSets", "inSample", "hindcast", "hindcastYE", "yearTotals"), bind),
                            c("parameterSets", "inSample", "hindcast", "hindcastYE", "yearTotals"))
  tables$spreadSizes <- lapply(per, `[[`, "spreadSizes")
  tables$runs <- runs
  tables$nationalSummary <- nationalOfTables(tables)
  tables
}

## the national summary of every table, as one table: the stage-level metrics, the hindcast metrics and the year-effect sds
nationalOfTables <- function(tables) {
  long <- function(tab, table, metric, value) if (!is.null(tab) && nrow(tab))
    data.frame(arm = tab$arm, role = tab$role, ELF = tab$ELF, table = table, metric = metric(tab), value = value(tab), stringsAsFactors = FALSE)
  ps <- tables$parameterSets[tables$parameterSets$source != "spread", ]
  long <- rbind(long(tables$inSample, "inSample", function(t) paste(t$stage, t$metric), function(t) t$value),
                long(tables$hindcast, "hindcast", function(t) t$metric, function(t) t$value),
                long(tables$hindcastYE, "hindcastYE", function(t) t$metric, function(t) t$value),
                long(ps, "parameterSets", function(t) paste(t$source, t$term), function(t) t$median))
  nationalSummary(long, "value", c("table", "metric"))
}

#' What to draw
#'
#' @param tables [buildReportTables()]. @return A list of jobs, each `list(name, data, fn, size, arm, role, ELF, kind)`;
#'   `name` is the file stem. The spread figure is left out for an ELF without `inSampleSizes_spread` tables.
figureJobs <- function(tables) {
  jobs <- list()
  add <- function(arm, role, elf, kind, data, fn) jobs[[length(jobs) + 1L]] <<-
    list(name = fileStem(kind, arm, elf), data = data, fn = fn, size = figureSizes[[kind]], arm = arm, role = role, ELF = elf, kind = kind)
  yt <- tables$yearTotals
  for (i in seq_len(nrow(tables$runs))) {
    arm <- tables$runs$arm[i]; role <- tables$runs$role[i]
    t <- yt[yt$arm == arm, ]
    for (elf in unique(t$ELF))
      add(arm, role, elf, "yearTotals", list(totals = t[t$ELF == elf, ], title = paste("Arm", arm, "ELF", elf, ": yearly totals, observed and predicted")), plotYearTotals)
    for (elf in names(tables$spreadSizes[[arm]])) {
      s <- tables$spreadSizes[[arm]][[elf]]
      if (!is.null(s$sizes) && !is.null(s$byYear))
        add(arm, role, elf, "spread", list(sizes = s$sizes, byYear = s$byYear, title = paste("Arm", arm, "ELF", elf, ": fitted fires, observed and simulated")), plotSpreadInSample)
    }
    h <- tables$hindcast[tables$hindcast$arm == arm & grepl("^(ratio|B:)", tables$hindcast$metric) & tables$hindcast$ELF != "all", ]
    if (nrow(h)) add(arm, role, "all", "bias", list(bias = h, title = paste("Arm", arm, ": hindcast bias by ELF")), plotBiasByELF)
  }
  jobs
}
