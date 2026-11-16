test_that("readRuns reads a csv or a data.frame and checks the columns, roles and arm labels", {
  root <- withr::local_tempdir()
  runs <- toyRuns(root)
  f <- file.path(root, "runs.csv"); utils::write.csv(runs, f, row.names = FALSE)
  expect_equal(readRuns(f)$arm, c("best", "alt"))
  expect_equal(readRuns(runs[, setdiff(names(runs), "ignitionLedgerYE")])$ignitionLedgerYE, c(NA_character_, NA_character_))
  expect_error(readRuns(runs[, -1]), "lacks the column")
  expect_error(readRuns(transform(runs, role = "other")), "role must be")
  expect_error(readRuns(transform(runs, arm = "x")), "unique")
  expect_equal(splitPaths("a; b"), c("a", "b"))
  expect_length(splitPaths(NA_character_), 0)
})

test_that("table 1 summarises each coefficient as median and 2.5-97.5% over the kept sets, and reads the year-effect sds", {
  root <- withr::local_tempdir()
  runs <- toyRuns(root)
  sp <- spreadParameterTable(readLedgerRows(runs$spreadLedger[1]))
  cmd <- sp[sp$ELF == "1.1" & sp$term == "CMD", ]
  expect_equal(cmd$median, 12 + 1); expect_equal(cmd$lo, unname(quantile(11:15, 0.025))); expect_equal(cmd$hi, unname(quantile(11:15, 0.975)))
  expect_equal(cmd$n, 5L); expect_equal(cmd$link, "powerLawSmooth")
  expect_equal(sp$median[sp$ELF == "2.2" & sp$term == "objFunVal"], 503)
  expect_equal(sp$lo[sp$ELF == "2.2" & sp$term == "powerLawGamma_spread"], 3)
  ig <- ignitionParameterTable(readLedgerRows(runs$ignitionLedger[1]), readLedgerRows(runs$ignitionLedgerYE[1]))
  expect_equal(ig$median[ig$ELF == "3.3" & ig$source == "ignition" & ig$term == "yearSD"], 0.5)
  expect_equal(ig$median[ig$ELF == "3.3" & ig$source == "escape" & ig$term == "priorSD"], 0.1)
  noPrior <- ignitionParameterTable(readLedgerRows(runs$ignitionLedger[1]))
  expect_true(all(is.na(noPrior$median[noPrior$term == "priorSD"])))
  expect_equal(nrow(spreadParameterTable(readLedgerRows(runs$spreadLedger[1], "2.2"))) > 0, TRUE)
})

test_that("table 2: ignition and escape totals give the bias ratio and the Spearman of the years; spread reads inSampleSizes_spread or gives NA rows", {
  root <- withr::local_tempdir()
  runs <- toyRuns(root)
  ign <- readLedgerRows(runs$ignitionLedger[1])
  totals <- yearTotalsTable(ign)
  expect_equal(sort(unique(totals$process)), c("escape", "ignition"))
  fit <- totalsFitTable(totals)
  d <- totals[totals$ELF == "1.1" & totals$process == "ignition", ]
  expect_equal(fit$value[fit$ELF == "1.1" & fit$stage == "ignition" & fit$metric == "totalRatio"], sum(d$predicted) / sum(d$observed))
  expect_equal(fit$value[fit$ELF == "1.1" & fit$stage == "ignition" & fit$metric == "spearman"], cor(d$observed, d$predicted, method = "spearman"))
  expect_equal(fit$value[fit$ELF == "1.1" & fit$stage == "ignition" & fit$metric == "nYears"], 10)
  withSizes <- spreadFitTable(inSampleSizesOf(readLedgerRows(runs$spreadLedger[1])))
  expect_equal(withSizes$value[withSizes$ELF == "2.2" & withSizes$metric == "fireBias"], 0.03)
  expect_false(any(grepl("^q(Obs|Sim)_", withSizes$metric)))
  noSizes <- spreadFitTable(inSampleSizesOf(readLedgerRows(runs$spreadLedger[2])))
  expect_equal(nrow(noSizes), 2); expect_true(all(is.na(noSizes$value))); expect_match(noSizes$note[1], "inSampleSizes_spread")
})

test_that("tables 3 and 4: skill rows, bias from the gates (with single-run ranges) or test B, several score folders, no folder", {
  root <- withr::local_tempdir()
  runs <- toyRuns(root)
  h <- hindcastTable(readScoreDirs(runs$scoreDir[1]))                      # two folders: pooled rows dropped
  expect_setequal(unique(h$ELF), c("1.1", "2.2", "3.3"))
  expect_equal(h$value[h$ELF == "3.3" & h$metric == "skill:S1:N0"], 0.2)
  r <- h[h$ELF == "2.2" & h$metric == "ratio area (G2)", ]
  expect_equal(c(r$value, r$lo, r$hi), c(1.0, 0.7, 1.5))
  expect_match(r$source, "derived from gates")
  expect_equal(h$value[h$ELF == "1.1" & h$metric == "ratio escapeRate (G6)"], 0.9)
  expect_equal(h$hi[h$ELF == "1.1" & h$metric == "ratio escapeRate (G6)"], 1.2)
  expect_equal(h$lo[h$ELF == "1.1" & h$metric == "ratio areaPerEscape (G6)"], 1.0)
  one <- hindcastTable(readScoreDirs(runs$scoreDirYE[1]))                  # one folder: pooled rows kept
  expect_true("all" %in% one$ELF)
  b <- hindcastTable(readScoreDirs(runs$scoreDir[2]))                      # test B present: used instead of the derived rows
  expect_true(all(b$source[grepl("^B:", b$metric)] == "wave1_results.csv"))
  expect_false(any(grepl("^ratio", b$metric)))
  expect_equal(nrow(hindcastTable(readScoreDirs(NA_character_))), 0)
  expect_setequal(unique(hindcastTable(readScoreDirs(runs$scoreDir[1]), "1.1")$ELF), "1.1")
})

test_that("the national summary is the median and 2.5-97.5% range over ELFs, never a mean; arms lay out side by side", {
  root <- withr::local_tempdir()
  tables <- buildReportTables(readRuns(toyRuns(root)))
  ns <- tables$nationalSummary
  best <- tables$hindcast[tables$hindcast$arm == "best" & tables$hindcast$metric == "ratio area (G2)" & tables$hindcast$ELF != "all", ]
  s <- ns[ns$arm == "best" & ns$table == "hindcast" & ns$metric == "ratio area (G2)", ]
  expect_equal(s$nELF, 3); expect_equal(s$median, median(best$value)); expect_equal(s$lo, unname(quantile(best$value, 0.025)))
  expect_equal(s$hi, unname(quantile(best$value, 0.975)))
  expect_false(any(ns$ELF == "all"))
  expect_true(all(c("hindcast", "hindcastYE", "inSample", "parameterSets") %in% ns$table))
  expect_true(all(c("arm", "role", "ELF") %in% names(tables$inSample)))
  expect_equal(tables$parameterSets$median[tables$parameterSets$arm == "best" & tables$parameterSets$ELF == "1.1" & tables$parameterSets$term == "priorSD" &
                                            tables$parameterSets$source == "ignition"], 0.25)   # from ignitionLedgerYE
  sbs <- sideBySide(tables$hindcast[tables$hindcast$metric == "skill:S1:N0", ], "value", c("ELF", "metric"))
  expect_true(all(c("best", "alt") %in% names(sbs)))
  expect_true(is.na(sbs$alt[sbs$ELF == "3.3"])); expect_false(is.na(sbs$best[sbs$ELF == "3.3"]))
  expect_setequal(unique(buildReportTables(readRuns(toyRuns(withr::local_tempdir())), "1.1")$inSample$ELF), "1.1")
})

test_that("figures: the ignition/escape and spread panels draw, the spread panel is skipped for a ledger without the tables", {
  root <- withr::local_tempdir()
  tables <- buildReportTables(readRuns(toyRuns(root)))
  jobs <- figureJobs(tables)
  kinds <- vapply(jobs, `[[`, "", "kind"); arms <- vapply(jobs, `[[`, "", "arm")
  expect_equal(sum(kinds == "yearTotals" & arms == "best"), 3); expect_equal(sum(kinds == "spread" & arms == "best"), 3)
  expect_equal(sum(kinds == "spread" & arms == "alt"), 0); expect_equal(sum(kinds == "bias"), 2)
  for (j in jobs) expect_s3_class(j$fn(j$data), "gg")
})
