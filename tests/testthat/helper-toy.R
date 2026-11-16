## Plain functions in R/ are sourced as simInit() would; toy ledgers and score folders are made in a temp dir.
Sys.setenv(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1")
data.table::setDTthreads(1)
suppressMessages({library(ggplot2); library(patchwork)})
for (f in list.files(testthat::test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

## a one-row scoreFireSizes() table and a per-year table, as the spread ledger's inSampleSizes_spread holds them
toyInSample <- function(seed) {
  set.seed(seed)
  q <- sprintf("q%02d", c(1, 10, 25, 50, 75, 90, 99)); obs <- sort(exp(rnorm(7, 3, 1)))
  sizes <- data.frame(years = 20, fires = 100, fireBias = 0.01 * seed, yearSpearman = 0.8, totalAreaRatio = 1 + 0.01 * seed)
  sizes[paste0("qObs_", q)] <- as.list(obs); sizes[paste0("qSim_", q)] <- as.list(obs * exp(rnorm(7, 0, 0.1)))
  area <- exp(rnorm(20, 7, 1))
  list(sizes = sizes, byYear = data.frame(yr = 1:20, obs = area, simMedian = area * exp(rnorm(20, 0, 0.1)),
                                          simLo = area * 0.7, simHi = area * 1.4))
}

## a national spread ledger: one row per ELF; `inSample` FALSE leaves the inSampleSizes_spread column out
toySpreadLedger <- function(file, elfs, inSample = TRUE, seed = 1) {
  set.seed(seed)
  x <- data.frame(polygonID = elfs, spreadLink = "powerLawSmooth", powerLawGamma_spread = 3, lowerAsymptote_spread = 0.13)
  x$params <- lapply(seq_along(elfs), function(i) data.frame(maxAsymptote = 0.25 + 0.001 * (1:5), CMD = c(10, 11, 12, 13, 14) + i, youngAge = -(1:5)))
  x$objFunVal <- lapply(seq_along(elfs), function(i) 500 + (1:5))
  if (inSample) x$inSampleSizes_spread <- lapply(seq_along(elfs), function(i) toyInSample(i + seed))
  saveRDS(x, file)
  file
}

## a national ignition ledger; `priorSD` FALSE leaves out modelList$yearEffects
toyIgnitionLedger <- function(file, elfs, priorSD = FALSE, seed = 1) {
  set.seed(seed)
  fitted <- function(i, sd) {
    obs <- rpois(10, 20); pred <- obs * exp(rnorm(10, 0, 0.3))
    ml <- list(model = list(yearTotals = data.frame(year = seq(-1, 1, length.out = 10), observed = obs, predicted = pred)), yearSD = sd)
    if (priorSD) ml$yearEffects <- data.frame(year = 1:10, priorSD = sd / 2)
    list(modelList = ml)
  }
  x <- data.frame(polygonID = elfs)
  x$fireSense_IgnitionFitted <- lapply(seq_along(elfs), fitted, sd = 0.5)
  x$fireSense_EscapeFitted <- lapply(seq_along(elfs), fitted, sd = 0.2)
  saveRDS(x, file)
  file
}

## a score folder: wave1_results.csv (+ wave1.rds with gates); `elfs` are scored in it, `bias` adds test "B" rows
toyScoreDir <- function(dir, elfs, bias = FALSE, pooled = TRUE) {
  dir.create(dir, recursive = TRUE)
  res <- do.call(rbind, lapply(c(if (pooled) "all", elfs), function(e)
    data.frame(test = "S1", null = c("N0", "N1"), level = if (e == "all") "pooled" else "ELF", ELF = e, measure = "skill",
               value = if (e == "all") c(0.3, 0.2) else c(0.1, 0.05) * (match(e, elfs) + 1), lo = NA_real_, hi = NA_real_,
               nELF = 1L, nUnits = 10L, verdict = NA_character_)))
  if (bias) res <- rbind(res, data.frame(test = "B", null = NA, level = "ELF", ELF = elfs, measure = "ratioArea", value = 1.1, lo = 0.9, hi = 1.4,
                                         nELF = 1L, nUnits = 10L, verdict = NA_character_))
  utils::write.csv(res, file.path(dir, "wave1_results.csv"), row.names = FALSE)
  link <- function(k, v) sprintf("%s %s (median-run 95%%: 0.5-1.5; single runs 95%%: %s-%s)", k, v, v - 0.2, v + 0.3)
  gates <- do.call(rbind, lapply(elfs, function(e) {
    i <- match(e, elfs)
    data.frame(ELF = e, gate = c("G2", "G4", "G6"), pass = c(TRUE, TRUE, TRUE), value = c(0.8 + 0.1 * i, 1.1, NA),
               detail = c("ratio", "ratio", paste(link("ignitions", 1), link("escapeRate", 0.9), link("areaPerEscape", 1.2), sep = "; ")),
               lo = NA_real_, hi = NA_real_, runLo = c(0.5 + 0.1 * i, 0.7, NA), runHi = c(1.3 + 0.1 * i, 1.6, NA))
  }))
  saveRDS(list(results = res, gates = gates), file.path(dir, "wave1.rds"))
  dir
}

## main arm "best": ELFs 1.1, 2.2, 3.3; appendix arm "alt": 1.1, 2.2 (no inSampleSizes_spread); returns the runs table
toyRuns <- function(root) {
  dir.create(root, showWarnings = FALSE)
  runs <- data.frame(
    arm = c("best", "alt"), role = c("main", "appendix"),
    spreadLedger = c(toySpreadLedger(file.path(root, "spreadBest.rds"), c("1.1", "2.2", "3.3"), seed = 1),
                     toySpreadLedger(file.path(root, "spreadAlt.rds"), c("1.1", "2.2"), inSample = FALSE, seed = 2)),
    ignitionLedger = c(toyIgnitionLedger(file.path(root, "ignBest.rds"), c("1.1", "2.2", "3.3"), seed = 1),
                       toyIgnitionLedger(file.path(root, "ignAlt.rds"), c("1.1", "2.2"), seed = 2)),
    ignitionLedgerYE = c(toyIgnitionLedger(file.path(root, "ignBestYE.rds"), c("1.1", "2.2", "3.3"), priorSD = TRUE, seed = 1), NA),
    scoreDir = c(paste(toyScoreDir(file.path(root, "scoreBestA"), c("1.1", "2.2")), toyScoreDir(file.path(root, "scoreBestB"), "3.3"), sep = ";"),
                 toyScoreDir(file.path(root, "scoreAlt"), c("1.1", "2.2"), bias = TRUE)),
    scoreDirYE = c(toyScoreDir(file.path(root, "scoreBestYE"), c("1.1", "2.2", "3.3")), NA),
    stringsAsFactors = FALSE)
  runs
}
