test_that("the module writes the four tables, the national summary and report.html with sortable tables and figures", {
  skip_if_not_installed("SpaDES.core")
  root <- withr::local_tempdir()
  runs <- toyRuns(root)
  anova <- file.path(root, "anova_terms.csv"); utils::write.csv(data.frame(term = c("arm", "ELF"), F = c(3.1, 8.2)), anova, row.names = FALSE)
  mp <- file.path(root, "modules"); dir.create(mp)
  file.symlink(normalizePath(testthat::test_path("..", "..")), file.path(mp, "fireSense_report"))
  withr::local_options(spades.useRequire = FALSE, spades.moduleCodeChecks = FALSE)
  out <- file.path(root, "outputs")
  sim <- SpaDES.core::simInitAndSpades(
    times = list(start = 0, end = 1), modules = "fireSense_report",
    params = list(fireSense_report = list(runs = runs, anovaFile = anova, .plots = "png", pageTitle = "Toy report")),
    paths = list(modulePath = mp, outputPath = out, inputPath = file.path(root, "inputs"), cachePath = file.path(root, "cache"),
                 scratchPath = file.path(root, "scratch")))
  for (n in c("parameterSets", "inSample", "hindcast", "hindcastYE", "nationalSummary"))
    expect_true(file.exists(file.path(out, paste0(n, ".csv"))), info = n)
  expect_true(all(c("parameterSets", "inSample", "hindcast", "hindcastYE", "nationalSummary") %in% names(sim$reportTables)))
  csv <- utils::read.csv(file.path(out, "inSample.csv"))
  expect_true(all(c("arm", "role", "ELF") %in% names(csv)))
  expect_setequal(unique(csv$ELF), c("1.1", "2.2", "3.3"))
  figs <- basename(list.files(file.path(out, "figures"), pattern = "png$", recursive = TRUE))  # figurePath() inside the event: figures/<module>
  expect_true(all(c("yearTotals_best_1_1.png", "spread_best_3_3.png", "bias_best_all.png", "yearTotals_alt_2_2.png") %in% figs))
  expect_false("spread_alt_1_1.png" %in% figs)
  html <- paste(readLines(file.path(out, "report.html")), collapse = "\n")
  expect_match(html, "Toy report"); expect_match(html, "Main text"); expect_match(html, "Appendix")
  expect_match(html, "data-v=\"0.25\"", fixed = TRUE); expect_match(html, "table class=sortable", fixed = TRUE)
  expect_match(html, "data:image/png;base64,", fixed = TRUE); expect_match(html, "ANOVA terms", fixed = TRUE)
  expect_match(html, "no inSampleSizes_spread", fixed = TRUE)
  expect_match(html, "no bias rows: rescore with fireSimulator_validation", fixed = TRUE)
})
