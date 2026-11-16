## Everything in this file and any files in the R directory are sourced during `simInit()`.
## The functions in R/ take plain tables; the module file hands them the inputs, draws through Plots() and saves the tables.
defineModule(sim, list(
  name = "fireSense_report",
  description = paste("Manuscript tables of fireSense fits and hindcast validation: parameter sets, in-sample fit per stage,",
                      "hindcast skill and bias with drawn and with fitted year effects; a main arm on all ELFs and appendix arms."),
  keywords = c("fire", "validation", "hindcast", "report"),
  authors = c(person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(fireSense_report = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list("README.md"),
  reqdPkgs = list("data.table", "ggplot2", "patchwork", "jsonlite",
                  "PredictiveEcology/SpaDES.core@development (>= 3.2.1.9037)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9173)"),
  parameters = bindrows(
    defineParameter("runs", "ANY", NA, NA, NA,
                    paste("A csv path or a data.frame, one row per arm: `arm` (label), `role` ('main' or 'appendix'), `spreadLedger` and `ignitionLedger`",
                          "(national ledgers, one row per ELF), `scoreDir` (fireSimulator_validation score folder, year effects drawn; several folders",
                          "separated by ';') and `scoreDirYE` (the same with fitted year effects; may be NA). Optional `ignitionLedgerYE`: an ignition",
                          "ledger of the arm with fitted year effects, where the year-effect prior sd is read.")),
    defineParameter("ELFs", "character", character(0), NA, NA, "ELFs to report; none: every ELF of the ledgers."),
    defineParameter("anovaFile", "character", NA_character_, NA, NA, "Path of `anova_terms.csv` of fireSimulator_validation's anova mode, shown in the appendix; NA: none."),
    defineParameter("pageTitle", "character", "FireSense fits and hindcast validation", NA, NA, "Title of report.html."),
    defineParameter(".plots", "character", "png", NA, NA, "Where the figures go, as for SpaDES.core::Plots(). The page shows the png files, so 'png' is needed for figures on it."),
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "When the figures are drawn on screen; NA for never.")
  ),
  inputObjects = bindrows(),
  outputObjects = bindrows(
    createsOutput("reportTables", "list", "The report's tables (parameterSets, inSample, hindcast, hindcastYE, nationalSummary, and the data of the figures). The four tables and the national summary are also written as csv to outputPath, and the page to report.html.")
  )
))

doEvent.fireSense_report <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      runs <- readRuns(P(sim)$runs)
      sim$reportTables <- buildReportTables(runs, P(sim)$ELFs)
      out <- outputPath(sim); dir.create(out, recursive = TRUE, showWarnings = FALSE)
      for (n in c("parameterSets", "inSample", "hindcast", "hindcastYE", "nationalSummary"))
        utils::write.csv(sim$reportTables[[n]], file.path(out, paste0(n, ".csv")), row.names = FALSE)
      jobs <- figureJobs(sim$reportTables)
      for (j in jobs) Plots(data = j$data, fn = j$fn, filename = j$name, ggsaveArgs = j$size)
      figs <- data.frame(arm = vapply(jobs, `[[`, "", "arm"), role = vapply(jobs, `[[`, "", "role"), ELF = vapply(jobs, `[[`, "", "ELF"),
                         kind = vapply(jobs, `[[`, "", "kind"), file = file.path(figurePath(sim), paste0(vapply(jobs, `[[`, "", "name"), ".png")),
                         stringsAsFactors = FALSE)
      anova <- if (!is.na(P(sim)$anovaFile)) utils::read.csv(P(sim)$anovaFile)
      buildReportPage(sim$reportTables, figs, file.path(out, "report.html"), title = P(sim)$pageTitle, anova = anova)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}
