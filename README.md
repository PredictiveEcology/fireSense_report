# fireSense_report

SpaDES module: manuscript tables of fireSense fits and hindcast validation (main arm on all ELFs; appendix arms).

One pipeline for two uses. The main text shows one arm with every ELF hindcast. The appendix compares several arms on a few ELFs. The per-arm tables are the same; the page lays them out by `role`.

## Input

Parameter `runs`: a csv path or a data.frame, one row per arm.

| column | meaning |
| --- | --- |
| `arm` | label |
| `role` | `main` or `appendix` |
| `spreadLedger`, `ignitionLedger` | national ledgers (`fireSenseParams_*.rds`, `fireSenseIgnitionParams_*.rds`), one row per ELF |
| `scoreDir` | fireSimulator_validation score folder, year effects drawn; several folders separated by `;` |
| `scoreDirYE` | the same with fitted year effects; may be `NA` |
| `ignitionLedgerYE` | optional: ignition ledger of the arm with `modelList$yearEffects`, where the year-effect prior sd is read |

Other parameters: `ELFs` (restrict), `anovaFile` (an `anova_terms.csv`, shown in the appendix), `pageTitle`, `.plots`.

## Output (in `outputPath`, and `sim$reportTables`)

1. `parameterSets.csv`: each coefficient as median and 2.5-97.5% over the kept sets; year-effect `yearSD` and `priorSD` of ignition and escape (read with `fireSenseUtils::readYearEffects()`).
2. `inSample.csv`: ignition and escape from the ledger's `modelList$model$yearTotals` (out-of-fold totals): total ratio, Spearman of the years; spread from the ledger column `inSampleSizes_spread`.
3. `hindcast.csv`: skill rows of `wave1_results.csv`; bias from the test B rows of the same file (fireSimulator_validation >= 0.0.0.9019); without them, NA rows with a note.
4. `hindcastYE.csv`: the same for `scoreDirYE`.
5. `nationalSummary.csv`: per metric, median and 2.5-97.5% range over ELFs.
6. `report.html`: sortable tables, main and appendix sections, figures from `Plots()`.
