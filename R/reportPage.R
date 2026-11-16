## report.html: sortable tables (data-v on every cell) in a main and an appendix section.

htmlEscape <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE), fixed = TRUE)

## a number as shown: 3 significant digits
shown <- function(x) ifelse(is.na(x), "", sprintf("%.3g", x))

## the notes of the rows that have no value, one line per arm and note, for tables laid out without the note column
missingNotes <- function(tab) {
  if (is.null(tab) || !"note" %in% names(tab)) return(character(0))
  m <- tab[is.na(tab$value) & nzchar(tab$note), ]
  if (!nrow(m)) return(character(0))
  u <- unique(paste0("<p class=note>Arm ", htmlEscape(m$arm), ", ELF ", htmlEscape(m$ELF), ": ", htmlEscape(m$note), "</p>"))
  u
}

#' A sortable HTML table
#'
#' Every cell carries `data-v`, the value the column sorts by (the number, or the text); the script at the foot of the page
#' sorts on a click of a column head.
#'
#' @param df A `data.frame`. @param caption Text above the table.
#' @return One string of HTML.
htmlTable <- function(df, caption = "") {
  if (is.null(df) || !nrow(df)) return(paste0("<p class=note>", htmlEscape(caption), ": no rows.</p>"))
  head <- paste0("<th><button>", htmlEscape(names(df)), "</button></th>", collapse = "")
  cell <- function(v) if (is.numeric(v)) sprintf("<td data-v=\"%s\">%s</td>", ifelse(is.na(v), "", sprintf("%.15g", v)), shown(v))
                      else sprintf("<td data-v=\"%s\">%s</td>", htmlEscape(v), htmlEscape(ifelse(is.na(v), "", v)))
  cells <- do.call(cbind, lapply(df, cell))
  body <- paste0("<tr>", apply(cells, 1, paste, collapse = ""), "</tr>", collapse = "\n")
  paste0("<h3>", htmlEscape(caption), "</h3><div class=tablebox><table class=sortable><thead><tr>", head, "</tr></thead><tbody>\n", body, "\n</tbody></table></div>")
}

## an <img> of a png file inlined, or a note
inlineFigure <- function(file, alt) {
  if (!file.exists(file)) return(paste0("<p class=note>", htmlEscape(alt), ": figure not written (set .plots to 'png').</p>"))
  paste0("<img loading=lazy alt=\"", htmlEscape(alt), "\" src=\"data:image/png;base64,",
         gsub("\n", "", jsonlite::base64_enc(readBin(file, "raw", file.size(file))), fixed = TRUE), "\">")
}

pageScript <- "
document.querySelectorAll('table.sortable').forEach(function (t) {
  t.querySelectorAll('th button').forEach(function (b, i) {
    b.addEventListener('click', function () {
      var asc = b.getAttribute('aria-sort') !== 'ascending';
      t.querySelectorAll('th button').forEach(function (o) { o.removeAttribute('aria-sort'); });
      b.setAttribute('aria-sort', asc ? 'ascending' : 'descending');
      var rows = Array.from(t.tBodies[0].rows);
      var val = function (r) { var v = r.cells[i].dataset.v; return v === '' ? null : (isNaN(+v) ? v : +v); };
      rows.sort(function (a, c) { var x = val(a), y = val(c);
        if (x === null) return 1; if (y === null) return -1;
        return (x < y ? -1 : x > y ? 1 : 0) * (asc ? 1 : -1); });
      rows.forEach(function (r) { t.tBodies[0].appendChild(r); });
    });
  });
});"

pageStyle <- "body{font:15px/1.5 system-ui,sans-serif;max-width:1500px;margin:1rem auto;padding:0 1rem;color:#16202a}
.tablebox{overflow-x:auto;border:1px solid #d9dfe6;border-radius:6px;margin-bottom:1rem}table{border-collapse:collapse;width:100%}
th,td{padding:.3rem .7rem;text-align:right;border-bottom:1px solid #d9dfe6;white-space:nowrap}th:first-child,td:first-child{text-align:left}
th button{font:inherit;font-weight:600;background:none;border:0;cursor:pointer}th button[aria-sort]{color:#0072b2}
img{max-width:100%;border:1px solid #d9dfe6;margin:.3rem 0}.note{color:#586474;font-size:.86rem}"

#' The report page
#'
#' @param tables The list of tables [buildReportTables()] makes.
#' @param figures A `data.frame` of the figures: `arm`, `role`, `ELF`, `kind` and `file` (png), as `figureIndex()` lists them.
#' @param file Where to write the page. @param title The page title.
#' @param anova The `anova_terms.csv` of fireSimulator_validation as a `data.frame`, or `NULL`.
#' @return `file`, invisibly.
buildReportPage <- function(tables, figures, file, title = "FireSense fits and hindcast validation", anova = NULL) {
  main <- unique(tables$runs$arm[tables$runs$role == "main"])
  app <- unique(tables$runs$arm[tables$runs$role == "appendix"])
  keep <- function(tab, arms) if (!is.null(tab)) tab[tab$arm %in% arms, , drop = FALSE]
  perElf <- list(parameterSets = "1. Parameter sets (median and 2.5-97.5% over the kept sets)",
                 inSample = "2. In-sample fit per stage", hindcast = "3. Hindcast, drawn year effects", hindcastYE = "4. Hindcast, fitted year effects")
  html <- c(sprintf("<!doctype html><html><head><meta charset=utf-8><title>%s</title><style>%s</style></head><body><h1>%s</h1>",
                    htmlEscape(title), pageStyle, htmlEscape(title)))
  html <- c(html, "<h2>Main text</h2>")
  if (!length(main)) html <- c(html, "<p class=note>No arm has role main.</p>")
  for (a in main) {
    html <- c(html, sprintf("<h2>Arm %s</h2>", htmlEscape(a)),
              htmlTable(keep(tables$nationalSummary, a), "National summary: median over ELFs, 2.5-97.5% range over ELFs"))
    for (n in names(perElf)) html <- c(html, htmlTable(keep(tables[[n]], a), perElf[[n]]))
    f <- figures[figures$arm == a, ]
    f <- f[order(f$kind, f$ELF), ]
    html <- c(html, "<h3>Figures per ELF</h3>", unlist(Map(function(file, elf, kind) paste0("<h4>", htmlEscape(paste(elf, kind)), "</h4>", inlineFigure(file, paste(elf, kind))),
                                                             f$file, f$ELF, f$kind)))
  }
  html <- c(html, "<h2>Appendix</h2>")
  if (!length(app)) html <- c(html, "<p class=note>No arm has role appendix.</p>")
  if (length(app)) {
    html <- c(html, "<p class=note>Arms side by side: one column per arm.</p>")
    for (n in names(perElf)) html <- c(html, htmlTable(sideBySide(keep(tables[[n]], app), if (n == "parameterSets") "median" else "value",
                                                                  intersect(c("ELF", "stage", "source", "term", "metric"), names(tables[[n]]))),
                                                       perElf[[n]]), missingNotes(keep(tables[[n]], app)))
    html <- c(html, htmlTable(keep(tables$nationalSummary, app), "National summary per arm"))
    for (a in app) {
      f <- figures[figures$arm == a, ]
      f <- f[order(f$kind, f$ELF), ]
      html <- c(html, sprintf("<h3>Figures, arm %s</h3>", htmlEscape(a)),
                unlist(Map(function(file, elf, kind) paste0("<h4>", htmlEscape(paste(elf, kind)), "</h4>", inlineFigure(file, paste(elf, kind))), f$file, f$ELF, f$kind)))
    }
  }
  if (!is.null(anova)) html <- c(html, htmlTable(anova, "ANOVA terms (fireSimulator_validation, anova mode)"))
  html <- c(html, sprintf("<script>%s</script></body></html>", pageScript))
  writeLines(html, file)
  invisible(file)
}
