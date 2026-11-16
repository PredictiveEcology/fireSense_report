## Figures. Each function takes its data as the first argument, so `SpaDES.core::Plots(data, fn = ...)` draws it.

## size of a figure file, inches
figureSizes <- list(yearTotals = list(width = 10, height = 8, dpi = 70), spread = list(width = 10, height = 4.5, dpi = 70),
                    bias = list(width = 10, height = 6, dpi = 70))

## observed against predicted, with the 1:1 line
oneToOne <- function(d, x, y, xlab, ylab, title, log = FALSE, also = character(0)) {
  lim <- range(unlist(d[c(x, y, also)]), finite = TRUE)   # `also`: columns drawn too (ranges), so no limit cuts them
  p <- ggplot2::ggplot(d, ggplot2::aes(.data[[x]], .data[[y]])) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = 2) +
    ggplot2::labs(x = xlab, y = ylab, title = title) + ggplot2::theme_bw(base_size = 11)
  if (log) p <- p + ggplot2::scale_x_log10(limits = lim) + ggplot2::scale_y_log10(limits = lim)
  else p <- p + ggplot2::coord_equal(xlim = lim, ylim = lim)
  p
}

#' Ignition and escape: observed and predicted yearly totals of one ELF
#'
#' Four panels: for ignitions and for escapes, the observed against the predicted total of each year with the 1:1 line,
#' and the two series year by year (years in order; the fit's year is standardised).
#'
#' @param data A list: `totals` ([yearTotalsTable()] of one ELF), `title`.
#' @return A patchwork of ggplots.
plotYearTotals <- function(data) {
  panels <- lapply(c("ignition", "escape"), function(p) {
    d <- data$totals[data$totals$process == p, ]
    d <- d[order(d$year), ]; d$order <- seq_len(nrow(d))
    sc <- oneToOne(d, "observed", "predicted", paste("observed", p, "per year"), paste("predicted", p, "per year"), p) + ggplot2::geom_point()
    long <- data.frame(order = rep(d$order, 2), n = c(d$observed, d$predicted), series = rep(c("observed", "predicted"), each = nrow(d)))
    ts <- ggplot2::ggplot(long, ggplot2::aes(order, n, colour = series)) + ggplot2::geom_line() + ggplot2::geom_point(size = 1) +
      ggplot2::scale_colour_manual(values = c(observed = "black", predicted = "#0072b2")) +
      ggplot2::labs(x = "year (in order)", y = paste(p, "per year"), title = p) + ggplot2::theme_bw(base_size = 11)
    list(sc, ts)
  })
  patchwork::wrap_plots(unlist(panels, recursive = FALSE), ncol = 2) + patchwork::plot_annotation(title = data$title)
}

#' Spread: observed and simulated sizes of the fitted fires of one ELF
#'
#' Left, the quantiles of the observed fire sizes against those of the simulated ones (`qObs_*`, `qSim_*` of
#' `fireSenseUtils::scoreFireSizes()`; log-log, 1:1 line): the ledger keeps these quantiles, not the sizes of each fire,
#' so there is no range per fire. Right, the observed annual area against the median of the simulated annual areas with
#' their 2.5-97.5% range ([inSampleSizesOf()] `byYear`; log-log, 1:1 line). Sizes are in pixels, as the fit counts them.
#'
#' @param data A list: `sizes` (the one-row table), `byYear`, `title`.
#' @return A patchwork of ggplots.
plotSpreadInSample <- function(data) {
  s <- as.data.frame(data$sizes)
  q <- sub("^qObs_", "", grep("^qObs_", names(s), value = TRUE))
  fires <- data.frame(q = q, observed = as.numeric(unlist(s[paste0("qObs_", q)])), simulated = as.numeric(unlist(s[paste0("qSim_", q)])))
  p1 <- oneToOne(fires, "observed", "simulated", "observed fire size (pixels)", "simulated fire size (pixels)", "fire-size quantiles", log = TRUE) +
    ggplot2::geom_point() + ggplot2::geom_text(ggplot2::aes(label = q), vjust = -0.7, size = 3)
  y <- as.data.frame(data$byYear)
  p2 <- oneToOne(y, "obs", "simMedian", "observed annual area (pixels)", "simulated annual area (pixels; median, 2.5-97.5%)", "annual area burned", log = TRUE, also = c("simLo", "simHi")) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = simLo, ymax = simHi), colour = "grey60") + ggplot2::geom_point()
  p1 + p2 + patchwork::plot_annotation(title = data$title)
}

#' Hindcast bias by ELF
#'
#' The simulated over observed ratio of every bias metric of the hindcast table, one point per ELF with its range
#' (the 2.5-97.5% of single runs), log scale, 1 marked.
#'
#' @param data A list: `bias` (rows of a hindcast table whose `metric` starts with `ratio` or `B:`), `title`.
#' @return A ggplot.
plotBiasByELF <- function(data) {
  d <- data$bias
  ggplot2::ggplot(d, ggplot2::aes(.data$value, stats::reorder(.data$ELF, .data$value))) +
    ggplot2::geom_vline(xintercept = 1, colour = "grey50", linetype = 2) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$lo, xmax = .data$hi), colour = "grey60", na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) + ggplot2::facet_wrap(~ metric, nrow = 1) + ggplot2::scale_x_log10() +
    ggplot2::labs(x = "simulated / observed", y = "ELF", title = data$title) + ggplot2::theme_bw(base_size = 11)
}
