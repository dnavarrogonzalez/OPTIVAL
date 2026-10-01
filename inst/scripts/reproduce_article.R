# ==============================================================================
# Reproduces all analyses, tables and figures of:
#   Navarro-Gonzalez, D., Ferrando, P. J., & Morales-Vives, F.
#   OPTIVAL: An R package for model-based optimal test validity analysis.
#   SoftwareX (revised version).
#
# Usage (from any working directory):
#   source(system.file("scripts", "reproduce_article.R", package = "OPTIVAL"))
# Figures are written to the current working directory (PNG and TIFF, 600 dpi).
# Runtime: about one minute for the two examples; the stability check
# (30 repetitions per example) takes a few additional minutes.
# ==============================================================================

library(OPTIVAL)

# Standard error of the (standardized) criterion loading of Example 2, taken
# from the Mplus MLMV calibration of the extended congeneric model.
SE_LAMBDA_Y_EX2 <- 0.040

# ------------------------------------------------------------------------------
# Example 1: simulated data (Section 3.1)
# ------------------------------------------------------------------------------
set.seed(123)
n <- 500; k <- 20
loadings <- seq(0.60, 0.25, length.out = k)
theta <- rnorm(n)
items <- sapply(loadings, function(lambda) {
  lambda * theta + sqrt(1 - lambda^2) * rnorm(n)
})
criterion <- 0.55 * theta + sqrt(1 - 0.55^2) * rnorm(n)
data_matrix <- cbind(items, criterion)
results <- OPTIVAL(data_matrix, n_bootstrap = 2000)

# ------------------------------------------------------------------------------
# Example 2: FIQ-HS home-based activities, precalibrated loadings (Section 3.2)
# ------------------------------------------------------------------------------
d2 <- as.matrix(read.csv(optival_example("example2"), header = FALSE))
l2 <- read.csv(optival_example("example2_loadings"), header = FALSE)[, 1]
set.seed(123)
results2 <- OPTIVAL(d2, precalibrated_loadings = l2, n_bootstrap = 2000,
                    criterion_loading_se = SE_LAMBDA_Y_EX2)

# ------------------------------------------------------------------------------
# Text and tables (Sections 3.1-3.2, Tables 1-2)
# ------------------------------------------------------------------------------
report <- function(r, label) {
  k <- r$optimal_n_items; m <- r$n_items
  cat("\n=====", label, "=====\n")
  if (!is.null(r$model_fit)) print(round(r$model_fit, 3))
  ci <- r$criterion_loading_ci
  cat(sprintf("Criterion loading: %.3f [%.3f, %.3f]\n", ci["estimate"], ci["lower"], ci["upper"]))
  cat("Optimal items (order of entry):", r$optimal_items, "\n")
  cat(sprintf("Observed validity, optimal subset: %.3f [%.3f, %.3f]; expected: %.3f\n",
              r$observed_validity[k], r$observed_validity_ci[k, 1],
              r$observed_validity_ci[k, 2], r$expected_validity[k]))
  cat(sprintf("Observed validity, full scale:     %.3f [%.3f, %.3f]\n",
              r$observed_validity[m], r$observed_validity_ci[m, 1], r$observed_validity_ci[m, 2]))
  cat(sprintf("Retained validity: %.1f%%; efficiency relative to ceiling: %.1f%%\n",
              100 * r$observed_validity[k] / r$observed_validity[m],
              100 * r$observed_validity[k] / r$criterion_loading))
  cat(sprintf("Correlation loadings - validity indices: r = %.3f\n", r$validity_correlation))
  o <- r$item_order
  tab <- data.frame(
    Rank = seq_len(m), Item = o,
    Loading = round(r$item_loadings[o], 4),
    Validity = sprintf("%.3f [%.3f, %.3f]", r$validity_indices[o],
                       r$validity_indices_ci[o, 1], r$validity_indices_ci[o, 2]),
    Increment = round(r$incremental_validity, 4),
    Selected = ifelse(o %in% r$optimal_items, "yes", "-"))
  print(tab, row.names = FALSE)
  invisible(tab)
}
tab1 <- report(results, "Example 1")
tab2 <- report(results2, "Example 2")

# ------------------------------------------------------------------------------
# Sensitivity to the selection thresholds (Section 3.3, Table 3)
# The thresholds do not affect the bootstrap curves, so the selection rule is
# re-applied to the stored results (identical to re-running OPTIVAL() with the
# same seed and different threshold arguments).
# ------------------------------------------------------------------------------
select_n <- function(r, eff, effe) {
  d <- r$incremental_validity; obs <- r$observed_validity; m <- length(d)
  n1 <- which(d < max(d) * eff)[1]; if (is.na(n1)) n1 <- m
  n2 <- which(obs / max(obs) >= effe)[1]; if (is.na(n2)) n2 <- m
  max(n1, n2)
}
grid <- expand.grid(efficiency = c(0.05, 0.10, 0.15), effectiveness = c(0.90, 0.95, 0.99))
for (lab in c("Example 1", "Example 2")) {
  r <- if (lab == "Example 1") results else results2
  nn <- mapply(function(a, b) select_n(r, a, b), grid$efficiency, grid$effectiveness)
  grid[[lab]] <- sprintf("%d (%.1f)", nn, 100 * r$observed_validity[nn] / r$observed_validity[r$n_items])
}
cat("\n===== Table 3 =====\n"); print(grid, row.names = FALSE)

# Stability of the default selection across random seeds
stability <- function(dm, pl = NULL, B = 30) {
  sapply(seq_len(B), function(s) {
    set.seed(1000 + s)
    OPTIVAL(dm, precalibrated_loadings = pl, n_bootstrap = 2000)$optimal_n_items
  })
}
cat("\nSelected length across 30 seeds, Example 1:\n"); print(table(stability(data_matrix)))
cat("Selected length across 30 seeds, Example 2:\n"); print(table(stability(d2, l2)))

# ------------------------------------------------------------------------------
# Figures 3 and 4
# ------------------------------------------------------------------------------
plot_optival <- function(r, file) {
  col_obs  <- "#2a78d6"   # observed (bootstrap) / selected items
  col_exp  <- "#eb6834"   # expected (model)
  col_cut  <- "#1baf7a"   # optimal cutoff
  col_band <- adjustcolor(col_obs, alpha.f = 0.16)
  ink <- "#2b2b29"; ink2 <- "#6b6a66"; grid_col <- "#e7e6e1"; muted <- "#b9b8b2"
  panel <- function(xlim, ylim, xlab, ylab, title, letter, xat = NULL) {
    plot.new(); plot.window(xlim, ylim)
    yat <- pretty(ylim, 6); xat <- if (is.null(xat)) pretty(xlim, 6) else xat
    abline(h = yat, v = xat, col = grid_col, lwd = 0.8)
    axis(1, at = xat, col = NA, col.ticks = muted, col.axis = ink2, tcl = -0.25)
    axis(2, at = yat, col = NA, col.ticks = muted, col.axis = ink2, tcl = -0.25)
    segments(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[3], col = muted)
    segments(par("usr")[1], par("usr")[3], par("usr")[1], par("usr")[4], col = muted)
    title(xlab = xlab, ylab = ylab, col.lab = ink)
    mtext(title, side = 3, line = 0.5, adj = 0, font = 2, col = ink, cex = 0.95)
    mtext(letter, side = 3, line = 0.5, adj = 0,
          at = grconvertX(par("fig")[1] + 0.008, from = "ndc", to = "user"),
          font = 2, cex = 1.25, col = ink)
  }
  draw <- function() {
    layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE), heights = c(1, 1.08))
    par(mar = c(3.8, 4.3, 2.4, 1.2), mgp = c(2.4, 0.55, 0), las = 1, cex.axis = 0.9,
        cex.lab = 1, family = "sans", fg = ink)
    sel <- seq_len(r$n_items) %in% r$optimal_items
    n <- r$n_items; k <- r$optimal_n_items
    # A: loadings vs validity indices
    xr <- range(r$item_loadings); yr <- range(r$validity_indices)
    panel(xr + c(-0.02, 0.02), yr + c(-0.02, 0.04),
          expression("Item factor loading (" * lambda[j] * ")"), "Item validity (bootstrap)",
          "Item loadings vs. item validity", "A")
    abline(r$validity_lm, lty = 2, col = ink2, lwd = 1.3)
    points(r$item_loadings[!sel], r$validity_indices[!sel], pch = 21, bg = "#d3d2cc", col = "white", cex = 1.6, lwd = 1.2)
    points(r$item_loadings[sel], r$validity_indices[sel], pch = 21, bg = col_obs, col = "white", cex = 1.6, lwd = 1.2)
    legend("topleft", c("Selected", "Not selected", sprintf("Linear fit (r = %.3f)", r$validity_correlation)),
           pch = c(21, 21, NA), pt.bg = c(col_obs, "#d3d2cc", NA), col = c("white", "white", ink2),
           lty = c(NA, NA, 2), pt.cex = 1.5, bty = "n", cex = 0.88, text.col = ink, inset = c(0.01, 0))
    # B: expected increments
    xat <- unique(c(1, seq(5, n, 5), n))
    panel(c(1, n), c(0, max(r$incremental_validity) * 1.08), "Number of items (order of entry)",
          expression("Expected increment (" * Delta * ")"), "Incremental validity", "B", xat = xat)
    abline(v = k, lty = 2, col = col_cut, lwd = 1.5)
    polygon(c(1, 1:n, n), c(0, r$incremental_validity, 0), col = adjustcolor(col_obs, 0.10), border = NA)
    lines(1:n, r$incremental_validity, col = col_obs, lwd = 2)
    points(1:n, r$incremental_validity, pch = 21, bg = ifelse(1:n <= k, col_obs, "white"), col = col_obs, cex = 1.2, lwd = 1.2)
    text(k, max(r$incremental_validity) * 0.95, paste0("  optimal n = ", k), adj = 0, cex = 0.88, col = ink, font = 2)
    # C: validity curves with bootstrap percentile band
    ci <- r$observed_validity_ci
    ylim <- c(min(ci[, 1], r$expected_validity) - 0.02, r$criterion_loading + 0.03)
    panel(c(1, n), ylim, "Number of items", "Test validity",
          "Test validity curves: observed vs. expected", "C", xat = 1:n)
    polygon(c(1:n, n:1), c(ci[, 1], rev(ci[, 2])), col = col_band, border = NA)
    abline(h = r$criterion_loading, lty = 3, col = ink, lwd = 1.4)
    abline(v = k, lty = 2, col = col_cut, lwd = 1.5)
    lines(1:n, r$expected_validity, col = col_exp, lwd = 2, lty = 2)
    points(1:n, r$expected_validity, pch = 21, bg = "white", col = col_exp, cex = 1.15, lwd = 1.4)
    lines(1:n, r$observed_validity, col = col_obs, lwd = 2)
    points(1:n, r$observed_validity, pch = 21, bg = col_obs, col = "white", cex = 1.35, lwd = 1)
    text(1, r$criterion_loading,
         bquote("Theoretical ceiling (" * lambda[y] * " = " * .(sprintf("%.3f", r$criterion_loading)) * ")"),
         adj = c(0, -0.5), cex = 0.88, col = ink)
    legend("bottomright", c("Observed (bootstrap)", "95% bootstrap interval", "Expected (model)", paste0("Optimal n = ", k)),
           pch = c(21, 15, 21, NA), pt.bg = c(col_obs, NA, "white", NA), col = c(col_obs, col_band, col_exp, col_cut),
           lty = c(1, NA, 2, 2), lwd = c(2, NA, 2, 1.5), pt.cex = c(1.3, 2.4, 1.1, NA), bty = "n", cex = 0.88,
           text.col = ink, bg = "white")
  }
  png(paste0(file, ".png"), width = 7, height = 6.2, units = "in", res = 600,
      pointsize = 10, family = "sans"); draw(); dev.off()
  tiff(paste0(file, ".tiff"), width = 7, height = 6.2, units = "in", res = 600,
       pointsize = 10, family = "sans", compression = "lzw"); draw(); dev.off()
}
plot_optival(results, "Figure3_Example1")
plot_optival(results2, "Figure4_Example2")
cat("\nFigures written to", getwd(), "\n")
