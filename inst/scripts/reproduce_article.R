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
# The figures are drawn by the package's plot() method, exactly as any user
# would obtain them.
save_figure <- function(r, file) {
  png(paste0(file, ".png"), width = 7, height = 6.2, units = "in", res = 600,
      pointsize = 10)
  plot(r); dev.off()
  tiff(paste0(file, ".tiff"), width = 7, height = 6.2, units = "in", res = 600,
       pointsize = 10, compression = "lzw")
  plot(r); dev.off()
}
save_figure(results, "Figure3_Example1")
save_figure(results2, "Figure4_Example2")
cat("\nFigures written to", getwd(), "\n")
