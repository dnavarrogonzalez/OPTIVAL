# ==============================================================================
# PLOT METHOD FOR OPTIVAL RESULTS
# ==============================================================================
# Static diagnostic plots for objects returned by OPTIVAL(). The Shiny
# application draws interactive versions of the same three panels with the
# same visual encoding (colours, line types and markers).
# ==============================================================================

# Colour palette shared by plot.OPTIVAL() and the Shiny application.
# Checked for colour-vision deficiency; every series is also distinguished by
# line type and marker fill, so the plots remain readable in greyscale.
.optival_colors <- list(
  observed = "#2a78d6",   # observed (bootstrap) validity, selected items
  expected = "#eb6834",   # expected (model) validity
  cutoff   = "#1baf7a",   # optimal test length
  unselected = "#d3d2cc", # items not selected
  ink      = "#2b2b29",   # text, ceiling line
  ink2     = "#6b6a66",   # secondary text, linear fit
  grid     = "#e7e6e1",
  axis     = "#b9b8b2"
)

#' Plot OPTIVAL Results
#'
#' Draws the three diagnostic plots of an OPTIVAL analysis: (A) item factor
#' loadings against bootstrap item validity indices, (B) expected incremental
#' validity by order of entry, and (C) observed (bootstrap) and expected
#' (model-implied) test validity curves, with the bootstrap percentile interval
#' of the observed validity and the theoretical validity ceiling.
#'
#' @param x An object of class \code{"OPTIVAL"}, as returned by \code{\link{OPTIVAL}}.
#' @param which Which plot(s) to draw: \code{"all"} (default; the three panels
#'   in a single figure), \code{"items"} (panel A), \code{"increments"}
#'   (panel B) or \code{"curves"} (panel C).
#' @param ci Logical. If \code{TRUE} (default), the bootstrap percentile interval
#'   of the observed validity is drawn as a shaded band in the validity curves.
#' @param ... Not used; included for compatibility with the generic.
#'
#' @return \code{x}, invisibly.
#'
#' @details The figures in the article describing the package were produced
#' with this method. To save a high-resolution figure, open a graphics device
#' before calling \code{plot()}, e.g.
#' \code{png("optival.png", width = 7, height = 6.2, units = "in", res = 600)},
#' followed by \code{plot(results)} and \code{dev.off()}.
#'
#' @examples
#' \dontrun{
#' d2 <- as.matrix(read.csv(optival_example("example2"), header = FALSE))
#' l2 <- read.csv(optival_example("example2_loadings"), header = FALSE)[, 1]
#' results <- OPTIVAL(d2, precalibrated_loadings = l2, n_bootstrap = 2000)
#' plot(results)                    # the three panels
#' plot(results, which = "curves")  # validity curves only
#' }
#'
#' @seealso \code{\link{OPTIVAL}}, \code{\link{run_optival}}
#'
#' @export
#' @importFrom graphics plot.new plot.window abline axis segments title mtext
#'   points lines polygon text legend layout par grconvertX
#' @importFrom grDevices adjustcolor
#' @importFrom stats coef
plot.OPTIVAL <- function(x, which = c("all", "items", "increments", "curves"),
                         ci = TRUE, ...) {
  which <- match.arg(which)
  r <- x
  col <- .optival_colors
  n <- r$n_items
  k <- r$optimal_n_items
  sel <- seq_len(n) %in% r$optimal_items

  panel <- function(xlim, ylim, xlab, ylab, main, letter, xat = NULL) {
    plot.new()
    plot.window(xlim, ylim)
    yat <- pretty(ylim, 6)
    xat <- if (is.null(xat)) pretty(xlim, 6) else xat
    abline(h = yat, v = xat, col = col$grid, lwd = 0.8)
    axis(1, at = xat, col = NA, col.ticks = col$axis, col.axis = col$ink2, tcl = -0.25)
    axis(2, at = yat, col = NA, col.ticks = col$axis, col.axis = col$ink2, tcl = -0.25)
    usr <- par("usr")
    segments(usr[1], usr[3], usr[2], usr[3], col = col$axis)
    segments(usr[1], usr[3], usr[1], usr[4], col = col$axis)
    title(xlab = xlab, ylab = ylab, col.lab = col$ink)
    mtext(main, side = 3, line = 0.5, adj = 0, font = 2, col = col$ink, cex = 0.95)
    if (!is.null(letter)) {
      mtext(letter, side = 3, line = 0.5, adj = 0,
            at = grconvertX(par("fig")[1] + 0.008, from = "ndc", to = "user"),
            font = 2, cex = 1.25, col = col$ink)
    }
  }

  draw_items <- function(letter) {
    xr <- range(r$item_loadings); yr <- range(r$validity_indices)
    panel(xr + c(-0.02, 0.02), yr + c(-0.02, 0.04),
          expression("Item factor loading (" * lambda[j] * ")"), "Item validity (bootstrap)",
          "Item loadings vs. item validity", letter)
    abline(coef(r$validity_lm), lty = 2, col = col$ink2, lwd = 1.3)
    points(r$item_loadings[!sel], r$validity_indices[!sel], pch = 21,
           bg = col$unselected, col = "white", cex = 1.6, lwd = 1.2)
    points(r$item_loadings[sel], r$validity_indices[sel], pch = 21,
           bg = col$observed, col = "white", cex = 1.6, lwd = 1.2)
    legend("topleft", c("Selected", "Not selected",
                        sprintf("Linear fit (r = %.3f)", r$validity_correlation)),
           pch = c(21, 21, NA), pt.bg = c(col$observed, col$unselected, NA),
           col = c("white", "white", col$ink2), lty = c(NA, NA, 2), pt.cex = 1.5,
           bty = "n", cex = 0.88, text.col = col$ink, inset = c(0.01, 0))
  }

  draw_increments <- function(letter) {
    d <- r$incremental_validity
    xat <- unique(c(1, seq(5, n, 5), n))
    panel(c(1, n), c(min(0, d), max(d) * 1.08), "Number of items (order of entry)",
          expression("Expected increment (" * Delta * ")"), "Incremental validity",
          letter, xat = xat)
    abline(v = k, lty = 2, col = col$cutoff, lwd = 1.5)
    polygon(c(1, 1:n, n), c(0, d, 0), col = adjustcolor(col$observed, 0.10), border = NA)
    lines(1:n, d, col = col$observed, lwd = 2)
    points(1:n, d, pch = 21, bg = ifelse(1:n <= k, col$observed, "white"),
           col = col$observed, cex = 1.2, lwd = 1.2)
    text(k, max(d) * 0.95, paste0("  optimal n = ", k), adj = 0, cex = 0.88,
         col = col$ink, font = 2)
  }

  draw_curves <- function(letter) {
    obs <- r$observed_validity; ex <- r$expected_validity
    band <- ci && !is.null(r$observed_validity_ci)
    lo <- if (band) r$observed_validity_ci[, 1] else obs
    ylim <- c(min(lo, ex) - 0.02, max(r$criterion_loading, obs, ex) + 0.03)
    panel(c(1, n), ylim, "Number of items", "Test validity",
          "Test validity curves: observed vs. expected", letter,
          xat = if (n <= 25) 1:n else pretty(c(1, n), 8))
    if (band) {
      cib <- r$observed_validity_ci
      polygon(c(1:n, n:1), c(cib[, 1], rev(cib[, 2])),
              col = adjustcolor(col$observed, alpha.f = 0.16), border = NA)
    }
    abline(h = r$criterion_loading, lty = 3, col = col$ink, lwd = 1.4)
    abline(v = k, lty = 2, col = col$cutoff, lwd = 1.5)
    lines(1:n, ex, col = col$expected, lwd = 2, lty = 2)
    points(1:n, ex, pch = 21, bg = "white", col = col$expected, cex = 1.15, lwd = 1.4)
    lines(1:n, obs, col = col$observed, lwd = 2)
    points(1:n, obs, pch = 21, bg = col$observed, col = "white", cex = 1.35, lwd = 1)
    text(1, r$criterion_loading,
         bquote("Theoretical ceiling (" * lambda[y] * " = " *
                  .(sprintf("%.3f", r$criterion_loading)) * ")"),
         adj = c(0, -0.5), cex = 0.88, col = col$ink)
    labs <- c("Observed (bootstrap)", "95% bootstrap interval", "Expected (model)",
              paste0("Optimal n = ", k))
    keep <- if (band) 1:4 else c(1, 3, 4)
    if (band && !is.null(r$observed_validity_ci)) {
      lev <- attr(r$observed_validity_ci, "ci_level")
      if (!is.null(lev)) labs[2] <- sprintf("%g%% bootstrap interval", 100 * lev)
    }
    legend("bottomright", labs[keep],
           pch = c(21, 15, 21, NA)[keep], pt.bg = c(col$observed, NA, "white", NA)[keep],
           col = c(col$observed, adjustcolor(col$observed, alpha.f = 0.16),
                   col$expected, col$cutoff)[keep],
           lty = c(1, NA, 2, 2)[keep], lwd = c(2, NA, 2, 1.5)[keep],
           pt.cex = c(1.3, 2.4, 1.1, NA)[keep], bty = "n", cex = 0.88, text.col = col$ink)
  }

  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(3.8, 4.3, 2.4, 1.2), mgp = c(2.4, 0.55, 0), las = 1, cex.axis = 0.9,
      cex.lab = 1, fg = col$ink)

  if (which == "all") {
    layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE), heights = c(1, 1.08))
    draw_items("A"); draw_increments("B"); draw_curves("C")
  } else {
    switch(which,
           items = draw_items(NULL),
           increments = draw_increments(NULL),
           curves = draw_curves(NULL))
  }
  invisible(x)
}
