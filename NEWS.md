# OPTIVAL 1.0.2

* New `plot()` method for `OPTIVAL` objects, which draws the three diagnostic
  plots (item validity, incremental validity and validity curves with the
  bootstrap interval), either together or one at a time (`which`). The
  figures in the article are produced with this method.
* The Shiny application uses the same visual encoding as `plot()`, shows the
  bootstrap interval of the observed validity curve, and its plots can be
  downloaded as high-resolution PNG files.

# OPTIVAL 1.0.1

## New features

* `OPTIVAL()` now reports the uncertainty of its main estimates:
  * `criterion_loading_ci`: estimate, standard error and confidence interval of
    the criterion loading (validity ceiling). When loadings are estimated
    internally, these come from the lavaan solution; with precalibrated
    loadings, the new argument `criterion_loading_se` (e.g., the S.E. reported
    by Mplus) is used to compute a Wald interval.
  * `validity_indices_se` and `validity_indices_ci`: bootstrap standard errors
    and percentile confidence intervals of the item validity indices.
  * `observed_validity_se` and `observed_validity_ci`: the same for the
    observed (bootstrap) validity at each test length; `validity_by_n` gains
    the columns `observed_lower` and `observed_upper`.
* New argument `ci_level` (default 0.95) sets the level of all intervals.
* The Shiny application shows these intervals in the summary and in the
  results table, and accepts the optional S.E. of a precalibrated criterion
  loading.

# OPTIVAL 1.0.0

## Initial Release (2026-01-08)

This is the first public release of OPTIVAL.

### Features

* **Core Functionality**
  * Model-based optimal test validity analysis
  * Bootstrap procedures for empirical validation
  * Incremental validity analysis
  * Optimal item subset selection using hybrid method
  
* **Statistical Methods**
  * Confirmatory Factor Analysis (CFA) calibration
  * Linear factor-analytic model for continuous items
  * Extended congeneric measurement model
  * Model-predicted validity curves
  
* **User Interface**
  * Interactive Shiny web application
  * File upload for data matrices and precalibrated loadings
  * Real-time progress indicators
  * Three interactive plots (plotly):
    - Item validity scatter plot
    - Incremental validity curve
    - Test validity curves (observed vs expected)
  * Comprehensive summary statistics
  * Downloadable results table
  
* **R Functions**
  * `OPTIVAL()`: Main analysis function
  * `run_optival()`: Launch Shiny application
  * Internal functions for bootstrap and validity calculations
  
* **Documentation**
  * Complete roxygen2 documentation for all functions
  * Comprehensive README with examples
  * Usage vignettes and FAQ

### References

P.J. Ferrando, F. Morales-Vives, S. Durán-Bonavila, D. Navarro-González, Assessing the unconditional and conditional external validity of noncognitive test scores: A unifying model-based proposal, Educational and Psychological Measurement (2026). https://doi.org/10.1177/00131644261440168

P.J. Ferrando, D. Navarro-González, F. Morales-Vives, Revisiting the test-length and validity problem from a model-based view: A proposal and recommendations, Anales de Psicología (in-press).
