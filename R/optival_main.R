# ==============================================================================
# MAIN OPTIVAL FUNCTION
# ==============================================================================
# This file contains the main OPTIVAL function that implements the complete
# model-based optimal validity analysis workflow.
# ==============================================================================

#' Model-Based Optimal Test Validity Analysis
#' 
#' Determines the optimal item subset that maximizes predictive validity with
#' respect to an external criterion variable, using a model-based approach.
#' 
#' @param data_matrix A numeric matrix where:
#'   \itemize{
#'     \item Rows represent subjects/respondents
#'     \item Columns 1 to (n-1) contain test item scores
#'     \item Column n contains the external criterion variable
#'   }
#'   All variables should be continuous or reasonably continuous. Missing data
#'   should be handled before analysis.
#'   
#' @param precalibrated_loadings Optional. A numeric vector of pre-calibrated
#'   factor loadings. If provided, must have length equal to the number of columns
#'   in \code{data_matrix} (i.e., includes both item loadings and criterion loading,
#'   with criterion loading as the last element). If \code{NULL} (default), loadings
#'   are estimated from the data using Confirmatory Factor Analysis.
#'   
#' @param n_bootstrap Integer. Number of bootstrap replications for empirical
#'   validation of model predictions. Default is 2000. Higher values provide
#'   more stable estimates but increase computation time.
#'   
#' @param efficiency_threshold Numeric in (0, 1). Items contributing less than
#'   this proportion of the maximum incremental validity are flagged as providing
#'   negligible additional information (efficiency criterion). Default is 0.10.
#'   
#' @param effectiveness_threshold Numeric in (0, 1). Minimum proportion of the
#'   maximum achievable validity that the selected subset must capture
#'   (effectiveness criterion). Default is 0.95.
#'   
#' @param verbose Logical. If \code{TRUE}, prints progress messages during
#'   execution. Default is \code{FALSE}.
#'   
#' @return An object of class \code{"OPTIVAL"}, which is a list containing:
#' 
#' \strong{Data Information:}
#' \itemize{
#'   \item \code{n_items} Number of items analyzed
#'   \item \code{n_subjects} Sample size
#'   \item \code{n_bootstrap} Number of bootstrap replications used
#' }
#' 
#' \strong{Factor Analysis Results:}
#' \itemize{
#'   \item \code{item_loadings} Vector of item factor loadings
#'   \item \code{criterion_loading} Criterion factor loading (theoretical validity ceiling)
#'   \item \code{item_order} Recommended item ordering by incremental validity
#'   \item \code{model_fit} Named vector of CFA fit indices (χ², CFI, TLI, RMSEA, SRMR),
#'     or \code{NULL} if precalibrated loadings were used
#' }
#' 
#' \strong{Validity Analysis:}
#' \itemize{
#'   \item \code{validity_indices} Bootstrap-based item-criterion correlations
#'   \item \code{validity_correlation} Correlation between item loadings and validity indices
#'   \item \code{incremental_validity} Expected incremental contribution of each item
#' }
#' 
#' \strong{Optimal Item Subset:}
#' \itemize{
#'   \item \code{optimal_n_items} Suggested optimal number of items
#'   \item \code{optimal_items} Vector of selected item indices
#'   \item \code{selection_diagnostics} List with diagnostic information about the
#'     selection process
#' }
#' 
#' \strong{Test Validity Curves:}
#' \itemize{
#'   \item \code{observed_validity} Bootstrap-estimated validity for each test length
#'   \item \code{expected_validity} Model-predicted validity for each test length
#'   \item \code{max_observed_validity} Maximum observed validity coefficient
#'   \item \code{max_observed_n} Test length at maximum validity
#'   \item \code{validity_by_n} Data frame with all validity measures by test length
#' }
#' 
#' @details 
#' OPTIVAL implements a model-based approach for optimizing test validity that:
#' \enumerate{
#'   \item Calibrates items using a unidimensional linear factor-analytic model
#'     (treated as a linear IRT model for continuous responses)
#'   \item Extends the model to include the external criterion variable
#'   \item Derives model-implied item-criterion correlations and expected validity
#'   \item Uses bootstrap procedures to obtain empirical validity estimates
#'   \item Determines the optimal item subset that maximizes validity while
#'     avoiding capitalization on chance
#' }
#' 
#' The methodology is based on an extended congeneric measurement model with
#' the assumption of local independence. Key advantages over purely empirical
#' approaches include better generalizability, stability across samples, and
#' clear theoretical foundation.
#' 
#' \strong{Model Assumptions:}
#' \itemize{
#'   \item Items measure a unidimensional construct
#'   \item Linear factor-analytic model holds
#'   \item Local independence (item residuals are uncorrelated)
#'   \item External criterion relates to items through the common latent construct
#' }
#' 
#' \strong{Optimal Subset Selection:}
#' 
#' The optimal number of items is determined using a hybrid method that combines:
#' \enumerate{
#'   \item \strong{Efficiency criterion:} Items with negligible incremental validity
#'     (< 10\% of maximum increment) are not included
#'   \item \strong{Effectiveness criterion:} Sufficient items are included to capture
#'     at least 95\% of maximum achievable validity
#' }
#' 
#' The final selection takes the maximum of both criteria to ensure both efficiency
#' and effectiveness are satisfied.
#' 
#' @references 
#' Burisch, M. (1984). Approaches to personality inventory construction: 
#' A comparison of merits. \emph{American Psychologist, 39}(3), 214-227.
#' 
#' Ferrando, P. J. (2009). Difficulty, discrimination, and information indices 
#' in the linear factor analysis model for continuous item responses. 
#' \emph{Applied Psychological Measurement, 33}(1), 9-24.
#' 
#' @examples
#' \dontrun{
#' # Simulate data: 20 items + 1 criterion, 500 subjects
#' set.seed(123)
#' n_subjects <- 500
#' n_items <- 20
#' 
#' # Generate latent construct
#' theta <- rnorm(n_subjects)
#' 
#' # Generate items with varying loadings
#' loadings <- seq(0.5, 0.9, length.out = n_items)
#' items <- sapply(loadings, function(lambda) {
#'   lambda * theta + sqrt(1 - lambda^2) * rnorm(n_subjects)
#' })
#' 
#' # Generate criterion
#' criterion_loading <- 0.85
#' criterion <- criterion_loading * theta + 
#'   sqrt(1 - criterion_loading^2) * rnorm(n_subjects)
#' 
#' # Combine data
#' data_matrix <- cbind(items, criterion)
#' 
#' # Run OPTIVAL
#' results <- OPTIVAL(data_matrix, n_bootstrap = 2000)
#' 
#' # View results
#' print(results$optimal_n_items)
#' print(results$optimal_items)
#' print(results$max_observed_validity)
#' 
#' # Plot validity curve
#' plot(results$validity_by_n$n_items, results$validity_by_n$observed,
#'      type = "b", xlab = "Number of Items", ylab = "Test Validity",
#'      main = "Test Validity Curve")
#' lines(results$validity_by_n$n_items, results$validity_by_n$expected,
#'       col = "blue", lty = 2)
#' legend("bottomright", legend = c("Observed", "Expected"),
#'        col = c("black", "blue"), lty = c(1, 2))
#' }
#' 
#' @seealso \code{\link{run_optival}} for launching the interactive Shiny application
#' 
#' @export
#' @importFrom lavaan cfa standardizedSolution fitMeasures
#' @importFrom stats cor lm
OPTIVAL <- function(data_matrix, 
                    precalibrated_loadings = NULL,
                    n_bootstrap = 2000,
                    efficiency_threshold = 0.10,
                    effectiveness_threshold = 0.95,
                    verbose = FALSE) {
  
  # Standardize data matrix
  Z_total <- scale(data_matrix)
  n_vars <- ncol(data_matrix)
  n_items <- n_vars - 1
  
  # Separate items and criterion
  Z_items <- Z_total[, 1:n_items, drop = FALSE]
  z_criterion <- Z_total[, n_vars, drop = FALSE]
  
  # ===========================================================================
  # CONFIRMATORY FACTOR ANALYSIS
  # ===========================================================================
  
  if (is.null(precalibrated_loadings)) {
    # Build CFA model syntax
    model_syntax <- paste0("F =~ ", paste0("V", 1:n_vars, collapse = " + "))
    colnames(Z_total) <- paste0("V", 1:n_vars)
    df_cfa <- as.data.frame(Z_total)
    
    calib <- tryCatch({
      fit <- lavaan::cfa(model_syntax, data = df_cfa, estimator = "MLM", std.lv = TRUE)
      
      std_solution <- lavaan::standardizedSolution(fit)
      loadings_all <- std_solution$est.std[std_solution$op == "=~"]
      
      list(item_loadings = loadings_all[1:n_items],
           criterion_loading = loadings_all[n_vars],
           fit_measures = lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", 
                                              "cfi", "tli", "rmsea", 
                                              "rmsea.ci.lower", "rmsea.ci.upper",
                                              "srmr")))
    }, error = function(e) {
      # Fallback: use correlation-based (first principal component) estimates
      warning("CFA estimation failed (", conditionMessage(e), 
              "). Using correlation-based estimates for the loadings.")
      cor_matrix <- cor(Z_total)
      eigen_decomp <- eigen(cor_matrix)
      loadings_pc <- eigen_decomp$vectors[, 1] * sqrt(eigen_decomp$values[1]) / 
        sqrt(eigen_decomp$values[1] + 1)
      loadings_pc <- loadings_pc * sign(sum(loadings_pc))
      
      list(item_loadings = loadings_pc[1:n_items],
           criterion_loading = loadings_pc[n_vars],
           fit_measures = NULL)
    })
    item_loadings <- calib$item_loadings
    criterion_loading <- calib$criterion_loading
    fit_measures <- calib$fit_measures
    
  } else {
    # Use precalibrated loadings
    if (!is.null(precalibrated_loadings)) {
      # Convert to numeric vector, handling different input formats
      # Can be: vector, 1-column matrix (nx1), 1-row matrix (1xn)
      
      # Check if first row might be a header (contains non-numeric values)
      if (is.data.frame(precalibrated_loadings) || is.matrix(precalibrated_loadings)) {
        first_value <- as.character(precalibrated_loadings[1, 1])
        if (suppressWarnings(is.na(as.numeric(first_value)))) {
          # First row is likely a header, remove it
          precalibrated_loadings <- precalibrated_loadings[-1, , drop = FALSE]
          if (verbose) cat("Detected and removed header row from precalibrated_loadings\n")
        }
      }
      
      precalibrated_loadings <- as.numeric(as.vector(unlist(precalibrated_loadings)))
    }
    
    if (length(precalibrated_loadings) != n_vars) {
      stop(sprintf("Length of precalibrated_loadings (%d) must equal the total number of variables (%d): 
                the vector must include the item loadings (1:%d) followed by 
                the criterion loading in the last position.", 
                   length(precalibrated_loadings), n_vars, n_items))
    }
    
    # Use the provided loadings for items
    item_loadings <- precalibrated_loadings[1:n_items]    
    criterion_loading <- precalibrated_loadings[n_vars]
    fit_measures <- NULL
    
  }
  
  # Order items by loading
  item_order <- order(item_loadings, decreasing = TRUE)
  
  # ===========================================================================
  # ITEM VALIDITY PLOT (Bootstrap-based)
  # ===========================================================================
  
  validity_indices <- compaboot(Z_items, z_criterion, item_loadings, n_bootstrap)
  
  fit_lm <- lm(validity_indices ~ item_loadings)
  r_validity <- cor(item_loadings, validity_indices)
  
  # ===========================================================================
  # INCREMENTAL VALIDITY ANALYSIS
  # ===========================================================================
  
  burisch_results <- Burisch(item_order, item_loadings, criterion_loading)
  
  # ===========================================================================
  # DETERMINE OPTIMAL NUMBER OF ITEMS - IMPROVED HYBRID METHOD
  # ===========================================================================
  # This method combines two criteria:
  # 1. Elbow method: Stop when incremental validity is too small (efficiency)
  # 2. Proximity method: Ensure we capture sufficient total validity (effectiveness)
  # The optimal n is the MAXIMUM of both methods to ensure we meet BOTH criteria
  # ===========================================================================
  
  # First, compute validity curves to get maximum achievable validity
  loadings_all <- c(item_loadings, criterion_loading)
  validity_results <- simulboot(loadings_all, item_order, Z_total, n_bootstrap)
  max_validity <- max(validity_results$medparc)
  
  # Method 1: Elbow based on relative increments (original method)
  max_delta <- max(burisch_results$deltabur)
  relative_threshold <- efficiency_threshold
  absolute_threshold <- max_delta * relative_threshold
  
  method1_n <- which(burisch_results$deltabur < absolute_threshold)[1]
  
  if (is.na(method1_n)) {
    # Use "elbow" detection: find where second derivative changes most
    if (n_items > 3) {
      delta_diff <- diff(burisch_results$deltabur)
      delta_diff2 <- diff(delta_diff)
      method1_n <- which.max(abs(delta_diff2)) + 2
      method1_n <- min(method1_n, n_items)
    } else {
      method1_n <- n_items
    }
  }
  
  # Method 2: Proximity to maximum validity (NEW)
  # Find first point where we reach X% of maximum possible validity
  proximity_threshold <- effectiveness_threshold
  validity_ratio <- validity_results$medparc / max_validity
  method2_n <- which(validity_ratio >= proximity_threshold)[1]
  
  if (is.na(method2_n)) {
    method2_n <- n_items
  }
  
  # Hybrid method: use MAXIMUM of both methods
  # This ensures we meet BOTH criteria:
  # - Don't add items with negligible marginal gain (efficiency)
  # - Capture sufficient total validity relative to maximum (effectiveness)
  optimal_n <- max(method1_n, method2_n)
  
  # Ensure valid range
  optimal_n <- max(1, min(optimal_n, n_items))
  
  # Store diagnostic information about the selection process
  selection_diagnostics <- list(
    method1_n = method1_n,
    method2_n = method2_n,
    method_used = ifelse(optimal_n == method1_n & optimal_n == method2_n,
                         "both_agree",
                         ifelse(optimal_n == method1_n, "elbow_dominant", "proximity_dominant")),
    relative_threshold = relative_threshold,
    proximity_threshold = proximity_threshold,
    validity_at_method1 = validity_results$medparc[method1_n],
    validity_at_method2 = validity_results$medparc[method2_n],
    validity_at_optimal = validity_results$medparc[optimal_n],
    pct_captured_at_optimal = 100 * validity_results$medparc[optimal_n] / max_validity,
    pct_captured_at_method1 = 100 * validity_results$medparc[method1_n] / max_validity
  )
  
  optimal_items <- item_order[1:optimal_n]
  
  # ===========================================================================
  # VALIDITY CURVES ALREADY COMPUTED
  # ===========================================================================
  # Note: validity_results already computed above in optimal n determination
  
  # Find maximum observed validity
  max_observed <- max(validity_results$medparc)
  max_observed_n <- which.max(validity_results$medparc)
  
  # ===========================================================================
  # RETURN RESULTS
  # ===========================================================================
  
  results <- list(
    # Data info
    n_items = n_items,
    n_subjects = nrow(data_matrix),
    n_bootstrap = n_bootstrap,
    
    # Factor analysis results
    item_loadings = item_loadings,
    criterion_loading = criterion_loading,
    item_order = item_order,
    model_fit = fit_measures,
    
    # Validity analysis results
    validity_indices = validity_indices,
    validity_correlation = r_validity,
    validity_lm = fit_lm,
    
    # Incremental validity
    incremental_validity = burisch_results$deltabur,
    
    # Optimal item set
    optimal_n_items = optimal_n,
    optimal_items = optimal_items,
    selection_diagnostics = selection_diagnostics,
    
    # Test validity curves
    observed_validity = validity_results$medparc,
    expected_validity = validity_results$medparm,
    max_observed_validity = max_observed,
    max_observed_n = max_observed_n,
    
    # All validity results
    validity_by_n = data.frame(
      n_items = validity_results$nite,
      observed = validity_results$medparc,
      expected = validity_results$medparm,
      incremental = burisch_results$deltabur
    )
  )
  
  class(results) <- "OPTIVAL"
  return(results)
}
