# ==============================================================================
# BOOTSTRAP FUNCTIONS FOR OPTIVAL
# ==============================================================================
# These functions implement bootstrap procedures for empirical validation
# of model-based predictions and estimation of validity indices.
# ==============================================================================

#' Bootstrap Sampling
#' 
#' Generates a bootstrap sample from a data matrix by sampling rows with 
#' replacement.
#' 
#' @param X Numeric matrix of data (rows = subjects, columns = variables)
#' @param nsam Integer, number of bootstrap samples to generate (typically 1)
#' 
#' @return A matrix with the same dimensions as X containing the bootstrap sample
#' 
#' @details This function implements standard bootstrap resampling by randomly
#' sampling rows (subjects) with replacement. When nsam > 1, only the last
#' bootstrap sample is returned.
#' 
#' @keywords internal
bootsam2 <- function(X, nsam) {
  N <- nrow(X)
  m <- ncol(X)
  
  RES <- array(0, dim = c(nsam, N, m))
  
  for (i in 1:nsam) {
    indices <- sample(1:N, N, replace = TRUE)
    Xb <- X[indices, ]
    RES[i, , ] <- Xb
  }
  
  Xb_b <- matrix(0, N, m)
  Xb_b <- RES[nsam, , ]
  
  return(Xb_b)
}

#' Bootstrap-Based Item Validity Indices
#' 
#' Computes bootstrap-based estimates of item-criterion validity coefficients,
#' together with their bootstrap standard errors and percentile confidence
#' intervals.
#' 
#' @param ZX Standardized matrix of item scores (N x m)
#' @param zy Standardized vector of criterion scores (N x 1)
#' @param lam Vector of item factor loadings (not used in current implementation
#'   but kept for compatibility)
#' @param nreplic Integer, number of bootstrap replications
#' @param ci_level Numeric in (0, 1), confidence level of the percentile
#'   intervals. Default is 0.95.
#' 
#' @return A list with four numeric vectors of length m:
#'   \item{mean}{Mean item-criterion correlation across bootstrap replications}
#'   \item{se}{Bootstrap standard error (SD across replications)}
#'   \item{lower}{Lower limit of the percentile confidence interval}
#'   \item{upper}{Upper limit of the percentile confidence interval}
#' 
#' @details This function estimates the item-criterion validity indices by:
#' 1. Generating bootstrap samples of the combined item and criterion data
#' 2. Computing item-criterion correlations in each bootstrap sample
#' 3. Averaging across replications to obtain stable estimates, and taking the
#'    SD and the empirical quantiles of the replications as the standard error
#'    and the percentile confidence interval
#' 
#' @keywords internal
compaboot <- function(ZX, zy, lam, nreplic, ci_level = 0.95) {
  n <- nrow(ZX)
  m <- ncol(ZX)
  TOTVAL <- matrix(0, nreplic, m)
  Ztot <- cbind(zy, ZX)
  
  for (i in 1:nreplic) {
    TMPZtot <- bootsam2(Ztot, 1)
    TMPR <- cor(TMPZtot)
    tmpcol <- TMPR[2:(m+1), 1]
    TOTVAL[i, ] <- tmpcol
  }
  
  alpha <- (1 - ci_level) / 2
  list(
    mean  = colMeans(TOTVAL),
    se    = apply(TOTVAL, 2, sd),
    lower = apply(TOTVAL, 2, quantile, probs = alpha, names = FALSE),
    upper = apply(TOTVAL, 2, quantile, probs = 1 - alpha, names = FALSE)
  )
}

#' Bootstrap Simulation of Test Validity Curves
#' 
#' Simulates the test validity curve (validity as a function of test length) 
#' using bootstrap resampling, and compares it with model-predicted values.
#' 
#' @param lam Vector of all factor loadings (items + criterion). The last 
#'   element is the criterion loading.
#' @param ordlam Integer vector specifying the order in which items should
#'   be added to the test
#' @param Ztot Standardized matrix containing all variables (items + criterion)
#' @param nreplic Integer, number of bootstrap replications
#' @param ci_level Numeric in (0, 1), confidence level of the percentile
#'   intervals for the observed curve. Default is 0.95.
#' 
#' @return A list with the following components:
#'   \item{nite}{Integer vector from 1 to m (number of items)}
#'   \item{medparc}{Numeric vector of mean observed (bootstrap) validity for 
#'     each test length}
#'   \item{medparm}{Numeric vector of expected (model-based) validity for 
#'     each test length}
#'   \item{separc}{Bootstrap standard error of the observed validity for each
#'     test length}
#'   \item{lowparc, upparc}{Limits of the percentile confidence interval of the
#'     observed validity for each test length}
#' 
#' @details This function:
#' 1. Computes the model-predicted validity curve based on factor loadings
#' 2. Generates bootstrap samples and computes empirical validity for each test length
#' 3. Averages bootstrap results to obtain stable empirical estimates
#' 4. Returns both observed and expected validity curves for comparison
#' 
#' The comparison between observed and expected curves provides evidence about
#' how well the model predictions match empirical data.
#' 
#' @keywords internal
simulboot <- function(lam, ordlam, Ztot, nreplic, ci_level = 0.95) {
  nit <- length(lam)
  ZX <- Ztot[, 1:(nit-1)]
  zy <- Ztot[, nit]
  
  n <- nrow(ZX)
  m <- ncol(ZX)
  
  nite <- 1:m
  TOTPARC <- matrix(0, nreplic, m)
  
  lamite <- lam[1:(nit-1)]
  lamy <- lam[nit]
  lamord <- lamite[ordlam]
  lamordfil <- lamord
  
  medparm <- numeric(m)
  
  # Compute model-predicted validity for each test length
  for (i in 1:m) {
    tmpnume1 <- lamordfil[1:i]
    tmpnume2 <- sum(tmpnume1)
    tmpnume <- lamy * tmpnume2
    
    TMPdeno1 <- outer(tmpnume1, tmpnume1)
    TMPdeno2 <- TMPdeno1 - diag(diag(TMPdeno1), nrow = nrow(TMPdeno1), ncol = ncol(TMPdeno1))
    TMPdeno3 <- TMPdeno2 + diag(i)
    
    tmpdeno <- sqrt(sum(TMPdeno3))
    medparm[i] <- tmpnume / tmpdeno
  }
  
  # Compute bootstrap-based empirical validity
  for (i in 1:nreplic) {
    TMPZtot <- bootsam2(Ztot, 1)
    TMPZX <- TMPZtot[, 1:(nit-1)]
    tmpzy <- TMPZtot[, nit]
    
    tmp1 <- increvalsim(TMPZX, tmpzy, ordlam, lam[1:(nit-1)], lam[nit])
    TOTPARC[i, ] <- tmp1
  }
  
  medparc <- colMeans(TOTPARC)
  alpha <- (1 - ci_level) / 2
  
  return(list(nite = nite, medparc = medparc, medparm = medparm,
              separc = apply(TOTPARC, 2, sd),
              lowparc = apply(TOTPARC, 2, quantile, probs = alpha, names = FALSE),
              upparc = apply(TOTPARC, 2, quantile, probs = 1 - alpha, names = FALSE)))
}
