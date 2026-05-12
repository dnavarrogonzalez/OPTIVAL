#' Locate example datasets shipped with OPTIVAL
#'
#' Returns the path to one of the CSV files used in the worked examples 
#' of Navarro-González, Ferrando, & Morales-Vives (2026).
#'
#' @param which One of "example1", "example2", or "example2_loadings".
#' @return Absolute path to the requested CSV file.
#' @export
#' @examples
#' \dontrun{
#' data <- read.csv(optival_example("example1"))
#' results <- OPTIVAL(as.matrix(data), n_bootstrap = 2000)
#' }
optival_example <- function(which = c("example1", "example2", "example2_loadings")) {
  which <- match.arg(which)
  file <- switch(which,
    example1          = "example1_simulated.csv",
    example2          = "example2_fiq_hs.csv",
    example2_loadings = "example2_loadings.csv"
  )
  system.file("extdata", file, package = "OPTIVAL", mustWork = TRUE)
}
