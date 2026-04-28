# ==============================================================================
# SHINY APPLICATION LAUNCHER
# ==============================================================================

#' Launch OPTIVAL Interactive Shiny Application
#' 
#' Launches an interactive web application for performing OPTIVAL analyses
#' through a user-friendly graphical interface.
#' 
#' @details 
#' The Shiny application provides a point-and-click interface for users who
#' prefer not to use R programming directly. Features include:
#' \itemize{
#'   \item Data upload (CSV, TXT, or other delimited formats)
#'   \item Optional precalibrated loadings upload
#'   \item Configurable bootstrap replications
#'   \item Automatic file format detection
#'   \item Interactive plots with zoom and hover information
#'   \item Downloadable results tables
#'   \item Comprehensive summary statistics
#' }
#' 
#' @return No return value. Launches the Shiny application in the default
#'   web browser or RStudio Viewer pane.
#' 
#' @examples
#' \dontrun{
#' # Launch the application
#' library(OPTIVAL)
#' run_optival()
#' }
#' 
#' @export
#' @importFrom shiny runApp
run_optival <- function() {
  # Primary lookup: works when the package is properly installed
  app_dir <- system.file("shiny-app/app", package = "OPTIVAL")
  
  # Fallback: works during development (devtools::load_all() or direct source())
  if (app_dir == "" || !dir.exists(app_dir)) {
    # Walk up from this file's location to find the package root
    this_file <- tryCatch(
      normalizePath(sys.frames()[[1]]$ofile),
      error = function(e) NULL
    )
    if (!is.null(this_file)) {
      pkg_root <- dirname(dirname(this_file))  # R/ -> package root
    } else {
      pkg_root <- getwd()
    }
    app_dir <- file.path(pkg_root, "inst", "shiny-app", "app")
  }
  
  if (!dir.exists(app_dir)) {
    stop(paste0(
      "Could not find the Shiny app directory.\n",
      "  Searched: ", app_dir, "\n",
      "If running from source, make sure your working directory is inside ",
      "the OPTIVAL package folder, or install the package first:\n",
      "  devtools::install('path/to/OPTIVAL-main')\n",
      "  library(OPTIVAL)\n",
      "  run_optival()"
    ))
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}
