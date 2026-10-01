# OPTIVAL SHINY APPLICATION - SERVER LOGIC
# ==============================================================================

library(shiny)
library(waiter)
library(DT)
library(plotly)
library(lavaan)

# Source OPTIVAL functions
# (These are already available when running as installed package)

server <- function(input, output, session) {
  
  # Reactive values to store results
  values <- reactiveValues(
    results = NULL,
    results_calculated = FALSE
  )
  
  # Run OPTIVAL analysis when button is clicked
  observeEvent(input$run_analysis, {
    req(input$data_file)
    
    # Create loading screen with waiter package
    waiter <- Waiter$new(
      html = tagList(
        spin_flower(),
        h3("Running OPTIVAL analysis...", style = "color: white; margin-top: 20px;"),
        h4("Please wait, this may take several minutes", style = "color: #cccccc; font-weight: normal;")
      ),
      color = "rgba(0, 0, 0, 0.85)"
    )
    waiter$show()
    
    tryCatch({
      # Read data file
      data_matrix <- read.table(
        input$data_file$datapath,
        header = input$has_header,
        sep = input$separator,
        stringsAsFactors = FALSE
      )
      
      # Convert to numeric matrix
      data_matrix <- as.matrix(data_matrix)
      if (!is.numeric(data_matrix)) {
        data_matrix <- apply(data_matrix, 2, as.numeric)
      }
      
      # Read precalibrated loadings if provided
      precalibrated_loadings <- NULL
      if (!is.null(input$loadings_file)) {
        # Read first two lines to detect header
        loadings_raw <- readLines(input$loadings_file$datapath, n = 2)
        
        # Try to convert first line to numeric - if it fails, its a header
        first_line_values <- strsplit(loadings_raw[1], "[,\\t ;]+")[[1]]
        first_line_numeric <- suppressWarnings(as.numeric(first_line_values))
        has_loadings_header <- any(is.na(first_line_numeric))
        
        # Read with detected header
        loadings_data <- read.table(
          input$loadings_file$datapath,
          header = has_loadings_header,
          sep = input$separator,
          stringsAsFactors = FALSE
        )
        
        # Convert to numeric vector
        precalibrated_loadings <- as.numeric(as.vector(unlist(loadings_data)))
        
        # Remove NAs
        precalibrated_loadings <- precalibrated_loadings[!is.na(precalibrated_loadings)]
        
        # Validate loadings
        if (length(precalibrated_loadings) == 0) {
          stop("The loadings file is empty or contains no valid numeric values")
        }
        
        # Check that loadings are in valid range
        if (any(precalibrated_loadings < 0) || any(precalibrated_loadings > 1.1)) {
          stop("Loadings must be between 0 and 1")
        }
        
        # Expected: ALL loadings including criterion
        expected_n_loadings <- ncol(data_matrix)
        if (length(precalibrated_loadings) != expected_n_loadings) {
          stop(sprintf("Expected %d loadings (items + criterion), but found %d. Include ALL loadings with criterion as last value.", 
                       expected_n_loadings, length(precalibrated_loadings)))
        }
      }
      
      # Run OPTIVAL analysis
      values$results <- OPTIVAL(
        data_matrix = data_matrix,
        precalibrated_loadings = precalibrated_loadings,
        n_bootstrap = input$n_bootstrap,
        criterion_loading_se = if (!is.null(precalibrated_loadings) &&
                                   isTRUE(input$criterion_se > 0)) input$criterion_se else NULL,
        verbose = FALSE
      )
      
      values$results_calculated <- TRUE
      
      waiter$hide()
      
      showNotification("Analysis completed successfully!", 
                       type = "message", 
                       duration = 3)
      
      # Automatic scroll to results after brief delay
      session$sendCustomMessage(type = "scrollToResults", message = list())
      
    }, error = function(e) {
      waiter$hide()
      showNotification(paste("Error:", e$message), 
                       type = "error", 
                       duration = 10)
    })
  })
  
  # Check if results are ready
  output$results_ready <- reactive({
    values$results_calculated
  })
  outputOptions(output, "results_ready", suspendWhenHidden = FALSE)
  
  # Summary UI rendering
  output$summary_ui <- renderUI({
    req(values$results)
    res <- values$results
    
    tagList(
      div(class = "summary-box",
          h4("General Information", style = "color: #2c3e50; margin-bottom: 12px; font-weight: 600;"),
          div(class = "summary-item",
              span(class = "summary-label", "Number of items:"),
              span(class = "summary-value", res$n_items)
          ),
          div(class = "summary-item",
              span(class = "summary-label", "Sample size:"),
              span(class = "summary-value", res$n_subjects)
          ),
          div(class = "summary-item",
              span(class = "summary-label", "Bootstrap replications:"),
              span(class = "summary-value", res$n_bootstrap)
          )
      ),
      
      div(class = "summary-box",
          h4("Preliminary Checks and Information", style = "color: #2c3e50; margin-bottom: 12px; font-weight: 600;"),
          div(class = "summary-item",
              span(class = "summary-label", "Correlation between item loadings and item validities:"),
              span(class = "summary-value", sprintf("r = %.4f", res$validity_correlation))
          ),
          div(class = "summary-item",
              span(class = "summary-label", "Criterion loading (theoretical validity ceiling):"),
              span(class = "summary-value",
                   if (is.na(res$criterion_loading_ci["lower"])) sprintf("λ = %.4f", res$criterion_loading)
                   else sprintf("λ = %.4f [%.4f, %.4f]", res$criterion_loading,
                                res$criterion_loading_ci["lower"], res$criterion_loading_ci["upper"]))
          )
      ),
      
      div(class = "summary-box",
          h4("Suggested Optimal Item Subset", style = "color: #2c3e50; margin-bottom: 12px; font-weight: 600;"),
          div(class = "summary-item",
              span(class = "summary-label", "Suggested optimal number of items:"),
              span(class = "summary-value", sprintf("%d (out of %d total)", 
                                                    res$optimal_n_items, res$n_items))
          ),
          div(class = "summary-item",
              span(class = "summary-label", "Selected items:"),
              span(class = "summary-value", paste(res$optimal_items, collapse = ", "))
          )
      ),
      
      div(class = "summary-box",
          h4("External Validity Results", style = "color: #2c3e50; margin-bottom: 12px; font-weight: 600;"),
          div(class = "summary-item",
              span(class = "summary-label", "Bootstrap empirical validity (optimal subset):"),
              span(class = "summary-value", 
                   sprintf("%.4f [%.4f, %.4f]", res$observed_validity[res$optimal_n_items],
                           res$observed_validity_ci[res$optimal_n_items, "lower"],
                           res$observed_validity_ci[res$optimal_n_items, "upper"]))
          ),
          div(class = "summary-item",
              span(class = "summary-label", "Expected validity (optimal subset):"),
              span(class = "summary-value", 
                   sprintf("%.4f", res$expected_validity[res$optimal_n_items]))
          ),
          div(class = "summary-item",
              span(class = "summary-label", "Maximum observed validity (full set):"),
              span(class = "summary-value", 
                   sprintf("%.4f", res$max_observed_validity))
          ),
          div(class = "summary-item",
              span(class = "summary-label", "Theoretical ceiling:"),
              span(class = "summary-value", sprintf("%.4f", res$criterion_loading))
          ),
          div(class = "summary-item",
              span(class = "summary-label", "Efficiency:"),
              span(class = "summary-value", 
                   sprintf("%.2f%%", 100 * res$max_observed_validity / res$criterion_loading))
          )
      ),
      
      if (!is.null(res$model_fit)) {
        div(class = "summary-box",
            h4("CFA Model Fit", style = "color: #2c3e50; margin-bottom: 12px; font-weight: 600;"),
            div(class = "summary-item",
                span(class = "summary-label", "χ²:"),
                span(class = "summary-value", 
                     sprintf("%.3f (df = %.0f, p = %.4f)", 
                             res$model_fit["chisq"], 
                             res$model_fit["df"],
                             res$model_fit["pvalue"]))
            ),
            div(class = "summary-item",
                span(class = "summary-label", "CFI:"),
                span(class = "summary-value", sprintf("%.3f", res$model_fit["cfi"]))
            ),
            div(class = "summary-item",
                span(class = "summary-label", "TLI:"),
                span(class = "summary-value", sprintf("%.3f", res$model_fit["tli"]))
            ),
            div(class = "summary-item",
                span(class = "summary-label", "RMSEA:"),
                span(class = "summary-value", 
                     sprintf("%.3f [%.3f, %.3f]", 
                             res$model_fit["rmsea"],
                             res$model_fit["rmsea.ci.lower"],
                             res$model_fit["rmsea.ci.upper"]))
            ),
            div(class = "summary-item",
                span(class = "summary-label", "SRMR:"),
                span(class = "summary-value", sprintf("%.3f", res$model_fit["srmr"]))
            )
        )
      }
    )
  })
  
  # Results table rendering
  output$results_table <- DT::renderDT({
    req(values$results)
    res <- values$results
    
    sel_order <- res$item_order  # índices originales en orden de entrada
    results_df <- data.frame(
      Rank = 1:res$n_items,
      Item = sel_order,
      Loading = round(res$item_loadings[sel_order], 4),
      `Validity (Bootstrap)` = round(res$validity_indices[sel_order], 4),
      `Validity CI` = sprintf("[%.3f, %.3f]", res$validity_indices_ci[sel_order, "lower"],
                              res$validity_indices_ci[sel_order, "upper"]),
      `Incremental Validity` = round(res$incremental_validity, 4),  # ya en orden de selección
      Selected = ifelse(sel_order %in% res$optimal_items, "Yes", "No"),
      check.names = FALSE
    )
    
    DT::datatable(
      results_df,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      class = "display nowrap"
    ) %>%
      DT::formatStyle(
        "Selected",
        target = "row",
        backgroundColor = DT::styleEqual(
          c("Yes", "No"),
          c("#d4edda", "white")
        )
      )
  })
  
  # ---------------------------------------------------------------------------
  # Interactive plots. Same visual encoding as plot.OPTIVAL(), so that the
  # figures produced in R and in the application look the same.
  # ---------------------------------------------------------------------------
  col <- list(observed = "#2a78d6", expected = "#eb6834", cutoff = "#1baf7a",
              unselected = "#d3d2cc", ink = "#2b2b29", ink2 = "#6b6a66",
              grid = "#e7e6e1", band = "rgba(42,120,214,0.16)")
  base_layout <- function(p, title, xtitle, ytitle, ...) {
    layout(p,
           title = list(text = paste0("<b>", title, "</b>"), x = 0.02, xanchor = "left",
                        font = list(size = 16, color = col$ink)),
           xaxis = list(title = xtitle, gridcolor = col$grid, zeroline = FALSE,
                        linecolor = "#b9b8b2", tickfont = list(color = col$ink2)),
           yaxis = list(title = ytitle, gridcolor = col$grid, zeroline = FALSE,
                        linecolor = "#b9b8b2", tickfont = list(color = col$ink2)),
           font = list(family = "Arial, Helvetica, sans-serif", color = col$ink),
           plot_bgcolor = "white", paper_bgcolor = "white", ...)
  }
  hi_res <- function(p, name) {
    config(p, toImageButtonOptions = list(format = "png", filename = name,
                                          width = 1050, height = 650, scale = 4))
  }
  
  # Plot 1: Item Validity
  output$plot_item_validity <- renderPlotly({
    req(values$results)
    res <- values$results
    sel <- 1:res$n_items %in% res$optimal_items
    ord <- order(res$item_loadings)
    hover <- paste0("Item ", 1:res$n_items,
                    "<br>Loading: ", sprintf("%.3f", res$item_loadings),
                    "<br>Validity: ", sprintf("%.3f [%.3f, %.3f]", res$validity_indices,
                                              res$validity_indices_ci[, "lower"],
                                              res$validity_indices_ci[, "upper"]))
    plot_ly() %>%
      add_trace(x = res$item_loadings[ord], y = fitted(res$validity_lm)[ord],
                type = "scatter", mode = "lines", hoverinfo = "skip",
                line = list(color = col$ink2, width = 2, dash = "dash"),
                name = sprintf("Linear fit (r = %.3f)", res$validity_correlation)) %>%
      add_trace(x = res$item_loadings[!sel], y = res$validity_indices[!sel],
                type = "scatter", mode = "markers", name = "Not selected",
                text = hover[!sel], hoverinfo = "text",
                marker = list(size = 12, color = col$unselected,
                              line = list(color = "white", width = 1.5))) %>%
      add_trace(x = res$item_loadings[sel], y = res$validity_indices[sel],
                type = "scatter", mode = "markers", name = "Selected",
                text = hover[sel], hoverinfo = "text",
                marker = list(size = 12, color = col$observed,
                              line = list(color = "white", width = 1.5))) %>%
      base_layout("Item loadings vs. item validity", "Item factor loading (&#955;<sub>j</sub>)",
                  "Item validity (bootstrap)",
                  legend = list(x = 0.02, y = 0.98, bgcolor = "rgba(255,255,255,0.8)",
                                traceorder = "reversed")) %>%
      hi_res("optival_item_validity")
  })
  
  # Plot 2: Incremental Validity
  output$plot_incremental <- renderPlotly({
    req(values$results)
    res <- values$results
    n <- res$n_items; k <- res$optimal_n_items; d <- res$incremental_validity
    plot_ly() %>%
      add_trace(x = 1:n, y = d, type = "scatter", mode = "lines+markers",
                fill = "tozeroy", fillcolor = "rgba(42,120,214,0.10)",
                line = list(color = col$observed, width = 2.5),
                marker = list(size = 9, color = ifelse(1:n <= k, col$observed, "white"),
                              line = list(color = col$observed, width = 1.5)),
                text = paste0("Item in position ", 1:n, "<br>&#916; = ", sprintf("%.4f", d)),
                hoverinfo = "text", name = "Expected increment") %>%
      base_layout("Incremental validity", "Number of items (order of entry)",
                  "Expected increment (&#916;)", showlegend = FALSE,
                  shapes = list(list(type = "line", x0 = k, x1 = k, yref = "paper", y0 = 0, y1 = 1,
                                     line = list(color = col$cutoff, width = 2, dash = "dash"))),
                  annotations = list(list(x = k, y = max(d) * 0.95, xanchor = "left",
                                          text = sprintf("  <b>optimal n = %d</b>", k),
                                          showarrow = FALSE, font = list(size = 12, color = col$ink)))) %>%
      layout(xaxis = list(range = c(0.5, n + 0.5), dtick = if (n <= 25) 1 else NULL)) %>%
      hi_res("optival_incremental_validity")
  })
  
  # Plot 3: Test Validity Curves
  output$plot_validity_curves <- renderPlotly({
    req(values$results)
    res <- values$results
    n <- res$n_items; k <- res$optimal_n_items
    obs <- res$observed_validity; ex <- res$expected_validity
    ci <- res$observed_validity_ci
    lev <- attr(ci, "ci_level"); if (is.null(lev)) lev <- 0.95
    plot_ly() %>%
      add_trace(x = c(1:n, n:1), y = c(ci[, "lower"], rev(ci[, "upper"])),
                type = "scatter", mode = "lines", fill = "toself", fillcolor = col$band,
                line = list(width = 0), hoverinfo = "skip",
                name = sprintf("%g%% bootstrap interval", 100 * lev)) %>%
      add_trace(x = 1:n, y = ex, type = "scatter", mode = "lines+markers",
                name = "Expected (model)",
                line = list(color = col$expected, width = 2.5, dash = "dash"),
                marker = list(size = 8, color = "white", line = list(color = col$expected, width = 2)),
                text = paste0("n = ", 1:n, "<br>Expected: ", sprintf("%.3f", ex)),
                hoverinfo = "text") %>%
      add_trace(x = 1:n, y = obs, type = "scatter", mode = "lines+markers",
                name = "Observed (bootstrap)",
                line = list(color = col$observed, width = 2.5),
                marker = list(size = 9, color = col$observed, line = list(color = "white", width = 1.5)),
                text = paste0("n = ", 1:n, "<br>Observed: ",
                              sprintf("%.3f [%.3f, %.3f]", obs, ci[, "lower"], ci[, "upper"])),
                hoverinfo = "text") %>%
      base_layout("Test validity curves: observed vs. expected", "Number of items", "Test validity",
                  legend = list(x = 0.98, xanchor = "right", y = 0.05, yanchor = "bottom",
                                bgcolor = "rgba(255,255,255,0.8)", traceorder = "reversed"),
                  shapes = list(
                    list(type = "line", x0 = k, x1 = k, yref = "paper", y0 = 0, y1 = 1,
                         line = list(color = col$cutoff, width = 2, dash = "dash")),
                    list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                         y0 = res$criterion_loading, y1 = res$criterion_loading,
                         line = list(color = col$ink, width = 1.5, dash = "dot"))),
                  annotations = list(
                    list(xref = "paper", x = 0.01, xanchor = "left", y = res$criterion_loading,
                         yanchor = "bottom", showarrow = FALSE,
                         text = sprintf("Theoretical ceiling (&#955;<sub>y</sub> = %.3f)", res$criterion_loading),
                         font = list(size = 12, color = col$ink)),
                    list(x = k, yref = "paper", y = 1, xanchor = "left", yanchor = "top",
                         showarrow = FALSE, text = sprintf("  <b>optimal n = %d</b>", k),
                         font = list(size = 12, color = col$ink)))) %>%
      layout(xaxis = list(range = c(0.5, n + 0.5), dtick = if (n <= 25) 1 else NULL)) %>%
      hi_res("optival_validity_curves")
  })
}


