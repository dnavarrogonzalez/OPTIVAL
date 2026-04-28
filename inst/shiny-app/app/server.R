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
              span(class = "summary-value", sprintf("λ = %.4f", res$criterion_loading))
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
                   sprintf("%.4f", res$observed_validity[res$optimal_n_items]))
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
    
    results_df <- data.frame(
      Item = 1:res$n_items,
      Loading = round(res$item_loadings, 4),
      `Validity (Bootstrap)` = round(res$validity_indices, 4),
      `Incremental Validity` = round(res$incremental_validity, 4),
      Order = match(1:res$n_items, res$item_order),
      Selected = ifelse(1:res$n_items %in% res$optimal_items, "Yes", "No"),
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
  
  # Plot 1: Item Validity
  output$plot_item_validity <- renderPlotly({
    req(values$results)
    res <- values$results
    
    p <- plot_ly() %>%
      add_trace(
        x = res$item_loadings,
        y = res$validity_indices,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 10,
          color = ifelse(1:res$n_items %in% res$optimal_items, "#27ae60", "#95a5a6"),
          line = list(color = "#34495e", width = 1)
        ),
        text = paste("Item:", 1:res$n_items,
                     "<br>Loading:", round(res$item_loadings, 4),
                     "<br>Validity:", round(res$validity_indices, 4),
                     "<br>Selected:", ifelse(1:res$n_items %in% res$optimal_items, "Yes", "No")),
        hoverinfo = "text",
        name = "Items"
      ) %>%
      add_trace(
        x = res$item_loadings,
        y = fitted(res$validity_lm),
        type = "scatter",
        mode = "lines",
        line = list(color = "#3498db", width = 2, dash = "dash"),
        name = sprintf("Linear fit (r = %.3f)", res$validity_correlation),
        hoverinfo = "skip"
      ) %>%
      layout(
        title = list(
          text = "<b>Item Factor Loadings vs. Bootstrap Validity Indices</b>",
          font = list(size = 16, color = "#2c3e50")
        ),
        xaxis = list(
          title = "Factor Loading",
          titlefont = list(size = 13, color = "#2c3e50"),
          gridcolor = "#ecf0f1",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Item-Criterion Validity",
          titlefont = list(size = 13, color = "#2c3e50"),
          gridcolor = "#ecf0f1",
          zeroline = FALSE
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        showlegend = TRUE,
        legend = list(x = 0.02, y = 0.98, font = list(size = 11))
      )
    
    p
  })
  
  # Plot 2: Incremental Validity
  output$plot_incremental <- renderPlotly({
    req(values$results)
    res <- values$results
    
    p <- plot_ly() %>%
      add_trace(
        x = 1:res$n_items,
        y = res$incremental_validity,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#2c3e50", width = 2),
        marker = list(
          size = 8,
          color = "#2c3e50",
          line = list(color = "#34495e", width = 1)
        ),
        name = "Incremental validity",
        text = paste("N items:", 1:res$n_items,
                     "<br>Δ validity:", round(res$incremental_validity, 4)),
        hoverinfo = "text"
      ) %>%
      add_trace(
        x = c(0, res$n_items + 1),
        y = c(0, 0),
        type = "scatter",
        mode = "lines",
        line = list(color = "#95a5a6", width = 1.5, dash = "dot"),
        name = "Zero line",
        hoverinfo = "skip"
      ) %>%
      layout(
        title = list(
          text = "<b>Item Incremental Validity</b>",
          font = list(size = 16, color = "#2c3e50")
        ),
        xaxis = list(
          title = "Number of Items in Test",
          titlefont = list(size = 13, color = "#2c3e50"),
          gridcolor = "#ecf0f1",
          zeroline = FALSE,
          range = c(0.5, res$n_items + 0.5)
        ),
        yaxis = list(
          title = "Expected Incremental Validity (Δ)",
          titlefont = list(size = 13, color = "#2c3e50"),
          gridcolor = "#ecf0f1",
          zeroline = TRUE,
          zerolinecolor = "#95a5a6",
          zerolinewidth = 1
        ),
        shapes = list(
          list(
            type = "line",
            x0 = res$optimal_n_items,
            x1 = res$optimal_n_items,
            y0 = min(res$incremental_validity) - 0.01,
            y1 = max(res$incremental_validity) + 0.01,
            line = list(
              color = "#27ae60",
              width = 2,
              dash = "dash"
            )
          )
        ),
        annotations = list(
          list(
            x = res$optimal_n_items,
            y = max(res$incremental_validity),
            text = sprintf("Optimal n = %d", res$optimal_n_items),
            showarrow = TRUE,
            arrowhead = 2,
            arrowsize = 1,
            arrowcolor = "#27ae60",
            ax = 40,
            ay = -40,
            font = list(size = 11, color = "#27ae60"),
            bgcolor = "rgba(255,255,255,0.9)",
            bordercolor = "#27ae60",
            borderwidth = 1,
            borderpad = 6
          )
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        showlegend = TRUE,
        legend = list(x = 0.7, y = 0.95, font = list(size = 11))
      )
    
    p
  })
  
  # Plot 3: Test Validity Curves
  output$plot_validity_curves <- renderPlotly({
    req(values$results)
    res <- values$results
    
    p <- plot_ly() %>%
      add_trace(
        x = 1:res$n_items,
        y = res$observed_validity,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#2c3e50", width = 2),
        marker = list(size = 7, color = "#2c3e50"),
        name = "Observed (Bootstrap)",
        text = paste("N items:", 1:res$n_items,
                     "<br>Observed:", round(res$observed_validity, 4)),
        hoverinfo = "text"
      ) %>%
      add_trace(
        x = 1:res$n_items,
        y = res$expected_validity,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#7f8c8d", width = 2, dash = "dash"),
        marker = list(size = 7, color = "#7f8c8d"),
        name = "Expected (Model)",
        text = paste("N items:", 1:res$n_items,
                     "<br>Expected:", round(res$expected_validity, 4)),
        hoverinfo = "text"
      ) %>%
      layout(
        title = list(
          text = "<b>Test Validity Curves: Observed vs Expected</b>",
          font = list(size = 16, color = "#2c3e50")
        ),
        xaxis = list(
          title = "Number of Items in Test",
          titlefont = list(size = 13, color = "#2c3e50"),
          gridcolor = "#ecf0f1",
          zeroline = FALSE,
          range = c(0.5, res$n_items + 0.5)
        ),
        yaxis = list(
          title = "Test Validity",
          titlefont = list(size = 13, color = "#2c3e50"),
          gridcolor = "#ecf0f1",
          zeroline = FALSE,
          range = c(0, max(c(res$observed_validity, 
                             res$expected_validity, 
                             res$criterion_loading)) * 1.1)
        ),
        shapes = list(
          # Optimal n line
          list(
            type = "line",
            x0 = res$optimal_n_items,
            x1 = res$optimal_n_items,
            y0 = 0,
            y1 = max(c(res$observed_validity, res$expected_validity, res$criterion_loading)) * 1.1,
            line = list(
              color = "#27ae60",
              width = 2,
              dash = "dash"
            )
          ),
          # Theoretical ceiling line
          list(
            type = "line",
            x0 = 0,
            x1 = res$n_items + 1,
            y0 = res$criterion_loading,
            y1 = res$criterion_loading,
            line = list(
              color = "#e74c3c",
              width = 1.5,
              dash = "dot"
            )
          )
        ),
        annotations = list(
          list(
            x = res$optimal_n_items,
            y = 0.05,
            text = sprintf("Optimal n = %d", res$optimal_n_items),
            showarrow = FALSE,
            font = list(size = 10, color = "#27ae60"),
            bgcolor = "rgba(255,255,255,0.9)",
            bordercolor = "#27ae60",
            borderwidth = 1,
            borderpad = 5
          ),
          list(
            x = res$n_items * 0.7,
            y = res$criterion_loading + 0.02,
            text = sprintf("Theoretical ceiling = %.4f", res$criterion_loading),
            showarrow = FALSE,
            font = list(size = 10, color = "#e74c3c"),
            bgcolor = "rgba(255,255,255,0.9)",
            bordercolor = "#e74c3c",
            borderwidth = 1,
            borderpad = 5
          )
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        showlegend = TRUE,
        legend = list(x = 0.65, y = 0.15, font = list(size = 11))
      )
    
    p
  })
}


