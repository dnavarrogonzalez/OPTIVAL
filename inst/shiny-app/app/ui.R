# ==============================================================================
# OPTIVAL SHINY APPLICATION - USER INTERFACE
# ==============================================================================

library(shiny)
library(waiter)
library(DT)
library(plotly)

ui <- fluidPage(
  useWaiter(),
  
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollToResults', function(message) {
        setTimeout(function() {
          var element = document.getElementById('results_anchor');
          if (element) {
            element.scrollIntoView({ behavior: 'smooth', block: 'start' });
          }
        }, 300);
      });
    ")),
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #fafafa;
      }
      .navbar {
        background: linear-gradient(to right, #2c3e50, #34495e);
        padding: 15px 0;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .navbar-brand {
        color: white !important;
        font-size: 22px;
        font-weight: 600;
        letter-spacing: 0.5px;
      }
      .main-container {
        padding-top: 30px;
        padding-bottom: 30px;
      }
      .input-panel {
        background-color: white;
        border-radius: 4px;
        padding: 25px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
        margin-bottom: 25px;
        border: 1px solid #e0e0e0;
      }
      .results-panel {
        background-color: white;
        border-radius: 4px;
        padding: 25px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
        margin-top: 25px;
        border: 1px solid #e0e0e0;
      }
      .section-title {
        color: #2c3e50;
        font-size: 18px;
        font-weight: 600;
        margin-bottom: 20px;
        border-bottom: 2px solid #34495e;
        padding-bottom: 8px;
      }
      .action-button {
        background: #34495e;
        color: white;
        border: none;
        padding: 10px 30px;
        font-size: 15px;
        font-weight: 500;
        border-radius: 3px;
        transition: all 0.2s ease;
      }
      .action-button:hover {
        background: #2c3e50;
        transform: translateY(-1px);
        box-shadow: 0 2px 4px rgba(0,0,0,0.15);
      }
      .nav-tabs {
        border-bottom: 1px solid #dee2e6;
      }
      .nav-tabs .nav-link {
        color: #495057;
        font-weight: 500;
        border: none;
        padding: 10px 20px;
      }
      .nav-tabs .nav-link.active {
        color: #2c3e50;
        background-color: transparent;
        border-bottom: 2px solid #34495e;
      }
      .summary-box {
        background: #f8f9fa;
        border-radius: 4px;
        padding: 18px;
        margin-bottom: 18px;
        border: 1px solid #e9ecef;
      }
      .summary-item {
        display: flex;
        justify-content: space-between;
        padding: 8px 0;
        border-bottom: 1px solid #e9ecef;
      }
      .summary-item:last-child {
        border-bottom: none;
      }
      .summary-label {
        font-weight: 500;
        color: #495057;
      }
      .summary-value {
        font-weight: 600;
        color: #2c3e50;
      }
      .info-box {
        background-color: #e7f3ff;
        border-left: 3px solid #1976d2;
        padding: 12px;
        margin: 15px 0;
        border-radius: 3px;
        font-size: 14px;
      }
      .warning-box {
        background-color: #fff3cd;
        border-left: 3px solid #ff9800;
        padding: 12px;
        margin: 15px 0;
        border-radius: 3px;
        font-size: 14px;
      }
    "))
  ),
  
  # Navbar
  div(class = "navbar",
      div(class = "container",
          span(class = "navbar-brand", "OPTIVAL - Optimal Test Validity Analysis")
      )
  ),
  
  # Main container
  div(class = "container main-container",
      
      # Input panel
      div(class = "input-panel",
          div(class = "section-title", "Analysis Configuration"),
          
          fluidRow(
            column(6,
                   fileInput("data_file", 
                             "Data Matrix (CSV/TXT)",
                             accept = c(".csv", ".txt"),
                             buttonLabel = "Browse...",
                             placeholder = "No file selected")
            ),
            column(6,
                   fileInput("loadings_file", 
                             "Precalibrated Loadings (optional, CSV/TXT)",
                             accept = c(".csv", ".txt"),
                             buttonLabel = "Browse...",
                             placeholder = "No file selected"),
                   numericInput("criterion_se",
                                "S.E. of the criterion loading (optional, only with precalibrated loadings)",
                                value = NA, min = 0, step = 0.001)
            )
          ),
          
          div(class = "info-box",
              p(strong("Data format:"), 
                "The matrix must have items in columns 1 to (n-1) and the external criterion variable in the last column. 
                Each row represents a subject/respondent."),
              p(strong("Precalibrated loadings (optional):"), 
                "If provided, the file must contain ", strong("ALL loadings including the criterion"), 
                " (items + criterion, with criterion as the last value). The file should be a vector (one column or one row) with numeric values between 0 and 1.")
          ),
          
          fluidRow(
            column(4,
                   numericInput("n_bootstrap", 
                                "Number of Bootstrap Replications",
                                value = 2000,
                                min = 500,
                                max = 10000,
                                step = 500)
            ),
            column(4,
                   checkboxInput("has_header", 
                                 "Data has header row",
                                 value = TRUE)
            ),
            column(4,
                   selectInput("separator", 
                               "Column Separator",
                               choices = c("Comma" = ",", 
                                           "Semicolon" = ";", 
                                           "Tab" = "\t",
                                           "Space" = " "),
                               selected = ",")
            )
          ),
          
          div(style = "text-align: center; margin-top: 25px;",
              actionButton("run_analysis", 
                           "Run OPTIVAL Analysis",
                           class = "action-button",
                           icon = icon("play-circle"))
          )
      ),
      
      # Results panel
      conditionalPanel(
        condition = "output.results_ready",
        
        # Anchor for automatic scroll
        tags$div(id = "results_anchor"),
        
        div(class = "results-panel",
            div(class = "section-title", "Analysis Results"),
            
            # Tabs for different outputs
            tabsetPanel(
              id = "results_tabs",
              type = "tabs",
              
              # Summary tab
              tabPanel("Summary",
                       br(),
                       uiOutput("summary_ui")
              ),
              
              # Table tab
              tabPanel("Results Table",
                       br(),
                       DT::dataTableOutput("results_table")
              ),
              
              # Item validity plot
              tabPanel("Plot 1: Item Validity",
                       br(),
                       plotlyOutput("plot_item_validity", height = "600px")
              ),
              
              # Incremental validity plot
              tabPanel("Plot 2: Incremental Validity",
                       br(),
                       plotlyOutput("plot_incremental", height = "600px")
              ),
              
              # Test validity curves plot
              tabPanel("Plot 3: Validity Curves",
                       br(),
                       plotlyOutput("plot_validity_curves", height = "600px")
              )
            )
        )
      )
  )
)
