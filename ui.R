########################################################################################################################################
# Define the UI
########################################################################################################################################
source("global.R",  local = TRUE) # Load the global variables and packages

ui <- dashboardPage(
  dashboardHeader(title = "FluorCam Data Analysis Toolbox"),
  dashboardSidebar(
    # Empty sidebar since you're using sidebarLayout inside dashboardBody
  ),
  dashboardBody(
    # Move the CSS inside dashboardBody
    tags$head(
      tags$style(HTML("
        /* Dashboard background improvements */
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        
        /* Improve info boxes in Analysis Results */
        .info-box {
          background: white;
          padding: 20px;
          margin: 15px 0;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          border-left: 4px solid #3c8dbc;
        }
        
        /* Button improvements */
        .btn-block {
          margin-bottom: 15px;
          border-radius: 5px;
        }
        
        /* Panel heading improvements */
        .panel-heading {
          cursor: pointer;
          transition: background-color 0.3s ease;
        }
        .panel-heading:hover {
          background-color: #2c5282 !important;
        }
        
        /* Alert boxes improvements */
        .alert {
          border-radius: 6px;
          border: none;
          box-shadow: 0 1px 4px rgba(0,0,0,0.1);
        }
        
        /* Tab improvements */
        .nav-tabs {
          border-bottom: 2px solid #3c8dbc;
        }
        .nav-tabs > li.active > a {
          background-color: #3c8dbc !important;
          color: white !important;
          border-color: #3c8dbc !important;
        }
        
        /* Table improvements */
        .table {
          background: white;
          border-radius: 5px;
          overflow: hidden;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        /* FIXED: Plot container improvements */
        #plot_result {
          background: white;
          padding: 20px;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          margin: 15px 0;
          overflow: hidden;  /* Prevents plot from extending beyond container */
          width: 100%;
          box-sizing: border-box;  /* Includes padding in width calculation */
        }
        
        /* Ensure plot fits within container */
        #plot_result .shiny-plot-output {
          width: 100% !important;
          height: auto !important;
          max-width: 100%;
        }
        
        /* Fix plot image sizing */
        #plot_result img {
          max-width: 100%;
          height: auto;
          display: block;
          margin: 0 auto;
        }
        
        /* Input field improvements */
        .form-control {
          border-radius: 4px;
          border: 1px solid #ddd;
          transition: border-color 0.3s ease;
        }
        .form-control:focus {
          border-color: #3c8dbc;
          box-shadow: 0 0 0 0.2rem rgba(60, 141, 188, 0.25);
        }
      "))
    ),
    
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          # Wrap all panels in an accordion container
          div(class = "panel-group", id = "accordion",
            
            # Section 1: File Name Configuration
            div(class = "panel panel-primary",
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse",
                              `data-parent` = "#accordion",  # Add this for accordion behavior
                              href = "#collapse1",
                              icon("tag"), " 1. File Name Configuration"))
                ),
                div(id = "collapse1", class = "panel-collapse collapse in",
                    div(class = "panel-body",
                        # Enhanced naming pattern visibility
                        div(class = "alert alert-info", style = "margin-bottom: 15px;",
                            icon("info-circle"),
                            strong(" Required File Naming Pattern:"),
                            br(), br(),  # Double line break for more space
                            tags$code("VAR1_VAR2_VAR3.TXT", style = "font-size: 14px; background-color: #f8f9fa; padding: 15px;"),
                            br(), br(),  # Double line break for more space
                            em("Example: Day1_LineA_Plant001.TXT")
                        ),
                        # Variable mapping inputs
                        helpText("Define what each variable represents in your file names:"),
                        fluidRow(
                          column(4, 
                                 div(style = "text-align: center;",
                                     strong("VAR1"),
                                     textInput("var1", NULL, value = "Day", placeholder = "e.g., Day")
                                 )
                          ),
                          column(4, 
                                 div(style = "text-align: center;",
                                     strong("VAR2"),
                                     textInput("var2", NULL, value = "Line", placeholder = "e.g., Line")
                                 )
                          ),
                          column(4, 
                                 div(style = "text-align: center;",
                                     strong("VAR3"),
                                     textInput("var3", NULL, value = "PlantID", placeholder = "e.g., PlantID")
                                 )
                          )
                        ),
                        # Preview of expected filename
                        div(class = "well well-sm", style = "margin-top: 15px; background-color: #f0f8ff;",
                            strong("Expected filename example: "),
                            tags$span(id = "filename_preview", style = "font-family: monospace; color: #2c3e50;",
                                     "Day1_LineA_Plant001.TXT")
                        )
                    )
                )
            ),
            
            # Section 2: Data Loading
            div(class = "panel panel-primary",
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse", 
                              `data-parent` = "#accordion",  # Add this for accordion behavior
                              href = "#collapse2",
                              icon("database"), " 2. Load Data"))
                ),
                div(id = "collapse2", class = "panel-collapse collapse",
                    div(class = "panel-body",
                        shinyDirButton("dir", "Select Directory", 
                                       "Choose Folder", 
                                       icon = icon("folder-open"),
                                       class = "btn-primary btn-block"),
                        br(),
                        verbatimTextOutput("dirpath"),
                        textInput("pattern", "File Pattern", 
                                  value = ".TXT",
                                  placeholder = "e.g., .TXT, .csv"),
                        actionButton("show_all", "Toggle File List", 
                                     icon = icon("list"),
                                     class = "btn-info btn-sm"),
                        br(), br(),
                        actionButton("load", "Load Data", 
                                     icon = icon("play"),
                                     class = "btn-success btn-block")
                    )
                )
            ),
            
            # Section 3: Analysis Parameters
            div(class = "panel panel-primary",
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse", 
                              `data-parent` = "#accordion",  # Add this for accordion behavior
                              href = "#collapse3",
                              icon("chart-line"), " 3. Analysis Parameters"))
                ),
                div(id = "collapse3", class = "panel-collapse collapse",
                    div(class = "panel-body",
                        # Better graph type selection with icons
                        radioButtons("graph_type", 
                                     "Graph Type",
                                     choices = list(
                                       "Bar Plot" = "Bar plot",
                                       "Line Chart" = "Curve"
                                     ),
                                     selected = "Bar plot",
                                     inline = TRUE),
                        
                        uiOutput("rootSelect"),
                        uiOutput("columnSelect"),
                        uiOutput("editParamsBtn"),
                        
                        # Add help text and tooltips
                        div(
                          tags$label(
                            strong("Facet Variable"), 
                            span(": Choose how to split your data into separate panels", style = "font-weight: normal;"),
                            style = "margin-bottom: 10px; display: block;"
                          ),
                          selectInput("facet_var", NULL,  # Set label to NULL since we're using custom label above
                                      choices = c("var1", "var2"))
                        ),
                        
                        # Order controls
                        helpText("Drag to reorder:"),
                        uiOutput("var2_order_ui"), 
                        uiOutput("var1_order_ui"),
                        
                        # Color controls
                        helpText("Select colors:"),
                        uiOutput("dynamic_color_inputs"),
                        
                        hr(),
                        actionButton("start_analysis", "Start Analysis", 
                                     icon = icon("play"),
                                     class = "btn-warning btn-block")
                    )
                )
            ),
            
            # Section 4: Export Results
            div(class = "panel panel-primary",
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse",
                              `data-parent` = "#accordion",  # Add this for accordion behavior
                              href = "#collapse4",
                              icon("download"), " 4. Export Results"))
                ),
                div(id = "collapse4", class = "panel-collapse collapse",
                    div(class = "panel-body",
                        # Statistics download
                        strong("Statistical Results:"),
                        br(), br(),
                        downloadButton("download_parametric", 
                                       "Parametric Analysis", 
                                       icon = icon("file-excel"),
                                       class = "btn-success btn-block"),
                        br(),
                        downloadButton("download_non_parametric", 
                                       "Non-parametric Analysis", 
                                       icon = icon("file-excel"),
                                       class = "btn-warning btn-block"),
                        
                        hr(),
                        
                        # Plot download
                        strong("Save Plot"),
                        br(), br(),
                        fluidRow(
                          column(4, 
                            selectInput("file_format", "Format", 
                                        choices = list(
                                          "SVG (Vector)" = "svg",
                                          "PNG (High-res)" = "png", 
                                          "PDF (Print)" = "pdf"
                                        ))
                          ),
                          column(4,
                            selectInput("size_unit", "Unit", 
                                        choices = list(
                                          "Centimeters" = "cm",
                                          "Inches" = "in"
                                        ),
                                        selected = "cm")
                          ),
                          column(4,
                            numericInput("plot_width", "Width", 
                                         value = 20, min = 5, max = 50, step = 0.5)
                          )
                        ),
                        # Add dynamic label showing current unit
                        textOutput("width_label"),
                        br(),
                        downloadButton("save_plot", "Save Plot", 
                                       icon = icon("image"),
                                       class = "btn-primary btn-block")
                    )
                )
            )
          )
       ),
          
          # Main panel with organized tabs
          mainPanel(
            # Add tabs for better organization
            tabsetPanel(
              id = "main_tabs",  # Add this ID
              tabPanel("Data Overview", 
                       icon = icon("table"),
                       br(),
                       tableOutput("selected_files"),
                       hr(),
                       tableOutput("processed_data")
              ),
              tabPanel("Analysis Results", 
                       icon = icon("chart-line"),
                       br(),
                       fluidRow(
                         column(6, 
                           div(class = "info-box",
                             h4("Selected Variable"),
                             textOutput("selectedValue")
                           )
                         ),
                         column(6,
                           div(class = "info-box",
                             h4("Normality Test"),
                             verbatimTextOutput("normality_result")
                           )
                         )
                       ),
                       hr(),
                       plotOutput("plot_result", height = "600px")
              ),
              
              tabPanel("Help", 
                       icon = icon("question-circle"),
                       includeMarkdown("help.md")
              )
            )
          )
        )
      )
    )
  )