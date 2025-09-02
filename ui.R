########################################################################################################################################
# Define the UI
########################################################################################################################################
# STRATEGY: Create a comprehensive Shiny dashboard for FluorCam data analysis
# - Uses dashboardPage layout for professional appearance
# - Implements collapsible accordion panels for organized workflow
# - Provides step-by-step guided analysis process
# - Supports both bar plot and curve analysis workflows

source("global.R",  local = TRUE) # Load the global variables and packages

ui <- dashboardPage(
  # HEADER: Simple title bar for the application
  dashboardHeader(
    title = div(
      class = "brand",
      tags$img(src = "fluorcam_toolbox_logo.png", alt = "FluorCam", class = "brand-logo"),
      span("FluorCam Toolbox", class = "brand-title")
    ),
    titleWidth = 360
  ),
  
  # SIDEBAR: Empty because we use sidebarLayout inside dashboardBody
  # STRATEGY: This allows more flexibility in layout while keeping dashboard styling
  dashboardSidebar(disable = TRUE),
  
  # MAIN DASHBOARD BODY
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Header brand */
        .main-header .logo {
          display: flex !important;
          align-items: center;
          padding: 0 15px;
        }
        .brand {
          display: flex;
          align-items: center;
          gap: 10px;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .brand-logo {
          max-height: 32px;   /* s'intègre bien dans un header de 50px */
          width: auto;
          display: block;
        }
        .brand-title {
          font-weight: 600;
          font-size: 18px;
          line-height: 1;
          letter-spacing: 0.2px;
          color: #fff; /* conserve le contraste du header AdminLTE */
        }
        /* Ajustements mobiles */
        @media (max-width: 480px) {
          .brand-logo { max-height: 24px; }
          .brand-title { font-size: 16px; }
        }
        
        /* Dashboard background improvements */
        /* STRATEGY: Light background for better readability */
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        
        /* Improve info boxes in Analysis Results */
        /* STRATEGY: Card-like design for better information hierarchy */
        .info-box {
          background: white;
          padding: 20px;
          margin: 15px 0;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          border-left: 4px solid #3c8dbc;
        }
        
        /* Button improvements */
        /* STRATEGY: Consistent button spacing and rounded corners */
        .btn-block {
          margin-bottom: 15px;
          border-radius: 5px;
        }
        
        /* Panel heading improvements */
        /* STRATEGY: Interactive hover effects to indicate clickable accordion panels */
        .panel-heading {
          cursor: pointer;
          transition: background-color 0.3s ease;
        }
        .panel-heading:hover {
          background-color: #2c5282 !important;
        }
        
        /* Alert boxes improvements */
        /* STRATEGY: Modern alert design without harsh borders */
        .alert {
          border-radius: 6px;
          border: none;
          box-shadow: 0 1px 4px rgba(0,0,0,0.1);
        }
        
        /* Tab improvements */
        /* STRATEGY: Branded tab design matching overall color scheme */
        .nav-tabs {
          border-bottom: 2px solid #3c8dbc;
        }
        .nav-tabs > li.active > a {
          background-color: #3c8dbc !important;
          color: white !important;
          border-color: #3c8dbc !important;
        }
        
        /* Table improvements */
        /* STRATEGY: Clean table design with subtle shadows */
        .table {
          background: white;
          border-radius: 5px;
          overflow: hidden;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        /* FIXED: Plot container improvements */
        /* STRATEGY: Solve plot overflow issues and ensure responsive design */
        /* PROBLEM SOLVED: Plots were extending beyond container boundaries */
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
        /* STRATEGY: Modern input styling with focus effects */
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
    
    # MAIN LAYOUT STRUCTURE
    # STRATEGY: Use fluidPage with sidebarLayout for responsive design
    # - Sidebar for controls and parameters
    # - Main panel for results and data display
    fluidPage(
      sidebarLayout(
        
        # ===========================================
        # SIDEBAR PANEL: ALL USER CONTROLS
        # ===========================================
        # STRATEGY: Organized as step-by-step workflow using accordion panels
        # - Guides users through logical analysis sequence
        # - Collapsible panels reduce visual clutter
        # - Each section builds on the previous one
        sidebarPanel(
          
          # ACCORDION CONTAINER
          # STRATEGY: Bootstrap accordion for organized workflow
          # BENEFIT: Users can focus on one step at a time
          div(class = "panel-group", id = "accordion",
            
            # ===========================================
            # SECTION 1: FILE NAME CONFIGURATION
            # ===========================================
            # PURPOSE: Define the naming pattern for data files
            # STRATEGY: Set up variable mapping before loading data
            # WHY FIRST: File naming pattern must be understood before data loading
            div(class = "panel panel-primary",
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse",
                              `data-parent` = "#accordion",  # Accordion behavior
                              href = "#collapse1",
                              icon("tag"), " 1. File Name Configuration"))
                ),
                # START EXPANDED: This is the first step users need to complete
                div(id = "collapse1", class = "panel-collapse collapse in",
                    div(class = "panel-body",
                        
                        # CLEAR INSTRUCTIONS
                        # STRATEGY: Prominent alert box explains required format
                        # BENEFIT: Reduces user confusion about file naming
                        div(class = "alert alert-info", style = "margin-bottom: 15px;",
                            icon("info-circle"),
                            strong(" Required File Naming Pattern:"),
                            br(), br(),  # Extra spacing for readability
                            tags$code("VAR1_VAR2_VAR3.TXT", style = "font-size: 14px; background-color: #f8f9fa; padding: 15px;"),
                            br(), br(),
                            em("Example: Day1_LineA_Plant001.TXT")
                        ),
                        
                        # VARIABLE MAPPING INPUTS
                        # STRATEGY: Three-column layout for clear variable definition
                        # PURPOSE: Map generic VAR1/VAR2/VAR3 to actual experiment variables
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
                        
                        # FILENAME PREVIEW
                        # STRATEGY: Real-time preview helps users verify their setup
                        # BENEFIT: Immediate feedback prevents file loading errors
                        div(class = "well well-sm", style = "margin-top: 15px; background-color: #f0f8ff;",
                            strong("Expected filename example: "),
                            tags$span(id = "filename_preview", style = "font-family: monospace; color: #2c3e50;",
                                     "Day1_LineA_Plant001.TXT")
                        )
                    )
                )
            ),
            
            # ===========================================
            # SECTION 2: DATA LOADING
            # ===========================================
            # PURPOSE: Load and preview data files
            # STRATEGY: Directory selection → file pattern → preview → load
            # WHY SECOND: Can only load data after understanding file naming
            div(class = "panel panel-primary",
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse", 
                              `data-parent` = "#accordion",
                              href = "#collapse2",
                              icon("database"), " 2. Load Data"))
                ),
                # COLLAPSED BY DEFAULT: Users complete Section 1 first
                div(id = "collapse2", class = "panel-collapse collapse",
                    div(class = "panel-body",
                        
                        # DIRECTORY SELECTION
                        # STRATEGY: Large, prominent button for main action
                        shinyDirButton("dir", "Select Directory", 
                                       "Choose Folder", 
                                       icon = icon("folder-open"),
                                       class = "btn-primary btn-block"),
                        br(),
                        # FEEDBACK: Show selected directory path
                        verbatimTextOutput("dirpath"),
                        
                        # FILE PATTERN INPUT
                        # PURPOSE: Allow different file extensions (.TXT, .csv, etc.)
                        textInput("pattern", "File Pattern", 
                                  value = ".TXT",
                                  placeholder = "e.g., .TXT, .csv"),
                        
                        # FILE PREVIEW TOGGLE
                        # STRATEGY: Optional file list preview before loading
                        # BENEFIT: Users can verify correct files will be loaded
                        actionButton("show_all", "Toggle File List", 
                                     icon = icon("list"),
                                     class = "btn-info btn-sm"),
                        br(), br(),
                        
                        # MAIN LOAD ACTION
                        # STRATEGY: Prominent success-colored button for final action
                        actionButton("load", "Load Data", 
                                     icon = icon("play"),
                                     class = "btn-success btn-block")
                    )
                )
            ),
            
            # ===========================================
            # SECTION 3: ANALYSIS PARAMETERS
            # ===========================================
            # PURPOSE: Configure all analysis settings
            # STRATEGY: Comprehensive parameter setup in logical order
            # WHY THIRD: Requires loaded data to populate options
            div(class = "panel panel-primary",
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse", 
                              `data-parent` = "#accordion",
                              href = "#collapse3",
                              icon("chart-line"), " 3. Analysis Parameters"))
                ),
                div(id = "collapse3", class = "panel-collapse collapse",
                    div(class = "panel-body",
                        
                        # ANALYSIS TYPE SELECTION
                        # STRATEGY: Radio buttons for mutually exclusive choices
                        # PURPOSE: Determines entire analysis workflow
                        radioButtons("graph_type", 
                                     "Graph Type",
                                     choices = list(
                                       "Bar Plot" = "Bar plot",      # For group comparisons
                                       "Line Chart" = "Curve"        # For time course/trends
                                     ),
                                     selected = "Bar plot",
                                     inline = TRUE),
                        
                        # DYNAMIC UI OUTPUTS
                        # STRATEGY: Server-generated UI based on loaded data
                        # BENEFIT: Options automatically populate from actual data
                        uiOutput("rootSelect"),      # Variable selection
                        uiOutput("columnSelect"),    # Parameter column selection
                        uiOutput("editParamsBtn"),   # Time parameters (Line Chart only)
                        
                        # FACET VARIABLE SELECTION
                        # PURPOSE: How to split data into multiple panels
                        # STRATEGY: Clear labeling with explanation
                        div(
                          tags$label(
                            strong("Facet Variable"), 
                            span(": Choose how to split your data into separate panels", style = "font-weight: normal;"),
                            style = "margin-bottom: 10px; display: block;"
                          ),
                          selectInput("facet_var", NULL,
                                      choices = c("var1", "var2"))
                        ),
                        
                        # GROUP ORDERING CONTROLS
                        # STRATEGY: Drag-and-drop interface for intuitive reordering
                        # PURPOSE: Control order of groups in plots and legends
                        helpText("Drag to reorder:"),
                        uiOutput("var2_order_ui"), 
                        uiOutput("var1_order_ui"),
                        
                        # CONTROL GROUP SELECTION
                        # STRATEGY: Conditional display - only for Line Chart analysis
                        # PURPOSE: Statistical comparisons need a reference group
                        # WHY CONDITIONAL: Bar plots don't use control groups
                        conditionalPanel(
                          condition = "input.graph_type == 'Curve'",
                          div(
                            tags$label(
                              strong("Control Group"),
                              span(": Select the control group for statistical analysis", style = "font-weight: normal;"),
                              style = "margin-bottom: 10px; display: block;"
                            ),
                            uiOutput("control_group_ui")
                          )
                        ),
                        hr(),

                        # COLOR CUSTOMIZATION
                        # STRATEGY: Dynamic color inputs based on number of groups
                        # PURPOSE: Allow custom color schemes for better visualization
                        div(
                          tags$label(
                            strong("Select colors:"),
                            style = "margin-bottom: 10px; display: block;"
                          )
                        ),
                        uiOutput("dynamic_color_inputs"),
                        hr(),
                        
                        # ANALYSIS EXECUTION
                        # STRATEGY: Warning-colored button indicates significant action
                        # PURPOSE: Execute the configured analysis
                        actionButton("start_analysis", "Start Analysis", 
                                     icon = icon("play"),
                                     class = "btn-warning btn-block")
                    )
                )
            ),
            
            # ===========================================
            # SECTION 4: EXPORT RESULTS
            # ===========================================
            # PURPOSE: Save analysis results and plots
            # STRATEGY: Separate exports for data and plots with full customization
            # WHY LAST: Only available after analysis is complete
            div(class = "panel panel-primary",
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse",
                              `data-parent` = "#accordion",
                              href = "#collapse4",
                              icon("download"), " 4. Export Results"))
                ),
                div(id = "collapse4", class = "panel-collapse collapse",
                    div(class = "panel-body",
                        
                        # STATISTICAL DATA EXPORT
                        # STRATEGY: .zip format with multiple .txt files
                        # BENEFIT: Organized, readable, and universally compatible
                        strong("Statistical Results:"),
                        br(), br(),
                        textInput("stats_filename", "Filename (without extension)", 
                                  value = "statistical_results", 
                                  placeholder = "Enter filename"),
                        br(),
                        downloadButton("download_stats", "Download Statistical Data", 
                                       class = "btn-primary", icon = icon("download")),
                        
                        hr(),
                        
                        # PLOT EXPORT SECTION
                        # STRATEGY: Comprehensive customization options
                        # PURPOSE: Professional-quality plot output
                        strong("Export Plot:"),
                        br(), br(),
                        
                        # FILENAME AND FORMAT
                        fluidRow(
                          column(6, 
                            textInput("plot_filename", "Filename", 
                                      value = "plot", 
                                      placeholder = "Enter filename")
                          ),
                          column(6,
                            # MULTIPLE FORMAT OPTIONS
                            # STRATEGY: Different formats for different use cases
                            selectInput("plot_format", "Format", 
                                        choices = list(
                                          "PNG (High-res)" = "png",    # General use
                                          "PDF (Print)" = "pdf",       # Publications
                                          "SVG (Vector)" = "svg",      # Scalable graphics
                                          "JPEG" = "jpg"               # Compressed
                                        ),
                                        selected = "png")
                          )
                        ),
                        
                        # DIMENSIONS AND UNITS
                        # STRATEGY: Scientific units (cm) by default, with alternatives
                        # PURPOSE: Professional control over plot sizing
                        fluidRow(
                          column(4,
                            numericInput("plot_width", "Width", 
                                         value = 30, min = 10, max = 50, step = 1)
                          ),
                          column(4,
                            numericInput("plot_height", "Height", 
                                         value = 20, min = 5, max = 40, step = 1)
                          ),
                          column(4,
                            # SCIENTIFIC UNITS BY DEFAULT
                            selectInput("plot_units", "Units",
                                        choices = list(
                                          "Centimeters" = "cm",    # Scientific standard
                                          "Inches" = "in",         # US standard
                                          "Millimeters" = "mm"     # Precision work
                                        ),
                                        selected = "cm")
                          )
                        ),
                        br(),
                        
                        # EXPORT ACTION
                        downloadButton("download_plot", "Download Plot", 
                                       class = "btn-success", icon = icon("image"))
                    )
                )
            )
          )
       ),
          
          # ===========================================
          # MAIN PANEL: RESULTS AND DATA DISPLAY
          # ===========================================
          # STRATEGY: Tabbed interface for organized information display
          # PURPOSE: Separate data overview, results, and help information
          mainPanel(
            
            # TABBED INTERFACE
            # STRATEGY: Logical separation of different information types
            # BENEFIT: Reduces cognitive load and improves navigation
            tabsetPanel(
              id = "main_tabs",
              
              # ===========================================
              # TAB 1: DATA OVERVIEW
              # ===========================================
              # PURPOSE: Preview loaded data and file information
              # STRATEGY: Interactive table with toggle functionality
              tabPanel("Data Overview", 
                       icon = icon("table"),
                       br(),
                       
                       # FILE INFORMATION
                       # PURPOSE: Show which files were successfully loaded
                       tableOutput("selected_files"),
                       hr(),
                       
                       # TABLE TOGGLE BUTTON
                       # STRATEGY: User control over data display level
                       # PURPOSE: Preview (5 rows) vs full table view
                       uiOutput("toggle_button"),
                       br(),
                       
                       # MAIN DATA TABLE
                       # STRATEGY: DT datatable for interactive features
                       # FEATURES: Horizontal scroll, filtering, pagination
                       DT::dataTableOutput("processed_data")
              ),
              
              # ===========================================
              # TAB 2: ANALYSIS RESULTS
              # ===========================================
              # PURPOSE: Display analysis outputs and visualizations
              # STRATEGY: Information boxes + main plot display
              tabPanel("Analysis Results", 
                       icon = icon("chart-line"),
                       br(),
                       
                       # INFORMATION BOXES
                       # STRATEGY: Key information displayed prominently
                       fluidRow(
                         # SELECTED VARIABLE INFO
                         column(6, 
                           div(class = "info-box",
                             h4("Selected Variable"),
                             textOutput("selectedValue")
                           )
                         ),
                         
                         # NORMALITY TEST RESULTS
                         # STRATEGY: Conditional display - only for Bar plots
                         # WHY: Line charts don't use normality testing
                         conditionalPanel(
                           condition = "input.graph_type == 'Bar plot'",
                           column(6,
                             div(class = "info-box",
                               h4("Normality Test"),
                               verbatimTextOutput("normality_text")
                             )
                           )
                         )
                       ),
                       hr(),
                       
                       # MAIN PLOT DISPLAY
                       # STRATEGY: Fixed height for consistent layout
                       # CSS: Responsive sizing handled in stylesheet above
                       plotOutput("plot_result", height = "600px")
              ),
              
              # ===========================================
              # TAB 3: HELP DOCUMENTATION
              # ===========================================
              # PURPOSE: Comprehensive user documentation
              # STRATEGY: Markdown file for easy editing and formatting
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