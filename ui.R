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
  # ===========================================
  # HEADER SECTION
  # ===========================================
  # STRATEGY: Professional branding with logo and title
  # PURPOSE: Clear application identity and professional appearance
  dashboardHeader(
    title = div(
      class = "brand",
      tags$img(src = "fluorcam_toolbox_logo.png", alt = "FluorCam", class = "brand-logo"),
      span("FluorCam Toolbox", class = "brand-title")
    ),
    titleWidth = 360  # Fixed width to accommodate logo and title
  ),
  
  # ===========================================
  # SIDEBAR CONFIGURATION
  # ===========================================
  # STRATEGY: Disabled sidebar to use custom layout
  # PURPOSE: More flexibility while keeping dashboard styling
  dashboardSidebar(disable = TRUE),

  # ===========================================
  # MAIN DASHBOARD BODY
  # ===========================================
  # STRATEGY: Custom CSS + fluidPage for responsive design
  # PURPOSE: Professional styling with responsive layouts
  dashboardBody(
    # ===========================================
    # CSS STYLING SECTION
    # ===========================================
    # STRATEGY: Comprehensive custom CSS for professional appearance
    # PURPOSE: Improve visual design, user experience, and fix layout issues
    tags$head(
      tags$style(HTML("
        /* ===========================================
           HEADER AND BRANDING STYLES
           =========================================== */
        /* STRATEGY: Responsive logo and title layout */
        /* PURPOSE: Professional branding that works on all screen sizes */
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
          max-height: 32px;   /* Fits well in 50px header */
          width: auto;
          display: block;
        }
        .brand-title {
          font-weight: 600;
          font-size: 18px;
          line-height: 1;
          letter-spacing: 0.2px;
          color: #fff; /* Maintains AdminLTE header contrast */
        }
        
        /* MOBILE RESPONSIVE ADJUSTMENTS */
        /* STRATEGY: Smaller elements on mobile devices */
        /* PURPOSE: Maintain readability on small screens */
        @media (max-width: 480px) {
          .brand-logo { max-height: 24px; }
          .brand-title { font-size: 16px; }
        }
        
        /* ===========================================
           GENERAL LAYOUT IMPROVEMENTS
           =========================================== */
        /* STRATEGY: Light background for better readability */
        /* PURPOSE: Reduce eye strain and improve content visibility */
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        
        /* STRATEGY: Card-like design for information hierarchy */
        /* PURPOSE: Clear visual separation of content blocks */
        .info-box {
          background: white;
          padding: 20px;
          margin: 15px 0;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          border-left: 4px solid #3c8dbc;
        }
        
        /* ===========================================
           INTERACTIVE ELEMENT STYLING
           =========================================== */
        /* STRATEGY: Consistent button spacing and modern design */
        /* PURPOSE: Professional appearance and improved usability */
        .btn-block {
          margin-bottom: 15px;
          border-radius: 5px;
        }
        
        /* STRATEGY: Interactive hover effects for accordion panels */
        /* PURPOSE: Visual feedback that panels are clickable */
        .panel-heading {
          cursor: pointer !important;
          transition: background-color 0.3s ease !important;
          padding: 0 !important;
        }
        .panel-heading:hover {
          background-color: rgba(0, 123, 255, 0.1) !important;
        }
        
        .panel-heading a {
          display: block !important;
          width: 100% !important;
          padding: 15px 20px !important;
          text-decoration: none !important;
          color: inherit !important;
        }
        
        .panel-heading a:hover {
          text-decoration: none !important;
          color: inherit !important;
        }
        
        .panel-title {
          margin: 0 !important;
          width: 100% !important;
        }
        
        /* STRATEGY: Modern alert design without harsh borders */
        /* PURPOSE: Softer, more professional notification appearance */
        .alert {
          border-radius: 6px;
          border: none;
          box-shadow: 0 1px 4px rgba(0,0,0,0.1);
        }
        
        /* STRATEGY: Branded tab design matching color scheme */
        /* PURPOSE: Consistent visual identity throughout application */
        .nav-tabs {
          border-bottom: 2px solid #3c8dbc;
        }
        .nav-tabs > li.active > a {
          background-color: #3c8dbc !important;
          color: white !important;
          border-color: #3c8dbc !important;
        }
        
        /* STRATEGY: Clean table design with subtle shadows */
        /* PURPOSE: Professional data presentation */
        .table {
          background: white;
          border-radius: 5px;
          overflow: hidden;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        /* ===========================================
           PLOT CONTAINER FIXES
           =========================================== */
        /* STRATEGY: Solve plot overflow issues and ensure responsive design */
        /* PURPOSE: Prevent plots from extending beyond container boundaries */
        /* PROBLEM SOLVED: Plots were overflowing their containers */
        #plot_result {
          background: white;
          padding: 20px;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          margin: 15px 0;
          overflow: hidden;  /* Prevents plot overflow */
          width: 100%;
          box-sizing: border-box;  /* Includes padding in width calculation */
        }
        
        /* STRATEGY: Force plot to fit within container */
        /* PURPOSE: Responsive plot sizing on all devices */
        #plot_result .shiny-plot-output {
          width: 100% !important;
          height: auto !important;
          max-width: 100%;
        }
        
        /* STRATEGY: Responsive image sizing */
        /* PURPOSE: Ensure plot images scale properly */
        #plot_result img {
          max-width: 100%;
          height: auto;
          display: block;
          margin: 0 auto;
        }
        
        /* ===========================================
           FORM ELEMENT IMPROVEMENTS
           =========================================== */
        /* STRATEGY: Modern input styling with focus effects */
        /* PURPOSE: Better user interaction feedback */
        .form-control {
          border-radius: 4px;
          border: 1px solid #ddd;
          transition: border-color 0.3s ease;
        }
        .form-control:focus {
          border-color: #3c8dbc;
          box-shadow: 0 0 0 0.2rem rgba(60, 141, 188, 0.25);
        }
        
        /* ===========================================
           HELP TAB SCROLLABLE CONTAINER
           =========================================== */
        /* STRATEGY: Fixed height container with custom scrollbar */
        /* PURPOSE: Better navigation of long documentation content */
        .help-container {
          height: 80vh; /* 80% of screen height */
          overflow-y: auto;
          overflow-x: hidden;
          padding: 20px;
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          margin: 10px 0;
        }
        
        /* STRATEGY: Custom scrollbar styling for better aesthetics */
        /* PURPOSE: Professional appearance even for scroll elements */
        .help-container::-webkit-scrollbar {
          width: 8px;
        }
        
        .help-container::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 4px;
        }
        
        .help-container::-webkit-scrollbar-thumb {
          background: #c1c1c1;
          border-radius: 4px;
        }
        
        .help-container::-webkit-scrollbar-thumb:hover {
          background: #a8a8a8;
        }
        
        /* ===========================================
           HELP CONTENT MARKDOWN STYLING
           =========================================== */
        /* STRATEGY: Professional documentation styling */
        /* PURPOSE: Improved readability and visual hierarchy */
        .help-container h1, .help-container h2, .help-container h3 {
          color: #3c8dbc;
          border-bottom: 2px solid #e9ecef;
          padding-bottom: 8px;
          margin-top: 30px;
          margin-bottom: 15px;
        }
        
        .help-container h1 { font-size: 28px; }
        .help-container h2 { font-size: 24px; }
        .help-container h3 { font-size: 20px; }
        
        .help-container p {
          line-height: 1.6;
          margin-bottom: 15px;
          text-align: justify;
        }
        
        /* STRATEGY: Distinctive code styling */
        /* PURPOSE: Clear differentiation between code and text */
        .help-container code {
          background-color: #f8f9fa;
          border: 1px solid #e9ecef;
          border-radius: 4px;
          padding: 2px 6px;
          font-family: 'Courier New', monospace;
          color: #e83e8c;
        }
        
        .help-container pre {
          background-color: #f8f9fa;
          border: 1px solid #e9ecef;
          border-radius: 4px;
          padding: 15px;
          overflow-x: auto;
        }
        
        /* STRATEGY: Improved list styling */
        /* PURPOSE: Better visual hierarchy and spacing */
        .help-container ul, .help-container ol {
          margin-left: 20px;
          margin-bottom: 15px;
        }
        
        .help-container li {
          margin-bottom: 8px;
          line-height: 1.5;
        }
      "))
    ),
    
    # ===========================================
    # MAIN LAYOUT STRUCTURE
    # ===========================================
    # STRATEGY: Responsive design using fluidPage with sidebarLayout
    # PURPOSE: Optimal layout for desktop and mobile devices
    fluidPage(
      sidebarLayout(

        # ===========================================
        # SIDEBAR PANEL: WORKFLOW CONTROLS
        # ===========================================
        # STRATEGY: Step-by-step guided workflow using accordion panels
        # PURPOSE: Organized, progressive user interface that guides analysis
        sidebarPanel(

          # ===========================================
          # ACCORDION CONTAINER
          # ===========================================
          # STRATEGY: Bootstrap accordion for collapsible sections
          # PURPOSE: Reduce visual clutter and guide users through workflow
          div(class = "panel-group", id = "accordion",
            
            # ===========================================
            # SECTION 1: FILE NAMING CONFIGURATION
            # ===========================================
            # TITLE: File Name Configuration
            # STRATEGY: Define variable mapping before any data operations
            # PURPOSE: Establish file naming convention understanding
            div(class = "panel panel-primary",
                # ACCORDION HEADER
                # STRATEGY: Clickable header with clear section numbering
                # PURPOSE: Visual hierarchy and intuitive navigation
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse",
                              `data-parent` = "#accordion",
                              href = "#collapse1",
                              icon("tag"), " 1. File Name Configuration"))
                ),
                # PANEL CONTENT - STARTS EXPANDED
                # STRATEGY: First step should be immediately visible
                # PURPOSE: Guide users to complete setup before proceeding
                div(id = "collapse1", class = "panel-collapse collapse in",
                    div(class = "panel-body",

                        # NAMING CONVENTION EXPLANATION
                        # STRATEGY: Prominent alert box with clear examples
                        # PURPOSE: Prevent user confusion about required file format
                        div(class = "alert alert-info", style = "margin-bottom: 15px;",
                            icon("info-circle"),
                            strong(" Required File Naming Pattern:"),
                            br(), br(),
                            tags$code("VAR1_VAR2_VAR3.TXT", style = "font-size: 14px; background-color: #f8f9fa; padding: 15px;"),
                            br(), br(),
                            em("Example: Day1_LineA_Plant001.TXT")
                        ),

                        # VARIABLE DEFINITION INPUTS
                        # STRATEGY: Three-column layout for logical organization
                        # PURPOSE: Map generic placeholders to actual experiment variables
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

                        # LIVE FILENAME PREVIEW
                        # STRATEGY: Real-time feedback using reactive inputs
                        # PURPOSE: Immediate validation of user setup
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
            # TITLE: Data Loading Interface
            # STRATEGY: File upload → pattern matching → preview → load
            # PURPOSE: Secure, server-friendly data loading workflow
            div(class = "panel panel-primary",
                # ACCORDION HEADER
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse", 
                              `data-parent` = "#accordion",
                              href = "#collapse2",
                              icon("database"), " 2. Load Data"))
                ),
                # PANEL CONTENT - COLLAPSED BY DEFAULT
                # STRATEGY: Users complete Section 1 before proceeding
                # PURPOSE: Logical workflow progression
                div(id = "collapse2", class = "panel-collapse collapse",
                    div(class = "panel-body",

                        # FILE UPLOAD INTERFACE
                        # STRATEGY: Direct file upload for server deployment compatibility
                        # PURPOSE: Works in both local and deployed environments
                        div(
                          style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                          h4(icon("upload"), "Upload FluorCam Files",
                             style = "color: #495057; margin-bottom: 15px;"),

                          # MULTIPLE FILE INPUT
                          # STRATEGY: Accept multiple files with extension filtering
                          # PURPOSE: Bulk upload of related data files
                          fileInput(
                            "uploaded_files",
                            label = div(
                              strong("Select FluorCam .TXT files:"),
                              br(),
                              span("Choose multiple files that follow the naming pattern VAR1_VAR2_VAR3.TXT",
                                   style = "font-size: 12px; color: #6c757d;")
                            ),
                            multiple = TRUE,
                            accept = c(".txt", ".TXT"),
                            width = "100%"
                          ),

                          # UPLOAD FEEDBACK
                          # STRATEGY: Real-time status updates
                          # PURPOSE: User feedback during file processing
                          verbatimTextOutput("upload_status")
                        ),

                        # FILE PATTERN FILTER
                        # STRATEGY: Flexible pattern matching for different file types
                        # PURPOSE: Support various file extensions and naming patterns
                        textInput("pattern", "File Pattern", 
                                  value = ".TXT",
                                  placeholder = "e.g., .TXT, .csv"),

                        # FILE PREVIEW CONTROLS
                        # STRATEGY: Optional file list with conditional toggle
                        # PURPOSE: Verify correct files before loading
                        uiOutput("show_all_button"),
                        br(), br(),

                        # MAIN LOAD ACTION BUTTON
                        # STRATEGY: Prominent action button with success styling
                        # PURPOSE: Clear call-to-action for data loading
                        actionButton("load", "Load Data",
                                     icon = icon("play"),
                                     class = "btn-success btn-block")
                    )
                )
            ),
            
            # ===========================================
            # SECTION 3: ANALYSIS CONFIGURATION
            # ===========================================
            # TITLE: Analysis Parameters Setup
            # STRATEGY: Comprehensive parameter configuration interface
            # PURPOSE: All analysis settings in logical, organized layout
            div(class = "panel panel-primary",
                # ACCORDION HEADER
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse", 
                              `data-parent` = "#accordion",
                              href = "#collapse3",
                              icon("chart-line"), " 3. Analysis Parameters"))
                ),
                # PANEL CONTENT - REQUIRES DATA TO BE LOADED
                div(id = "collapse3", class = "panel-collapse collapse",
                    div(class = "panel-body",

                        # ANALYSIS TYPE SELECTION
                        # STRATEGY: Radio buttons for mutually exclusive analysis types
                        # PURPOSE: Determine entire downstream workflow
                        radioButtons("graph_type", 
                                     "Graph Type",
                                     choices = list(
                                       "Bar Plot" = "Bar plot",      # Static group comparisons
                                       "Line Chart" = "Curve"        # Time course analysis
                                     ),
                                     selected = "Bar plot",
                                     inline = TRUE),
                        
                        # DYNAMIC UI SECTIONS
                        # STRATEGY: Server-generated UI based on loaded data structure
                        # PURPOSE: Automatically populate options from actual data
                        uiOutput("rootSelect"),      # Parameter root selection (curves only)
                        uiOutput("columnSelect"),    # Column/parameter selection
                        uiOutput("editParamsBtn"),   # Time parameter configuration (curves only)
                        
                        # FACETING CONFIGURATION
                        # STRATEGY: Clear labeling with explanatory text
                        # PURPOSE: Control how data is split into multiple plot panels
                        div(
                          tags$label(
                            strong("Facet Variable"), 
                            span(": Choose how to split your data into separate panels", style = "font-weight: normal;"),
                            style = "margin-bottom: 10px; display: block;"
                          ),
                          selectInput("facet_var", NULL,
                                      choices = c("var1", "var2"))
                        ),
                        
                        # GROUP ORDERING INTERFACE
                        # STRATEGY: Drag-and-drop sortable lists
                        # PURPOSE: Intuitive control over group order in plots
                        helpText("Drag to reorder:"),
                        uiOutput("var2_order_ui"),  # X-axis variable order
                        uiOutput("var1_order_ui"),  # Facet variable order
                        
                        # CONTROL GROUP SELECTION (CONDITIONAL)
                        # STRATEGY: Only display for curve analysis
                        # PURPOSE: Statistical comparisons require reference group
                        conditionalPanel(
                          condition = "input.graph_type == 'Curve'",
                          div(
                            tags$label(
                              strong("Control Group"),
                              span(": Select the control group for statistical analysis", style = "font-weight: normal;"),
                              style = "margin-bottom: 10px; display: block;"
                            ),
                            uiOutput("control_group_ui")
                          ),
                          # AJOUTEZ ICI LE CONTRÔLE POUR LE PARAMÈTRE k
                          br(),
                          numericInput("k_param", 
                                       "qGAM Smoothing Parameter (k)", 
                                       value = 5, 
                                       min = 3, 
                                       max = 20, 
                                       step = 1),
                          helpText("Higher values = smoother curves, lower values = more flexible curves")
                        ),
                        hr(),

                        # COLOR CUSTOMIZATION INTERFACE
                        # STRATEGY: Dynamic color inputs based on number of groups
                        # PURPOSE: Custom color schemes for better visualization
                        div(
                          tags$label(
                            strong("Select colors:"),
                            style = "margin-bottom: 10px; display: block;"
                          )
                        ),
                        uiOutput("dynamic_color_inputs"),
                        hr(),

                        # ANALYSIS EXECUTION BUTTON
                        # STRATEGY: Warning-colored button indicates significant action
                        # PURPOSE: Execute configured analysis with all parameters
                        actionButton("start_analysis", "Start Analysis",
                                     icon = icon("play"),
                                     class = "btn-warning btn-block")
                    )
                )
            ),
            
            # ===========================================
            # SECTION 4: RESULTS EXPORT
            # ===========================================
            # TITLE: Export Analysis Results
            # STRATEGY: Separate exports for statistical data and plots
            # PURPOSE: Professional output options with full customization
            div(class = "panel panel-primary",
                # ACCORDION HEADER
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse",
                              `data-parent` = "#accordion",
                              href = "#collapse4",
                              icon("download"), " 4. Export Results"))
                ),
                # PANEL CONTENT - ONLY AVAILABLE AFTER ANALYSIS
                div(id = "collapse4", class = "panel-collapse collapse",
                    div(class = "panel-body",

                        # STATISTICAL DATA EXPORT SECTION
                        # STRATEGY: ZIP archive with multiple text files
                        # PURPOSE: Organized, readable statistical output
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
                        # STRATEGY: Comprehensive customization for publication quality
                        # PURPOSE: Professional plot output with format options
                        strong("Export Plot:"),
                        br(), br(),

                        # FILENAME AND FORMAT SELECTION
                        # STRATEGY: Side-by-side layout for related options
                        # PURPOSE: Efficient use of space for related settings
                        fluidRow(
                          column(6,
                            textInput("plot_filename", "Filename",
                                      value = "plot",
                                      placeholder = "Enter filename")
                          ),
                          column(6,
                            # FORMAT OPTIONS FOR DIFFERENT USE CASES
                            # STRATEGY: Multiple formats for different applications
                            selectInput("plot_format", "Format",
                                        choices = list(
                                          "PNG (High-res)" = "png",    # General use, high quality
                                          "PDF (Print)" = "pdf",       # Publication ready
                                          "SVG (Vector)" = "svg",      # Scalable graphics
                                          "JPEG" = "jpg"               # Compressed format
                                        ),
                                        selected = "png")
                          )
                        ),

                        # PLOT DIMENSIONS CONFIGURATION
                        # STRATEGY: Scientific units (cm) with alternatives
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
                            # UNIT SELECTION FOR DIFFERENT STANDARDS
                            # STRATEGY: Scientific (cm) default with alternatives
                            selectInput("plot_units", "Units",
                                        choices = list(
                                          "Centimeters" = "cm",    # Scientific standard
                                          "Inches" = "in",         # US publishing standard
                                          "Millimeters" = "mm"     # High precision
                                        ),
                                        selected = "cm")
                          )
                        ),
                        br(),

                        # PLOT EXPORT ACTION BUTTON
                        # STRATEGY: Success-colored button for positive action
                        # PURPOSE: Download configured plot with custom settings
                        downloadButton("download_plot", "Download Plot",
                                       class = "btn-success", icon = icon("image"))
                    )
                )
            ),
            
            # ===========================================
            # SECTION 5: DATA EXPORT
            # ===========================================
            # TITLE: Export Processed Data Table
            # STRATEGY: Flexible data export options for user convenience
            # PURPOSE: Allow users to download the processed data in various formats
            div(class = "panel panel-primary",
                # ACCORDION HEADER
                div(class = "panel-heading",
                    h4(class = "panel-title",
                       tags$a(`data-toggle` = "collapse",
                              `data-parent` = "#accordion",
                              href = "#collapse5",
                              icon("table"), " 5. Data Export"))
                ),
                # PANEL CONTENT - ONLY AVAILABLE AFTER DATA IS PROCESSED
                div(id = "collapse5", class = "panel-collapse collapse",
                    div(class = "panel-body",

                        # DATA TABLE EXPORT SECTION
                        div(
                          class = "card",
                          style = "margin-bottom: 20px;",
                          div(
                            class = "card-header",
                            style = "background-color: #17a2b8; color: white; padding: 10px 15px;",
                            h5(icon("table"), " Data Table Export", style = "margin: 0; display: inline-block;")
                          ),
                          div(
                            class = "card-body",
                            
                            # COLUMN SELECTION FOR DATA EXPORT
                            conditionalPanel(
                              condition = "output.data_loaded",
                              div(
                                style = "margin-bottom: 15px;",
                                h6("Select columns to export:", style = "margin-bottom: 10px;"),
                                div(
                                  style = "display: flex; gap: 10px; margin-bottom: 15px;",
                                  actionButton("select_all_cols", "Select All", class = "btn-sm btn-outline-primary"),
                                  actionButton("deselect_all_cols", "Deselect All", class = "btn-sm btn-outline-secondary")
                                ),
                                uiOutput("column_selection_ui")
                              )
                            ),
                            
                            # DATA EXPORT CONTROLS
                            conditionalPanel(
                              condition = "output.data_loaded",
                              fluidRow(
                                column(6,
                                       textInput("data_filename", "Filename:", value = "fluorcam_data")
                                ),
                                column(6,
                                       selectInput("data_format", "Format:",
                                                  choices = list("CSV" = "csv", "Excel" = "xlsx", "Tab-delimited" = "tsv"),
                                                  selected = "csv")
                                )
                              ),
                              
                              div(
                                class = "text-center",
                                style = "margin-top: 15px;",
                                downloadButton("download_data", "Download Data Table",
                                               class = "btn-info",
                                               icon = icon("download"))
                              )
                            ),
                            
                            # NO DATA MESSAGE
                            conditionalPanel(
                              condition = "!output.data_loaded",
                              div(
                                class = "alert alert-info text-center",
                                icon("info-circle"),
                                " Load data first to enable table export"
                              )
                            )
                          )
                        )
                    )
                )
            )
          )
        ),
          
        # ===========================================
        # MAIN PANEL: RESULTS DISPLAY AREA
        # ===========================================
        # STRATEGY: Tabbed interface for organized information presentation
        # PURPOSE: Logical separation of data, results, and documentation
        mainPanel(

          # ===========================================
          # TABBED INTERFACE CONTAINER
          # ===========================================
          # STRATEGY: Three-tab structure for complete workflow coverage
          # PURPOSE: Separate concerns while maintaining easy navigation
          tabsetPanel(
            id = "main_tabs",

            # ===========================================
            # TAB 1: DATA OVERVIEW
            # ===========================================
            # TITLE: Data Overview and File Management
            # STRATEGY: Interactive data preview with file information
            # PURPOSE: Allow users to verify loaded data before analysis
            tabPanel("Data Overview", 
                     icon = icon("table"),
                     br(),

                    # UPLOADED FILES INFORMATION SECTION
                    # STRATEGY: Conditional display with toggle functionality
                    # PURPOSE: Show file upload status without cluttering interface
                    conditionalPanel(
                       condition = "output.files_uploaded",
                       div(
                         h4("Uploaded Files:", style = "margin-bottom: 15px; color: #3c8dbc;"),
                         # TOGGLE BUTTON FOR FILE LIST
                         # STRATEGY: Collapsible file list to save space
                         # PURPOSE: Optional detailed file information
                         uiOutput("toggle_files_button"),
                         br(),
                         # CONDITIONAL FILE TABLE DISPLAY
                         # STRATEGY: Show table only when requested
                         # PURPOSE: Clean interface with optional detail
                         uiOutput("uploaded_files_display"),
                         hr()
                       )
                     ),

                    # PROCESSED FILES PREVIEW
                    # STRATEGY: Always visible file information
                    # PURPOSE: Show which files are ready for analysis
                    tableOutput("selected_files"),

                    # CONDITIONAL SEPARATOR
                    # STRATEGY: Only show separator when files are present
                    # PURPOSE: Clean layout without unnecessary elements
                    conditionalPanel(
                      condition = "output.selected_files",
                      hr()
                    ),

                    # DATA TABLE TOGGLE CONTROLS
                    # STRATEGY: User control over table detail level
                    # PURPOSE: Balance between quick preview and full exploration
                    uiOutput("toggle_button"),
                    br(),

                    # MAIN INTERACTIVE DATA TABLE
                    # STRATEGY: DT datatable for full interactive features
                    # PURPOSE: Comprehensive data exploration with filtering and pagination
                    DT::dataTableOutput("processed_data")
            ),

            # ===========================================
            # TAB 2: ANALYSIS RESULTS
            # ===========================================
            # TITLE: Analysis Results and Visualizations
            # STRATEGY: Information summary + main visualization display
            # PURPOSE: Present analysis outputs in organized, readable format
            tabPanel("Analysis Results",
                     icon = icon("chart-line"),
                     br(),

                     # ANALYSIS INFORMATION SUMMARY
                     # STRATEGY: Key information in prominent info boxes
                     # PURPOSE: Quick reference for analysis parameters and results
                     fluidRow(
                       # SELECTED PARAMETER INFORMATION
                       # STRATEGY: Always visible parameter confirmation
                       # PURPOSE: User confirmation of what was analyzed
                       column(6,
                         div(class = "info-box",
                           h4("Selected Variable"),
                           textOutput("selectedValue")
                         )
                       ),

                       # NORMALITY TEST RESULTS (BAR PLOTS ONLY)
                       # STRATEGY: Conditional display based on analysis type
                       # PURPOSE: Statistical information relevant to bar plot analysis only
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

                     # MAIN VISUALIZATION DISPLAY
                     # STRATEGY: Fixed height container for consistent layout
                     # PURPOSE: Professional plot presentation with responsive sizing
                     plotOutput("plot_result", height = "600px")
            ),
            
            # ===========================================
            # TAB 3: HELP DOCUMENTATION
            # ===========================================
            # TITLE: User Help and Documentation
            # STRATEGY: Comprehensive documentation in scrollable container
            # PURPOSE: Complete user guidance and reference material
            tabPanel("Help",
                     icon = icon("question-circle"),
                     br(),
                     
                     # SCROLLABLE HELP CONTAINER
                     # STRATEGY: Fixed height container with custom scrolling
                     # PURPOSE: Better navigation of extensive documentation
                     div(class = "help-container",
                         includeMarkdown("help.md")  # External markdown file for easy editing
                     )
            )
          )
        )
      )
    )
  )
)