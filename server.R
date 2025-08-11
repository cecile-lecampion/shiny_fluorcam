########################################################################################################################################
# Define the server
########################################################################################################################################
# STRATEGY: Reactive server logic for FluorCam data analysis
# - Modular approach with logical sections for different functionalities
# - Reactive programming paradigm for efficient updates
# - Error handling and user feedback throughout
# - Separation of data processing, UI generation, and analysis logic

source("global.R",  local = TRUE) # Load the global variables and packages
source("helpers.R", local = TRUE) # Load the helper functions

server <- function(input, output, session) { 
  
  # ===========================================
  # SECTION 1: INITIALIZATION & SETUP
  # ===========================================
  # STRATEGY: Set up core reactive infrastructure and directory browsing
  # PURPOSE: Foundation for all subsequent functionality
  
  # DIRECTORY BROWSING SETUP
  # STRATEGY: Cross-platform directory selection using shinyDirChoose
  # BENEFIT: Works on Windows, Mac, and Linux
  volumes <- c(Home = fs::path_home(), "R" = R.home(), getVolumes()())
  shinyDirChoose(input, "dir", roots = volumes, session = session)
  
  # REACTIVE DIRECTORY PATH
  # PURPOSE: Convert shinyDirChoose output to usable path string
  # STRATEGY: Reactive expression for automatic updates when directory changes
  dirpath <- reactive({
    parseDirPath(volumes, input$dir)
  })
  
  # DISPLAY SELECTED DIRECTORY
  # PURPOSE: User feedback - show currently selected directory
  output$dirpath <- renderText({ dirpath() })
  
  # ===========================================
  # SECTION 1.5: SESSION-SPECIFIC FILE MANAGEMENT
  # ===========================================
  # STRATEGY: Create unique workspace for each user session
  # PURPOSE: Isolate user files and enable automatic cleanup
  
  # CREATE UNIQUE SESSION DIRECTORY
  # STRATEGY: Use session token for unique folder names
  # PURPOSE: Prevent file mixing between concurrent users
  session_id <- session$token
  session_dir <- file.path(tempdir(), paste0("fluorcam_session_", session_id))
  
  # ENSURE SESSION DIRECTORY EXISTS
  if (!dir.exists(session_dir)) {
    dir.create(session_dir, recursive = TRUE)
  }
  
  # SESSION CLEANUP ON DISCONNECT
  # STRATEGY: Automatic cleanup when user disconnects
  # PURPOSE: Remove files immediately when session ends
  session$onSessionEnded(function() {
    if (dir.exists(session_dir)) {
      unlink(session_dir, recursive = TRUE)
      cat("Cleaned up session directory:", session_id, "\n")
    }
  })
  
  # PERIODIC CLEANUP FUNCTION
  # STRATEGY: Background cleanup for orphaned directories
  # PURPOSE: Safety net for cleanup in case of unexpected disconnections
  cleanup_old_sessions <- function() {
    temp_base <- tempdir()
    session_dirs <- list.dirs(temp_base, pattern = "fluorcam_session_", 
                             full.names = TRUE, recursive = FALSE)
    current_time <- Sys.time()
    
    for (dir in session_dirs) {
      dir_time <- file.info(dir)$mtime
      # CLEANUP AFTER 2 HOURS
      if (difftime(current_time, dir_time, units = "hours") > 2) {
        unlink(dir, recursive = TRUE)
        cat("Cleaned up old session directory:", basename(dir), "\n")
      }
    }
  }
  
  # SCHEDULE PERIODIC CLEANUP
  # STRATEGY: Run cleanup every 30 minutes
  observeEvent(reactiveTimer(30 * 60 * 1000)(), {
    cleanup_old_sessions()
  })

  # ===========================================
  # SECTION 2: FILE UPLOAD FUNCTIONALITY  
  # ===========================================
  # STRATEGY: Handle file uploads with validation and session isolation
  # PURPOSE: Replace directory browsing with file upload
  
  # FILE UPLOAD PROCESSING
  observeEvent(input$uploaded_files, {
    req(input$uploaded_files)
    
    tryCatch({
      # COPY UPLOADED FILES TO SESSION DIRECTORY
      # STRATEGY: Move files from temp upload to session-specific folder
      # PURPOSE: Organize files and enable processing
      uploaded_paths <- input$uploaded_files$datapath
      original_names <- input$uploaded_files$name
      
      # VALIDATE FILE NAMES
      # STRATEGY: Check naming convention before processing
      # PURPOSE: Early validation to prevent processing errors
      invalid_files <- c()
      valid_files <- c()
      
      for (i in seq_along(original_names)) {
        # CHECK NAMING PATTERN: VAR1_VAR2_VAR3.txt
        if (grepl("^[^_]+_[^_]+_[^_]+\\.(txt|TXT)$", original_names[i])) {
          # COPY TO SESSION DIRECTORY
          dest_path <- file.path(session_dir, original_names[i])
          file.copy(uploaded_paths[i], dest_path, overwrite = TRUE)
          valid_files <- c(valid_files, original_names[i])
        } else {
          invalid_files <- c(invalid_files, original_names[i])
        }
      }
      
      # VALIDATION FEEDBACK
      if (length(invalid_files) > 0) {
        showNotification(
          paste("Invalid file names (must be VAR1_VAR2_VAR3.txt):", 
                paste(invalid_files, collapse = ", ")), 
          type = "warning", duration = 10
        )
      }
      
      if (length(valid_files) > 0) {
        showNotification(
          paste("Successfully uploaded", length(valid_files), "files"), 
          type = "message"
        )
      }
      
    }, error = function(e) {
      showNotification(paste("Error uploading files:", e$message), type = "error")
    })
  })
  
  # UPLOAD STATUS DISPLAY
  output$upload_status <- renderText({
    if (is.null(input$uploaded_files)) {
      return("No files uploaded yet.")
    }
    
    files_in_session <- list.files(session_dir, pattern = "\\.(txt|TXT)$")
    if (length(files_in_session) > 0) {
      paste("Files ready for analysis:", length(files_in_session), "files")
    } else {
      "No valid files found. Please check file naming convention."
    }
  })
  
  # FILES UPLOADED FLAG
  output$files_uploaded <- reactive({
    !is.null(input$uploaded_files) && length(list.files(session_dir, pattern = "\\.(txt|TXT)$")) > 0
  })
  outputOptions(output, "files_uploaded", suspendWhenHidden = FALSE)
  
  # UPLOADED FILES TABLE
  output$uploaded_files_table <- renderTable({
    files_in_session <- list.files(session_dir, pattern = "\\.(txt|TXT)$")
    if (length(files_in_session) > 0) {
      data.frame(
        `File Name` = files_in_session,
        `Size (KB)` = round(file.size(file.path(session_dir, files_in_session)) / 1024, 1),
        check.names = FALSE
      )
    }
  })

  # SESSION DIRECTORY PATH
  # STRATEGY: Use session directory instead of user-selected directory
  # PURPOSE: Work with uploaded files in isolated session space
  dirpath <- reactive({
    session_dir
  })

  # REMOVE the directory display output (output$dirpath) as it's no longer needed
  
  # ===========================================
  # SECTION 2: FILE PREVIEW FUNCTIONALITY
  # ===========================================
  # STRATEGY: Allow users to preview files before loading
  # PURPOSE: Prevent loading wrong files, provide transparency
  
  # FILE LIST TOGGLE STATE
  # STRATEGY: ReactiveVal for simple boolean state management
  # PURPOSE: Control whether to show all files or just first 5
  show_all_files <- reactiveVal(FALSE)
  observeEvent(input$show_all, { show_all_files(!show_all_files()) })
  
  # DYNAMIC FILE LIST DISPLAY (MODIFIED)
  # STRATEGY: Show uploaded files instead of directory files
  # PURPOSE: Preview uploaded files before processing
  output$selected_files <- renderTable({
    req(input$pattern)  # Only require pattern, not directory
    files <- list.files(path = session_dir, pattern = input$pattern, full.names = TRUE)
    # CONDITIONAL DISPLAY: Show 5 or all based on toggle state
    if (!show_all_files()) files <- head(files, 5)
    if (length(files) > 0) {
      data.frame(Files = basename(files))  # Show only filenames, not full paths
    } else {
      data.frame(Files = "No files match the pattern")
    }
  })
  
  # ===========================================
  # SECTION 3: CORE DATA STRUCTURES
  # ===========================================
  # STRATEGY: Centralized reactive data storage
  # PURPOSE: Single source of truth for data and parameters across app
  
  # MAIN DATA STORAGE
  # STRATEGY: ReactiveValues for mutable data storage
  # PURPOSE: Store processed data that can be updated and accessed by multiple functions
  result_df <- reactiveValues(data = NULL)
  
  # USER PARAMETER STORAGE
  # STRATEGY: Separate reactive storage for analysis parameters
  # PURPOSE: Store time values, units, and selected parameters for curve analysis
  user_params <- reactiveValues(times = NULL, unit = NULL, selected_params = NULL)
  
  # TABLE DISPLAY CONTROL
  # STRATEGY: Global state for table view mode (preview vs full)
  # WHY OUTSIDE LOAD EVENT: Persists across data reloads
  show_full_table <- reactiveValues(full = FALSE)
  
  # TABLE TOGGLE OBSERVER
  # STRATEGY: Global observer for table view toggle
  # PURPOSE: Switch between 5-row preview and full table display
  observeEvent(input$toggle_table, {
    show_full_table$full <- !show_full_table$full
  })

  # ===========================================
  # SECTION 4: DATA LOADING FUNCTIONALITY
  # ===========================================
  # STRATEGY: Robust data loading with validation and error handling
  # PURPOSE: Load and validate data files, provide user feedback
  
  observeEvent(input$load, {
    # INPUT VALIDATION
    req(input$pattern, input$var1, input$var2, input$var3)
    
    # CHECK IF FILES EXIST IN SESSION
    available_files <- list.files(session_dir, pattern = input$pattern, full.names = TRUE)
    if (length(available_files) == 0) {
      showNotification("No files found. Please upload files first.", type = "error")
      return()
    }
    
    # VALIDATE SESSION SIZE
    if (!validate_session_size()) {
      return()
    }
    
    # EXTRACT INPUT VALUES
    pattern <- input$pattern
    var1 <- input$var1
    var2 <- input$var2
    var3 <- input$var3
    
    # MAIN DATA PROCESSING
    tryCatch({
      # CALL DATA PROCESSING FUNCTION WITH SESSION DIRECTORY
      processed_data <- process_data_files(
        pattern = pattern, 
        areas = "", 
        var1 = var1, 
        var2 = var2, 
        var3 = var3, 
        dirpath = session_dir  # Use session directory
      )
      
      # DATA VALIDATION
      # STRATEGY: Check data quality before assignment
      # PURPOSE: Ensure only valid data is stored and used
      if(!is.null(processed_data) && nrow(processed_data) > 0) {
        result_df$data <- processed_data
        showNotification("Data loaded successfully!", type = "message")
      } else {
        showNotification("No valid data found in the files.", type = "warning")
        return()
      }
      
      # TABLE TOGGLE BUTTON CREATION
      # STRATEGY: Generate UI dynamically after successful data load
      # PURPOSE: Button only appears when data is available
      output$toggle_button <- renderUI({
        req(result_df$data)
        actionButton("toggle_table", "Show Full Table", class = "btn-info")
      })
      
    }, error = function(e) {
      showNotification(paste("Failed to load data files:", e$message), type = "error")
      print(e)
    })
  })
  
  # ===========================================
  # SECTION 5: DATA TABLE RENDERING
  # ===========================================
  # STRATEGY: Interactive data table with responsive design
  # PURPOSE: Allow users to explore loaded data with horizontal scrolling and filtering
  # WHY OUTSIDE LOAD EVENT: Reactive to show_full_table changes
  
  output$processed_data <- DT::renderDataTable({
    req(result_df$data)  # Only render when data is available
    
    if (show_full_table$full) {
      # FULL TABLE MODE
      # STRATEGY: Full featured interactive table for data exploration
      DT::datatable(
        result_df$data,
        options = list(
          scrollX = TRUE,              # SOLUTION: Horizontal scrolling for wide data
          scrollY = "400px",           # Vertical scroll with fixed height
          pageLength = 25,             # Default rows per page
          lengthMenu = c(10, 25, 50, 100),  # User selectable page sizes
          autoWidth = TRUE,            # Automatic column width adjustment
          columnDefs = list(
            list(width = "100px", targets = "_all")  # Minimum column width
          )
        ),
        class = "display nowrap",      # CSS classes for better display
        filter = "top"                 # Column filters for data exploration
      )
    } else {
      # PREVIEW MODE
      # STRATEGY: Limited view for quick data inspection
      DT::datatable(
        head(result_df$data, 5),       # Only first 5 rows
        options = list(
          scrollX = TRUE,
          scrollY = "200px",           # Smaller height for preview
          pageLength = 5,              # Fixed at 5 rows
          lengthMenu = c(5),           # No other options
          paging = FALSE,              # No pagination needed for 5 rows
          autoWidth = TRUE,
          columnDefs = list(
            list(width = "100px", targets = "_all")
          )
        ),
        class = "display nowrap",
        filter = "top"
      )
    }
  }, server = FALSE)  # Client-side processing for better performance
  
  # TABLE TOGGLE BUTTON TEXT UPDATE
  # STRATEGY: Dynamic button text based on current state
  # PURPOSE: Clear indication of what clicking the button will do
  observeEvent(show_full_table$full, {
    req(result_df$data)
    button_text <- if(show_full_table$full) "Show Preview (5 rows)" else "Show Full Table"
    output$toggle_button <- renderUI({
      actionButton("toggle_table", button_text, class = "btn-info")
    })
  })
  
  # ===========================================
  # SECTION 6: DYNAMIC UI GENERATION
  # ===========================================
  # STRATEGY: Generate UI elements based on loaded data and user selections
  # PURPOSE: Adaptive interface that responds to data structure and analysis type
  
  # 6.1 ROOT PARAMETER SELECTION (CURVE ANALYSIS ONLY)
  # STRATEGY: Extract parameter roots from column names automatically
  # PURPOSE: Identify available parameters without manual specification
  output$rootSelect <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Curve")  # Only for curve analysis
    
    all_cols <- colnames(result_df$data)
    # PATTERN EXTRACTION: Find columns with time point suffixes (_L1, _D2, etc.)
    # STRATEGY: Regular expression to identify parameter families
    roots <- unique(sub("(_L[0-9]+|_D[0-9]+)$", "", all_cols[grepl("(_L[0-9]+|_D[0-9]+)$", all_cols)]))
    selectInput("root", "Select the parameter root", choices = roots)
  })
  
  # 6.2 COLUMN SELECTION (ADAPTIVE TO ANALYSIS TYPE)
  # STRATEGY: Different UI for bar plot vs curve analysis
  # PURPOSE: Match column selection to analysis requirements
  output$columnSelect <- renderUI({
    req(result_df$data)
    
    if (input$graph_type == "Curve") {
      # CURVE ANALYSIS: Multiple time point columns
      req(input$root)
      all_cols <- colnames(result_df$data)
      # BUILD PATTERN: Find all columns matching the selected root
      pattern <- paste0("^", input$root, "(_L[0-9]+|_D[0-9]+)$")
      available_choices <- all_cols[grepl(pattern, all_cols)]
      
      # PROVIDE CONVENIENCE BUTTON + MULTI-SELECT
      tagList(
        div(style = "margin-bottom: 10px;",
            actionButton("select_all_params", "Select All Parameters", 
                         class = "btn-info btn-sm")
        ),
        selectInput("column", "Select parameters to analyse", 
                    choices = available_choices, 
                    multiple = TRUE)
      )
    } else {
      # BAR PLOT: Single column selection
      # STRATEGY: Smart default selection (prefer "Fm" if available)
      selected <- if ("Fm" %in% colnames(result_df$data)) "Fm" else colnames(result_df$data)[1]
      selectInput("column", "Select parameter to analyse", 
                  choices = colnames(result_df$data), 
                  selected = selected)
    }
  })
  
  # SELECT ALL PARAMETERS BUTTON
  # STRATEGY: Convenience function for curve analysis
  # PURPOSE: Quick selection of all time points for a parameter
  observeEvent(input$select_all_params, {
    req(input$root)
    all_cols <- colnames(result_df$data)
    pattern <- paste0("^", input$root, "(_L[0-9]+|_D[0-9]+)$")
    available_choices <- all_cols[grepl(pattern, all_cols)]
    
    # UPDATE SELECTION: Use updateSelectInput for programmatic changes
    updateSelectInput(session, "column", 
                      selected = available_choices)
  })
  
  # 6.3 MEASUREMENT PARAMETERS BUTTON (CURVE ANALYSIS ONLY)
  # STRATEGY: Modal dialog for time parameter configuration
  # PURPOSE: Allow users to specify time values and units
  output$editParamsBtn <- renderUI({
    if (input$graph_type == "Curve" && !is.null(input$column) && length(input$column) > 0) {
      div(class = "text-center", style = "margin: 20px 0;",
          actionButton("edit_params", 
                       HTML("<i class='fa fa-flask'></i> Set Measurement Parameters"), 
                       class = "btn-primary",
                       style = "font-weight: 600; 
                               border-radius: 25px; 
                               padding: 10px 25px; 
                               font-size: 13px;
                               text-transform: uppercase;
                               letter-spacing: 0.5px;
                               background: #00b8e9;
                               color: white;
                               border: none;
                               box-shadow: 0 4px 15px rgba(0, 123, 255, 0.3);
                               transition: all 0.3s ease;")
      )
    }
  })

  # 6.4 CONTROL GROUP SELECTION (CURVE ANALYSIS ONLY)
  # STRATEGY: Populate choices from grouping variable
  # PURPOSE: Statistical comparisons require a reference group
  output$control_group_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Curve")  # Only for curve analysis
    selectInput(
      "control_group",
      label = NULL,
      choices = unique(result_df$data[[x_var()]]),  # Use grouping variable levels
      selected = NULL
    )
  })
  
  # ===========================================
  # SECTION 7: MODAL DIALOG FOR TIME PARAMETERS
  # ===========================================
  # STRATEGY: Modal dialog for complex parameter input
  # PURPOSE: Configure time values and units for curve analysis
  # WHY MODAL: Keeps main UI clean while allowing detailed parameter setup
  
  observeEvent(input$edit_params, {
    req(input$column)
    params <- input$column
    
    # DYNAMIC INPUT GENERATION
    # STRATEGY: Create input fields for each selected parameter
    # PURPOSE: Allow individual time value specification
    value_inputs <- lapply(params, function(param) {
      div(
        style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        numericInput(paste0("value_", param), 
                    label = div(style = "font-weight: 600; color: #495057;", param), 
                    value = NA)
      )
    })
    
    # MODAL DIALOG CONSTRUCTION
    # STRATEGY: Professional styling with clear sections
    showModal(modalDialog(
      title = div(
        style = "text-align: center; background: #00b8e9; color: white; padding: 15px; margin: -15px -15px 20px -15px; border-radius: 8px 8px 0 0;",
        icon("flask"),
        " Configure Measurement Parameters"
      ),
      size = "m",
      easyClose = FALSE,  # Require explicit action
      
      # INSTRUCTIONS SECTION
      # STRATEGY: Clear guidance for users
      div(
        class = "alert alert-info",
        style = "background-color: #e8f4f8; border: 1px solid #bee5eb; border-radius: 5px; margin-bottom: 20px;",
        div(
          icon("info-circle", style = "margin-right: 8px;"), 
          strong("Enter the unit and values for your selected parameters")
        )
      ),
      
      # UNIT INPUT SECTION
      # STRATEGY: Common unit for all parameters
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
        textInput("unit_common", 
                  label = div(
                    icon("ruler", style = "margin-right: 5px; color: #007bff;"), 
                    "Measurement Unit"
                  ),
                  value = "", 
                  placeholder = "e.g., seconds, minutes, hours")
      ),
      
      # PARAMETER VALUES SECTION
      # STRATEGY: Scrollable container for many parameters
      div(
        div(
          icon("chart-line", style = "margin-right: 5px; color: #28a745;"), 
          h5("Parameter Values", style = "display: inline; margin: 0;")
        ),
        style = "margin-bottom: 15px;",
        
        div(
          style = "max-height: 300px; overflow-y: auto; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; background-color: white;",
          do.call(tagList, value_inputs)
        )
      ),
      
      # MODAL FOOTER WITH ACTION BUTTONS
      # STRATEGY: Clear cancel/confirm options
      footer = tagList(
        actionButton("cancel_modal", "Cancel", 
                     class = "btn-secondary",
                     onclick = "Shiny.setInputValue('modal_dismiss', Math.random())"),
        actionButton("validate_params", "Validate", 
                     class = "btn-success",
                     style = "background-color: #00b8e9; border-color: #00b8e9; font-weight: 600;",
                     icon = icon("check"))
      )
    ))
  })
  
  # MODAL CANCELLATION HANDLER
  # STRATEGY: Handle cancel button clicks
  observeEvent(input$modal_dismiss, {
    removeModal()
  })
  
  # PARAMETER VALIDATION AND STORAGE
  # STRATEGY: Store user parameters for analysis
  # PURPOSE: Save time values and units for curve analysis
  observeEvent(input$validate_params, {
    params <- input$column
    user_params$selected_params <- params  # Store selected parameters
    user_params$times <- sapply(params, function(param) input[[paste0("value_", param)]])
    user_params$unit  <- input$unit_common
    removeModal()
    showNotification("Parameters validated successfully!", type = "message")
  })
  
  # ===========================================
  # SECTION 8: REACTIVE EXPRESSIONS FOR ANALYSIS
  # ===========================================
  # STRATEGY: Centralized reactive logic for data organization
  # PURPOSE: Consistent variable handling across analysis functions
  
  # SELECTED VALUE REACTIVE
  # STRATEGY: Standardized access to selected columns
  VALUE <- reactive({
    req(input$column)
    input$column
  })
  
  # VARIABLE ASSIGNMENT BASED ON FACETING CHOICE
  # STRATEGY: Dynamic variable assignment for flexible faceting
  # PURPOSE: Allow users to choose which variable to use for grouping vs faceting
  facet_var <- reactive({
    if (input$facet_var == "var1") input$var1 else input$var2
  })
  
  x_var <- reactive({
    if (input$facet_var == "var1") input$var2 else input$var1
  })
  
  # ===========================================
  # SECTION 9: GROUP ORDERING UI
  # ===========================================
  # STRATEGY: Drag-and-drop interface for group ordering
  # PURPOSE: User control over plot appearance and legend order
  
  # GROUPING VARIABLE ORDER
  output$var2_order_ui <- renderUI({
    req(result_df$data)
    rank_list(
      text = paste("Order of", x_var()),
      labels = unique(result_df$data[[x_var()]]),
      input_id = "var2_order"
    )
  })
  
  # FACETING VARIABLE ORDER
  output$var1_order_ui <- renderUI({
    req(result_df$data)
    rank_list(
      text = paste("Order of", facet_var()),
      labels = unique(result_df$data[[facet_var()]]),
      input_id = "var1_order"
    )
  })
  
  # ===========================================
  # SECTION 10: DYNAMIC COLOR INPUTS
  # ===========================================
  # STRATEGY: Adaptive color selection based on analysis type
  # PURPOSE: Appropriate color controls for different plot types

  output$dynamic_color_inputs <- renderUI({
    if (input$graph_type == "Bar plot") {
      # BAR PLOT: Three color components
      # STRATEGY: Separate controls for different plot elements
      tagList(
        colourInput("line_color", 
                    label = tags$span("Line color", style = "font-weight: normal;"), 
                    value = "darkgrey"),
        colourInput("fill_color", 
                    label = tags$span("Fill color", style = "font-weight: normal;"), 
                    value = "ivory1"),
        colourInput("point_color", 
                    label = tags$span("Point color", style = "font-weight: normal;"), 
                    value = "darkgreen")
      )
    } else if (input$graph_type == "Curve") {
      # CURVE ANALYSIS: One color per group
      # STRATEGY: Dynamic number of color inputs based on data
      req(input$var2)
      n_lines <- length(unique(result_df$data[[input$var2]]))
      color_inputs <- lapply(seq_len(n_lines), function(i) {
        colourInput(
          inputId = paste0("curve_color_", i),
          label = tags$span(paste("Color for VAR", i), style = "font-weight: normal;"),
          value = scales::hue_pal()(n_lines)[i]  # Default rainbow colors
        )
      })
      do.call(tagList, color_inputs)
    }
  })
  
  # ===========================================
  # SECTION 11: ANALYSIS RESULT DISPLAYS
  # ===========================================
  # STRATEGY: Dynamic result display based on analysis type
  # PURPOSE: Show relevant statistical information to users
  
  # NORMALITY TEST RESULT (BAR PLOT ONLY)
  # STRATEGY: Real-time normality assessment
  # PURPOSE: Inform users about statistical test appropriateness
  output$normality_result <- renderText({
    req(input$start_analysis > 0)  # Only after analysis initiated
    req(result_df$data, input$graph_type == "Bar plot", input$column)
    req(input$column %in% colnames(result_df$data))
    
    var1 <- facet_var()
    var2 <- x_var()
    MEASURE_COL <- VALUE()
    
    # SHAPIRO-WILK TEST
    # STRATEGY: Group-wise normality testing
    shapiro_df <- result_df$data %>%
      dplyr::group_by(!!sym(var2), !!sym(var1)) %>%
      rstatix::shapiro_test(!!sym(MEASURE_COL))
    
    # INTERPRET RESULTS
    # STRATEGY: Simple yes/no interpretation for users
    flag_normal <- check_normality(shapiro_df)
    if (flag_normal) {
      "Datas follow a normal law."
    } else {
      "Datas don't follow a normal law."
    }
  })

  # SELECTED VALUE DISPLAY
  # STRATEGY: Confirmation of user selection
  output$selectedValue <- renderText({
    req(input$start_analysis > 0)  # Only after analysis initiated
    req(result_df$data)
    req(VALUE())
    paste("Selected value for analysis:", paste(VALUE(), collapse = ", "))
  })

  # AUTOMATIC TAB SWITCHING
  # STRATEGY: Guide user to results after analysis
  # PURPOSE: Immediate feedback and result visibility
  observeEvent(input$start_analysis, {
    print("Switching to Analysis Results tab")
    updateTabsetPanel(session, "main_tabs", selected = "Analysis Results")
  })
  
  # ===========================================
  # SECTION 12: MAIN ANALYSIS ENGINE
  # ===========================================
  # STRATEGY: Centralized analysis execution with result storage
  # PURPOSE: Execute statistical analysis and generate plots
  
  # REACTIVE STORAGE FOR EXPORT
  # STRATEGY: Separate storage for plot and statistical data
  # PURPOSE: Enable export functionality independent of display
  current_plot <- reactiveVal(NULL)
  current_stats <- reactiveVal(NULL)
  
  # MAIN ANALYSIS REACTIVE
  # STRATEGY: Event-driven analysis execution
  # PURPOSE: Run analysis only when user clicks "Start Analysis"
  analysis_results <- eventReactive(input$start_analysis, {
    # INPUT VALIDATION
    # STRATEGY: Comprehensive requirement checking
    req(result_df$data)
    req(input$graph_type)
    req(input$facet_var)
    req(input$column %in% colnames(result_df$data))
    req(input$var1_order)
    req(input$var2_order)
    
    if (input$graph_type == "Bar plot") {
      # BAR PLOT ANALYSIS
      # STRATEGY: Delegate to specialized analysis function
      barplot_results <- analyse_barplot(
        data = result_df$data,
        var1 = facet_var(),
        var2 = x_var(),
        measure_col = VALUE(),
        var1_order = input$var1_order,
        var2_order = input$var2_order,
        fill_color = input$fill_color,
        line_color = input$line_color,
        point_color = input$point_color
      )
      
      # STORE RESULTS FOR EXPORT
      # STRATEGY: Separate storage enables independent export functionality
      current_plot(barplot_results$plot)
      current_stats(barplot_results)
      
      return(barplot_results$plot)
      
    } else if (input$graph_type == "Curve") {
      # CURVE ANALYSIS
      # STRATEGY: Additional validation for curve-specific requirements
      req(input$var2)
      req(user_params$selected_params)
      req(input$column)
      req(input$control_group)  # Control group required for statistical comparisons

      # COLOR VECTOR PREPARATION
      # STRATEGY: Extract user-selected colors for each group
      n_lines <- length(unique(result_df$data[[input$var2]]))
      curve_colors <- sapply(seq_len(n_lines), function(i) {
        color_input <- input[[paste0("curve_color_", i)]]
        if(is.null(color_input)) "#000000" else color_input
      })

      # DATA TRANSFORMATION: WIDE TO LONG FORMAT
      # STRATEGY: Convert time points from columns to rows for analysis
      # PURPOSE: Analysis functions expect long format data
      long_data <- result_df$data %>%
        select(all_of(c(x_var(), facet_var(), input$column))) %>%
        pivot_longer(
          cols = all_of(input$column),        # Time point columns to reshape
          names_to = "time_point",            # New column with time point names
          values_to = "parameter_value"       # New column with measurement values
        ) %>%
        mutate(
          # TIME VALUE MAPPING
          # STRATEGY: Map time point names to user-provided numeric values
          # PURPOSE: Convert column names (L1, L2, D1, D2) to actual time values
          time_numeric = case_when(
            str_detect(time_point, "L1") ~ user_params$times[1],
            str_detect(time_point, "L2") ~ user_params$times[2],
            str_detect(time_point, "L3") ~ user_params$times[3],
            str_detect(time_point, "L4") ~ user_params$times[4],
            str_detect(time_point, "L5") ~ user_params$times[5],
            str_detect(time_point, "L6") ~ user_params$times[6],
            str_detect(time_point, "L7") ~ user_params$times[7],
            str_detect(time_point, "L8") ~ user_params$times[8],
            str_detect(time_point, "L9") ~ user_params$times[9],
            str_detect(time_point, "D1") ~ user_params$times[10],
            str_detect(time_point, "D2") ~ user_params$times[11],
            TRUE ~ NA_real_
          )
        )
      
      # CURVE ANALYSIS EXECUTION
      # STRATEGY: Delegate to specialized qGAM analysis function
      curve_results <- analyse_curve(
        df = long_data,
        col_vector = curve_colors,
        parameter_col = "parameter_value",     # Transformed parameter column
        time_col = "time_numeric",             # Transformed time column
        grouping_col = x_var(),
        facet_col = facet_var(),
        control_group = input$control_group,
        user_params = reactiveValuesToList(user_params)
      )
      
      # STORE RESULTS FOR EXPORT
      current_plot(curve_results$plot)
      current_stats(curve_results)
      
      return(curve_results$plot)
    }
  })

  # PLOT RENDERING
  # STRATEGY: Display analysis results in main panel
  # PURPOSE: Show generated plots to users
  output$plot_result <- renderPlot({
    analysis_results()
  })
  
  # RESULT AVAILABILITY CONFIRMATION
  # STRATEGY: Ensure export functionality knows when results are ready
  observeEvent(analysis_results(), {
    req(analysis_results())
    # Results are automatically stored in current_plot and current_stats
    # This observer ensures they're available for export
  })
  
  # ===========================================
  # SECTION 13: EXPORT FUNCTIONALITY
  # ===========================================
  # STRATEGY: Professional export capabilities for data and plots
  # PURPOSE: Enable users to save and share analysis results
  
  # 13.1 STATISTICAL DATA EXPORT
  # STRATEGY: Multiple text files in zip archive for universal compatibility
  # PURPOSE: Organized, readable statistical output
  observeEvent(input$export_stats, {
    req(current_stats())  # Require completed analysis
    
    zip_filename <- paste0(input$stats_filename, ".zip")
    
    tryCatch({
      temp_dir <- tempdir()
      file_list <- c()
      stats_data <- current_stats()  # Get stored statistical results
      
      if (input$graph_type == "Bar plot") {
        # BAR PLOT STATISTICAL EXPORTS
        # STRATEGY: Separate file for each type of statistical result
        # PURPOSE: Organized output that's easy to navigate
        
        if (!is.null(stats_data$summary)) {
          summary_file <- file.path(temp_dir, "summary_statistics.txt")
          write.table(stats_data$summary, file = summary_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, summary_file)
        }
        
        if (!is.null(stats_data$shapiro)) {
          shapiro_file <- file.path(temp_dir, "shapiro_normality_test.txt")
          write.table(stats_data$shapiro, file = shapiro_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, shapiro_file)
        }
        
        if (!is.null(stats_data$anova)) {
          anova_file <- file.path(temp_dir, "anova_results.txt")
          write.table(stats_data$anova, file = anova_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, anova_file)
        }
        
        if (!is.null(stats_data$tukey)) {
          tukey_file <- file.path(temp_dir, "tukey_hsd_test.txt")
          write.table(stats_data$tukey, file = tukey_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, tukey_file)
        }
        
        if (!is.null(stats_data$kruskal)) {
          kruskal_file <- file.path(temp_dir, "kruskal_wallis_test.txt")
          write.table(stats_data$kruskal, file = kruskal_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, kruskal_file)
        }
        
        if (!is.null(stats_data$dunn)) {
          dunn_file <- file.path(temp_dir, "dunn_post_hoc_test.txt")
          write.table(stats_data$dunn, file = dunn_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, dunn_file)
        }
        
        if (!is.null(stats_data$cld)) {
          cld_file <- file.path(temp_dir, "compact_letter_display.txt")
          write.table(stats_data$cld, file = cld_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, cld_file)
        }
        
      } else if (input$graph_type == "Curve") {
        # CURVE ANALYSIS STATISTICAL EXPORTS
        # STRATEGY: Export qGAM predictions and statistical test results
        
        if (!is.null(stats_data$qgam_predictions)) {
          qgam_file <- file.path(temp_dir, "qgam_model_predictions.txt")
          write.table(stats_data$qgam_predictions, file = qgam_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, qgam_file)
        }
        
        if (!is.null(stats_data$statistical_results) && nrow(stats_data$statistical_results) > 0) {
          stats_file <- file.path(temp_dir, "curve_statistical_tests.txt")
          write.table(stats_data$statistical_results, file = stats_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, stats_file)
        }
        
        if (!is.null(stats_data$median_points)) {
          median_file <- file.path(temp_dir, "median_data_points.txt")
          write.table(stats_data$median_points, file = median_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, median_file)
        }
      }
      
      # ANALYSIS PARAMETERS SUMMARY
      # STRATEGY: Include analysis settings for reproducibility
      # PURPOSE: Document how analysis was performed
      params_file <- file.path(temp_dir, "analysis_parameters.txt")
      params_info <- data.frame(
        Parameter = c("Analysis Type", "Parameter Column", "Grouping Variable", "Facet Variable", 
                     if(input$graph_type == "Curve") c("Control Group", "Time Unit") else NULL),
        Value = c(input$graph_type, 
                 if(input$graph_type == "Bar plot") input$column else paste(input$column, collapse = ", "),
                 input$var1, input$facet_var,
                 if(input$graph_type == "Curve") c(input$control_group %||% "None", 
                                                  user_params$unit %||% "Not specified") else NULL),
        stringsAsFactors = FALSE
      )
      write.table(params_info, file = params_file, 
                 sep = "\t", row.names = FALSE, quote = FALSE)
      file_list <- c(file_list, params_file)
      
      # ZIP ARCHIVE CREATION
      # STRATEGY: Temporary directory manipulation for clean zip structure
      # PURPOSE: Create zip with just filenames, not full paths
      if (length(file_list) > 0) {
        current_wd <- getwd()
        setwd(temp_dir)
        zip(zipfile = file.path(current_wd, zip_filename), 
            files = basename(file_list))
        setwd(current_wd)
        
        showNotification(
          paste("Statistical data exported successfully as", zip_filename, "with", length(file_list), "files!"), 
          type = "message", duration = 5
        )
      } else {
        showNotification("No statistical data available to export.", type = "warning")
      }
      
    }, error = function(e) {
      showNotification(paste("Error exporting statistical data:", e$message), type = "error")
    })
  })
  
  # 13.2 PLOT EXPORT
  # STRATEGY: High-quality plot export with user customization
  # PURPOSE: Professional plot output for publications and presentations
  observeEvent(input$export_plot, {
    req(current_plot())  # Require completed analysis
    
    # FILENAME CONSTRUCTION
    # STRATEGY: Ensure proper file extension regardless of user input
    # PURPOSE: Prevent file extension conflicts and ensure correct format
    base_filename <- input$plot_filename
    base_filename <- tools::file_path_sans_ext(base_filename)  # Remove existing extension
    filename <- paste0(base_filename, ".", input$plot_format)  # Add correct extension
    full_path <- file.path(getwd(), filename)
    
    # PLOT EXPORT EXECUTION
    # STRATEGY: Format-specific export with user-defined dimensions
    # PURPOSE: High-quality output suitable for different use cases
    tryCatch({
      if (input$plot_format %in% c("png", "jpg", "jpeg")) {
        # RASTER FORMATS: High DPI for quality
        ggsave(
          filename = full_path,
          plot = current_plot(),
          width = as.numeric(input$plot_width),
          height = as.numeric(input$plot_height),
          units = input$plot_units,  # User-selected units (cm, in, mm)
          dpi = 300  # High resolution for publications
        )
      } else if (input$plot_format == "pdf") {
        # PDF FORMAT: Vector format for scalability
        ggsave(
          filename = full_path,
          plot = current_plot(),
          width = as.numeric(input$plot_width),
          height = as.numeric(input$plot_height),
          units = input$plot_units
        )
      } else if (input$plot_format == "svg") {
        # SVG FORMAT: Scalable vector graphics
        ggsave(
          filename = full_path,
          plot = current_plot(),
          width = as.numeric(input$plot_width),
          height = as.numeric(input$plot_height),
          units = input$plot_units
        )
      }
      
      showNotification(paste("Plot exported successfully as", filename, "!"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error exporting plot:", e$message), type = "error")
    })
  })
  
  # ===========================================
  # SECURITY AND RESOURCE MANAGEMENT
  # ===========================================
  # STRATEGY: Implement comprehensive security and resource limits
  # PURPOSE: Protect server resources and ensure fair usage
  
  # FILE SIZE LIMITS
  # STRATEGY: Prevent abuse through large file uploads
  # Maximum total upload size per session
  MAX_SESSION_SIZE <- 100 * 1024 * 1024  # 100 MB
  
  # VALIDATE SESSION SIZE
  validate_session_size <- function() {
    if (dir.exists(session_dir)) {
      total_size <- sum(file.size(list.files(session_dir, full.names = TRUE)), na.rm = TRUE)
      if (total_size > MAX_SESSION_SIZE) {
        showNotification("Session file size limit exceeded. Please reduce file sizes.", 
                        type = "error")
        return(FALSE)
      }
    }
    return(TRUE)
  }
  
  # FILE TYPE VALIDATION
  # STRATEGY: Only allow text files to prevent security issues
  validate_file_content <- function(filepath) {
    tryCatch({
      # READ FIRST FEW LINES TO CHECK FORMAT
      lines <- readLines(filepath, n = 10, warn = FALSE)
      # CHECK IF IT LOOKS LIKE A FLUORCAM FILE
      return(length(lines) > 2 && any(grepl("Measurement|Time|Area", lines, ignore.case = TRUE)))
    }, error = function(e) {
      return(FALSE)
    })
  }
  # ===========================================
  # END OF SERVER FUNCTION
  # ===========================================
}

