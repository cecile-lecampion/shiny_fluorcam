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
  # PURPOSE: Works on Windows, Mac, and Linux
  volumes <- c(Home = fs::path_home(), "R" = R.home(), getVolumes()())
  shinyDirChoose(input, "dir", roots = volumes, session = session)
  
  # REACTIVE DIRECTORY PATH
  # STRATEGY: Convert shinyDirChoose output to usable path string
  # PURPOSE: Reactive expression for automatic updates when directory changes
  dirpath <- reactive({
    parseDirPath(volumes, input$dir)
  })
  
  # DISPLAY SELECTED DIRECTORY
  # PURPOSE: User feedback - show currently selected directory
  output$dirpath <- renderText({ dirpath() })
  
  # ===========================================
  # SECTION 1.1: SESSION-SPECIFIC FILE MANAGEMENT
  # ===========================================
  # STRATEGY: Create unique workspace for each user session
  # PURPOSE: Isolate user files and enable automatic cleanup

  # CREATE UNIQUE SESSION DIRECTORY
  # STRATEGY: Use session token for unique folder names
  # PURPOSE: Prevent file mixing between concurrent users
  session_id <- session$token
  session_dir <- file.path(tempdir(), paste0("fluorcam_session_", session_id))

  # ENSURE SESSION DIRECTORY EXISTS
  # STRATEGY: Create session directory if it doesn't exist
  # PURPOSE: Ensure workspace is ready for file operations
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
    all_dirs <- list.dirs(temp_base, full.names = TRUE, recursive = FALSE)
    session_dirs <- all_dirs[grepl("fluorcam_session_", basename(all_dirs))]
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
  # STRATEGY: Run cleanup every 30 minutes using reactive timer
  # PURPOSE: Maintain server resources by removing old session files
  observeEvent(reactiveTimer(30 * 60 * 1000)(), {
    cleanup_old_sessions()
  })
   
  # ===========================================
  # SECTION 1.2: DYNAMIC UI FOR VARIABLE NAMING PATTERN
  # ===========================================
  # STRATEGY: Generate dynamic UI for file naming configuration
  # PURPOSE: Allow flexible number of variables in file names

  # Generate naming pattern info dynamically
  output$naming_pattern_info <- renderUI({
    num_vars <- input$num_vars
    if (is.null(num_vars)) num_vars <- 3
    
    # Create pattern string
    pattern_parts <- paste0("VAR", 1:num_vars)
    pattern <- paste(pattern_parts, collapse = "_")
    pattern_full <- paste0(pattern, ".TXT")
    
    # Create example
    example_values <- c("Day1", "LineA", "Plant001", "Rep1", "Treatment1", "Block1")
    example <- paste(example_values[1:num_vars], collapse = "_")
    example_full <- paste0(example, ".TXT")
    
    div(class = "alert alert-info", style = "margin-bottom: 15px;",
        icon("info-circle"),
        strong(" Required File Naming Pattern:"),
        br(), br(),
        tags$code(pattern_full, 
                 style = "font-size: 14px; background-color: #f8f9fa; padding: 15px; display: block;"),
        br(),
        em(paste0("Example: ", example_full))
    )
  })

  # Generate dynamic variable name inputs
  output$dynamic_var_inputs <- renderUI({
    num_vars <- input$num_vars
    if (is.null(num_vars)) num_vars <- 3
    
    # Default names for variables
    default_names <- c("Day", "Line", "PlantID", "Replicate", "Treatment", "Block")
    
    # Calculate number of columns based on number of variables
    col_width <- 12 / num_vars
    if (col_width < 3) col_width <- 3  # Minimum width
    # Create inputs for each variable
    var.inputs <- lapply(1:num_vars, function(i) {
      default_value <- if (i <= length(default_names)) default_names[i] else paste0("Var", i)
      
      column(col_width,
             div(style = "text-align: center; margin-bottom: 10px;",
                 strong(paste0("VAR", i)),
                 textInput(paste0("var", i), 
                          NULL, 
                          value = default_value, 
                          placeholder = paste0("e.g., ", default_value))
             )
      )
    })
    
    # Return in a fluidRow
    do.call(fluidRow, var.inputs)
  })

  # Generate filename example
  output$filename_example <- renderText({
    num_vars <- input$num_vars
    if (is.null(num_vars)) num_vars <- 3
    
    # Get variable names from inputs
    var_names <- sapply(1:num_vars, function(i) {
      var_name <- input[[paste0("var", i)]]
      if (is.null(var_name) || var_name == "") {
        paste0("Var", i, "1")
      } else {
        paste0(var_name, ifelse(i == num_vars, "001", ifelse(i == 1, "1", "A")))
      }
    })
    
    # Create example filename
    paste0(paste(var_names, collapse = "_"), ".TXT")
  })

  # ===========================================
  # SECTION 2: FILE UPLOAD FUNCTIONALITY
  # ===========================================
  # STRATEGY: Handle file uploads with validation and session isolation
  # PURPOSE: Replace directory browsing with file upload

  # FILE UPLOAD PROCESSING
  # STRATEGY: Process uploaded files with validation and session organization
  # PURPOSE: Handle file uploads, validate naming convention, and organize in session directory
  observeEvent(input$uploaded_files, {
    req(input$uploaded_files)
    req(input$num_vars)  # Need to know expected number of variables

    tryCatch({
      # COPY UPLOADED FILES TO SESSION DIRECTORY
      # STRATEGY: Move files from temp upload to session-specific folder
      # PURPOSE: Organize files and enable processing
      uploaded_paths <- input$uploaded_files$datapath
      original_names <- input$uploaded_files$name

      # VALIDATE FILE NAMES
      # STRATEGY: Check naming convention before processing (dynamic based on num_vars)
      # PURPOSE: Early validation to prevent processing errors
      invalid_files <- c()
      valid_files <- c()
      
      # Build dynamic pattern based on number of variables
      # STRATEGY: Create regex pattern that matches exact number of underscores
      # PURPOSE: Ensure file naming convention matches user specification
      num_underscores <- input$num_vars - 1
      pattern <- paste0("^", paste(rep("[^_]+", input$num_vars), collapse = "_"), "\\.(txt|TXT)$")

      for (i in seq_along(original_names)) {
        # CHECK NAMING PATTERN: Dynamic based on num_vars
        if (grepl(pattern, original_names[i])) {
          # COPY TO SESSION DIRECTORY
          dest_path <- file.path(session_dir, original_names[i])
          file.copy(uploaded_paths[i], dest_path, overwrite = TRUE)
          valid_files <- c(valid_files, original_names[i])
        } else {
          invalid_files <- c(invalid_files, original_names[i])
        }
      }

      # VALIDATION FEEDBACK
      # STRATEGY: Provide user feedback on file validation results
      # PURPOSE: Inform user of successful uploads and naming issues
      if (length(invalid_files) > 0) {
        # Create expected pattern example for user
        pattern_example <- paste(rep("VAR", input$num_vars), collapse = "_")
        showNotification(
          paste0("Invalid file names (must be ", pattern_example, ".txt): ",
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
  # STRATEGY: Show current upload status and file count
  # PURPOSE: Provide real-time feedback on upload status
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
  # STRATEGY: Reactive flag to indicate if files are available
  # PURPOSE: Enable conditional UI elements based on file availability
  output$files_uploaded <- reactive({
    !is.null(input$uploaded_files) && length(list.files(session_dir, pattern = "\\.(txt|TXT)$")) > 0
  })
  outputOptions(output, "files_uploaded", suspendWhenHidden = FALSE)

  # REACTIVE VARIABLE FOR CONTROLLING UPLOADED FILES DISPLAY
  # STRATEGY: Use reactiveVal for simple boolean state management
  # PURPOSE: Control visibility of uploaded files list
  show_uploaded_files <- reactiveVal(FALSE)

  # TOGGLE BUTTON FOR UPLOADED FILES
  # STRATEGY: Dynamic toggle button with file count display
  # PURPOSE: Allow users to show/hide uploaded files list with context
  output$toggle_files_button <- renderUI({
    req(input$uploaded_files)
    
    file_count <- nrow(input$uploaded_files)
    
    if (show_uploaded_files()) {
      actionButton("toggle_uploaded_files", "Hide file list", 
                  icon = icon("chevron-up"),
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-bottom: 10px;")
    } else {
      actionButton("toggle_uploaded_files", paste("Show uploaded files (", file_count, ")"), 
                  icon = icon("chevron-down"),
                  class = "btn-sm btn-outline-secondary", 
                  style = "margin-bottom: 10px;")
    }
  })

  # OBSERVER FOR TOGGLE BUTTON
  # STRATEGY: Simple state toggle when button is clicked
  # PURPOSE: Toggle visibility state of uploaded files display
  observeEvent(input$toggle_uploaded_files, {
    show_uploaded_files(!show_uploaded_files())
  })

  # CONDITIONAL DISPLAY OF UPLOADED FILES TABLE
  # STRATEGY: Show table only when toggle is activated
  # PURPOSE: Clean UI with optional detailed file information
  output$uploaded_files_display <- renderUI({
    req(input$uploaded_files, show_uploaded_files())
    
    tableOutput("uploaded_files_table_main")
  })

  # UPLOADED FILES TABLE FOR MAIN PANEL
  # STRATEGY: Formatted table with file information and size formatting
  # PURPOSE: Display uploaded file details in user-friendly format
  output$uploaded_files_table_main <- renderTable({
    req(input$uploaded_files)
    
    files_df <- input$uploaded_files
    display_df <- files_df[, c("name", "size")]
    
    # FILE SIZE FORMATTING
    # STRATEGY: Convert bytes to human-readable format
    # PURPOSE: Display file sizes in appropriate units (B, KB, MB)
    display_df$size <- sapply(display_df$size, function(x) {
      if (x < 1024) paste(x, "B")
      else if (x < 1024^2) paste(round(x/1024, 1), "KB")
      else paste(round(x/1024^2, 1), "MB")
    })
    
    colnames(display_df) <- c("File Name", "Size")
    display_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # UPLOADED FILES TABLE
  # STRATEGY: Display files currently in session directory
  # PURPOSE: Show processed files ready for analysis
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

  # NOTE: Removed directory display output (output$dirpath) as it's no longer needed

  # ===========================================
  # SECURITY AND RESOURCE MANAGEMENT
  # ===========================================
  # STRATEGY: Implement comprehensive security and resource limits
  # PURPOSE: Protect server resources and ensure fair usage

  # FILE SIZE LIMITS
  # STRATEGY: Prevent abuse through large file uploads
  # PURPOSE: Set reasonable limits to protect server resources
  MAX_SESSION_SIZE <- 100 * 1024 * 1024  # 100 MB

  # VALIDATE SESSION SIZE
  # STRATEGY: Check total session size against limits
  # PURPOSE: Prevent resource exhaustion from large uploads
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
  # PURPOSE: Validate file content matches expected FluorCam format
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
  # SECTION 2.1: FILE PREVIEW FUNCTIONALITY
  # ===========================================
  # STRATEGY: Allow users to preview files before loading
  # PURPOSE: Prevent loading wrong files, provide transparency

  # FILE LIST TOGGLE STATE
  # STRATEGY: ReactiveVal for simple boolean state management
  # PURPOSE: Control whether to show all files or just first 5
  show_all_files <- reactiveVal(FALSE)
  observeEvent(input$show_all, { show_all_files(!show_all_files()) })
  
  # DYNAMIC FILE LIST DISPLAY (MODIFIED)
  # STRATEGY: Show uploaded files automatically in main panel
  # PURPOSE: Direct preview of uploaded files without toggle button
  output$selected_files <- renderTable({
    # Show files from session directory (uploaded files)
    files <- list.files(path = session_dir, pattern = "\\.(txt|TXT)$", full.names = TRUE)
    
    # Apply pattern filter if specified
    if (!is.null(input$pattern) && input$pattern != "") {
      files <- files[grepl(input$pattern, basename(files), ignore.case = TRUE)]
    }
    
    # CONDITIONAL DISPLAY: Show 5 or all based on toggle state
    if (!show_all_files()) files <- head(files, 5)
    
    if (length(files) > 0) {
      data.frame(
        Files = basename(files),
        `Size (KB)` = round(file.size(files) / 1024, 1),
        check.names = FALSE
      )
    } else {
      if (!is.null(input$pattern) && input$pattern != "") {
        data.frame(Files = "No uploaded files match the pattern", `Size (KB)` = NA, check.names = FALSE)
      } else {
        data.frame(Files = "No files uploaded yet", `Size (KB)` = NA, check.names = FALSE)
      }
    }
  })
  
  # REACTIVE TRIGGER FOR FILE LIST UPDATE
  # STRATEGY: Update file list when files are uploaded
  # PURPOSE: Automatic refresh of file display
  observeEvent(input$uploaded_files, {
    # Force update of the file list display
    output$selected_files <- renderTable({
      files <- list.files(path = session_dir, pattern = "\\.(txt|TXT)$", full.names = TRUE)
      
      # Apply pattern filter if specified
      if (!is.null(input$pattern) && input$pattern != "") {
        files <- files[grepl(input$pattern, basename(files), ignore.case = TRUE)]
      }
      
      # CONDITIONAL DISPLAY: Show 5 or all based on toggle state
      if (!show_all_files()) files <- head(files, 5)
      
      if (length(files) > 0) {
        data.frame(
          Files = basename(files),
          `Size (KB)` = round(file.size(files) / 1024, 1),
          check.names = FALSE
        )
      } else {
        if (!is.null(input$pattern) && input$pattern != "") {
          data.frame(Files = "No uploaded files match the pattern", `Size (KB)` = NA, check.names = FALSE)
        } else {
          data.frame(Files = "No files uploaded yet", `Size (KB)` = NA, check.names = FALSE)
        }
      }
    })
  })

  # UPDATE THE TOGGLE BUTTON TEXT
  # STRATEGY: Show appropriate text for file list toggle
  # PURPOSE: Clear indication of current state
  observeEvent(show_all_files(), {
    files <- list.files(path = session_dir, pattern = "\\.(txt|TXT)$")
    if (length(files) > 5) {
      button_text <- if(show_all_files()) "Show first 5 files" else "Show all files"
      updateActionButton(session, "show_all", label = button_text)
    }
  })

  # CONDITIONAL SHOW ALL BUTTON
  # STRATEGY: Only show toggle button when there are more than 5 files
  # PURPOSE: Clean UI when toggle is not needed
  output$show_all_button <- renderUI({
    files <- list.files(path = session_dir, pattern = "\\.(txt|TXT)$")
    if (length(files) > 5) {
      button_text <- if(show_all_files()) "Show first 5 files" else "Show all files"
      actionButton("show_all", button_text, class = "btn-info btn-sm")
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
  # PURPOSE: Persists across data reloads
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

  # DATA LOADING EVENT HANDLER
  # STRATEGY: Event-driven data processing with comprehensive validation
  # PURPOSE: Process uploaded files and prepare data for analysis
  observeEvent(input$load, {
    # INPUT VALIDATION - DYNAMIC BASED ON NUMBER OF VARIABLES
    req(input$pattern)
    req(input$num_vars)
    
    # Validate all variable name inputs
    var_names <- sapply(1:input$num_vars, function(i) {
      input[[paste0("var", i)]]
    })
    
    # Check if all variables are defined
    if (any(is.null(var_names)) || any(var_names == "")) {
      showNotification("Please define all variable names before loading data.", type = "error")
      return()
    }

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
    # STRATEGY: Store inputs in local variables for processing
    # PURPOSE: Clean code and consistent variable access
    pattern <- input$pattern
  
    # MAIN DATA PROCESSING
    tryCatch({
      # CALL DATA PROCESSING FUNCTION
      # STRATEGY: Pass variable names as character vector
      # PURPOSE: Support flexible number of variables
    
      processed_data <- process_data_files(
        pattern = pattern,
        var_names = var_names,  # Pass as simple character vector
        dirpath = session_dir
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
      # ERROR HANDLING
      # STRATEGY: User-friendly error messages + console logging for debugging
      showNotification(paste("Failed to load data files:", e$message), type = "error")
      print(e)
    })
})
  
  # ===========================================
  # SECTION 5: DATA TABLE RENDERING
  # ===========================================
  # STRATEGY: Interactive data table with responsive design
  # PURPOSE: Allow users to explore loaded data with horizontal scrolling and filtering

  # MAIN DATA TABLE RENDERER
  # STRATEGY: Responsive table with different modes (preview vs full)
  # PURPOSE: Provide appropriate data exploration interface based on user needs
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
    # PATTERN EXTRACTION: Find columns with time point suffixes
    # STRATEGY: Support both standard (_L1, _D1) and subsecond (_Lss1, _Dss1) formats
    # PURPOSE: Handle different FluorCam export formats
    pattern <- "(_L[0-9]+|_D[0-9]+|_Lss[0-9]+|_Dss[0-9]+)$"
    roots <- unique(sub(pattern, "", all_cols[grepl(pattern, all_cols)]))
    selectInput("root", "Select the parameter root", choices = roots)
  })

  # ANALYSIS VARIABLE CANDIDATES (FROM FILENAME VARIABLES)
  # STRATEGY: Use user-defined naming variables as experimental factors
  # PURPOSE: Allow >3 variables and explicit factor role assignment
  analysis_var_choices <- reactive({
    req(result_df$data)
    req(input$num_vars)

    vars <- sapply(seq_len(input$num_vars), function(i) {
      input[[paste0("var", i)]]
    })
    vars <- vars[!is.null(vars) & vars != ""]
    vars <- vars[vars %in% colnames(result_df$data)]

    if (length(vars) == 0) {
      vars <- colnames(result_df$data)[seq_len(min(2, ncol(result_df$data)))]
    }

    unique(vars)
  })

  # CURVE ANALYSIS CONFIGURATION UI
  # STRATEGY: Let user choose grouping, facet and optional extra split variable
  # PURPOSE: Extend line chart workflow beyond legacy var1/var2 layout
  output$curve_grouping_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Curve")

    var_choices <- analysis_var_choices()
    req(length(var_choices) >= 1)

    selected_group <- if (!is.null(input$curve_group_var) && input$curve_group_var %in% var_choices) {
      input$curve_group_var
    } else {
      var_choices[1]
    }

    remaining_for_facet <- setdiff(var_choices, selected_group)
    selected_facet <- if (!is.null(input$curve_facet_var) && input$curve_facet_var %in% remaining_for_facet) {
      input$curve_facet_var
    } else if (length(remaining_for_facet) > 0) {
      remaining_for_facet[1]
    } else {
      selected_group
    }

    remaining_for_split <- setdiff(var_choices, c(selected_group, selected_facet))
    selected_split <- if (!is.null(input$curve_split_var) &&
      (identical(input$curve_split_var, "None") || input$curve_split_var %in% remaining_for_split)) {
      input$curve_split_var
    } else {
      "None"
    }

    tagList(
      selectInput(
        "curve_group_var",
        label = "Grouping variable (color)",
        choices = var_choices,
        selected = selected_group
      ),
      selectInput(
        "curve_facet_var",
        label = "Stratification / Facet variable",
        choices = if (length(remaining_for_facet) > 0) remaining_for_facet else var_choices,
        selected = selected_facet
      ),
      selectInput(
        "curve_split_var",
        label = "Optional split variable",
        choices = c("None", remaining_for_split),
        selected = selected_split
      )
    )
  })

  # BAR PLOT ANALYSIS CONFIGURATION UI
  # STRATEGY: Dynamic controls based on selected statistical model
  # PURPOSE: Enable one-way, two-way and three-way ANOVA workflows
  output$bar_analysis_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Bar plot")

    var_choices <- analysis_var_choices()
    req(length(var_choices) >= 1)

    selected_a <- if (!is.null(input$bar_factor_a) && input$bar_factor_a %in% var_choices) {
      input$bar_factor_a
    } else {
      var_choices[1]
    }

    remaining_choices <- setdiff(var_choices, selected_a)
    selected_b <- if (!is.null(input$bar_factor_b) && input$bar_factor_b %in% remaining_choices) {
      input$bar_factor_b
    } else if (length(remaining_choices) > 0) {
      remaining_choices[1]
    } else {
      selected_a
    }

    remaining_after_b <- setdiff(remaining_choices, selected_b)
    selected_c <- if (!is.null(input$bar_factor_c) && input$bar_factor_c %in% remaining_after_b) {
      input$bar_factor_c
    } else if (length(remaining_after_b) > 0) {
      remaining_after_b[1]
    } else if (length(remaining_choices) > 0) {
      remaining_choices[1]
    } else {
      selected_a
    }

    tagList(
      selectInput(
        "stat_model",
        "Statistical model",
        choices = c(
          "One-way ANOVA (default)" = "oneway_anova",
          "Two-way ANOVA" = "twoway_anova",
          "Three-way ANOVA" = "threeway_anova"
        ),
        selected = "oneway_anova"
      ),
      selectInput(
        "bar_factor_a",
        "Primary factor (A)",
        choices = var_choices,
        selected = selected_a
      ),
      conditionalPanel(
        condition = "input.stat_model == 'oneway_anova'",
        selectInput(
          "oneway_parametric_strategy",
          "One-way parametric strategy",
          choices = c(
            "Classical (ANOVA + Tukey HSD)" = "classical",
            "Variance-robust (use Welch + Games-Howell if Levene p <= 0.05)" = "welch"
          ),
          selected = "classical"
        )
      ),
      conditionalPanel(
        condition = "input.stat_model == 'twoway_anova' || input.stat_model == 'threeway_anova'",
        selectInput(
          "bar_factor_b",
          "Secondary factor (B)",
          choices = if (length(remaining_choices) > 0) remaining_choices else var_choices,
          selected = selected_b
        )
      ),
      conditionalPanel(
        condition = "input.stat_model == 'threeway_anova'",
        selectInput(
          "bar_factor_c",
          "Third factor (C)",
          choices = if (length(remaining_after_b) > 0) remaining_after_b else var_choices,
          selected = selected_c
        )
      ),
      conditionalPanel(
        condition = "input.stat_model == 'oneway_anova' || input.stat_model == 'twoway_anova' || input.stat_model == 'threeway_anova'",
        selectInput(
          "bar_facet_var",
          "Stratification / Facet variable (optional)",
          choices = c("None", var_choices),
          selected = "None"
        )
      ),
      conditionalPanel(
        condition = "input.stat_model == 'twoway_anova' || input.stat_model == 'threeway_anova'",
        selectInput(
          "parametric_strategy",
          "Statistical decision strategy",
          choices = c(
            "Conservative (switch to ART if any Shapiro p <= 0.05)" = "conservative",
            "Robust parametric (retain ANOVA when variance/balance checks are acceptable)" = "robust_parametric"
          ),
          selected = "conservative"
        )
      )
    )
  })

  # ONE/TWO/THREE-WAY DESIGN DIAGNOSTIC UI (BAR PLOT)
  # STRATEGY: Show analyzable combinations before running ANOVA
  # PURPOSE: Help users identify facets with insufficient levels
  output$bar_design_diagnostic_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Bar plot")

    if (!identical(input$stat_model, "oneway_anova") && !identical(input$stat_model, "twoway_anova") && !identical(input$stat_model, "threeway_anova")) {
      return(NULL)
    }

    is_oneway <- identical(input$stat_model, "oneway_anova")
    is_threeway <- identical(input$stat_model, "threeway_anova")
    title_text <- if (is_oneway) {
      "One-way design check"
    } else if (is_threeway) {
      "Three-way design check"
    } else {
      "Two-way design check"
    }

    level_text <- if (is_oneway) {
      "Status is OK only when each facet has at least 2 levels in A."
    } else if (is_threeway) {
      "Status is OK only when each facet has at least 2 levels in A, B and C."
    } else {
      "Status is OK only when each facet has at least 2 levels in A and 2 levels in B."
    }

    tagList(
      div(
        class = "design-check-title",
        icon("table"),
        tags$span(title_text)
      ),
      uiOutput("bar_design_summary_ui"),
      div(
        class = "alert alert-info",
        style = "margin-top: 6px; margin-bottom: 10px;",
        "Counts are computed on complete rows (response and selected factors all non-missing).",
        tags$br(),
        level_text
      ),
      div(
        class = "design-check-container",
        DT::dataTableOutput("bar_design_diagnostic")
      )
    )
  })

  output$bar_design_summary_ui <- renderUI({
    diagnostic_tbl <- bar_design_diagnostic_data()
    req(is.data.frame(diagnostic_tbl), nrow(diagnostic_tbl) > 0)

    ok_n <- sum(diagnostic_tbl$Status == "OK", na.rm = TRUE)
    warn_n <- sum(diagnostic_tbl$Status == "Insufficient levels", na.rm = TRUE)
    facet_n <- if ("Facet" %in% names(diagnostic_tbl)) length(unique(diagnostic_tbl$Facet)) else 0

    div(
      class = "design-summary-row",
      tags$span(class = "design-summary-pill", paste("Rows:", nrow(diagnostic_tbl))),
      tags$span(class = "design-summary-pill", paste("Facets:", facet_n)),
      tags$span(class = "design-summary-pill ok", paste("OK:", ok_n)),
      tags$span(class = "design-summary-pill warn", paste("Insufficient:", warn_n))
    )
  })

  bar_design_diagnostic_data <- reactive({
    req(result_df$data)
    req(input$graph_type == "Bar plot")
    req(identical(input$stat_model, "oneway_anova") || identical(input$stat_model, "twoway_anova") || identical(input$stat_model, "threeway_anova"))
    req(input$bar_factor_a)
    req(length(VALUE()) >= 1)

    is_oneway <- identical(input$stat_model, "oneway_anova")
    is_threeway <- identical(input$stat_model, "threeway_anova")

    measure_col <- VALUE()[1]
    factor_a <- input$bar_factor_a
    factor_b <- if (is_oneway) NULL else input$bar_factor_b
    factor_c <- if (is_threeway) input$bar_factor_c else NULL
    facet_col <- if (!is.null(input$bar_facet_var) && input$bar_facet_var != "None") input$bar_facet_var else NULL

    req(all(c(factor_a, measure_col) %in% names(result_df$data)))
    if (!is_oneway) {
      req(factor_b)
      req(factor_b %in% names(result_df$data))
    }
    if (is_threeway) {
      req(factor_c)
      req(factor_c %in% names(result_df$data))
    }
    if (!is.null(facet_col)) {
      req(facet_col %in% names(result_df$data))
    }

    diagnostic_df <- data.frame(
      facet = if (is.null(facet_col)) rep("All data", nrow(result_df$data)) else as.character(result_df$data[[facet_col]]),
      factor_a = as.character(result_df$data[[factor_a]]),
      response = suppressWarnings(as.numeric(result_df$data[[measure_col]])),
      stringsAsFactors = FALSE
    )
    if (!is_oneway) {
      diagnostic_df$factor_b <- as.character(result_df$data[[factor_b]])
    }
    if (is_threeway) {
      diagnostic_df$factor_c <- as.character(result_df$data[[factor_c]])
    }

    required_not_na <- c("facet", "factor_a", "response")
    if (!is_oneway) {
      required_not_na <- c(required_not_na, "factor_b")
    }
    if (is_threeway) {
      required_not_na <- c(required_not_na, "factor_c")
    }
    keep <- rep(TRUE, nrow(diagnostic_df))
    for (col in required_not_na) {
      keep <- keep & !is.na(diagnostic_df[[col]])
    }

    diagnostic_df <- diagnostic_df[keep, , drop = FALSE]

    if (nrow(diagnostic_df) == 0) {
      if (is_oneway) {
        return(data.frame(
          Facet = "No complete rows",
          Level_A = "",
          n = 0,
          Levels_A = 0,
          Status = "Insufficient levels",
          stringsAsFactors = FALSE
        ))
      }

      if (is_threeway) {
        return(data.frame(
          Facet = "No complete rows",
          Level_A = "",
          Level_B = "",
          Level_C = "",
          n = 0,
          Levels_A = 0,
          Levels_B = 0,
          Levels_C = 0,
          Status = "Insufficient levels",
          stringsAsFactors = FALSE
        ))
      }

      return(data.frame(
        Facet = "No complete rows",
        Level_A = "",
        Level_B = "",
        n = 0,
        Levels_A = 0,
        Levels_B = 0,
        Status = "Insufficient levels",
        stringsAsFactors = FALSE
      ))
    }

    if (is_oneway) {
      counts_df <- diagnostic_df %>%
        dplyr::count(facet, factor_a, name = "n") %>%
        tidyr::complete(facet, factor_a, fill = list(n = 0))

      levels_a_df <- counts_df %>%
        dplyr::group_by(facet, factor_a) %>%
        dplyr::summarise(total = sum(n), .groups = "drop") %>%
        dplyr::group_by(facet) %>%
        dplyr::summarise(Levels_A = sum(total > 0), .groups = "drop")

      summary_df <- counts_df %>%
        dplyr::left_join(levels_a_df, by = "facet") %>%
        dplyr::mutate(
          Status = ifelse(Levels_A >= 2, "OK", "Insufficient levels")
        ) %>%
        dplyr::arrange(facet, factor_a)

      names(summary_df) <- c("Facet", "Level_A", "n", "Levels_A", "Status")
      return(summary_df)
    }

    if (is_threeway) {
      counts_df <- diagnostic_df %>%
        dplyr::count(facet, factor_a, factor_b, factor_c, name = "n") %>%
        tidyr::complete(facet, factor_a, factor_b, factor_c, fill = list(n = 0))

      levels_a_df <- counts_df %>%
        dplyr::group_by(facet, factor_a) %>%
        dplyr::summarise(total = sum(n), .groups = "drop") %>%
        dplyr::group_by(facet) %>%
        dplyr::summarise(Levels_A = sum(total > 0), .groups = "drop")

      levels_b_df <- counts_df %>%
        dplyr::group_by(facet, factor_b) %>%
        dplyr::summarise(total = sum(n), .groups = "drop") %>%
        dplyr::group_by(facet) %>%
        dplyr::summarise(Levels_B = sum(total > 0), .groups = "drop")

      levels_c_df <- counts_df %>%
        dplyr::group_by(facet, factor_c) %>%
        dplyr::summarise(total = sum(n), .groups = "drop") %>%
        dplyr::group_by(facet) %>%
        dplyr::summarise(Levels_C = sum(total > 0), .groups = "drop")

      summary_df <- counts_df %>%
        dplyr::left_join(levels_a_df, by = "facet") %>%
        dplyr::left_join(levels_b_df, by = "facet") %>%
        dplyr::left_join(levels_c_df, by = "facet") %>%
        dplyr::mutate(
          Status = ifelse(Levels_A >= 2 & Levels_B >= 2 & Levels_C >= 2, "OK", "Insufficient levels")
        ) %>%
        dplyr::arrange(facet, factor_a, factor_b, factor_c)

      names(summary_df) <- c("Facet", "Level_A", "Level_B", "Level_C", "n", "Levels_A", "Levels_B", "Levels_C", "Status")
      return(summary_df)
    }

    counts_df <- diagnostic_df %>%
      dplyr::count(facet, factor_a, factor_b, name = "n") %>%
      tidyr::complete(facet, factor_a, factor_b, fill = list(n = 0))

    levels_a_df <- counts_df %>%
      dplyr::group_by(facet, factor_a) %>%
      dplyr::summarise(total = sum(n), .groups = "drop") %>%
      dplyr::group_by(facet) %>%
      dplyr::summarise(Levels_A = sum(total > 0), .groups = "drop")

    levels_b_df <- counts_df %>%
      dplyr::group_by(facet, factor_b) %>%
      dplyr::summarise(total = sum(n), .groups = "drop") %>%
      dplyr::group_by(facet) %>%
      dplyr::summarise(Levels_B = sum(total > 0), .groups = "drop")

    summary_df <- counts_df %>%
      dplyr::left_join(levels_a_df, by = "facet") %>%
      dplyr::left_join(levels_b_df, by = "facet") %>%
      dplyr::mutate(
        Status = ifelse(Levels_A >= 2 & Levels_B >= 2, "OK", "Insufficient levels")
      ) %>%
      dplyr::arrange(facet, factor_a, factor_b)

    names(summary_df) <- c("Facet", "Level_A", "Level_B", "n", "Levels_A", "Levels_B", "Status")
    summary_df
  })

  output$bar_design_diagnostic <- DT::renderDataTable({
    diagnostic_tbl <- bar_design_diagnostic_data()

    if ("Status" %in% names(diagnostic_tbl)) {
      diagnostic_tbl <- diagnostic_tbl[, c("Status", setdiff(names(diagnostic_tbl), "Status")), drop = FALSE]
    }

    status_col_idx <- which(names(diagnostic_tbl) == "Status")

    dt <- DT::datatable(
      diagnostic_tbl,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        dom = "t",
        paging = FALSE,
        lengthChange = FALSE,
        ordering = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = "260px",
        scrollCollapse = TRUE
      )
    )

    if (length(status_col_idx) == 1) {
      dt <- dt %>%
        DT::formatStyle(
          columns = status_col_idx,
          fontWeight = "600",
          color = DT::styleEqual(c("OK", "Insufficient levels"), c("#1e7e34", "#a94442")),
          backgroundColor = DT::styleEqual(c("OK", "Insufficient levels"), c("#e8f7ed", "#fdeceb")),
          borderRadius = "4px",
          padding = "2px 8px"
        )
    }

    dt
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
      # BUILD PATTERN: Support both standard and subsecond formats
      # STRATEGY: Extended regex to match all time point formats
      pattern <- paste0("^", input$root, "(_L[0-9]+|_D[0-9]+|_Lss[0-9]+|_Dss[0-9]+)$")
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
      # BAR PLOT: Single response selection for ANOVA workflows
      factor_cols <- analysis_var_choices()
      measure_choices <- setdiff(colnames(result_df$data), factor_cols)
      if (length(measure_choices) == 0) {
        measure_choices <- colnames(result_df$data)
      }

      selected <- if ("Fm" %in% measure_choices) "Fm" else measure_choices[1]
      selectInput(
        "column",
        "Select parameter to analyse",
        choices = measure_choices,
        selected = selected,
        multiple = FALSE
      )
    }
  })
  
  # SELECT ALL PARAMETERS BUTTON
  # STRATEGY: Convenience function for curve analysis
  # PURPOSE: Quick selection of all time points for a parameter
  observeEvent(input$select_all_params, {
    req(input$root)
    all_cols <- colnames(result_df$data)
    # UPDATED PATTERN: Support both standard and subsecond formats
    pattern <- paste0("^", input$root, "(_L[0-9]+|_D[0-9]+|_Lss[0-9]+|_Dss[0-9]+)$")
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

  # 6.5 DATA EXPORT UI COMPONENTS
  # STRATEGY: Dynamic UI for data export based on loaded data
  # PURPOSE: Allow users to select specific columns for export

  # DATA LOADED FLAG FOR UI
  # STRATEGY: Reactive flag to show/hide data export UI
  # PURPOSE: Only show export options when data is available
  output$data_loaded <- reactive({
    !is.null(result_df$data)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # COLUMN SELECTION UI FOR DATA EXPORT
  # STRATEGY: Multi-select input with all available columns
  # PURPOSE: Allow selective column export
  output$column_selection_ui <- renderUI({
    req(result_df$data)
    
    selectInput("export_columns", 
                label = NULL,
                choices = colnames(result_df$data),
                selected = colnames(result_df$data), # All columns selected by default
                multiple = TRUE,
                size = min(10, length(colnames(result_df$data))), # Limit height
                selectize = FALSE) # Use basic HTML select for better UX with many columns
  })

  # SELECT ALL COLUMNS BUTTON
  # STRATEGY: Convenience function to select all columns
  # PURPOSE: Quick selection for complete data export
  observeEvent(input$select_all_cols, {
    req(result_df$data)
    updateSelectInput(session, "export_columns",
                      selected = colnames(result_df$data))
  })

  # DESELECT ALL COLUMNS BUTTON
  # STRATEGY: Convenience function to clear selection
  # PURPOSE: Quick deselection for starting fresh
  observeEvent(input$deselect_all_cols, {
    updateSelectInput(session, "export_columns",
                      selected = character(0))
  })

  # ===========================================
  # SECTION 7: MODAL DIALOG FOR TIME PARAMETERS
  # ===========================================
  # STRATEGY: Modal dialog for complex parameter input
  # PURPOSE: Configure time values and units for curve analysis

  # EDIT PARAMETERS MODAL HANDLER
  # STRATEGY: Dynamic modal creation based on selected parameters
  # PURPOSE: Provide interface for time parameter configuration
  observeEvent(input$edit_params, {
    req(input$column)
    params <- input$column
    
    # DYNAMIC INPUT GENERATION
    # STRATEGY: Create input fields for each selected parameter
    # PURPOSE: Allow individual time value specification
    value.inputs <- lapply(params, function(param) {
      div(
        style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        numericInput(paste0("value_", param), 
                    label = div(style = "font-weight: 600; color: #495057;", param), 
                    value = NA)
      )
    })
    
    # MODAL DIALOG CONSTRUCTION
    # STRATEGY: Professional styling with clear sections
    # PURPOSE: Provide clean interface for parameter input
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
      # PURPOSE: Explain what users need to input
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
      # PURPOSE: Standardize time measurement across all parameters
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
      # PURPOSE: Handle variable number of time points efficiently
      div(
        div(
          icon("chart-line", style = "margin-right: 5px; color: #28a745;"), 
          h5("Parameter Values", style = "display: inline; margin: 0;")
        ),
        style = "margin-bottom: 15px;",
        
        div(
          style = "max-height: 300px; overflow-y: auto; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; background-color: white;",
          do.call(tagList, value.inputs)
        )
      ),
      
      # MODAL FOOTER WITH ACTION BUTTONS
      # STRATEGY: Clear cancel/confirm options
      # PURPOSE: Provide standard modal interaction patterns
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
  # PURPOSE: Close modal without saving changes
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
  # PURPOSE: Provide consistent interface for analysis functions
  VALUE <- reactive({
    req(input$column)
    input$column
  })

  # VARIABLE ASSIGNMENT FOR CURVE ANALYSIS
  # STRATEGY: Dynamic grouping/faceting from all available filename variables
  # PURPOSE: Support >2 variables in line chart workflow
  x_var <- reactive({
    var_choices <- analysis_var_choices()
    req(length(var_choices) >= 1)

    if (!is.null(input$curve_group_var) && input$curve_group_var %in% var_choices) {
      input$curve_group_var
    } else {
      var_choices[1]
    }
  })

  facet_var <- reactive({
    var_choices <- analysis_var_choices()
    req(length(var_choices) >= 1)

    current_group <- x_var()
    remaining <- setdiff(var_choices, current_group)

    if (!is.null(input$curve_facet_var) && input$curve_facet_var %in% remaining) {
      input$curve_facet_var
    } else if (length(remaining) > 0) {
      remaining[1]
    } else {
      current_group
    }
  })

  curve_split_var <- reactive({
    var_choices <- analysis_var_choices()
    if (is.null(input$curve_split_var) || identical(input$curve_split_var, "None")) {
      return("None")
    }

    allowed <- setdiff(var_choices, c(x_var(), facet_var()))
    if (input$curve_split_var %in% allowed) input$curve_split_var else "None"
  })

  curve_facet_levels <- reactive({
    req(result_df$data)
    req(facet_var() %in% names(result_df$data))

    base_vals <- as.character(result_df$data[[facet_var()]])
    split_name <- curve_split_var()
    if (!identical(split_name, "None") && split_name %in% names(result_df$data)) {
      split_vals <- as.character(result_df$data[[split_name]])
      return(unique(paste(base_vals, split_vals, sep = " | ")))
    }

    unique(base_vals)
  })
  
  # ===========================================
  # SECTION 9: GROUP ORDERING UI
  # ===========================================
  # STRATEGY: Drag-and-drop interface for group ordering
  # PURPOSE: User control over plot appearance and legend order

  # GROUPING VARIABLE ORDER UI
  # STRATEGY: Sortable list for x-axis variable ordering
  # PURPOSE: Allow users to control order of groups in plots
  output$var2_order_ui <- renderUI({
    req(result_df$data)
    rank_list(
      text = paste("Order of", x_var()),
      labels = unique(result_df$data[[x_var()]]),
      input_id = "var2_order"
    )
  })
  
  # FACETING VARIABLE ORDER UI
  # STRATEGY: Sortable list for faceting variable ordering
  # PURPOSE: Allow users to control order of facets in plots
  output$var1_order_ui <- renderUI({
    req(result_df$data)
    rank_list(
      text = if (identical(curve_split_var(), "None")) {
        paste("Order of", facet_var())
      } else {
        paste("Order of", facet_var(), "x", curve_split_var())
      },
      labels = curve_facet_levels(),
      input_id = "var1_order"
    )
  })

  output$bar_var2_order_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Bar plot")
    req(identical(input$stat_model, "oneway_anova"))
    req(input$bar_factor_a)
    req(input$bar_factor_a %in% names(result_df$data))

    labels <- unique(as.character(result_df$data[[input$bar_factor_a]]))
    labels <- labels[!is.na(labels)]
    if (length(labels) <= 1) {
      return(NULL)
    }

    rank_list(
      text = paste("Order of", input$bar_factor_a),
      labels = labels,
      input_id = "bar_var2_order"
    )
  })

  output$bar_var1_order_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Bar plot")
    req(identical(input$stat_model, "oneway_anova"))

    if (is.null(input$bar_facet_var) || identical(input$bar_facet_var, "None")) {
      return(NULL)
    }

    req(input$bar_facet_var %in% names(result_df$data))
    labels <- unique(as.character(result_df$data[[input$bar_facet_var]]))
    labels <- labels[!is.na(labels)]
    if (length(labels) <= 1) {
      return(NULL)
    }

    rank_list(
      text = paste("Order of", input$bar_facet_var),
      labels = labels,
      input_id = "bar_var1_order"
    )
  })

  output$bar_order_a_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Bar plot")
    req(identical(input$stat_model, "twoway_anova") || identical(input$stat_model, "threeway_anova"))
    req(input$bar_factor_a)
    req(input$bar_factor_a %in% names(result_df$data))

    labels <- unique(as.character(result_df$data[[input$bar_factor_a]]))
    labels <- labels[!is.na(labels)]
    if (length(labels) <= 1) return(NULL)

    rank_list(
      text = paste("Order of", input$bar_factor_a, "(Factor A)"),
      labels = labels,
      input_id = "bar_factor_a_order"
    )
  })

  output$bar_order_b_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Bar plot")
    req(identical(input$stat_model, "twoway_anova") || identical(input$stat_model, "threeway_anova"))
    req(input$bar_factor_b)
    req(input$bar_factor_b %in% names(result_df$data))

    labels <- unique(as.character(result_df$data[[input$bar_factor_b]]))
    labels <- labels[!is.na(labels)]
    if (length(labels) <= 1) return(NULL)

    rank_list(
      text = paste("Order of", input$bar_factor_b, "(Factor B / x-axis)"),
      labels = labels,
      input_id = "bar_factor_b_order"
    )
  })

  output$bar_order_c_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Bar plot")
    req(identical(input$stat_model, "threeway_anova"))
    req(input$bar_factor_c)
    req(input$bar_factor_c %in% names(result_df$data))

    labels <- unique(as.character(result_df$data[[input$bar_factor_c]]))
    labels <- labels[!is.na(labels)]
    if (length(labels) <= 1) return(NULL)

    rank_list(
      text = paste("Order of", input$bar_factor_c, "(Factor C)"),
      labels = labels,
      input_id = "bar_factor_c_order"
    )
  })

  output$bar_order_facet_ui <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Bar plot")
    req(identical(input$stat_model, "twoway_anova") || identical(input$stat_model, "threeway_anova"))

    if (is.null(input$bar_facet_var) || identical(input$bar_facet_var, "None")) {
      return(NULL)
    }

    req(input$bar_facet_var %in% names(result_df$data))
    labels <- unique(as.character(result_df$data[[input$bar_facet_var]]))
    labels <- labels[!is.na(labels)]
    if (length(labels) <= 1) return(NULL)

    rank_list(
      text = paste("Order of", input$bar_facet_var, "(Facet)"),
      labels = labels,
      input_id = "bar_facet_order"
    )
  })
  
  # ===========================================
  # SECTION 10: DYNAMIC COLOR INPUTS
  # ===========================================
  # STRATEGY: Adaptive color selection based on analysis type
  # PURPOSE: Appropriate color controls for different plot types

  # DYNAMIC COLOR INPUT GENERATOR
  # STRATEGY: Generate different color controls based on plot type
  # PURPOSE: Provide relevant color customization options
  output$dynamic_color_inputs <- renderUI({
    if (input$graph_type == "Bar plot") {
      current_model <- if (is.null(input$stat_model)) "oneway_anova" else input$stat_model

      # TWO/THREE-WAY BAR PLOT: One color per level of factor A
      if (identical(current_model, "twoway_anova") || identical(current_model, "threeway_anova")) {
        req(result_df$data)
        req(input$bar_factor_a)
        req(input$bar_factor_a %in% names(result_df$data))

        factor_levels <- unique(as.character(result_df$data[[input$bar_factor_a]]))
        factor_levels <- factor_levels[!is.na(factor_levels)]
        if (length(factor_levels) == 0) {
          factor_levels <- "Level1"
        }

        default_palette <- scales::hue_pal()(length(factor_levels))
        fill_inputs <- lapply(seq_along(factor_levels), function(i) {
          colourInput(
            inputId = paste0("bar_fill_color_", i),
            label = tags$span(paste0("Fill color - ", factor_levels[i]), style = "font-weight: normal;"),
            value = default_palette[i]
          )
        })

        point_inputs <- lapply(seq_along(factor_levels), function(i) {
          colourInput(
            inputId = paste0("bar_point_color_", i),
            label = tags$span(paste0("Point color - ", factor_levels[i]), style = "font-weight: normal;"),
            value = default_palette[i]
          )
        })

        tagList(
          colourInput("line_color",
                      label = tags$span("Bar border color", style = "font-weight: normal;"),
                      value = "darkgrey"),
          div(
            class = "palette-preset-row",
            selectInput(
              "bar_palette_preset",
              label = "Palette preset",
              choices = c(
                "Default hue" = "hue",
                "Colorblind-friendly" = "okabe_ito",
                "Pastel" = "pastel",
                "Vibrant" = "vibrant",
                "Dark" = "dark"
              ),
              selected = "hue"
            ),
            actionButton("apply_bar_palette", "Apply preset", class = "btn-info")
          ),
          tags$details(
            class = "color-group-toggle",
            tags$summary(paste0("Fill colors (", length(factor_levels), " levels)")),
            div(
              class = "color-group-body",
              fill_inputs
            )
          ),
          tags$details(
            class = "color-group-toggle",
            tags$summary(paste0("Point colors (", length(factor_levels), " levels)")),
            div(
              class = "color-group-body",
              point_inputs,
              colourInput("point_color",
                          label = tags$span("Point color (single-level fallback)", style = "font-weight: normal;"),
                          value = "darkgreen")
            )
          )
        )
      } else {
        # ONE-WAY BAR PLOT: Three global color components
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
      }
    } else if (input$graph_type == "Curve") {
      # CURVE ANALYSIS: One color per group
      # STRATEGY: Dynamic number of color inputs based on data
      req(result_df$data)
      req(x_var())
      req(x_var() %in% names(result_df$data))
      group_levels <- unique(as.character(result_df$data[[x_var()]]))
      group_levels <- group_levels[!is.na(group_levels)]
      n_lines <- length(group_levels)

      color.inputs <- lapply(seq_len(n_lines), function(i) {
        colourInput(
          inputId = paste0("curve_color_", i),
          label = tags$span(paste("Color for", group_levels[i]), style = "font-weight: normal;"),
          value = scales::hue_pal()(n_lines)[i]  # Default rainbow colors
        )
      })

      tagList(
        div(
          class = "palette-preset-row",
          selectInput(
            "curve_palette_preset",
            label = "Palette preset",
            choices = c(
              "Default hue" = "hue",
              "Colorblind-friendly" = "okabe_ito",
              "Pastel" = "pastel",
              "Vibrant" = "vibrant",
              "Dark" = "dark"
            ),
            selected = "hue"
          ),
          actionButton("apply_curve_palette", "Apply preset", class = "btn-info")
        ),
        tags$details(
          class = "color-group-toggle",
          tags$summary(paste0("Curve colors (", n_lines, " groups)")),
          div(
            class = "color-group-body",
            do.call(tagList, color.inputs)
          )
        )
      )
    }
  })

  build_bar_palette <- function(preset_name, n_levels) {
    if (is.null(n_levels) || n_levels <= 0) {
      return(character(0))
    }

    preset_key <- if (is.null(preset_name) || identical(preset_name, "")) "hue" else preset_name

    switch(
      preset_key,
      okabe_ito = grDevices::palette.colors(n_levels, palette = "Okabe-Ito"),
      pastel = grDevices::hcl.colors(n_levels, "Pastel 1"),
      vibrant = grDevices::hcl.colors(n_levels, "Dynamic"),
      dark = grDevices::hcl.colors(n_levels, "Dark 3"),
      scales::hue_pal()(n_levels)
    )
  }

  observeEvent(input$apply_bar_palette, {
    req(input$graph_type == "Bar plot")
    current_model <- if (is.null(input$stat_model)) "oneway_anova" else input$stat_model
    req(identical(current_model, "twoway_anova") || identical(current_model, "threeway_anova"))
    req(result_df$data)
    req(input$bar_factor_a)
    req(input$bar_factor_a %in% names(result_df$data))

    factor_levels <- unique(as.character(result_df$data[[input$bar_factor_a]]))
    factor_levels <- factor_levels[!is.na(factor_levels)]
    req(length(factor_levels) > 0)

    preset_colors <- build_bar_palette(input$bar_palette_preset, length(factor_levels))

    for (i in seq_along(factor_levels)) {
      updateColourInput(session, paste0("bar_fill_color_", i), value = preset_colors[i])
      updateColourInput(session, paste0("bar_point_color_", i), value = preset_colors[i])
    }
  })

  observeEvent(input$apply_curve_palette, {
    req(input$graph_type == "Curve")
    req(result_df$data)
    req(x_var())
    req(x_var() %in% names(result_df$data))

    group_levels <- unique(as.character(result_df$data[[x_var()]]))
    group_levels <- group_levels[!is.na(group_levels)]
    req(length(group_levels) > 0)

    preset_colors <- build_bar_palette(input$curve_palette_preset, length(group_levels))

    for (i in seq_along(group_levels)) {
      updateColourInput(session, paste0("curve_color_", i), value = preset_colors[i])
    }
  })
  
  # ===========================================
  # SECTION 11: ANALYSIS RESULT DISPLAYS
  # ===========================================
  # STRATEGY: Dynamic result display based on analysis type
  # PURPOSE: Show relevant statistical information to users

  # SELECTED VALUE DISPLAY
  # STRATEGY: Confirmation of user selection
  # PURPOSE: Provide feedback on what parameter is being analyzed
  output$selectedValue <- renderText({
    req(input$start_analysis > 0)  # Only after analysis initiated
    req(result_df$data)
    req(VALUE())
    paste("Selected value for analysis:", paste(VALUE(), collapse = ", "))
  })

  # AUTOMATIC TAB SWITCHING
  # STRATEGY: Guide user to results after analysis
  # PURPOSE: Immediate feedback and result visibility
  observeEvent(analysis_results(), {
    req(analysis_results())
    updateTabsetPanel(session, "main_tabs", selected = "Analysis Results")
  })
  # ===========================================
  # SECTION 11.1: CONVERT TO CURVE BUTTON
  # ===========================================
  # STRATEGY: Allow conversion of bar plot to line plot
  # PURPOSE: Visualize time series trends from bar plot data
  
  # SHOW CONVERT BUTTON ONLY FOR BAR PLOTS
  # STRATEGY: Conditional UI based on analysis type
  # PURPOSE: Button appears only when bar plot is generated
  build_convert_to_curve_ui <- function(show_original = FALSE) {
    button_label <- if (show_original) {
      HTML("<i class='fa fa-chart-bar'></i> Show Original Bar Plot")
    } else {
      HTML("<i class='fa fa-chart-line'></i> Convert to Curve")
    }

    button_class <- if (show_original) "btn-secondary btn-sm" else "btn-primary btn-sm"
    button_style <- if (show_original) {
      "font-weight: 600;
       border-radius: 20px;
       padding: 8px 20px;"
    } else {
      "background-color: #17a2b8;
       border-color: #17a2b8;
       font-weight: 600;
       border-radius: 20px;
       padding: 8px 20px;"
    }

    div(
      style = "text-align: center; margin: 20px 0;",
      div(
        class = "alert alert-info",
        style = "display: inline-block; text-align: left; margin-bottom: 12px; padding: 10px 14px; max-width: 720px;",
        tags$strong("Visual conversion only"),
        tags$br(),
        tags$span("Convert to Curve changes the current Bar Plot into a curve-style display."),
        tags$br(),
        tags$span("It does not run the dedicated Line Chart qGAM analysis or control-group statistical comparisons.")
      ),
      actionButton(
        "convert_to_curve",
        button_label,
        class = button_class,
        style = button_style
      )
    )
  }

  build_bar_filtering_summary <- function(data, measure_col, factor_cols) {
    factor_cols <- unique(factor_cols[!is.na(factor_cols) & nzchar(factor_cols)])
    total_rows <- nrow(data)
    missing_response <- is.na(data[[measure_col]])

    if (length(factor_cols) > 0) {
      factor_missing_matrix <- is.na(data[, factor_cols, drop = FALSE])
      missing_factor <- if (length(factor_cols) == 1) {
        as.logical(factor_missing_matrix)
      } else {
        apply(factor_missing_matrix, 1, any)
      }
    } else {
      missing_factor <- rep(FALSE, total_rows)
    }

    retained <- !(missing_response | missing_factor)

    list(
      total_rows = total_rows,
      retained_rows = sum(retained, na.rm = TRUE),
      removed_rows = sum(!retained, na.rm = TRUE),
      missing_response = sum(missing_response, na.rm = TRUE),
      missing_factors = sum(missing_factor, na.rm = TRUE),
      required_columns = c(factor_cols, measure_col)
    )
  }

  build_curve_filtering_summary <- function(data, parameter_col, time_col, grouping_col, facet_col) {
    total_rows <- nrow(data)
    missing_response <- is.na(data[[parameter_col]])
    missing_time <- is.na(data[[time_col]])
    missing_grouping <- is.na(data[[grouping_col]])
    missing_facet <- is.na(data[[facet_col]])
    retained <- !(missing_response | missing_time | missing_grouping | missing_facet)

    list(
      total_rows = total_rows,
      retained_rows = sum(retained, na.rm = TRUE),
      removed_rows = sum(!retained, na.rm = TRUE),
      missing_response = sum(missing_response, na.rm = TRUE),
      missing_time = sum(missing_time, na.rm = TRUE),
      missing_grouping_or_facet = sum(missing_grouping | missing_facet, na.rm = TRUE)
    )
  }

  output$convert_to_curve_button <- renderUI({
    req(current_plot())
    req(input$graph_type == "Bar plot")

    build_convert_to_curve_ui(show_original = FALSE)
  })
  # ===========================================
  # SECTION 11.2: BAR PLOT TO CURVE CONVERSION
  # ===========================================
  # STRATEGY: Convert bar plot faceted data to line plot with multiple curves
  # PURPOSE: Visualize trends across time/groups in a single plot
  
  # CONVERTED CURVE PLOT STORAGE
  # STRATEGY: Separate reactive storage for converted plot
  # PURPOSE: Allow switching between original and converted views
  converted_plot <- reactiveVal(NULL)
  show_converted <- reactiveVal(FALSE)
  
  # CONVERSION AND TOGGLE EVENT HANDLER (COMBINED)
  # STRATEGY: Single event handler for both conversion and toggling
  # PURPOSE: Avoid conflicts between multiple observers on same input
  observeEvent(input$convert_to_curve, {
    req(current_plot())
    req(result_df$data)
    req(input$graph_type == "Bar plot")
    
    # CHECK IF WE'RE TOGGLING BACK TO ORIGINAL
    if (!is.null(converted_plot()) && show_converted()) {
      # TOGGLE BACK TO BAR PLOT
      show_converted(FALSE)
      
      # RESTORE ORIGINAL BUTTON
      output$convert_to_curve_button <- renderUI({
        build_convert_to_curve_ui(show_original = FALSE)
      })
      return()  # Stop here, don't do conversion
    }
    
    # IF NOT TOGGLING BACK, DO THE CONVERSION
    tryCatch({
      # STORE COLUMN NAMES AS SIMPLE STRINGS
      value_col <- as.character(VALUE()[1])

      x_col <- if (!is.null(input$bar_factor_a) && input$bar_factor_a %in% names(result_df$data)) {
        as.character(input$bar_factor_a)
      } else {
        as.character(x_var())
      }

      facet_col <- if (!is.null(input$bar_facet_var) && input$bar_facet_var != "None" &&
                       input$bar_facet_var %in% names(result_df$data)) {
        as.character(input$bar_facet_var)
      } else if (!is.null(input$bar_factor_b) && input$bar_factor_b %in% names(result_df$data) &&
                 !identical(input$bar_factor_b, x_col)) {
        as.character(input$bar_factor_b)
      } else {
        as.character(facet_var())
      }

      converted_curve <- build_converted_curve_plot(
        data = result_df$data,
        x_col = x_col,
        facet_col = facet_col,
        value_col = value_col,
        x_order = NULL
      )
      
      # STORE CONVERTED PLOT
      converted_plot(converted_curve$plot)
      show_converted(TRUE)
      
      # UPDATE BUTTON TEXT
      output$convert_to_curve_button <- renderUI({
        build_convert_to_curve_ui(show_original = TRUE)
      })
      
      showNotification("Bar plot converted to curve successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error converting to curve:", e$message), type = "error")
      print(e)
      print(traceback())
    })
  })
  
  # NOTE: Legacy duplicate convert_to_curve observer was removed.
  # Keeping a single observer avoids conflicting toggle behaviors.
  
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

  # RESET CONVERTED PLOT ON NEW ANALYSIS
  # STRATEGY: Clear converted plot when new analysis is run
  # PURPOSE: Prevent showing old converted plot with new data
  observeEvent(input$start_analysis, {
    converted_plot(NULL)
    show_converted(FALSE)
  })

  curve_analysis_issue <- reactive({
    if (!identical(input$graph_type, "Curve")) {
      return(NULL)
    }

    selected_columns <- input$column
    if (is.null(selected_columns) || length(selected_columns) == 0) {
      return("Select at least one parameter to analyse before starting curve analysis.")
    }

    validated_columns <- user_params$selected_params
    validated_times <- user_params$times
    if (is.null(validated_columns) || length(validated_columns) == 0 ||
        !identical(as.character(validated_columns), as.character(selected_columns)) ||
        is.null(validated_times) || length(validated_times) != length(selected_columns) ||
        any(is.na(validated_times))) {
      return("Click 'Set Measurement Parameters', enter the time values, then click 'Validate' before starting curve analysis.")
    }

    if (is.null(input$control_group) || identical(input$control_group, "") || is.na(input$control_group)) {
      return("Select a control group before starting curve analysis.")
    }

    NULL
  })

  observeEvent(input$start_analysis, {
    issue <- isolate(curve_analysis_issue())
    if (!is.null(issue)) {
      showNotification(issue, type = "error", duration = 8)
    }
  })

  # MAIN ANALYSIS REACTIVE
  # STRATEGY: Event-driven analysis execution
  # PURPOSE: Run analysis only when user clicks "Start Analysis"
  analysis_results <- eventReactive(input$start_analysis, {
    # INPUT VALIDATION
    # STRATEGY: Comprehensive requirement checking
    # PURPOSE: Ensure all necessary inputs are available before analysis
    req(result_df$data)
    req(input$graph_type)

    if (identical(input$graph_type, "Curve")) {
      req(is.null(curve_analysis_issue()))
    }

    req(all(input$column %in% colnames(result_df$data)))
    

    if (input$graph_type == "Bar plot") {
      current_model <- if (is.null(input$stat_model)) "oneway_anova" else input$stat_model
      bar_facet_var <- if (!is.null(input$bar_facet_var) && input$bar_facet_var != "None") input$bar_facet_var else NULL
      fill_color_value <- if (!is.null(input$fill_color)) input$fill_color else "ivory1"
      line_color_value <- if (!is.null(input$line_color)) input$line_color else "darkgrey"
      point_color_value <- if (!is.null(input$point_color)) input$point_color else "darkgreen"

      custom_fill_palette <- NULL
      custom_point_palette <- NULL
      if ((identical(current_model, "twoway_anova") || identical(current_model, "threeway_anova")) &&
          !is.null(input$bar_factor_a) && input$bar_factor_a %in% names(result_df$data)) {
        factor_levels <- unique(as.character(result_df$data[[input$bar_factor_a]]))
        factor_levels <- factor_levels[!is.na(factor_levels)]

        if (length(factor_levels) > 0) {
          fill_values <- sapply(seq_along(factor_levels), function(i) input[[paste0("bar_fill_color_", i)]])
          if (length(fill_values) == length(factor_levels) && all(!is.na(fill_values)) && all(fill_values != "")) {
            custom_fill_palette <- setNames(as.character(fill_values), factor_levels)
          }

          point_values <- sapply(seq_along(factor_levels), function(i) input[[paste0("bar_point_color_", i)]])
          if (length(point_values) == length(factor_levels) && all(!is.na(point_values)) && all(point_values != "")) {
            custom_point_palette <- setNames(as.character(point_values), factor_levels)
          }
        }
      }

      apply_requested_order <- function(df, col_name, requested_order = NULL) {
        if (is.null(col_name) || !(col_name %in% names(df))) {
          return(df)
        }

        current_levels <- unique(as.character(df[[col_name]]))
        current_levels <- current_levels[!is.na(current_levels)]
        if (length(current_levels) == 0) {
          return(df)
        }

        if (is.null(requested_order) || length(requested_order) == 0) {
          requested_order <- current_levels
        }

        full_order <- c(requested_order, setdiff(current_levels, requested_order))
        df[[col_name]] <- factor(as.character(df[[col_name]]), levels = full_order)
        df
      }

      ordered_bar_data <- result_df$data
      if (identical(current_model, "twoway_anova") || identical(current_model, "threeway_anova")) {
        ordered_bar_data <- apply_requested_order(ordered_bar_data, input$bar_factor_a, input$bar_factor_a_order)
        ordered_bar_data <- apply_requested_order(ordered_bar_data, input$bar_factor_b, input$bar_factor_b_order)
        if (identical(current_model, "threeway_anova")) {
          ordered_bar_data <- apply_requested_order(ordered_bar_data, input$bar_factor_c, input$bar_factor_c_order)
        }
        if (!is.null(bar_facet_var)) {
          ordered_bar_data <- apply_requested_order(ordered_bar_data, bar_facet_var, input$bar_facet_order)
        }
      }

      if (identical(current_model, "twoway_anova")) {
        req(input$bar_factor_a)
        req(input$bar_factor_b)
        barplot_results <- analyse_barplot_twoway(
          data = ordered_bar_data,
          factor_a = input$bar_factor_a,
          factor_b = input$bar_factor_b,
          measure_col = VALUE()[1],
          facet_var = bar_facet_var,
          decision_mode = if (!is.null(input$parametric_strategy)) input$parametric_strategy else "conservative",
          fill_color = fill_color_value,
          line_color = line_color_value,
          point_color = point_color_value,
          fill_palette = custom_fill_palette,
          point_palette = custom_point_palette
        )
        barplot_results$filtering_summary <- build_bar_filtering_summary(
          data = result_df$data,
          measure_col = VALUE()[1],
          factor_cols = c(input$bar_factor_a, input$bar_factor_b, bar_facet_var)
        )
      } else if (identical(current_model, "threeway_anova")) {
        req(input$bar_factor_a)
        req(input$bar_factor_b)
        req(input$bar_factor_c)
        barplot_results <- analyse_barplot_threeway(
          data = ordered_bar_data,
          factor_a = input$bar_factor_a,
          factor_b = input$bar_factor_b,
          factor_c = input$bar_factor_c,
          measure_col = VALUE()[1],
          facet_var = bar_facet_var,
          decision_mode = if (!is.null(input$parametric_strategy)) input$parametric_strategy else "conservative",
          fill_color = fill_color_value,
          line_color = line_color_value,
          point_color = point_color_value,
          fill_palette = custom_fill_palette,
          point_palette = custom_point_palette
        )
        barplot_results$filtering_summary <- build_bar_filtering_summary(
          data = result_df$data,
          measure_col = VALUE()[1],
          factor_cols = c(input$bar_factor_a, input$bar_factor_b, input$bar_factor_c, bar_facet_var)
        )
      } else {
        req(input$bar_factor_a)
        bar_data <- result_df$data
        bar_var1 <- if (is.null(bar_facet_var)) ".AllData" else bar_facet_var
        if (identical(bar_var1, ".AllData")) {
          bar_data[[bar_var1]] <- "AllData"
        }

        bar_var2_levels <- unique(as.character(bar_data[[input$bar_factor_a]]))
        bar_var2_levels <- bar_var2_levels[!is.na(bar_var2_levels)]
        selected_var2_order <- input$bar_var2_order
        if (is.null(selected_var2_order) || length(selected_var2_order) == 0) {
          selected_var2_order <- bar_var2_levels
        }
        missing_var2 <- setdiff(bar_var2_levels, selected_var2_order)
        bar_var2_order <- c(selected_var2_order, missing_var2)

        bar_var1_order <- NULL
        if (!identical(bar_var1, ".AllData")) {
          bar_var1_levels <- unique(as.character(bar_data[[bar_var1]]))
          bar_var1_levels <- bar_var1_levels[!is.na(bar_var1_levels)]
          selected_var1_order <- input$bar_var1_order
          if (is.null(selected_var1_order) || length(selected_var1_order) == 0) {
            selected_var1_order <- bar_var1_levels
          }
          missing_var1 <- setdiff(bar_var1_levels, selected_var1_order)
          bar_var1_order <- c(selected_var1_order, missing_var1)
        }

        barplot_results <- analyse_barplot(
          data = bar_data,
          var1 = bar_var1,
          var2 = input$bar_factor_a,
          measure_col = VALUE()[1],
          var1_order = bar_var1_order,
          var2_order = bar_var2_order,
          fill_color = fill_color_value,
          line_color = line_color_value,
          point_color = point_color_value,
          parametric_strategy = if (is.null(input$oneway_parametric_strategy)) "classical" else input$oneway_parametric_strategy
        )
        barplot_results$filtering_summary <- build_bar_filtering_summary(
          data = bar_data,
          measure_col = VALUE()[1],
          factor_cols = c(input$bar_factor_a, bar_facet_var)
        )
      }

      # Extract the normality flag
      flag_normal <- barplot_results$normality

      # NORMALITY TEST RESULT DISPLAY
      # STRATEGY: Display normality test results to user
      # PURPOSE: Inform statistical approach taken in analysis
      output$normality_text <- renderText({
        if (!is.null(barplot_results$decision_reason)) {
          diag <- barplot_results$decision_diagnostics
          diag_lines <- character(0)
          if (!is.null(diag)) {
            if (!is.null(diag$levene_p) && !is.na(diag$levene_p)) {
              diag_lines <- c(diag_lines, paste0("Levene p-value: ", signif(diag$levene_p, 3)))
            }
            if (!is.null(diag$min_cell_n) && !is.na(diag$min_cell_n) && !is.null(diag$max_cell_n) && !is.na(diag$max_cell_n)) {
              diag_lines <- c(diag_lines, paste0("Cell counts (min to max): ", diag$min_cell_n, " to ", diag$max_cell_n))
            }
            if (!is.null(diag$imbalance_ratio) && is.finite(diag$imbalance_ratio)) {
              diag_lines <- c(diag_lines, paste0("Imbalance ratio (max/min): ", signif(diag$imbalance_ratio, 3)))
            }
          }

          method_line <- if (!is.null(barplot_results$method) && identical(barplot_results$method, "art")) {
            if (identical(current_model, "threeway_anova")) {
              "Method used: Three-way ART + emmeans (Holm)"
            } else {
              "Method used: Two-way ART + emmeans (Holm)"
            }
          } else if (!is.null(barplot_results$method) && identical(barplot_results$method, "welch")) {
            "Method used: One-way Welch ANOVA + Games-Howell"
          } else if (!is.null(barplot_results$method) && identical(barplot_results$method, "kruskal")) {
            if (!is.null(barplot_results$dunn)) {
              "Method used: Kruskal-Wallis + Dunn"
            } else {
              "Method used: Kruskal-Wallis"
            }
          } else if (identical(current_model, "threeway_anova")) {
            "Method used: Three-way ANOVA + emmeans (Tukey)"
          } else if (identical(current_model, "twoway_anova")) {
            "Method used: Two-way ANOVA + emmeans (Tukey)"
          } else {
            "Method used: One-way ANOVA + Tukey HSD"
          }

          return(paste(c(method_line, barplot_results$decision_reason, diag_lines), collapse = "\n"))
        }

        if (identical(current_model, "threeway_anova")) {
          if (!is.null(barplot_results$method) && identical(barplot_results$method, "art")) {
            paste(
              "Method used: Three-way ART + emmeans (Holm)",
              "Reason: at least one group failed Shapiro-Wilk or returned NA",
              "Rule: ANOVA is kept only when all tested groups pass normality",
              sep = "\n"
            )
          } else {
            paste(
              "Method used: Three-way ANOVA + emmeans (Tukey)",
              "Reason: all tested groups passed Shapiro-Wilk",
              "Rule: ART is used only when normality is not supported",
              sep = "\n"
            )
          }
        } else if (identical(current_model, "twoway_anova")) {
          if (!is.null(barplot_results$method) && identical(barplot_results$method, "art")) {
            paste(
              "Method used: Two-way ART + emmeans (Holm)",
              "Reason: at least one group failed Shapiro-Wilk or returned NA",
              "Rule: ANOVA is kept only when all tested groups pass normality",
              sep = "\n"
            )
          } else {
            paste(
              "Method used: Two-way ANOVA + emmeans (Tukey)",
              "Reason: all tested groups passed Shapiro-Wilk",
              "Rule: ART is used only when normality is not supported",
              sep = "\n"
            )
          }
        } else if (isTRUE(flag_normal)) {
          paste(
            "Method used: One-way ANOVA + Tukey HSD",
            "Reason: all tested groups passed Shapiro-Wilk",
            sep = "\n"
          )
        } else if (isFALSE(flag_normal) && !is.null(barplot_results$dunn)) {
          paste(
            "Method used: Kruskal-Wallis + Dunn",
            "Reason: at least one group failed Shapiro-Wilk or returned NA",
            sep = "\n"
          )
        } else if (isFALSE(flag_normal)) {
          paste(
            "Method used: Kruskal-Wallis",
            "Reason: at least one group failed Shapiro-Wilk or returned NA",
            sep = "\n"
          )
        } else {
          "Normality test result is unavailable"
        }
      })

      output$filtering_text <- renderText({
        summary_info <- barplot_results$filtering_summary
        if (is.null(summary_info)) {
          return("Filtering summary unavailable")
        }

        paste(
          paste0("Rows loaded: ", summary_info$total_rows),
          paste0("Rows retained for analysis: ", summary_info$retained_rows),
          paste0("Rows removed: ", summary_info$removed_rows),
          paste0("Rows with missing response: ", summary_info$missing_response),
          paste0("Rows with missing factor/facet values: ", summary_info$missing_factors),
          paste0("Required columns: ", paste(summary_info$required_columns, collapse = ", ")),
          sep = "\n"
        )
      })

      output$report_summary_text <- renderText({
        method_label <- if (!is.null(barplot_results$method) && identical(barplot_results$method, "art")) {
          if (identical(current_model, "threeway_anova")) {
            "Three-way ART + emmeans"
          } else {
            "Two-way ART + emmeans"
          }
        } else if (!is.null(barplot_results$method) && identical(barplot_results$method, "welch")) {
          "One-way Welch ANOVA + Games-Howell"
        } else if (!is.null(barplot_results$method) && identical(barplot_results$method, "kruskal")) {
          if (!is.null(barplot_results$dunn)) "Kruskal-Wallis + Dunn" else "Kruskal-Wallis"
        } else if (identical(current_model, "threeway_anova")) {
          "Three-way ANOVA + emmeans"
        } else if (identical(current_model, "twoway_anova")) {
          "Two-way ANOVA + emmeans"
        } else {
          "One-way ANOVA + Tukey HSD"
        }

        posthoc_used <- if (!is.null(barplot_results$posthoc) && nrow(barplot_results$posthoc) > 0) {
          "Yes (emmeans pairwise)"
        } else if (!is.null(barplot_results$tukey) && nrow(barplot_results$tukey) > 0) {
          "Yes (Tukey HSD)"
        } else if (!is.null(barplot_results$games_howell) && nrow(barplot_results$games_howell) > 0) {
          "Yes (Games-Howell)"
        } else if (!is.null(barplot_results$dunn) && nrow(barplot_results$dunn) > 0) {
          "Yes (Dunn)"
        } else {
          "No"
        }

        filtering <- barplot_results$filtering_summary
        retained_ratio <- if (!is.null(filtering) && !is.null(filtering$total_rows) && filtering$total_rows > 0) {
          paste0(round(100 * filtering$retained_rows / filtering$total_rows, 1), "%")
        } else {
          "Unavailable"
        }

        warning_text <- if (!is.null(barplot_results$message) && nzchar(barplot_results$message)) {
          barplot_results$message
        } else {
          "None"
        }

        paste(
          paste0("Analysis type: Bar Plot (", current_model, ")"),
          paste0("Response parameter: ", VALUE()[1]),
          paste0("Method used: ", method_label),
          paste0("Post-hoc performed: ", posthoc_used),
          paste0("Rows retained: ", if (!is.null(filtering)) filtering$retained_rows else "Unavailable", "/", if (!is.null(filtering)) filtering$total_rows else "Unavailable", " (", retained_ratio, ")"),
          paste0("Warnings: ", warning_text),
          sep = "\n"
        )
      })

      if (!is.null(barplot_results$message)) {
        showNotification(barplot_results$message, type = "message")
      }

      # STORE RESULTS FOR EXPORT
      # STRATEGY: Separate storage enables independent export functionality
      current_plot(barplot_results$plot)
      current_stats(barplot_results)

      return(barplot_results$plot)

    } else if (input$graph_type == "Curve") {
      # CURVE ANALYSIS
      # STRATEGY: Additional validation for curve-specific requirements
      # PURPOSE: Ensure curve analysis has all required parameters
      req(x_var())
      req(facet_var())
      req(user_params$selected_params)
      req(input$column)
      req(input$control_group)  # Control group required for statistical comparisons

      # COLOR VECTOR PREPARATION
      # STRATEGY: Extract user-selected colors for each group
      # PURPOSE: Apply custom colors to curve analysis
      n_lines <- length(unique(result_df$data[[x_var()]]))
      curve_colors <- sapply(seq_len(n_lines), function(i) {
        color_input <- input[[paste0("curve_color_", i)]]
        if(is.null(color_input)) "#000000" else color_input
      })

      # DATA TRANSFORMATION: WIDE TO LONG FORMAT
      # STRATEGY: Convert time points from columns to rows for analysis
      # PURPOSE: Analysis functions expect long format data
      group_col <- x_var()
      facet_base_col <- facet_var()
      split_col <- curve_split_var()
      long_required <- c(group_col, facet_base_col, input$column)
      if (!identical(split_col, "None")) {
        long_required <- c(long_required, split_col)
      }

      long_data <- result_df$data %>%
        select(all_of(unique(long_required))) %>%
        pivot_longer(
          cols = all_of(input$column),        # Time point columns to reshape
          names_to = "time_point",            # New column with time point names
          values_to = "parameter_value"       # New column with measurement values
        )

      if (!identical(split_col, "None") && split_col %in% names(long_data)) {
        long_data$.curve_facet <- paste(
          as.character(long_data[[facet_base_col]]),
          as.character(long_data[[split_col]]),
          sep = " | "
        )
      } else {
        long_data$.curve_facet <- as.character(long_data[[facet_base_col]])
      }

      # TIME LOOKUP PREPARATION
      # STRATEGY: Map selected parameter columns to validated numeric time values by exact name
      # PURPOSE: Support any column naming style (e.g., L1, T24h, Week2)
      time_lookup <- stats::setNames(
        as.numeric(user_params$times),
        as.character(user_params$selected_params)
      )

      long_data <- long_data %>%
        mutate(
          # TIME VALUE MAPPING
          # STRATEGY: First use exact validated mapping, then fallback to numeric extraction from label
          # PURPOSE: Robust conversion when time labels use custom text patterns
          time_numeric = dplyr::coalesce(
            as.numeric(unname(time_lookup[as.character(time_point)])),
            suppressWarnings(as.numeric(stringr::str_extract(as.character(time_point), "-?[0-9]+(?:\\.[0-9]+)?")))
          )
        )

      # CURVE ANALYSIS EXECUTION
      # STRATEGY: Delegate to specialized qGAM analysis function
      # PURPOSE: Perform time series analysis with statistical testing
      curve_results <- analyse_curve(
        df = long_data,
        col_vector = curve_colors,
        parameter_col = "parameter_value",     # Transformed parameter column
        time_col = "time_numeric",             # Transformed time column
        grouping_col = group_col,
        facet_col = ".curve_facet",
        control_group = input$control_group,
        k = input$k_param,  # Use user-selected k value
        user_params = reactiveValuesToList(user_params)
      )

      curve_results$filtering_summary <- build_curve_filtering_summary(
        data = long_data,
        parameter_col = "parameter_value",
        time_col = "time_numeric",
        grouping_col = group_col,
        facet_col = ".curve_facet"
      )

      output$k_effective_text <- renderText({
        k_info <- curve_results$k_summary
        if (is.null(k_info)) {
          return("k transparency info unavailable")
        }

        used_text <- if (!is.na(k_info$used_min) && !is.na(k_info$used_max)) {
          if (identical(k_info$used_min, k_info$used_max)) {
            as.character(k_info$used_min)
          } else {
            paste0(k_info$used_min, " to ", k_info$used_max)
          }
        } else {
          "none (insufficient time points)"
        }

        paste(
          paste0("Requested k: ", k_info$requested),
          paste0("Effective k used: ", used_text),
          paste0("Observed distinct time points per curve: ", k_info$n_time_min, " to ", k_info$n_time_max),
          paste0("Curves skipped (<3 distinct time points): ", k_info$skipped_curves),
          "Rule: k_effective = max(3, min(k_requested, n_time)), with n_time >= 3 required.",
          sep = "\n"
        )
      })

      output$filtering_text <- renderText({
        summary_info <- curve_results$filtering_summary
        if (is.null(summary_info)) {
          return("Filtering summary unavailable")
        }

        paste(
          paste0("Rows loaded after wide-to-long conversion: ", summary_info$total_rows),
          paste0("Rows retained for analysis: ", summary_info$retained_rows),
          paste0("Rows removed: ", summary_info$removed_rows),
          paste0("Rows with missing parameter value: ", summary_info$missing_response),
          paste0("Rows with missing mapped time value: ", summary_info$missing_time),
          paste0("Rows with missing grouping/facet values: ", summary_info$missing_grouping_or_facet),
          sep = "\n"
        )
      })

      output$report_summary_text <- renderText({
        stat_tbl <- curve_results$statistical_results
        n_tests <- if (!is.null(stat_tbl)) nrow(stat_tbl) else 0
        n_sig <- if (!is.null(stat_tbl) && "p.value" %in% names(stat_tbl)) {
          sum(!is.na(stat_tbl$p.value) & stat_tbl$p.value < 0.05)
        } else {
          0
        }

        filtering <- curve_results$filtering_summary
        retained_ratio <- if (!is.null(filtering) && !is.null(filtering$total_rows) && filtering$total_rows > 0) {
          paste0(round(100 * filtering$retained_rows / filtering$total_rows, 1), "%")
        } else {
          "Unavailable"
        }

        k_info <- curve_results$k_summary
        k_used <- if (!is.null(k_info) && !is.na(k_info$used_min) && !is.na(k_info$used_max)) {
          if (identical(k_info$used_min, k_info$used_max)) as.character(k_info$used_min) else paste0(k_info$used_min, " to ", k_info$used_max)
        } else {
          "none"
        }

        paste(
          "Analysis type: Line Chart (qGAM)",
          paste0("Control group: ", ifelse(is.null(input$control_group) || identical(input$control_group, ""), "Not set", input$control_group)),
          paste0("Selected columns: ", paste(input$column, collapse = ", ")),
          paste0("qGAM k requested/used: ", input$k_param, " / ", k_used),
          paste0("Control comparisons: ", n_sig, " significant out of ", n_tests, " tests"),
          paste0("Rows retained: ", if (!is.null(filtering)) filtering$retained_rows else "Unavailable", "/", if (!is.null(filtering)) filtering$total_rows else "Unavailable", " (", retained_ratio, ")"),
          sep = "\n"
        )
      })

      # STORE RESULTS FOR EXPORT
      current_plot(curve_results$plot)
      current_stats(curve_results)

      return(curve_results$plot)
    }
  })

  # PLOT RENDERING (MODIFIED)
  # STRATEGY: Display converted plot when available, otherwise show original
  # PURPOSE: Seamless switching between bar and line representations
  output$plot_result <- renderPlot({
    # PRIORITY: Show converted plot if available and flag is TRUE
    if (!is.null(converted_plot()) && show_converted()) {
      return(converted_plot())
    }
    
    # DEFAULT: Show original analysis result
    analysis_results()
  })

  # ===========================================
  # SECTION 13: EXPORT FUNCTIONALITY
  # ===========================================
  # STRATEGY: Professional export capabilities with downloadHandler
  # PURPOSE: Enable users to save and share analysis results locally

  # 13.1 STATISTICAL DATA EXPORT
  # STRATEGY: Multiple text files in zip archive using downloadHandler
  # PURPOSE: Organized, readable statistical output accessible on client side
  output$download_stats <- downloadHandler(
    filename = function() {
      paste0(input$stats_filename, ".zip")
    },
    content = function(file) {
      req(current_stats())  # Require completed analysis

      tryCatch({
        # CREATE TEMPORARY DIRECTORY FOR FILES
        # STRATEGY: Organize multiple files before zipping
        # PURPOSE: Clean export structure with multiple statistical outputs
        temp_dir <- tempdir()
        file_list <- c()
        stats_data <- current_stats()  # Get stored statistical results

        # Avoid partial matching on list elements (e.g. anova -> anova2)
        get_stat <- function(name) {
          stats_data[[name, exact = TRUE]]
        }

        if (input$graph_type == "Bar plot") {
          # BAR PLOT STATISTICAL EXPORTS
          # STRATEGY: Separate file for each type of statistical result
          # PURPOSE: Organized output that's easy to navigate

          if (!is.null(get_stat("summary"))) {
            summary_file <- file.path(temp_dir, "summary_statistics.txt")
            write.table(get_stat("summary"), file = summary_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, summary_file)
          }

          if (!is.null(get_stat("shapiro"))) {
            shapiro_file <- file.path(temp_dir, "shapiro_normality_test.txt")
            write.table(get_stat("shapiro"), file = shapiro_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, shapiro_file)
          }

          if (!is.null(get_stat("anova"))) {
            anova_file <- file.path(temp_dir, "anova_results.txt")
            write.table(get_stat("anova"), file = anova_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, anova_file)
          }

          if (!is.null(get_stat("welch"))) {
            welch_file <- file.path(temp_dir, "welch_anova_results.txt")
            write.table(get_stat("welch"), file = welch_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, welch_file)
          }

          if (!is.null(get_stat("tukey"))) {
            tukey_file <- file.path(temp_dir, "tukey_hsd_test.txt")
            write.table(get_stat("tukey"), file = tukey_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, tukey_file)
          }

          if (!is.null(get_stat("games_howell"))) {
            games_howell_file <- file.path(temp_dir, "games_howell_test.txt")
            write.table(get_stat("games_howell"), file = games_howell_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, games_howell_file)
          }

          if (!is.null(get_stat("kruskal"))) {
            kruskal_file <- file.path(temp_dir, "kruskal_wallis_test.txt")
            write.table(get_stat("kruskal"), file = kruskal_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, kruskal_file)
          }

          if (!is.null(get_stat("dunn"))) {
            dunn_file <- file.path(temp_dir, "dunn_post_hoc_test.txt")
            write.table(get_stat("dunn"), file = dunn_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, dunn_file)
          }

          if (!is.null(get_stat("cld"))) {
            cld_file <- file.path(temp_dir, "compact_letter_display.txt")
            write.table(get_stat("cld"), file = cld_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, cld_file)
          }

          if (!is.null(get_stat("analysis_data")) && nrow(get_stat("analysis_data")) > 0) {
            analysis_data_file <- file.path(temp_dir, "analysis_dataset_used.txt")
            write.table(get_stat("analysis_data"), file = analysis_data_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, analysis_data_file)
          }

          if (!is.null(get_stat("anova2"))) {
            anova2_file <- file.path(temp_dir, "anova_two_way_results.txt")
            write.table(get_stat("anova2"), file = anova2_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, anova2_file)
          }

          if (!is.null(get_stat("anova3"))) {
            anova3_file <- file.path(temp_dir, "anova_three_way_results.txt")
            write.table(get_stat("anova3"), file = anova3_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, anova3_file)
          }

          if (!is.null(get_stat("posthoc")) && nrow(get_stat("posthoc")) > 0) {
            posthoc_file <- file.path(temp_dir, "posthoc_emmeans_results.txt")
            write.table(get_stat("posthoc"), file = posthoc_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, posthoc_file)
          }

          decision_diag <- get_stat("decision_diagnostics")
          decision_metadata <- data.frame(
            Parameter = c(
              "Analysis Type",
              "Model",
              "Method Used",
              "Decision Strategy",
              "Decision Reason",
              "Normality Status",
              "Levene p-value",
              "Minimum Cell Size",
              "Maximum Cell Size",
              "Imbalance Ratio"
            ),
            Value = c(
              input$graph_type,
              ifelse(is.null(get_stat("model")), "Unknown", get_stat("model")),
              ifelse(is.null(get_stat("method")), "Unknown", get_stat("method")),
              ifelse(is.null(get_stat("decision_mode")) || is.na(get_stat("decision_mode")), "Not applicable", get_stat("decision_mode")),
              ifelse(is.null(get_stat("decision_reason")), "Not available", get_stat("decision_reason")),
              if (isTRUE(get_stat("normality"))) "Normal" else if (isFALSE(get_stat("normality"))) "Non-normal" else "Unknown",
              if (!is.null(decision_diag) && !is.null(decision_diag$levene_p) && !is.na(decision_diag$levene_p)) format(decision_diag$levene_p, digits = 4) else "Not applicable",
              if (!is.null(decision_diag) && !is.null(decision_diag$min_cell_n) && !is.na(decision_diag$min_cell_n)) as.character(decision_diag$min_cell_n) else "Not applicable",
              if (!is.null(decision_diag) && !is.null(decision_diag$max_cell_n) && !is.na(decision_diag$max_cell_n)) as.character(decision_diag$max_cell_n) else "Not applicable",
              if (!is.null(decision_diag) && !is.null(decision_diag$imbalance_ratio) && !is.na(decision_diag$imbalance_ratio)) format(decision_diag$imbalance_ratio, digits = 4) else "Not applicable"
            ),
            stringsAsFactors = FALSE
          )

          decision_file <- file.path(temp_dir, "statistical_decision_metadata.txt")
          write.table(decision_metadata, file = decision_file,
                     sep = "\t", row.names = FALSE, quote = FALSE)
          file_list <- c(file_list, decision_file)

        } else if (input$graph_type == "Curve") {
          # CURVE ANALYSIS STATISTICAL EXPORTS
          # STRATEGY: Export qGAM predictions and statistical test results
          # PURPOSE: Provide comprehensive curve analysis results

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

          if (!is.null(get_stat("analysis_data")) && nrow(get_stat("analysis_data")) > 0) {
            analysis_data_file <- file.path(temp_dir, "analysis_dataset_used.txt")
            write.table(get_stat("analysis_data"), file = analysis_data_file,
                       sep = "\t", row.names = FALSE, quote = FALSE)
            file_list <- c(file_list, analysis_data_file)
          }
        }

        # ANALYSIS PARAMETERS SUMMARY
        # STRATEGY: Include analysis settings for reproducibility
        # PURPOSE: Document how analysis was performed
        params_file <- file.path(temp_dir, "analysis_parameters.txt")
        
        # Create base parameters
        group_label <- if (input$graph_type == "Bar plot") input$bar_factor_a else x_var()
        facet_label <- if (input$graph_type == "Bar plot") input$bar_facet_var else {
          if (identical(curve_split_var(), "None")) {
            facet_var()
          } else {
            paste0(facet_var(), " x ", curve_split_var())
          }
        }
        model_label <- if (input$graph_type == "Bar plot") {
          if (is.null(input$stat_model) || input$stat_model == "") "oneway_anova" else input$stat_model
        } else {
          "curve_qgam"
        }

        base_params <- data.frame(
          Parameter = c("Analysis Type", "Statistical Model", "Parameter Column", "Grouping Variable", "Facet Variable"),
          Value = c(input$graph_type,
                   model_label,
                   paste(input$column, collapse = ", "),
                   group_label,
                   ifelse(is.null(facet_label) || facet_label == "None", "None", facet_label)),
          stringsAsFactors = FALSE
        )
        
        # Add analysis-specific parameters
        if (input$graph_type == "Bar plot") {
          # Get normality status from stored results
          normality_status <- if (isTRUE(get_stat("normality"))) {
            "Normal"
          } else if (isFALSE(get_stat("normality"))) {
            "Non-normal"
          } else {
            "Unknown"
          }
          
          # Determine statistical test used
          statistical_test <- if (!is.null(get_stat("model")) && get_stat("model") == "threeway_anova") {
            if (!is.null(get_stat("method")) && identical(get_stat("method"), "art")) {
              if (!is.null(get_stat("posthoc")) && nrow(get_stat("posthoc")) > 0) {
                "Three-way ART + emmeans post-hoc"
              } else {
                "Three-way ART"
              }
            } else if (!is.null(get_stat("posthoc")) && nrow(get_stat("posthoc")) > 0) {
              "Three-way ANOVA + emmeans post-hoc"
            } else {
              "Three-way ANOVA"
            }
          } else if (!is.null(get_stat("model")) && get_stat("model") == "twoway_anova") {
            if (!is.null(get_stat("method")) && identical(get_stat("method"), "art")) {
              if (!is.null(get_stat("posthoc")) && nrow(get_stat("posthoc")) > 0) {
                "Two-way ART + emmeans post-hoc"
              } else {
                "Two-way ART"
              }
            } else if (!is.null(get_stat("posthoc")) && nrow(get_stat("posthoc")) > 0) {
              "Two-way ANOVA + emmeans post-hoc"
            } else {
              "Two-way ANOVA"
            }
          } else if (!is.null(get_stat("normality"))) {
            if (!is.null(get_stat("method")) && identical(get_stat("method"), "welch")) {
              "Variance-robust (Welch ANOVA + Games-Howell)"
            } else if (isTRUE(get_stat("normality"))) {
              "Parametric (ANOVA + Tukey HSD)"
            } else if (!is.null(get_stat("dunn"))) {
              "Non-parametric (Kruskal-Wallis + Dunn)"
            } else {
              "Non-parametric (Kruskal-Wallis only)"
            }
          } else {
            "Unknown"
          }

          if (identical(model_label, "oneway_anova")) {
            additional_params <- rbind(
              additional_params,
              data.frame(
                Parameter = "One-way parametric strategy",
                Value = if (is.null(input$oneway_parametric_strategy)) "classical" else input$oneway_parametric_strategy,
                stringsAsFactors = FALSE
              )
            )
          }
          
          additional_params <- data.frame(
            Parameter = c("Data Normality", "Statistical Test Used"),
            Value = c(normality_status, statistical_test),
            stringsAsFactors = FALSE
          )
          
        } else if (input$graph_type == "Curve") {
          k_summary_text <- if (!is.null(stats_data$k_summary)) {
            ks <- stats_data$k_summary
            used <- if (!is.na(ks$used_min) && !is.na(ks$used_max)) {
              if (identical(ks$used_min, ks$used_max)) as.character(ks$used_min) else paste0(ks$used_min, " to ", ks$used_max)
            } else {
              "none"
            }
            paste0("requested=", ks$requested, "; used=", used, "; skipped_curves=", ks$skipped_curves)
          } else {
            "Unavailable"
          }

          additional_params <- data.frame(
            Parameter = c("Control Group", "Time Unit", "qGAM Smoothing Parameter (k)", "qGAM Effective k summary"),
            Value = c(input$control_group %||% "None",
                     user_params$unit %||% "Not specified",
                     as.character(input$k_param),
                     k_summary_text),
            stringsAsFactors = FALSE
          )
        }
        
        # Combine all parameters
        params_info <- rbind(base_params, additional_params)
        
        # Add plants per group information
        if (input$graph_type == "Bar plot") {
          if (!is.null(input$bar_factor_a) && input$bar_factor_a %in% names(result_df$data)) {
            if (!is.null(input$bar_facet_var) && input$bar_facet_var != "None" && input$bar_facet_var %in% names(result_df$data)) {
              plants_per_group <- aggregate(
                rep(1, nrow(result_df$data)),
                by = list(result_df$data[[input$bar_factor_a]], result_df$data[[input$bar_facet_var]]),
                FUN = length
              )
              plants_summary <- paste(
                paste(plants_per_group[[1]], plants_per_group[[2]], plants_per_group[[3]], sep = ": "),
                collapse = "; "
              )
            } else {
              plants_per_group <- aggregate(
                rep(1, nrow(result_df$data)),
                by = list(result_df$data[[input$bar_factor_a]]),
                FUN = length
              )
              plants_summary <- paste(
                paste(plants_per_group[[1]], plants_per_group[[2]], sep = ": "),
                collapse = "; "
              )
            }
          } else {
            plants_summary <- "Unavailable"
          }
        } else {
          if (identical(curve_split_var(), "None")) {
            plants_per_group <- aggregate(
              rep(1, nrow(result_df$data)),
              by = list(result_df$data[[x_var()]], result_df$data[[facet_var()]]),
              FUN = length
            )
            plants_summary <- paste(
              paste(plants_per_group[[1]], plants_per_group[[2]], plants_per_group[[3]], sep = ": "),
              collapse = "; "
            )
          } else {
            plants_per_group <- aggregate(
              rep(1, nrow(result_df$data)),
              by = list(result_df$data[[x_var()]], result_df$data[[facet_var()]], result_df$data[[curve_split_var()]]),
              FUN = length
            )
            plants_summary <- paste(
              paste(plants_per_group[[1]], plants_per_group[[2]], plants_per_group[[3]], plants_per_group[[4]], sep = ": "),
              collapse = "; "
            )
          }
        }
        
        plants_info <- data.frame(
          Parameter = "Number of plants per group",
          Value = plants_summary,
          stringsAsFactors = FALSE
        )
        
        params_info <- rbind(params_info, plants_info)
        
        write.table(params_info, file = params_file,
                   sep = "\t", row.names = FALSE, quote = FALSE)
        file_list <- c(file_list, params_file)

        # ZIP ARCHIVE CREATION
        # STRATEGY: Create zip file in temporary location then copy to download location
        # PURPOSE: Clean server-side approach that works in deployed environments
        if (length(file_list) > 0) {
          # Save current directory
          curr_dir <- getwd()
          # Change to temp directory for zipping
          setwd(temp_dir)
          # Create the zip file with relative paths
          zip_file <- file.path(temp_dir, "stats_export.zip")
          zip(zipfile = zip_file, files = basename(file_list))
          # Return to original directory
          setwd(curr_dir)
          # Copy the zip file to the download location
          file.copy(zip_file, file, overwrite = TRUE)
        }

      }, error = function(e) {
        # ERROR HANDLING FOR EXPORT
        # STRATEGY: Provide error file instead of failing silently
        # PURPOSE: User feedback when export fails
        writeLines(paste("Error exporting statistical data:", e$message), file)
        showNotification(paste("Error exporting statistical data:", e$message), type = "error")
      })
    },
    contentType = "application/zip"
  )

  # 13.2 PLOT EXPORT (MODIFIED)
  # STRATEGY: Export currently displayed plot (original or converted)
  # PURPOSE: Allow export of either representation
  output$download_plot <- downloadHandler(
    filename = function() {
      base_filename <- tools::file_path_sans_ext(input$plot_filename)
      # ADD SUFFIX IF CONVERTED PLOT
      if (!is.null(converted_plot()) && show_converted()) {
        base_filename <- paste0(base_filename, "_curve")
      }
      paste0(base_filename, ".", input$plot_format)
    },
    content = function(file) {
      # DETERMINE WHICH PLOT TO EXPORT
      plot_to_export <- if (!is.null(converted_plot()) && show_converted()) {
        converted_plot()
      } else {
        current_plot()
      }
      
      req(plot_to_export)

      tryCatch({
        export_filename <- paste0(tools::file_path_sans_ext(input$plot_filename), 
                                 if (!is.null(converted_plot()) && show_converted()) "_curve" else "",
                                 ".", input$plot_format)
        
        ggsave(
          filename = file,
          plot = plot_to_export,
          width = as.numeric(input$plot_width),
          height = as.numeric(input$plot_height),
          units = input$plot_units,
          dpi = 300
        )
        showNotification(paste("Plot exported successfully as", export_filename, "!"),
                        type = "message")
      }, error = function(e) {
        showNotification(paste("Error exporting plot:", e$message), type = "error")
      })
    })

  # 13.3 DATA TABLE EXPORT
  # STRATEGY: Flexible data export with column selection and multiple formats
  # PURPOSE: Enable users to download processed data in various formats
  output$download_data <- downloadHandler(
    filename = function() {
      # ENSURE PROPER FILE EXTENSION
      base_filename <- tools::file_path_sans_ext(input$data_filename)
      extension <- switch(input$data_format,
                       "csv" = ".csv",
                       "xlsx" = ".xlsx", 
                       "tsv" = ".tsv")
      paste0(base_filename, extension)
    },
    content = function(file) {
      req(result_df$data)
      req(input$export_columns)
      
      tryCatch({
        # PREPARE DATA FOR EXPORT
        # STRATEGY: Extract only selected columns
        # PURPOSE: Give users control over what data to export
        export_data <- result_df$data[, input$export_columns, drop = FALSE]
        
        # EXPORT BASED ON FORMAT
        # STRATEGY: Format-specific export functions
        # PURPOSE: Support multiple common data formats
        switch(input$data_format,
          "csv" = write.csv(export_data, file, row.names = FALSE),
          "xlsx" = {
            # EXCEL EXPORT
            # STRATEGY: Use openxlsx for Excel compatibility
            # PURPOSE: Professional Excel output with proper formatting
            if (!requireNamespace("openxlsx", quietly = TRUE)) {
              stop("openxlsx package is required for Excel export")
            }
            openxlsx::write.xlsx(export_data, file, rowNames = FALSE)
          },
          "tsv" = write.table(export_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
        )
        
        # SUCCESS NOTIFICATION
        # STRATEGY: User feedback on successful export
        # PURPOSE: Confirm export completion and details
        showNotification(
          paste("Data exported successfully!", 
                nrow(export_data), "rows,", 
                ncol(export_data), "columns"),
          type = "message"
        )
        
      }, error = function(e) {
        # ERROR HANDLING
        # STRATEGY: Graceful error handling with user feedback
        # PURPOSE: Inform user of export issues
        showNotification(paste("Error exporting data:", e$message), type = "error")
      })
    }
  )
  # ===========================================
  # END OF SERVER FUNCTION
  # ===========================================
}

