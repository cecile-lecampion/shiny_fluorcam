########################################################################################################################################
# Define the server
########################################################################################################################################
source("global.R",  local = TRUE) # Load the global variables and packages
source("helpers.R", local = TRUE) # Load the helper functions

server <- function(input, output, session) { 
  volumes <- c(Home = fs::path_home(), "R" = R.home(), getVolumes()())
  shinyDirChoose(input, "dir", roots = volumes, session = session)
  
  dirpath <- reactive({
    parseDirPath(volumes, input$dir)
  })
  
  output$dirpath <- renderText({ dirpath() })
  
  show_all_files <- reactiveVal(FALSE)
  observeEvent(input$show_all, { show_all_files(!show_all_files()) })
  
  output$selected_files <- renderTable({
    req(dirpath(), input$pattern)
    files <- list.files(path = dirpath(), pattern = input$pattern, full.names = TRUE)
    if (!show_all_files()) files <- head(files, 5)
    data.frame(Files = basename(files))
  })
  
  result_df <- reactiveValues(data = NULL)
  user_params <- reactiveValues(times = NULL, unit = NULL, selected_params = NULL)
  
  observeEvent(input$load, {
    req(dirpath(), input$pattern, input$var1, input$var2, input$var3)
    
    pattern <- input$pattern
    var1 <- input$var1
    var2 <- input$var2
    var3 <- input$var3
    dir_path <- dirpath()
    
    # Validate inputs
    if(is.null(dir_path) || dir_path == "") {
      showNotification("Please select a directory first.", type = "error")
      return()
    }
    
    tryCatch({
      processed_data <- process_data_files(
        pattern = pattern, 
        areas = "", 
        var1 = var1, 
        var2 = var2, 
        var3 = var3, 
        dirpath = dir_path
      )
      
      # Only assign if data is valid
      if(!is.null(processed_data) && nrow(processed_data) > 0) {
        result_df$data <- processed_data
        showNotification("Data loaded successfully!", type = "message")
      } else {
        showNotification("No valid data found in the files.", type = "warning")
        return()
      }
      
      output$toggle_button <- renderUI({
        req(result_df$data)
        actionButton("toggle_table", "Show/Hide Full Table")
      })
      
      show_full_table <- reactiveValues(full = FALSE)
      observeEvent(input$toggle_table, {
        show_full_table$full <- !show_full_table$full
      })
      
      output$processed_data <- renderTable({
        req(result_df$data)
        if (show_full_table$full) {
          result_df$data
        } else {
          head(result_df$data, 5)
        }
      })
    }, error = function(e) {
      showNotification(paste("Failed to load data files:", e$message), type = "error")
      print(e)
    })
  })
  
  # Graph/facet/column UI logic
  # 1. Sélection de la racine (uniquement pour "Curve")
  output$rootSelect <- renderUI({
    req(result_df$data)
    req(input$graph_type == "Curve")
    all_cols <- colnames(result_df$data)
    # Extraire les racines des colonnes de type "xxx_Ln" ou "xxx_Dn"
    roots <- unique(sub("(_L[0-9]+|_D[0-9]+)$", "", all_cols[grepl("(_L[0-9]+|_D[0-9]+)$", all_cols)]))
    selectInput("root", "Select the parameter root", choices = roots)
  })
  
  # 2. Sélection des colonnes selon la racine choisie
  output$columnSelect <- renderUI({
    req(result_df$data)
    if (input$graph_type == "Curve") {
      req(input$root)
      all_cols <- colnames(result_df$data)
      pattern <- paste0("^", input$root, "(_L[0-9]+|_D[0-9]+)$")
      available_choices <- all_cols[grepl(pattern, all_cols)]
      
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
      selected <- if ("Fm" %in% colnames(result_df$data)) "Fm" else colnames(result_df$data)[1]
      selectInput("column", "Select parameter to analyse", 
                  choices = colnames(result_df$data), 
                  selected = selected)
    }
  })
  
  # Observer for "Select All" button
  observeEvent(input$select_all_params, {
    req(input$root)
    all_cols <- colnames(result_df$data)
    pattern <- paste0("^", input$root, "(_L[0-9]+|_D[0-9]+)$")
    available_choices <- all_cols[grepl(pattern, all_cols)]
    
    updateSelectInput(session, "column", 
                      selected = available_choices)
  })
  
  # 3. Bouton pour ouvrir le modal (affiché seulement si Curve et colonnes sélectionnées)
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
  
  # 4. Modal pour saisir valeurs et unité (uniquement pour Curve) - Fixed modalButton
  observeEvent(input$edit_params, {
    req(input$column)
    params <- input$column
    
    # Create parameter inputs properly
    value_inputs <- lapply(params, function(param) {
      div(
        style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        numericInput(paste0("value_", param), 
                    label = div(style = "font-weight: 600; color: #495057;", param), 
                    value = NA)
      )
    })
    
    showModal(modalDialog(
      title = div(
        style = "text-align: center; background: #00b8e9; color: white; padding: 15px; margin: -15px -15px 20px -15px; border-radius: 8px 8px 0 0;",
        icon("flask"),
        " Configure Measurement Parameters"
      ),
      size = "m",
      easyClose = FALSE,
      
      # Instructions
      div(
        class = "alert alert-info",
        style = "background-color: #e8f4f8; border: 1px solid #bee5eb; border-radius: 5px; margin-bottom: 20px;",
        div(
          icon("info-circle", style = "margin-right: 8px;"), 
          strong("Enter the unit and values for your selected parameters")
        )
      ),
      
      # Unit input
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
      
      # Parameter values
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
      
      # FIXED: Remove class argument from modalButton and use actionButton instead
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
  
  # Add observer to handle cancel button
  observeEvent(input$modal_dismiss, {
    removeModal()
  })
  
  # 5. Traitement des valeurs après validation
  # Initialisation - ADD selected_params here
  user_params <- reactiveValues(times = NULL, unit = NULL, selected_params = NULL)

  # À la validation du modal - STORE the selected parameters
  observeEvent(input$validate_params, {
    params <- input$column
    user_params$selected_params <- params  # ADD this line
    user_params$times <- sapply(params, function(param) input[[paste0("value_", param)]])
    user_params$unit  <- input$unit_common
    removeModal()
    showNotification("Parameters validated successfully!", type = "message")  # Optional feedback
  })
  
  VALUE <- reactive({                            # Define the reactive value for the selected value
    req(input$column)
    input$column
  })
  # Display the selected value
  #output$selectedValue <- eventReactive(input$start_analysis, {
  #  req(result_df$data)
  #  req(VALUE())
  #  paste("Selected value for analysis:", paste(VALUE(), collapse = ", "))
  #})
  # Reactive expressions for faceting and x-axis variables
  facet_var <- reactive({
    if (input$facet_var == "var1") input$var1 else input$var2
  })
  
  x_var <- reactive({
    if (input$facet_var == "var1") input$var2 else input$var1
  })
  
  # Update the order of lines and facets based on selected faceting variable
  output$var2_order_ui <- renderUI({
    req(result_df$data)
    rank_list(
      text = paste("Order of", x_var()),
      labels = unique(result_df$data[[x_var()]]),
      input_id = "var2_order"
    )
  })
  
  output$var1_order_ui <- renderUI({
    req(result_df$data)
    rank_list(
      text = paste("Order of", facet_var()),
      labels = unique(result_df$data[[facet_var()]]),
      input_id = "var1_order"
    )
  })
  
  # Dynamic color UI
  output$dynamic_color_inputs <- renderUI({
    if (input$graph_type == "Bar plot") {
      tagList(
        colourInput("line_color", "Line color", value = "darkgrey"),
        colourInput("fill_color", "Fill color", value = "ivory1"),
        colourInput("point_color", "Point color", value = "darkgreen")
      )
    } else if (input$graph_type == "Curve") {
      req(input$var2)
      n_lines <- length(unique(result_df$data[[input$var2]]))
      color_inputs <- lapply(seq_len(n_lines), function(i) {
        colourInput(
          inputId = paste0("curve_color_", i),
          label = paste("Color for Line", i),
          value = scales::hue_pal()(n_lines)[i]
        )
      })
      do.call(tagList, color_inputs)
    }
  })
  
  # Show normality result for barplot
  output$normality_result <- renderText({
    req(input$start_analysis > 0)  # Only run after start_analysis is clicked
    req(result_df$data, input$graph_type == "Bar plot", input$column)
    req(input$column %in% colnames(result_df$data))
    var1 <- facet_var()
    var2 <- x_var()
    MEASURE_COL <- VALUE()
    shapiro_df <- result_df$data %>%
      dplyr::group_by(!!sym(var2), !!sym(var1)) %>%
      rstatix::shapiro_test(!!sym(MEASURE_COL))
    flag_normal <- check_normality(shapiro_df)
    if (flag_normal) {
      "Datas follow a normal law."
    } else {
      "Datas don't follow a normal law."
    }
  })

  # Fix selectedValue - change from eventReactive to renderText  
  output$selectedValue <- renderText({
    req(input$start_analysis > 0)  # Only run after start_analysis is clicked
    req(result_df$data)
    req(VALUE())
    paste("Selected value for analysis:", paste(VALUE(), collapse = ", "))
  })

  # Add this as a separate observer (not inside analysis_results)
  observeEvent(input$start_analysis, {
    print("Switching to Analysis Results tab")
    updateTabsetPanel(session, "main_tabs", selected = "Analysis Results")
  })
  
  # Main plot: conditional on graph type
  analysis_results <- eventReactive(input$start_analysis, {
    req(result_df$data)
    req(input$graph_type)
    req(input$facet_var)
    req(input$column %in% colnames(result_df$data))
    req(input$var1_order)
    req(input$var2_order)
    
    
    if (input$graph_type == "Bar plot") {
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
      return(barplot_results$plot)  # <-- RETURN the plot object
    } else if (input$graph_type == "Curve") {
      req(input$var2)
      req(user_params$selected_params)  # This should now work
      req(input$column)  # Also require the column selection
  
      n_lines <- length(unique(result_df$data[[input$var2]]))
      curve_colors <- sapply(seq_len(n_lines), function(i) {
        color_input <- input[[paste0("curve_color_", i)]]
        if(is.null(color_input)) "#000000" else color_input
    })

  # Use the validated user parameters
  curve_plot <- analyse_curve(
    df = result_df$data,
    col_vector = curve_colors,
    user_params = reactiveValuesToList(user_params),  # Pass all user parameters
    grouping_col = x_var(),    # Use x_var() instead of input$var2
    facet_col = facet_var()    # Use facet_var() instead of input$var1
  )
  return(curve_plot)
    } else {
      return(NULL)
    }
  })
  
  # Download handlers for statistical results
  output$download_parametric <- downloadHandler(
    filename = function() {
      paste("parametric_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # This would need to be implemented based on your specific analysis results
      write.csv(data.frame(message = "Parametric results not yet implemented"), file, row.names = FALSE)
    }
  )
  
  output$download_non_parametric <- downloadHandler(
    filename = function() {
      paste("non_parametric_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # This would need to be implemented based on your specific analysis results
      write.csv(data.frame(message = "Non-parametric results not yet implemented"), file, row.names = FALSE)
    }
  )

  # Download handler for saving the plot
   output$save_plot <- downloadHandler(
     filename = function() {
       paste("plot_", Sys.Date(), ".", input$file_format, sep = "")
     },
     content = function(file) {
       ggsave(file, plot = last_plot(), device = input$file_format, width = 8, height = 6)
     }
   )
  
   # start analysis
  output$plot_result <- renderPlot({
    req(analysis_results())
    print(analysis_results())
  })
}

