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
  observeEvent(input$load, {
    pattern <- input$pattern
    var1 <- input$var1
    var2 <- input$var2
    var3 <- input$var3
    dir_path <- dirpath()
    tryCatch({
      result_df$data <- process_data_files(
        pattern = pattern, 
        areas = "", 
        var1 = var1, 
        var2 = var2, 
        var3 = var3, 
        dirpath = dir_path
      )
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
      showNotification("Failed to load data files. Please check the inputs and try again.", type = "error")
      print(e)
    })
  })
  
  # Graph/facet/column UI logic
  # 1. Sélection de la racine commune (uniquement pour "Curve")
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
      choices <- all_cols[grepl(pattern, all_cols)]
      selectInput("column", "Select parameters to analyse", choices = choices, multiple = TRUE)
    } else {
      selected <- if ("Fm" %in% colnames(result_df$data)) "Fm" else colnames(result_df$data)[1]
      selectInput("column", "Select parameter to analyse", choices = colnames(result_df$data), selected = selected)
    }
  })
  
  # 3. Bouton pour ouvrir le modal (affiché seulement si Curve et colonnes sélectionnées)
  output$editParamsBtn <- renderUI({
    if (input$graph_type == "Curve" && !is.null(input$column) && length(input$column) > 0) {
      actionButton("edit_params", "Add L1-L9, D1, D2 values and unit")
    }
  })
  
  # 4. Modal pour saisir valeurs et unité (uniquement pour Curve)
  observeEvent(input$edit_params, {
    req(input$column)
    params <- input$column
    unit_input <- textInput("unit_common", "Unit", value = "")
    value_inputs <- lapply(params, function(param) {
      numericInput(paste0("value_", param), paste("Value", param), value = NA)
    })
    showModal(modalDialog(
      title = "Add L1-L9, D1, D2 values and unit",
      unit_input,
      do.call(tagList, value_inputs),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("validate_params", "Validate")
      )
    ))
  })
  
  # 5. Traitement des valeurs après validation
  # Initialisation
  user_params <- reactiveValues(times = NULL, unit = NULL)
  
  # À la validation du modal
  observeEvent(input$validate_params, {
    params <- input$column
    user_params$times <- sapply(params, function(param) input[[paste0("value_", param)]])
    user_params$unit  <- input$unit_common
    removeModal()
  })
  
  VALUE <- reactive({                            # Define the reactive value for the selected value
    req(input$column)
    input$column
  })
  # Display the selected value
  output$selectedValue <- eventReactive(input$start_analysis, {
    req(result_df$data)
    req(VALUE())
    paste("Selected value for analysis:", paste(VALUE(), collapse = ", "))
  })
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
  output$normality_result <- eventReactive(input$start_analysis, {
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
      n_lines <- length(unique(result_df$data[[input$var2]]))
      curve_colors <- sapply(seq_len(n_lines), function(i) input[[paste0("curve_color_", i)]])
      # f_analyseNPQ must take (df, col_vector) at least
      curve_plot <- analyse_curve(
        df = result_df$data,
        col_vector = curve_colors
      )
      return(curve_plot)
    } else {
      return(NULL)
    }
  })
  
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

