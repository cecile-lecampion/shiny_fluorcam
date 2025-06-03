########################################################################################################################################
# Define the server
########################################################################################################################################
# server.R
source("global.R",  local = TRUE) # Load the global variables and packages
source("helpers.R", local = TRUE) # Load the helper functions


server <- function(input, output, session) { 
  volumes <- c(Home = fs::path_home(), "R" = R.home(), getVolumes()()) # Define the volumes to search for the files
  
  shinyDirChoose(input, "dir", roots = volumes, session = session)     # Choose the directory where the files are
  
  dirpath <- reactive({                                                # Define the reactive value for the directory path
    parseDirPath(volumes, input$dir)
  })
  
  output$dirpath <- renderText({                                       # Display the directory path
    dirpath()
  })
  
  show_all_files <- reactiveVal(FALSE)                                 # Define the reactive value to show all files or only the first 5
  
  observeEvent(input$show_all, {                                       # Observe the event to show all files or only the first 5
    show_all_files(!show_all_files())
  })
  
  output$selected_files <- renderTable({                               # Display the selected files
    req(dirpath(), input$pattern)
    files <- list.files(path = dirpath(), pattern = input$pattern, full.names = TRUE)
    
    if (!show_all_files()) {                                          # Show all files or only the first 5
      files <- head(files, 5)
    }
    
    data.frame(Files = basename(files))
  })
  
  result_df <- reactiveValues(data = NULL)                             # Define the reactive values for the processed data to make them available for later use
  
  observeEvent(input$load, {                                           # Observe the event to load the data
    pattern <- input$pattern                                           # Define the pattern
    areas <- ""                                              # Define the areas
    var1 <- input$var1                                                 # Define the name of VAR1
    var2 <- input$var2                                                 # Define the name of VAR2
    var3 <- input$var3                                                 # Define the name of VAR3
    dir_path <- dirpath()                                              # Define the directory path
    
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
      selectInput("column", "Select parameter to analyse", choices = colnames(result_df$data))
    }
  })
  
  # 3. Bouton pour ouvrir le modal (affiché seulement si Curve et colonnes sélectionnées)
  output$editParamsBtn <- renderUI({
    if (input$graph_type == "Curve" && !is.null(input$column) && length(input$column) > 0) {
      actionButton("edit_params", "Add L1-L9, D1, D2 vlues and unit")
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
      title = "Add L1-L9, D1, D2 vlues and unit",
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

  
  VALUE <- reactive({                                                  # Define the reactive value for the selected value
    req(input$column)
    input$column
  })
  
  # Reactive expressions for faceting and x-axis variables
  facet_var <- reactive({
    if (input$facet_var == "var1") {
      return(input$var1)
    } else {
      return(input$var2)
    }
  })
  
  x_var <- reactive({
    if (input$facet_var == "var1") {
      return(input$var2)
    } else {
      return(input$var1)
    }
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
  
  # color selection
  line_color <- reactive({ 
    req(input$line_color)
    input$line_color
  })
  
  fill_color <- reactive({
    req(input$fill_color)
    input$fill_color
  })
  
  point_color <- reactive({
    req(input$point_color)
    input$point_color
  })
  
  # Data processing and statistical analysis
  analysis_result <- eventReactive(input$start_analysis, { 
    req(result_df$data, VALUE())
    
    var1 <- facet_var()
    var2 <- x_var()
    MEASURE_COL <- VALUE() 
    
    # Convert variables to factors
    result_df$data[[var1]] <- as.factor(result_df$data[[var1]])
    result_df$data[[var2]] <- as.factor(result_df$data[[var2]])
    
    # Compute Shapiro Test
    shapiro_result <- reactive({ 
      req(var1, var2, MEASURE_COL)
      
      shapiro_df <- result_df$data %>%
        group_by(!!sym(var2), !!sym(var1)) %>%
        shapiro_test(!!sym(MEASURE_COL))
      
      return(shapiro_df)
    })
    
    # Normality check
    normality_check <- reactive({ 
      shapiro_df <- shapiro_result()
      flag_normal <- check_normality(shapiro_df)
      return(flag_normal)
    })
    
    # Displaying the result of the normality test
    output$normality_result <- renderText({
      if (normality_check()) {
        "Datas follow a normal law."
      } else {
        "Datas don't follow a normal law."
      }
    })
    
    # Data analysis according to normality status
    if (normality_check()) {                                  # If the data follow a normal law
      # Summary
      library(plyr)
      library(Rmisc)
      my_summary <- summarySE(result_df$data, measurevar = MEASURE_COL, groupvars = c(var2, var1)) 
      
      # Statistical tests
      # ANOVA Test
      anova_result <- result_df$data %>%
        group_by(!!sym(var1)) %>%
        rstatix::anova_test(formula = reformulate(var2, MEASURE_COL))
      
      # Tukey HSD Test
      tukey_results <- result_df$data %>%
        group_by(!!sym(var1)) %>% 
        rstatix::tukey_hsd(as.formula(paste(MEASURE_COL, "~", var2)))
      
      # CLD letters for parametric test
      cld_table_parametric <- generate_cld_parametric(tukey_results, var1, var2)
      
      # Plotting
      df2 <- merge(my_summary, cld_table_parametric, by.x = c(var2, var1), by.y = c(var2, var1))
      # Variable as factor
      df2[[var1]] <- factor(df2[[var1]], levels = input$var1_order) 
      df2[[var2]] <- factor(df2[[var2]], levels = input$var2_order)
      result_df$data[[var1]] <- factor(result_df$data[[var1]], levels = input$var1_order)
      result_df$data[[var2]] <- factor(result_df$data[[var2]], levels = input$var2_order)
      
      p <- df2 %>%
        mutate(!!sym(var1) := as.factor(!!sym(var1)),
               !!sym(var2) := as.factor(!!sym(var2))) %>%
        ggplot(aes(x = !!sym(var2), y = !!sym(MEASURE_COL), fill = !!sym(var2))) +
        geom_col(color = line_color(), width = 0.6, position = position_dodge2(padding = 0.05)) +
        scale_fill_manual(values = rep(fill_color(), length(unique(df2[[var2]])))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        geom_quasirandom(data = result_df$data, 
                         aes(x = !!sym(var2), y = !!sym(MEASURE_COL)), 
                         color = point_color(), 
                         width = 0.3, alpha = 0.6) +
        geom_segment(aes(x = !!sym(var2), xend = !!sym(var2), y = pmax(0, !!sym(MEASURE_COL) - ci), yend = !!sym(MEASURE_COL) + ci), 
                     color = "black") +
        geom_text(aes(x = !!sym(var2), y = !!sym(MEASURE_COL) + (0.15 * max(df2[[MEASURE_COL]], na.rm = TRUE)), label = cld), 
                  size = 3, inherit.aes = TRUE) +
        theme_classic() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.line.x = element_line(linewidth = 0.5),
          axis.line.y = element_line(linewidth = 0.5),
          panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill = 'transparent', color = NA),
          axis.text.y = element_text(vjust = 1),
          legend.position = "none",
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(face = "plain", size = 10, color = "black", hjust = 0.5)) +
        facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
        labs(x = var1, y = MEASURE_COL)
      
      output$plot_result <- renderPlot({
        print(p)
      })
      
      #ggsave("plot_para.svg", width = 8, height = 6)
      
      # Download all the tables as a ZIP file for parametric data
      output$download_parametric <- downloadHandler(
        filename = function() {
          paste("parametric_analysis_results_", Sys.Date(), ".zip", sep = "")
        },
        content = function(file) {
          temp_dir <- tempdir()
          shapiro_file <- file.path(temp_dir, "shapiro_test_results.csv")
          write.csv(shapiro_result(), shapiro_file, row.names = FALSE)
          my_summary_file <- file.path(temp_dir, "summary_results.csv")
          write.csv(my_summary, my_summary_file, row.names = FALSE)
          anova_file <- file.path(temp_dir, "anova_results.csv")
          write.csv(anova_result, anova_file, row.names = FALSE)
          tukey_file <- file.path(temp_dir, "tukey_results.csv")
          write.csv(tukey_results, tukey_file, row.names = FALSE)
          cld_file <- file.path(temp_dir, "cld_table_parametric.csv")
          write.csv(cld_table_parametric, cld_file, row.names = FALSE)
          
          zip::zipr(file, 
                    files = c(shapiro_file, my_summary_file, anova_file, tukey_file, cld_file), 
                    compression_level = 9)
        }
      )
      
    } else {                                                 # If the data don't follow a normal law
      # Summary
      conf_int <- groupwiseMedian(
        data = result_df$data,
        var = MEASURE_COL,
        group = c(var2, var1),
        conf = 0.95,
        R = 5000,
        percentile = TRUE,
        bca = FALSE,
        digits = 3)
      
      # Statistical tests
      # Kruskal-Wallis Test
      kruskal_pval <- result_df$data %>%
        group_by(!!sym(var1)) %>%
        kruskal_test(as.formula(paste(MEASURE_COL, "~", var2))) %>%
        dplyr::select(all_of(var1), p)
      
      significant <- any(kruskal_pval$p < 0.05)
      
      # Dunn Test and CLD letters
      if (significant) {
        pval_dunn <- test_dunn(result_df$data, var1, var2, MEASURE_COL)
        
        # CLD letters for non-parametric test
        
        cld_table_nonparametric <- generate_cld_nonparametric(pval_dunn, var1, var2) 
        
        # Plotting
        df2 <- merge(conf_int, cld_table_nonparametric, by.x = c(var2, var1), by.y = c(var2, var1))
        # Variable as factor
        df2[[var1]] <- factor(df2[[var1]], levels = input$var1_order)
        df2[[var2]] <- factor(df2[[var2]], levels = input$var2_order)
        result_df$data[[var1]] <- factor(result_df$data[[var1]], levels = input$var1_order)
        result_df$data[[var2]] <- factor(result_df$data[[var2]], levels = input$var2_order)
        
        p <- df2 %>%
          ggplot(aes(x = !!sym(var2), y = Median, fill = !!sym(var2))) +
          geom_col(color = line_color(), width = 0.6, position = position_dodge2(padding = 0.05)) +
          scale_fill_manual(values = rep(fill_color(), length(unique(df2[[var2]])))) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
          geom_quasirandom(data = result_df$data, 
                           aes(x = !!sym(var2), y = !!sym(MEASURE_COL)), 
                           color = point_color(), width = 0.3, alpha = 0.6) +
          geom_segment(aes(x = !!sym(var2), xend = !!sym(var2), y = pmax(0, Percentile.lower), yend = Percentile.upper), 
                       color = "black") +
          geom_text(aes(x = !!sym(var2), y = Percentile.upper + (0.15 * Median), label = cld), 
                    size = 3, inherit.aes = TRUE) +
          theme_classic() +
          theme(
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.line.x = element_line(linewidth = 0.5),
            axis.line.y = element_line(linewidth = 0.5),
            panel.background = element_rect(fill = 'transparent', color = NA),
            plot.background = element_rect(fill = 'transparent', color = NA),
            axis.text.y = element_text(vjust = 1),
            legend.position = "none",
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(face = "plain", size = 10, color = "black", hjust = 0.5)) +
          facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
          labs(x = var1, y = MEASURE_COL)
        
        output$plot_result <- renderPlot({
          print(p)
        })
        
        #ggsave("plot_nonpara.svg", width = 8, height = 6)
        
      } else {                                                             # If the data are not significantly different
        print("Data are not significantly different, the Dunn test was not performed.")
        
      }
    }
    
    # Download all the tables as a ZIP file for non-parametric data
    output$download_non_parametric <- downloadHandler(
      filename = function() {
        paste("non_parametric_analysis_results_", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        temp_dir <- tempdir()
        shapiro_file <- file.path(temp_dir, "shapiro_test_results.csv")
        write.csv(shapiro_result(), shapiro_file, row.names = FALSE)
        conf_int_file <- file.path(temp_dir, "confidence_interval_results.csv")
        write.csv(conf_int, conf_int_file, row.names = FALSE)
        kruskal_file <- file.path(temp_dir, "kruskal_results.csv")
        write.csv(kruskal_pval, kruskal_file, row.names = FALSE)
        dunn_file <- file.path(temp_dir, "dunn_results.csv")
        write.csv(pval_dunn, dunn_file, row.names = FALSE)
        cld_file <- file.path(temp_dir, "cld_table_nonparametric.csv")
        write.csv(cld_table_nonparametric, cld_file, row.names = FALSE)
        
        zip::zipr(file, 
                  files = c(shapiro_file, conf_int_file, kruskal_file, dunn_file, cld_file), 
                  compression_level = 9)
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
    
  })
  
  observeEvent(input$start_analysis, {
    analysis_result()
  })
}