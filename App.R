# Installer les packages nécessaires si ce n'est pas déjà fait
# install.packages("shiny")
# install.packages("shinyFiles")
# install.packages("tidyr")
# install.packages("data.table")
# install.packages("sortable")
# install.packages("colourpicker")
# if (!require(dplyr)) { install.packages("dplyr") }
# if (!require(tidyverse)) { install.packages("tidyverse") }
# if (!require(devtools)) { install.packages("devtools") }
# if (!require(rstatix)) { install.packages("rstatix", repos = "https://cloud.r-project.org") }
# if (!require(ggbeeswarm)) { install.packages("ggbeeswarm") }
# if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
# if (!require(rcompanion)) { install.packages("rcompanion") }
# if (!require(readxl)) { install.packages("readxl") }

library(shiny)
library(shinyFiles)
library(fs)
library(tidyr)
library(data.table)
library(sortable)
library(colourpicker)
library(ggbeeswarm) # for function geom_quasirandom
library(RColorBrewer) # to define colors
library(rstatix)  # for statistical test
library(rcompanion) # to compute confidence interval for non parametric data
library(forcats) # for fct_reorder

# Définition de la fonction pour vérifier la normalité
check_normality <- function(shapiro_df) {
  # Assume normality is true initially
  flag_normal <- TRUE
  
  for (i in 1:nrow(shapiro_df)) {
    if (shapiro_df$p.value[i] <= 0.05) {
      # If any data group does not follow a normal law, stop and flag as non-normal
      flag_normal <- FALSE
      break
    }
  }
  
  return(flag_normal)
}


##
test_dunn <- function(df_data, var1, var2, MEASURE_COL) {
  pval <- df_data %>%
    group_by(!!sym(var1)) %>%
    dunn_test(formula = as.formula(paste(MEASURE_COL, "~", var2)), p.adjust.method = "BH") %>%
    as.data.frame()
  return(pval)
}


ui <- fluidPage(
  titlePanel("Fluorcam data analysis tool box"),
  
  sidebarLayout(
    sidebarPanel(
      tags$p(
        style = "font-size: 120%; color: blue;",
        tags$strong("1- Load data:")
      ),
      shinyDirButton("dir", "Directory selection", "Select Directory"),
      verbatimTextOutput("dirpath"),
      textInput("pattern", "Select files pattern", value = ".TXT"),
      actionButton("show_all", "Show Files : All/Short list"),
      textInput("areas", "Define names of areas", value = "Area 1,Area 2,Area 3,Area 4"),
      tags$hr(),
      tags$p(tags$strong("Collect informations about sample in file name :"),
             tags$br(),
             "Your file name must be VAR1_VAR2_VAR3.TXT",
             tags$br(),
             "Define correspondance to variable :"),
      textInput("var1", "Name of VAR1", value = "Day"),
      textInput("var2", "Name of VAR2", value = "Line"),
      textInput("var3", "Name of VAR3", value = "PlantID"),
      actionButton("load", "Load data"),
      tags$p(
        style = "font-size: 120%; margin-top: 20px; color: blue;",
        tags$strong("2- Graph and statistical analysis parameters :")
      ),
      tags$p(tags$strong("Define order of the lines and of the facets : \n"),
             "Drag the lines to change the order.",
             uiOutput("var2_order_ui"),
             uiOutput("var1_order_ui")),
      tags$p(tags$strong("Select colors for your graph :")),
      colourInput("line_color", "Line color", value = "darkgrey"),
      colourInput("fill_color", "Fill color", value = "ivory1"),
      colourInput("point_color", "Point color", value = "darkgreen"),
      uiOutput("columnSelect"),
      actionButton("start_analysis", "Start Analysis"),
      textOutput("normality_result")
    ),
    
    mainPanel(
      tableOutput("selected_files"),
      tags$hr(),
      tableOutput("processed_data"),
      uiOutput("toggle_button"), # Afficher le bouton ici
      tags$hr(),
      textOutput("selectedValue"),
      plotOutput("plot_result")
    )
  )
)

server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), "R" = R.home(), getVolumes()())
  
  shinyDirChoose(input, "dir", roots = volumes, session = session)
  
  dirpath <- reactive({
    parseDirPath(volumes, input$dir)
  })
  
  output$dirpath <- renderText({
    dirpath()
  })
  
  show_all_files <- reactiveVal(FALSE)
  
  observeEvent(input$show_all, {
    show_all_files(!show_all_files())
  })
  
  output$selected_files <- renderTable({
    req(dirpath(), input$pattern)
    files <- list.files(path = dirpath(), pattern = input$pattern, full.names = TRUE)
    
    if (!show_all_files()) {
      files <- head(files, 5)
    }
    
    data.frame(Fichier = basename(files))
  })
  
  process_data_files <- function(pattern, areas, var1, var2, var3, dirpath) {
    areas <- unlist(strsplit(areas, ","))
    
    remove_first_two_lines <- function(file_name) {
      data <- read.table(file_name, skip = 2, sep = "\t", header = TRUE)
      return(data)
    }
    
    compute_Fv_Fm <- function(df) {
      df$Fv_Fm <- df$Fv / df$Fm
      return(df)
    }
    
    add_name_column <- function(df, name) {
      df$Name <- name
      return(df)
    }
    
    divide_name <- function(df) {
      df <- tidyr::separate(data = df, col = "Name", into = c(var1, var2, var3), sep = "_", remove = TRUE)
      return(df)
    }
    
    files <- list.files(path = dirpath, pattern = pattern, full.names = TRUE)
    print(paste("Files found:", files))
    
    Liste <- lapply(files, remove_first_two_lines)
    names(Liste) <- tools::file_path_sans_ext(basename(files))
    
    Liste <- lapply(Liste, data.table::transpose, make.names = "X")
    
    Liste <- lapply(Liste, compute_Fv_Fm)
    
    Liste <- lapply(names(Liste), function(name) {
      add_name_column(Liste[[name]], name)
    })
    
    Liste <- lapply(Liste, divide_name)
    
    df <- do.call(rbind, Liste)
    df <- cbind(Area = rep(areas, length.out = nrow(df)), df)
    
    return(df)
  }
  
  result_df <- reactiveValues(data = NULL)
  
  observeEvent(input$load, {
    pattern <- input$pattern
    areas <- input$areas
    var1 <- input$var1
    var2 <- input$var2
    var3 <- input$var3
    dir_path <- dirpath()
    
    result_df$data <- process_data_files(
      pattern = pattern, 
      areas = areas, 
      var1 = var1, 
      var2 = var2, 
      var3 = var3, 
      dirpath = dir_path
    )
    
    # Afficher le bouton de repli du tableau
    output$toggle_button <- renderUI({
      req(result_df$data)
      actionButton("toggle_table", "Show/Hide Full Table")
    })
  
    # Stocker l'état du bouton
    show_full_table <- reactiveValues(full = FALSE)
    
    # Observer le bouton pour basculer l'état
    observeEvent(input$toggle_table, {
      show_full_table$full <- !show_full_table$full
    })
    
    # Afficher le tableau
    output$processed_data <- renderTable({
      req(result_df$data)
      
      if (show_full_table$full) {
        result_df$data
      } else {
        head(result_df$data, 5) # Afficher les 5 premières lignes
      }
    })
  })
  
  output$columnSelect <- renderUI({
    req(result_df$data)
    selectInput("column", "Select the column containing the parameter to analyse", 
                choices = colnames(result_df$data))
  })
  
  VALUE <- reactive({
    req(input$column)
    input$column
  })
  
  # # Utilisation de VALUE dans l'analyse
  # observe({
  #   req(VALUE())
  #   print(paste("La valeur de la variable VALUE est:", VALUE()))
  #   
  #   if (!is.null(result_df$data)) {
  #     selected_column_values <- result_df$data[, VALUE()]
  #     print(selected_column_values)
  #   }
  # })
  
  output$var2_order_ui <- renderUI({
    req(result_df$data)
    rank_list(
      text = "Order of lines (VAR2)",
      labels = unique(result_df$data[[input$var2]]),
      input_id = "var2_order"
    )
  })
  
  output$var1_order_ui <- renderUI({
    req(result_df$data)
    rank_list(
      text = "Order of facets (VAR1)",
      labels = unique(result_df$data[[input$var1]]),
      input_id = "var1_order"
    )
  })
  
  # color slection
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
  
  #traitement statistique des données
  
  
  # Traitement des données et analyse statistique
  analysis_result <- eventReactive(input$start_analysis, {
    req(result_df$data, VALUE())
    
    var1 <- input$var1
    var2 <- input$var2
    MEASURE_COL <- VALUE()
    
    # Conversion des variables en facteurs
    result_df$data[[var1]] <- as.factor(result_df$data[[var1]])
    result_df$data[[var2]] <- as.factor(result_df$data[[var2]])
    
    # Calcul du test de Shapiro
    shapiro_result <- reactive({
      req(var1(), var2(), MEASURE_COL())
      
      shapiro_df <- result_df %>%
        group_by(!!sym(var2()), !!sym(var1())) %>%
        shapiro_test(!!sym(MEASURE_COL()))
      
      return(shapiro_df)
    })
    
    # Vérification de la normalité
    normality_check <- reactive({
      shapiro_df <- shapiro_result()
      flag_normal <- check_normality(shapiro_df)
      return(flag_normal)
    })
    
    # Affichage du résultat
    output$normality_result <- renderText({
      if (normality_check()) {
        "Les données suivent une loi normale."
      } else {
        "Les données ne suivent pas une loi normale."
      }
    })
  })
}



shinyApp(ui = ui, server = server)
