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
library(Rmisc) # for the command summarySE
library(plyr) # rmisc dependency
library(multcompView) # for cld letters
library(ggplot2) # for plotting
library(dplyr) # for data manipulation
library(ggbeeswarm) # for geom_quasirandom
library(zip) # to create zip files

# Définition de la fonction pour vérifier la normalité
check_normality <- function(shapiro_df) {
  # Assume normality is true initially
  flag_normal <- TRUE
  
  for (i in 1:nrow(shapiro_df)) {
    if (shapiro_df$p[i] <= 0.05) {
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
      downloadButton("download_parametric", "Download All Analysis Results for parametric data"),
      downloadButton("download_non_parametric", "Download All Analysis Results for non-parametric data")
    ),
    
    mainPanel(
      tableOutput("selected_files"),
      tags$hr(),
      tableOutput("processed_data"),
      uiOutput("toggle_button"), # Afficher le bouton ici
      tags$hr(),
      textOutput("selectedValue"),
      verbatimTextOutput("normality_result"),
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
      req(var1, var2, MEASURE_COL)
      
      shapiro_df <- result_df$data %>%
        group_by(!!sym(var2), !!sym(var1)) %>%
        shapiro_test(!!sym(MEASURE_COL))
      
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
          "Datas follow a normal law."
        } else {
          "Datas don't follow a normal law."
        }
  })
 
  
  # Analyse des données selon le statut de normalité
  if (normality_check()) {
    # Summary
    library(plyr)
    library(Rmisc)
    my_summary <- summarySE(result_df$data, measurevar = MEASURE_COL, groupvars = c(var2, var1))
    
    # Stats
    anova_result <- result_df$data %>%
      group_by(!!sym(var1)) %>%
      rstatix::anova_test(formula = reformulate(var2, MEASURE_COL))
    
    # Tukey HSD Test
    tukey_results <- result_df$data %>%
      group_by(!!sym(var1)) %>% 
      rstatix::tukey_hsd(as.formula(paste(MEASURE_COL, "~", var2)))
    
    # CLD letters for parametric test
    cld_table_parametric <- tukey_results %>%
      group_by(!!sym(var1)) %>%
      summarise(
        cld = list(multcompView::multcompLetters(setNames(p.adj, paste(group1, group2, sep = "-")), Letters = letters)$Letters),
        .groups = 'drop'
      ) %>%
      unnest_longer(cld) %>%
      mutate(!!var2 := names(cld))
    
    # Plotting
    df2 <- merge(my_summary, cld_table_parametric, by.x = c(var2, var1), by.y = c(var2, var1))
    df2[[var1]] <- factor(df2[[var1]], levels = input$var1_order)
    df2[[var2]] <- factor(df2[[var2]], levels = input$var2_order)
    result_df$data[[var1]] <- factor(result_df$data[[var1]], levels = input$var1_order)
    result_df$data[[var2]] <- factor(result_df$data[[var2]], levels = input$var2_order)
    
    p <- df2 %>%
      mutate(
        !!sym(var1) := as.factor(!!sym(var1)),
        !!sym(var2) := as.factor(!!sym(var2))
      ) %>%
      ggplot(aes(x = !!sym(var2), y = !!sym(MEASURE_COL), fill = !!sym(var2))) +
      geom_col(color = line_color(), width = 0.6, position = position_dodge2(padding = 0.05)) +
      scale_fill_manual(values = rep(fill_color(), length(unique(df2[[var2]])))) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.1)), 
        breaks = seq(0, 1, by = 0.2))  +
      geom_quasirandom(data = result_df$data, aes(x = !!sym(var2), y = !!sym(MEASURE_COL)), color = point_color(), width = 0.3, alpha = 0.6) +
      geom_segment(aes(x = !!sym(var2), xend = !!sym(var2), y = pmax(0, !!sym(MEASURE_COL) - ci), yend = !!sym(MEASURE_COL) + ci), color = "black") +
      geom_text(aes(x = !!sym(var2), y = !!sym(MEASURE_COL) + (0.15 * max(df2[[MEASURE_COL]], na.rm = TRUE)), label = cld), size = 3, inherit.aes = TRUE) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.5),
        axis.line.y = element_line(linewidth = 0.5),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'transparent', color = NA),
        plot.background = element_rect(fill = 'transparent', color = NA),
        axis.text.y = element_text(vjust = 1),
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "plain", size = 10, color = "black", hjust = 0.5)) +
      facet_wrap(as.formula(paste("~", var1)), nrow = 1) +
      labs(x = "Treatment", y = "Median Value")
    
    output$plot_result <- renderPlot({
      print(p)
    })
    
    ggsave("plot_para.svg", width = 8, height = 6)
    
    # Enregistrement de tous les tableaux en tant que fichier ZIP pour les données paramétriques
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
    
  } else {
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
    
    # Stats
    kruskal_pval <- result_df$data %>%
      group_by(!!sym(var1)) %>%
      kruskal_test(as.formula(paste(MEASURE_COL, "~", var2))) %>%
      dplyr::select(all_of(var1), p)
    
    significant <- any(kruskal_pval$p < 0.05)
    
    if (significant) {
      pval_dunn <- test_dunn(result_df$data, var1, var2, MEASURE_COL)
      
      # CLD letters for non-parametric test
      cld_table_nonparametric <- pval_dunn %>%
        group_by(!!sym(var1)) %>%
        summarise(
          cld = list(multcompView::multcompLetters(setNames(p.adj, paste(group1, group2, sep = "-")), Letters = letters)$Letters),
          .groups = 'drop'
        ) %>%
        unnest_longer(cld) %>%
        mutate(!!var2 := names(cld))
      
      # Plotting
      df2 <- merge(conf_int, cld_table_nonparametric, by.x = c(var2, var1), by.y = c(var2, var1))
      df2[[var1]] <- factor(df2[[var1]], levels = input$var1_order)
      df2[[var2]] <- factor(df2[[var2]], levels = input$var2_order)
      result_df$data[[var1]] <- factor(result_df$data[[var1]], levels = input$var1_order)
      result_df$data[[var2]] <- factor(result_df$data[[var2]], levels = input$var2_order)
      
      p <- df2 %>%
        ggplot(aes(x = !!sym(var2), y = Median, fill = !!sym(var2))) +
        geom_col(color = line_color(), width = 0.6, position = position_dodge2(padding = 0.05)) +
        scale_fill_manual(values = rep(fill_color(), length(unique(df2[[var2]])))) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1)), 
          breaks = seq(0, 1, by = 0.2) ) +
        geom_quasirandom(data = result_df$data, aes(x = !!sym(var2), y = !!sym(MEASURE_COL)), color = point_color(), width = 0.3, alpha = 0.6) +
        geom_segment(aes(x = !!sym(var2), xend = !!sym(var2), y = pmax(0, Percentile.lower), yend = Percentile.upper), color = "black") +
        geom_text(aes(x = !!sym(var2), y = Percentile.upper + (0.15 * Median), label = cld), size = 3, inherit.aes = TRUE) +
        theme_classic() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(linewidth = 0.5),
          axis.line.y = element_line(linewidth = 0.5),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill = 'transparent', color = NA),
          axis.text.y = element_text(vjust = 1),
          legend.position = "none",
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(face = "plain", size = 10, color = "black", hjust = 0.5)
        ) +
        facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
        labs(x = "Treatment", y = "Median Value")
      
      output$plot_result <- renderPlot({
        print(p)
      })
      
      ggsave("plot_nonpara.svg", width = 8, height = 6)
      
    } else {
      print("Data are not significantly different, the Dunn test was not performed.")
      
    }
  }
  
    # Enregistrement de tous les tableaux en tant que fichier ZIP pour les données paramétriques
    output$download_non_parametric <- downloadHandler(
      filename = function() {
        paste("parametric_analysis_results_", Sys.Date(), ".zip", sep = "")
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
  })
  
  observeEvent(input$start_analysis, {
    analysis_result()
  })
}


shinyApp(ui = ui, server = server)