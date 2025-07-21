########################################################################################################################################
# Function used in the script
########################################################################################################################################

# Define a function to check normality status of the data
#========================================================================================================================================

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



# Generate CLD values for parametric data
#========================================================================================================================================

generate_cld_parametric <- function(tukey_df, var1_col, var2_col) {
  tukey_df %>%
    group_by(!!sym(var1_col)) %>%
    summarise(
      cld = list(multcompView::multcompLetters(
        setNames(p.adj, paste(group1, group2, sep = "-")),
        Letters = letters
      )$Letters)
    ) %>%
    unnest_longer(cld) %>%
    rename(!!sym(var2_col) := cld_id)
}


# Generate CLD values for non-parametric data
#=======================================================================================================================================

generate_cld_nonparametric <- function(dunn_df, var1_col, var2_col) {
  dunn_df %>%
    dplyr::group_by(!!sym(var1_col)) %>%
    dplyr::summarise(
      cld = list(multcompView::multcompLetters(
        setNames(p.adj, paste(group1, group2, sep = "-")),
        Letters = letters
      )$Letters),
      .groups = 'drop'
    ) %>%
    tidyr::unnest_longer(cld) %>%
    dplyr::mutate(!!sym(var2_col) := names(cld)) %>%
    dplyr::select(!!sym(var1_col), !!sym(var2_col), cld)
}

# Define a function to perform Dunn test
#========================================================================================================================================

test_dunn <- function(df_data, var1, var2, MEASURE_COL) {
  pval <- df_data %>%
    group_by(!!sym(var1)) %>%
    dunn_test(formula = as.formula(paste(MEASURE_COL, "~", var2)), p.adjust.method = "BH") %>%
    as.data.frame()
  return(pval)
}

# Function to extract the "area" from the file header
extract_area_from_header <- function(file_name) {
  lines <- readLines(file_name, n = 5) # Adjust n if the header is longer
  area_line <- grep("Area:", lines, value = TRUE)
  if (length(area_line) > 0) {
    gsub(".*Area:\\s*", "", area_line)
  } else {
    return(NA)
  }
}

# Function to process data files
#========================================================================================================================================

process_data_files <- function(pattern, areas, var1, var2, var3, dirpath) {
  files <- list.files(path = dirpath, pattern = pattern, full.names = TRUE)
  print(paste("Files found:", files))
  
  remove_first_two_lines <- function(file_name, area) {
    # Lire toutes les lignes du fichier
    lines <- readLines(file_name)
    # Retirer les lignes vides 
    lines <- lines[lines != ""]
    # Retirer les deux premières lignes
    if(length(lines) > 2){
      lines <- lines[-c(1,2)]
    } else {
      stop("Le fichier ne contient pas assez de lignes.")
    }
    data <- read.table(text = lines, sep = "\t", header = TRUE)
    return(data)
  }
  
  # Function to compute Fv/Fm
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
  
  Liste <- lapply(files, remove_first_two_lines, area = "")
  names(Liste) <- tools::file_path_sans_ext(basename(files))
  
  Liste <- lapply(Liste, data.table::transpose, make.names = "X")
  Liste <- lapply(Liste, compute_Fv_Fm)
  
  Liste <- lapply(names(Liste), function(name) {
    add_name_column(Liste[[name]], name)
  })
  
  Liste <- lapply(Liste, divide_name)
  
  df <- do.call(rbind, Liste)
  #df <- cbind(Area = rep(areas, length.out = nrow(df)), df)
  
  return(df)
}



# Function to plot Bar plot
#========================================================================================================================================

analyse_barplot <- function(
    data, 
    var1, var2, measure_col, 
    var1_order = NULL, var2_order = NULL,
    fill_color = "ivory1", line_color = "darkgrey", point_color = "darkgreen"
) {
  # Validate inputs
  if(is.null(data) || nrow(data) == 0) {
    stop("Data is empty or NULL")
  }

  if(!var1 %in% colnames(data)) {
    stop(paste("Variable", var1, "not found in data"))
  }

  if(!var2 %in% colnames(data)) {
    stop(paste("Variable", var2, "not found in data"))
  }

  if(!measure_col %in% colnames(data)) {
    stop(paste("Measure column", measure_col, "not found in data"))
  }

  # Convert variables to factors and apply order if provided
  if(!is.null(var1_order)) {
    data[[var1]] <- factor(data[[var1]], levels = var1_order)
  } else {
    data[[var1]] <- as.factor(data[[var1]])
  }

  if(!is.null(var2_order)) {
    data[[var2]] <- factor(data[[var2]], levels = var2_order)
  } else {
    data[[var2]] <- as.factor(data[[var2]])
  }
  
  # Shapiro test
  shapiro_df <- data %>%
    group_by(!!sym(var2), !!sym(var1)) %>%
    rstatix::shapiro_test(!!sym(measure_col))
  
  flag_normal <- check_normality(shapiro_df)
  
  if (flag_normal) {
    my_summary <- summarySE(data, measurevar = measure_col, groupvars = c(var2, var1))
    
    # ANOVA & Tukey
    anova_result <- data %>%
      group_by(!!sym(var1)) %>%
      rstatix::anova_test(reformulate(var2, measure_col))
    tukey_results <- data %>%
      group_by(!!sym(var1)) %>% 
      rstatix::tukey_hsd(as.formula(paste(measure_col, "~", var2)))
    cld_table_parametric <- generate_cld_parametric(tukey_results, var1, var2)
    
    df2 <- merge(my_summary, cld_table_parametric, by = c(var2, var1), all.x = TRUE)
    
    # Ensure factor levels are maintained
    if(!is.null(var1_order)) {
      df2[[var1]] <- factor(df2[[var1]], levels = var1_order)
    }
    if(!is.null(var2_order)) {
      df2[[var2]] <- factor(df2[[var2]], levels = var2_order)
    }
    
    p <- df2 %>%
      mutate(!!sym(var1) := as.factor(!!sym(var1)),
             !!sym(var2) := as.factor(!!sym(var2))) %>%
      ggplot(aes(x = !!sym(var2), y = !!sym(measure_col), fill = !!sym(var2))) +
      geom_col(color = line_color, width = 0.6, position = position_dodge2(padding = 0.05)) +
      scale_fill_manual(values = rep(fill_color, length(unique(df2[[var2]])))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      geom_quasirandom(
        data = data, 
        aes(x = !!sym(var2), y = !!sym(measure_col)), 
        color = point_color, width = 0.3, alpha = 0.6
      ) +
      geom_segment(aes(x = !!sym(var2), xend = !!sym(var2), y = pmax(0, !!sym(measure_col) - ci), yend = !!sym(measure_col) + ci), 
                   color = "black") +
      geom_text(aes(x = !!sym(var2), y = !!sym(measure_col) + (0.15 * max(df2[[measure_col]], na.rm = TRUE)), label = cld), 
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
        strip.text = element_text(face = "plain", size = 10, color = "black", hjust = 0.5)
      ) +
      facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
      labs(x = var1, y = measure_col)
    
    return(list(
      plot = p,
      summary = my_summary,
      shapiro = shapiro_df,
      anova = anova_result,
      tukey = tukey_results,
      cld = cld_table_parametric
    ))
    
  } else {
    # Non-parametric
    conf_int <- groupwiseMedian(
      data = data,
      var = measure_col,
      group = c(var2, var1),
      conf = 0.95,
      R = 5000,
      percentile = TRUE,
      bca = FALSE,
      digits = 3
    )
    kruskal_pval <- data %>%
      group_by(.data[[var1]]) %>%
      rstatix::kruskal_test(as.formula(paste(measure_col, "~", var2))) %>%
      dplyr::select(all_of(var1), p)
    significant <- any(kruskal_pval$p < 0.05)
    
    if (significant) {
      pval_dunn <- test_dunn(data, var1, var2, measure_col)
      cld_table_nonparametric <- generate_cld_nonparametric(pval_dunn, var1, var2)
      df2 <- merge(conf_int, cld_table_nonparametric, by.x = c(var2, var1), by.y = c(var2, var1))
      df2[[var1]] <- factor(df2[[var1]], levels = var1_order)
      df2[[var2]] <- factor(df2[[var2]], levels = var2_order)
      
      p <- df2 %>%
        ggplot(aes(x = !!sym(var2), y = Median, fill = !!sym(var2))) +
        geom_col(color = line_color, width = 0.6, position = position_dodge2(padding = 0.05)) +
        scale_fill_manual(values = rep(fill_color, length(unique(df2[[var2]])))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        geom_quasirandom(
          data = data, 
          aes(x = !!sym(var2), y = !!sym(measure_col)), 
          color = point_color, width = 0.3, alpha = 0.6
        ) +
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
          strip.text = element_text(face = "plain", size = 10, color = "black", hjust = 0.5)
        ) +
        facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
        labs(x = var1, y = measure_col)
      
      return(list(
        plot = p,
        summary = conf_int,
        shapiro = shapiro_df,
        kruskal = kruskal_pval,
        dunn = pval_dunn,
        cld = cld_table_nonparametric
      ))
    } else {
      # Not significantly different
      return("Data are not significantly different, the Dunn test was not performed.")
    }
  }
}

# Function to plot curve
#========================================================================================================================================
analyse_curve <- function(df, col_vector) {
  # This function needs to be adapted based on your specific data structure
  # For now, return a simple placeholder plot
  library(ggplot2)
  
  # Create a simple placeholder plot if the expected columns don't exist
  if(!"value" %in% colnames(df)) {
    p <- ggplot(df, aes(x = 1, y = 1)) + 
      geom_text(aes(label = "Curve analysis needs to be configured for your data structure")) +
      theme_minimal()
    return(p)
  }
  
  # Original function logic continues here if data structure is correct
  # ... rest of the function
  # Determining data normality status
  shapiro_df <- df %>%
    dplyr::group_by(time, line, secondes) %>%
    summarise(
      statistic = shapiro.test(value)$statistic,
      p.value = shapiro.test(value)$p.value,
      .groups = "drop"
    )
  
  flag_normal <- check_normality(shapiro_df)
  
  if (flag_normal) {
    # Summary
    my_summary <- Rmisc::summarySE(df, measurevar = "value", groupvars = c("time", "line", "secondes"))
    
    p <- my_summary %>%
      ggplot(aes(x = secondes, y = value, group = line)) +
      geom_ribbon(aes(ymin = value - ci, ymax = value + ci, fill = line), alpha = 0.3, linetype = 0) +
      scale_fill_manual(values = col_vector) +
      geom_point(aes(color = line)) +
      geom_line(aes(color = line), size = 1) +
      scale_colour_manual(values = col_vector) +
      facet_wrap(~time) +
      theme_classic() +
      theme(legend.title = element_blank())
  } else {
    conf_int <- groupwiseMedian(
      data = df,
      var = "value",
      group = c("time", "line", "secondes"),
      conf = 0.95,
      R = 5000,
      percentile = TRUE,
      bca = FALSE,
      digits = 3
    )
    p <- conf_int %>%
      ggplot(aes(x = secondes, y = Median, group = line)) +
      geom_ribbon(aes(ymin = Percentile.lower, ymax = Percentile.upper, fill = line), alpha = 0.3, linetype = 0) +
      scale_fill_manual(values = col_vector) +
      geom_point(aes(color = line)) +
      geom_line(aes(color = line), size = 1) +
      scale_colour_manual(values = col_vector) +
      facet_wrap(~time) +
      theme_classic() +
      theme(legend.title = element_blank())
  }
  return(p)
}