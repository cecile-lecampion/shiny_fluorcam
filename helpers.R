########################################################################################################################################
# Function used in the script
########################################################################################################################################

# Define a function to check normality status of the data
#========================================================================================================================================

check_normality <- function(shapiro_df) {
  # Assume normality is true initially
  flag_normal <- TRUE

  for (i in seq_len(nrow(shapiro_df))) {
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
        setNames(p.adj, paste(group1, group2, sep = "-")), # nolint: object_usage_linter.
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
analyse_curve <- function(df, col_vector, 
                          user_params = NULL,
                          grouping_col = NULL, 
                          facet_col = NULL) {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Validate that user parameters are provided
  if(is.null(user_params) || is.null(user_params$selected_params)) {
    p <- ggplot() + 
      geom_text(aes(x = 1, y = 1, label = "Please validate parameters using 'AddL1-L9, D1, D1, values and unit' button first.")) +
      theme_minimal() +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    return(p)
  }
  
  # Extract validated parameters
  parameter_cols <- user_params$selected_params
  time_values <- user_params$times
  unit <- user_params$unit
  
  # Check if the selected parameters exist in the dataframe
  missing_params <- setdiff(parameter_cols, colnames(df))
  if(length(missing_params) > 0) {
    p <- ggplot() + 
      geom_text(aes(x = 1, y = 1, label = paste("Missing parameters in data:", paste(missing_params, collapse = ", ")))) +
      theme_minimal() +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    return(p)
  }
  
  # Use provided grouping columns or detect them
  if(is.null(grouping_col) || is.null(facet_col)) {
    var_cols <- colnames(df)[!colnames(df) %in% parameter_cols]
    if(length(var_cols) >= 2) {
      grouping_col <- var_cols[2]  # Typically "Line" 
      facet_col <- var_cols[1]     # Typically "Day"
    } else {
      stop("Cannot determine grouping variables. Please specify grouping_col and facet_col.")
    }
  }
  
  # Reshape data to long format for plotting
  df_long <- df %>%
    select(all_of(c(grouping_col, facet_col, parameter_cols))) %>%
    pivot_longer(cols = all_of(parameter_cols), 
                 names_to = "parameter", 
                 values_to = "value") %>%
    filter(!is.na(value))
  
  # Add time values from user parameters
  if(!is.null(time_values) && length(time_values) == length(parameter_cols)) {
    time_mapping <- setNames(time_values, parameter_cols)
    df_long$time <- time_mapping[df_long$parameter]
  } else {
    # Fallback: extract time from parameter names
    df_long$time <- as.numeric(gsub(".*_([LD])([0-9]+)$", "\\2", df_long$parameter))
  }
  
  # Remove rows where time couldn't be assigned
  df_long <- df_long %>% filter(!is.na(time))
  
  # Convert factors
  df_long[[grouping_col]] <- as.factor(df_long[[grouping_col]])
  df_long[[facet_col]] <- as.factor(df_long[[facet_col]])
  
  # Create color mapping
  unique_groups <- unique(df_long[[grouping_col]])
  if(length(col_vector) >= length(unique_groups)) {
    color_mapping <- setNames(col_vector[1:length(unique_groups)], unique_groups)
  } else {
    color_mapping <- setNames(rainbow(length(unique_groups)), unique_groups)
  }
  
  # Calculate summary statistics for smooth curves
  summary_data <- df_long %>%
    group_by(!!sym(facet_col), !!sym(grouping_col), time) %>%
    dplyr::summarise(
      mean_value = mean(value, na.rm = TRUE),
      median_value = median(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE),
      count = n(),  # This is the correct way to use n()
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = sd_value / sqrt(count)  # Calculate SE after the summarise
    )
  
  # Create the plot
  p <- ggplot(summary_data, aes(x = time, y = median_value, color = !!sym(grouping_col))) +
    geom_point(size = 2, alpha = 0.7) +
    geom_line(size = 1) +
    geom_errorbar(aes(ymin = median_value - se, ymax = median_value + se), 
                  width = 0.1, alpha = 0.5) +
    scale_color_manual(values = color_mapping) +
    facet_wrap(as.formula(paste("~", facet_col))) +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(face = "plain", size = 10, color = "black", hjust = 0.5)
    ) +
    labs(
      x = if(!is.null(unit)) paste("Time (", unit, ")") else "Time",
      y = "Fv/Fm",
      title = paste("Time Course Analysis -", length(parameter_cols), "time points")
    )
  
  return(p)
}