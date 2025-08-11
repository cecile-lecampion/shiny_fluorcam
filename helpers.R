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

# Plot curve qGAM version
#========================================================================================================================================

analyse_curve <- function(df, col_vector, 
                          parameter_col = "value", 
                          time_col = "secondes", 
                          grouping_col = "line", 
                          facet_col = "time",
                          control_group = NULL,
                          k = 5) {
  
  # Load required packages for qGAM
  required_packages <- c("qgam", "dplyr", "ggplot2")
  for(pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  
  # Data validation and preparation
  validate_and_prepare_data <- function(df, parameter_col, time_col, grouping_col, facet_col) {
    # Check required columns exist
    required_cols <- c(parameter_col, time_col, grouping_col, facet_col)
    missing_cols <- setdiff(required_cols, colnames(df))
    if (length(missing_cols) > 0) {
      stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Convert data types
    df[[grouping_col]] <- as.factor(df[[grouping_col]])
    df[[facet_col]] <- as.factor(df[[facet_col]])
    df[[time_col]] <- suppressWarnings(as.numeric(as.character(df[[time_col]])))
    df[[parameter_col]] <- suppressWarnings(as.numeric(df[[parameter_col]]))
    
    # Remove rows with missing values
    df <- df %>% 
      filter(!is.na(.data[[grouping_col]]) & 
               !is.na(.data[[facet_col]]) &
               !is.na(.data[[time_col]]) & 
               !is.na(.data[[parameter_col]]))
    
    if (nrow(df) == 0) {
      stop("No valid data remaining after cleaning")
    }
    
    return(df)
  }
  
  # Fit qGAM models
  fit_qgam_models <- function(df, grouping_col, facet_col, time_col, parameter_col, k) {
    # Create time grid for smooth predictions
    time_range <- range(df[[time_col]], na.rm = TRUE)
    time_grid <- seq(time_range[1], time_range[2], length.out = 200)
    
    qgam_preds <- df %>%
      group_by(!!sym(facet_col), !!sym(grouping_col)) %>%
      group_modify(~{
        tryCatch({
          # Fit qGAM model
          mod <- qgam::qgam(
            as.formula(paste(parameter_col, "~ s(", time_col, ", k=", k, ")")),
            data = .x,
            qu = 0.5  # Median regression
          )
          
          # Create prediction data
          newdat <- data.frame(setNames(list(time_grid), time_col))
          newdat[[grouping_col]] <- unique(.x[[grouping_col]])[1]
          newdat[[facet_col]] <- unique(.x[[facet_col]])[1]
          
          # Get predictions with confidence intervals
          preds <- predict(mod, newdata = newdat, se.fit = TRUE)
          newdat$fit <- preds$fit
          newdat$lwr <- preds$fit - 1.96 * preds$se.fit
          newdat$upr <- preds$fit + 1.96 * preds$se.fit
          
          return(newdat)
        }, error = function(e) {
          warning(paste("Failed to fit qGAM model for", 
                        unique(.x[[facet_col]])[1], "-", unique(.x[[grouping_col]])[1], 
                        ":", e$message))
          return(data.frame())
        })
      }) %>%
      ungroup()
    
    return(qgam_preds)
  }
  
  # Perform statistical tests if control group is specified
  perform_statistical_tests <- function(df, grouping_col, facet_col, time_col, parameter_col, control_group) {
    if (is.null(control_group) || !control_group %in% df[[grouping_col]]) {
      return(data.frame())
    }
    
    results <- df %>%
      group_by(!!sym(facet_col)) %>%
      group_modify(~{
        groups_to_compare <- setdiff(levels(factor(.x[[grouping_col]])), control_group)
        
        test_results <- data.frame(
          Group = groups_to_compare,
          p.value = NA_real_,
          stringsAsFactors = FALSE
        )
        names(test_results)[1] <- grouping_col
        
        for (i in seq_along(groups_to_compare)) {
          group <- groups_to_compare[i]
          df_sub <- .x %>% 
            filter(.data[[grouping_col]] %in% c(control_group, group)) %>%
            mutate(!!sym(grouping_col) := droplevels(factor(.data[[grouping_col]])))
          
          tryCatch({
            m1 <- qgam::qgam(
              as.formula(paste(parameter_col, "~ s(", time_col, ", by = ", grouping_col, ", k=5) +", grouping_col)),
              data = df_sub,
              qu = 0.5
            )
            s <- summary(m1)
            if (length(s$s.pv) >= 2) {
              test_results$p.value[i] <- s$s.pv[2]
            }
          }, error = function(e) {
            warning(paste("Statistical test failed for", group, "in", unique(.x[[facet_col]])[1], ":", e$message))
          })
        }
        
        return(test_results)
      }) %>%
      ungroup()
    
    return(results)
  }
  
  # Main execution
  tryCatch({
    # Validate and prepare data
    df_clean <- validate_and_prepare_data(df, parameter_col, time_col, grouping_col, facet_col)
    
    # Fit qGAM models
    qgam_preds <- fit_qgam_models(df_clean, grouping_col, facet_col, time_col, parameter_col, k)
    
    # Perform statistical tests if control group specified
    stat_results <- perform_statistical_tests(df_clean, grouping_col, facet_col, time_col, parameter_col, control_group)
    
    # Calculate median points for overlay
    median_points <- df_clean %>%
      group_by(!!sym(facet_col), !!sym(grouping_col), !!sym(time_col)) %>%
      summarise(median_value = median(.data[[parameter_col]], na.rm = TRUE), .groups = "drop")
    
    # Create color mapping
    unique_groups <- unique(df_clean[[grouping_col]])
    if (length(col_vector) >= length(unique_groups)) {
      color_mapping <- setNames(col_vector[1:length(unique_groups)], unique_groups)
    } else {
      # If not enough colors provided, use RColorBrewer
      color_mapping <- setNames(RColorBrewer::brewer.pal(max(3, length(unique_groups)), "Set1")[1:length(unique_groups)], unique_groups)
    }
    
    # Create the plot
    p <- ggplot() +
      # Confidence ribbon
      geom_ribbon(
        data = qgam_preds,
        aes(x = !!sym(time_col), ymin = lwr, ymax = upr, fill = !!sym(grouping_col)),
        alpha = 0.3, linetype = 0
      ) +
      # Smooth line
      geom_line(
        data = qgam_preds,
        aes(x = !!sym(time_col), y = fit, color = !!sym(grouping_col)),
        size = 1
      ) +
      # Median points overlay
      geom_point(
        data = median_points,
        aes(x = !!sym(time_col), y = median_value, color = !!sym(grouping_col)),
        size = 1.5, alpha = 0.7
      ) +
      # Color scales
      scale_fill_manual(values = color_mapping) +
      scale_colour_manual(values = color_mapping) +
      # Faceting
      facet_wrap(as.formula(paste("~", facet_col))) +
      # Theme
      theme_classic() +
      theme(
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "plain", size = 10, color = "black", hjust = 0.5)
      ) +
      labs(x = time_col, y = parameter_col)
    
    # Add statistical annotations if available
    if (nrow(stat_results) > 0 && !is.null(control_group)) {
      # Create annotation dataframe
      annotation_df <- stat_results %>%
        filter(!is.na(p.value)) %>%
        mutate(
          label = ifelse(p.value < 0.05,
                         paste0("p = ", format.pval(p.value, digits = 2, eps = .001)),
                         ""),
          y = max(df_clean[[parameter_col]], na.rm = TRUE) * 1.05,
          x = median(df_clean[[time_col]], na.rm = TRUE)
        ) %>%
        filter(label != "")
      
      if (nrow(annotation_df) > 0) {
        p <- p + geom_text(
          data = annotation_df,
          aes(x = x, y = y, label = label),
          size = 3, fontface = "bold",
          inherit.aes = FALSE
        )
      }
    }
    
    return(list(
      plot = p,
      qgam_predictions = qgam_preds,
      statistical_results = stat_results,
      median_points = median_points
    ))
    
  }, error = function(e) {
    stop(paste("Error in qGAM curve analysis:", e$message))
  })
}