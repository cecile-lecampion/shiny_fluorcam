# Replace the existing analyse_curve function with this qGAM version
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