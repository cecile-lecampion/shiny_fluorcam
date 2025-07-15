# ==========================================================
#   qGAM-based smoothed time series plot, grouped by var1
# ==========================================================

# NB: Good for non-parametric data.

# Install/load libraries as needed
required_packages <- c("readxl", "dplyr", "ggplot2", "qgam", "RColorBrewer")
for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ==== PARAMETERS TO EDIT ====
DATA <- "df_tidy.xlsx"       # Your data file
var1 <- "Line"               # Grouping variable
var2 <- "Day"                # Time variable (should be numeric or convertible)
MEASURE_COL <- "Fv_Fm"       # Measurement variable
control_group <- "Col"

# Plot parameters
COLUMN_LINE_COLOUR <- "darkgrey"
COLUMN_FILL_COLOUR <- "lightgrey"
POINT_COLOR <- "darkgreen"
plot_width <- 15
plot_height <- 5

# Function to validate and load data
load_and_validate_data <- function(file_path, var1, var2, measure_col) {
  if (!file.exists(file_path)) {
    stop(paste("Data file not found:", file_path))
  }
  
  df <- read_excel(file_path)
  
  # Check required columns exist
  required_cols <- c(var1, var2, measure_col)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Convert data types with error handling
  df[[var1]] <- as.factor(df[[var1]])
  df[[var2]] <- suppressWarnings(as.numeric(as.character(df[[var2]])))
  df[[measure_col]] <- suppressWarnings(as.numeric(df[[measure_col]]))
  
  # Remove rows with missing values in key columns
  df <- df %>% 
    filter(!is.na(.data[[var1]]) & 
           !is.na(.data[[var2]]) & 
           !is.na(.data[[measure_col]]))
  
  if (nrow(df) == 0) {
    stop("No valid data remaining after cleaning")
  }
  
  return(df)
}

# Function to fit qGAM models and generate predictions
fit_qgam_models <- function(df, var1, var2, measure_col, k = 5) {
  time_grid <- seq(min(df[[var2]], na.rm = TRUE), 
                   max(df[[var2]], na.rm = TRUE), 
                   length.out = 200)
  
  qgam_preds <- df %>%
    group_by(!!sym(var1)) %>%
    group_modify(~{
      tryCatch({
        mod <- qgam::qgam(
          as.formula(paste(measure_col, "~ s(", var2, ", k=", k, ")")),
          data = .x,
          qu = 0.5
        )
        newdat <- data.frame(setNames(list(time_grid), var2))
        newdat[[var1]] <- unique(.x[[var1]])[1]
        preds <- predict(mod, newdata = newdat, se.fit = TRUE)
        newdat$fit <- preds$fit
        newdat$lwr <- preds$fit - 1.96 * preds$se.fit
        newdat$upr <- preds$fit + 1.96 * preds$se.fit
        newdat
      }, error = function(e) {
        warning(paste("Failed to fit model for group", unique(.x[[var1]])[1], ":", e$message))
        return(data.frame())
      })
    }) %>%
    ungroup()
  
  return(qgam_preds)
}

# Function to perform statistical tests
perform_statistical_tests <- function(df, var1, var2, measure_col, control_group) {
  if (!control_group %in% df[[var1]]) {
    stop(paste("Control group", control_group, "not found in data"))
  }
  
  groups_to_compare <- setdiff(levels(df[[var1]]), control_group)
  
  results <- data.frame(
    Line = groups_to_compare,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(groups_to_compare)) {
    group <- groups_to_compare[i]
    df_sub <- df %>% 
      filter(.data[[var1]] %in% c(control_group, group)) %>% 
      droplevels()
    
    tryCatch({
      m1 <- qgam(
        as.formula(paste(measure_col, "~ s(", var2, ", by = ", var1, ", k=5) +", var1)),
        data = df_sub,
        qu = 0.5
      )
      s <- summary(m1)
      # Check if we have enough smooth terms
      if (length(s$s.pv) >= 2) {
        results$p.value[i] <- s$s.pv[2]
      } else {
        warning(paste("Not enough smooth terms for group", group))
      }
    }, error = function(e) {
      warning(paste("Statistical test failed for group", group, ":", e$message))
    })
  }
  
  # Add significance indicators
  results$signif <- ifelse(results$p.value < 0.05, "Yes", "No")
  results$p.value.pretty <- ifelse(
    results$p.value < 0.001, "<0.001",
    format(results$p.value, digits = 3, scientific = FALSE)
  )
  
  return(results)
}

# Main execution
tryCatch({
  # Load and validate data
  df <- load_and_validate_data(DATA, var1, var2, MEASURE_COL)
  
  # Optional: Set factor order for var1
  # order1 <- c("Col","72B1","73A2", "81F1")
  # df[[var1]] <- factor(df[[var1]], levels = order1)
  
  # Fit qGAM models
  qgam_preds <- fit_qgam_models(df, var1, var2, MEASURE_COL)
  
  # Perform statistical tests
  results <- perform_statistical_tests(df, var1, var2, MEASURE_COL, control_group)
  
  print(results)
  write.csv(results, "qgam_profile_vs_control.csv", row.names = FALSE)
  
  # Create annotation dataframe
  annotation_df <- results %>%
    mutate(
      label = ifelse(!is.na(p.value) & p.value < 0.05,
                     paste0("p = ", format.pval(p.value, digits = 2, eps = .001)),
                     ""),
      y = max(df[[MEASURE_COL]], na.rm = TRUE) * 1.05,
      x = median(df[[var2]], na.rm = TRUE)
    )
  colnames(annotation_df)[1] <- var1
  
  # Add control group row
  annotation_df <- bind_rows(
    annotation_df,
    tibble(
      !!var1 := control_group,
      p.value = NA_real_,
      label = "",
      y = max(df[[MEASURE_COL]], na.rm = TRUE) * 1.05,
      x = median(df[[var2]], na.rm = TRUE)
    )
  )
  
  # Calculate median points
  summary_points <- df %>%
    group_by(!!sym(var1), !!sym(var2)) %>%
    summarise(Median = median(.data[[MEASURE_COL]], na.rm = TRUE), .groups = "drop")
  
  # Calculate axis ranges
  data_min <- min(df[[MEASURE_COL]], na.rm = TRUE)
  data_max <- max(df[[MEASURE_COL]], na.rm = TRUE)
  data_range <- data_max - data_min
  lower_offset <- 0.02 * data_range
  upper_offset <- 0.05 * data_range
  
  # Update annotation position
  annotation_df$y <- data_max + upper_offset * 0.5
  
  # Create plot
  p <- ggplot() +
    geom_ribbon(
      data = qgam_preds,
      aes(x = !!sym(var2), ymin = lwr, ymax = upr, group = !!sym(var1)),
      fill = COLUMN_FILL_COLOUR, alpha = 0.3
    ) +
    geom_line(
      data = qgam_preds,
      aes(x = !!sym(var2), y = fit, group = !!sym(var1)),
      color = COLUMN_LINE_COLOUR, linewidth = 1
    ) +
    geom_point(
      data = summary_points,
      aes(x = !!sym(var2), y = Median),
      color = POINT_COLOR, size = 2
    ) +
    geom_text(
      data = annotation_df,
      aes(x = x, y = y, label = label, group = !!sym(var1)),
      inherit.aes = FALSE,
      size = 2,
      fontface = "bold"
    ) +
    coord_cartesian(ylim = c(data_min - lower_offset, data_max + upper_offset)) +
    facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
    theme_classic() +
    labs(x = var2, y = MEASURE_COL) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "none"
    )
  
  print(p)
  ggsave("plot_gam.svg", width = plot_width, height = plot_height)
  
  cat("Analysis completed successfully!\n")
  
}, error = function(e) {
  cat("Error in analysis:", e$message, "\n")
  stop(e)
})
