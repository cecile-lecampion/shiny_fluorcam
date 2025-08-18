########################################################################################################################################
# Function used in the script
########################################################################################################################################
# STRATEGY: Modular helper functions for data processing and statistical analysis
# - Statistical testing functions for normality and multiple comparisons
# - Data processing pipeline for FluorCam files
# - Visualization functions for bar plots and curve analysis
# - Separation of concerns: each function has a single responsibility
# - Reusable components that can be tested independently

# ===========================================
# SECTION 1: STATISTICAL TESTING FUNCTIONS
# ===========================================
# PURPOSE: Provide robust statistical analysis capabilities
# STRATEGY: Separate functions for different statistical procedures

# Define a function to check normality status of the data
#========================================================================================================================================
# STRATEGY: Global normality assessment across all data groups
# PURPOSE: Determine whether to use parametric or non-parametric tests
# LOGIC: If ANY group fails normality test, use non-parametric methods for ALL groups
# WHY: Ensures consistent statistical approach across entire analysis

check_normality <- function(shapiro_df) {
  # STRATEGY: Conservative approach to normality testing
  # PURPOSE: Assume normality unless proven otherwise
  flag_normal <- TRUE

  # LOOP THROUGH ALL GROUP RESULTS
  # STRATEGY: Break on first non-normal group for efficiency
  # PURPOSE: Single failure invalidates parametric assumptions
  for (i in seq_len(nrow(shapiro_df))) {
    if (shapiro_df$p[i] <= 0.05) {
      # CRITICAL DECISION POINT
      # STRATEGY: Strict alpha = 0.05 threshold for normality
      # PURPOSE: Conservative approach ensures valid statistical inference
      flag_normal <- FALSE
      break  # Exit immediately - no need to check remaining groups
    }
  }
  
  return(flag_normal)
}

# ===========================================
# SECTION 2: COMPACT LETTER DISPLAY FUNCTIONS
# ===========================================
# PURPOSE: Generate letter-based significance groupings for visualization
# STRATEGY: Separate functions for parametric vs non-parametric results
# BENEFIT: Clear visual indication of statistical differences in plots

# Generate CLD values for parametric data
#========================================================================================================================================
# STRATEGY: Process Tukey HSD results into compact letter display format
# PURPOSE: Convert pairwise comparison p-values into letter groupings
# INPUT: Tukey test results from rstatix::tukey_hsd()
# OUTPUT: Dataframe with groups and their significance letters

generate_cld_parametric <- function(tukey_df, var1_col, var2_col) {
  result <- tukey_df %>%
    dplyr::group_by(.data[[var1_col]]) %>%  # Use .data[[]] consistently
    dplyr::summarise(
      cld = list(multcompView::multcompLetters(
        setNames(p.adj, paste(group1, group2, sep = "-")),
        Letters = letters
      )$Letters),
      .groups = 'drop'
    ) %>%
    tidyr::unnest_longer(cld)  # Add tidyr:: prefix
  
  # CREATE CORRECTLY NAMED COLUMNS
  result[[var2_col]] <- names(result$cld)
  result$cld <- unname(result$cld)
  
  # SELECT FINAL COLUMNS
  result[c(var1_col, var2_col, "cld")]
}

# Generate CLD values for non-parametric data
#=======================================================================================================================================
# STRATEGY: Process Dunn test results into compact letter display format
# PURPOSE: Same as parametric version but for non-parametric post-hoc tests
# INPUT: Dunn test results from rstatix::dunn_test()
# OUTPUT: Dataframe with groups and their significance letters

generate_cld_nonparametric <- function(dunn_df, var1_col, var2_col) {
  result <- dunn_df %>%
    dplyr::group_by(.data[[var1_col]]) %>%  # Use .data[[]] consistently
    dplyr::summarise(
      cld = list(multcompView::multcompLetters(
        setNames(p.adj, paste(group1, group2, sep = "-")),
        Letters = letters
      )$Letters),
      .groups = 'drop'
    ) %>%
    tidyr::unnest_longer(cld)  # Add tidyr:: prefix
  
  # CREATE CORRECTLY NAMED COLUMNS
  result[[var2_col]] <- names(result$cld)
  result$cld <- unname(result$cld)
  
  # SELECT FINAL COLUMNS
  result[c(var1_col, var2_col, "cld")]
}

# Define a function to perform Dunn test
#========================================================================================================================================
# STRATEGY: Wrapper function for Dunn post-hoc testing
# PURPOSE: Standardized non-parametric multiple comparisons
# METHOD: Benjamini-Hochberg correction for family-wise error rate
# INPUT: Data frame, grouping variables, and measure column

test_dunn <- function(df_data, var1, var2, MEASURE_COL) {
  # DUNN TEST EXECUTION
  pval <- df_data %>%
    dplyr::group_by(.data[[var1]]) %>%  # Use .data[[]] consistently
    rstatix::dunn_test(
      formula = as.formula(paste(MEASURE_COL, "~", var2)),
      p.adjust.method = "BH"
    ) %>%
    as.data.frame()
  return(pval)
}

# Add the missing test_tukey function
test_tukey <- function(df_data, var1, var2, MEASURE_COL) {
  # TUKEY HSD TEST EXECUTION
  # STRATEGY: Group-wise Tukey testing with family-wise error control
  # PURPOSE: Pairwise comparisons with strict Type I error control
  pval <- df_data %>%
    dplyr::group_by(.data[[var1]]) %>%  # Use .data[[]] consistently
    rstatix::tukey_hsd(
      formula = as.formula(paste(MEASURE_COL, "~", var2))
    ) %>%
    as.data.frame()
  return(pval)
}

# ===========================================
# SECTION 3: DATA PROCESSING FUNCTIONS
# ===========================================
# PURPOSE: Handle FluorCam file processing and data preparation
# STRATEGY: Modular pipeline for file reading, cleaning, and transformation

# Function to extract the "area" from the file header
#========================================================================================================================================
# STRATEGY: Parse FluorCam file headers for metadata extraction
# PURPOSE: Extract area information for potential future use
# METHOD: Read first few lines and pattern match for "Area:" field
# NOTE: Currently not used in main pipeline but available for expansion

extract_area_from_header <- function(file_name) {
  # READ HEADER LINES ONLY
  # STRATEGY: Efficient reading - only first 5 lines needed
  # PURPOSE: Avoid loading entire file just for header info
  lines <- readLines(file_name, n = 5) # Adjust n if the header is longer
  
  # PATTERN MATCHING
  # STRATEGY: Use grep for flexible pattern matching
  # PURPOSE: Find line containing area information
  area_line <- grep("Area:", lines, value = TRUE)
  
  if (length(area_line) > 0) {
    # EXTRACT VALUE
    # STRATEGY: Remove everything before "Area:" and whitespace
    # PURPOSE: Clean extraction of numerical area value
    gsub(".*Area:\\s*", "", area_line)
  } else {
    # GRACEFUL FAILURE
    # STRATEGY: Return NA instead of error if area not found
    # PURPOSE: Allow processing to continue without area information
    return(NA)
  }
}

# Function to process data files
#========================================================================================================================================
# STRATEGY: Complete data processing pipeline for FluorCam files
# PURPOSE: Transform raw .TXT files into analysis-ready dataframe
# WORKFLOW: File discovery → cleaning → calculation → naming → merging
# INPUT: File pattern, directory path, and variable naming scheme
# OUTPUT: Combined dataframe ready for statistical analysis

process_data_files <- function(pattern, areas, var1, var2, var3, dirpath) {
  # FILE DISCOVERY
  # STRATEGY: Use pattern matching to find relevant files
  # PURPOSE: Flexible file selection based on user input
  files <- list.files(path = dirpath, pattern = pattern, full.names = TRUE)
  print(paste("Files found:", files))  # Debug output for troubleshooting
  
  # INNER FUNCTION: FILE CLEANING
  # STRATEGY: Nested function for single responsibility
  # PURPOSE: Remove FluorCam header lines and read data
  remove_first_two_lines <- function(file_name, area) {
    # READ ALL LINES
    # STRATEGY: Read entire file first for flexible processing
    lines <- readLines(file_name)
    
    # REMOVE EMPTY LINES
    # STRATEGY: Clean data by removing blank lines
    # PURPOSE: Prevent parsing errors from empty rows
    lines <- lines[lines != ""]
    
    # REMOVE HEADER LINES
    # STRATEGY: FluorCam files have 2-line headers that must be removed
    # PURPOSE: Leave only the data table for proper parsing
    if(length(lines) > 2){
      lines <- lines[-c(1,2)]  # Remove first two lines
    } else {
      # ERROR HANDLING
      # STRATEGY: Informative error message for insufficient data
      stop("Le fichier ne contient pas assez de lignes.")
    }
    
    # PARSE DATA TABLE
    # STRATEGY: Use read.table with tab separation (FluorCam standard)
    # PURPOSE: Convert cleaned text to structured dataframe
    data <- read.table(text = lines, sep = "\t", header = TRUE)
    return(data)
  }
  
  # INNER FUNCTION: Fv/Fm CALCULATION
  # STRATEGY: Automatic calculation of key fluorescence parameter
  # PURPOSE: Fv/Fm is standard measure of photosynthetic efficiency
  # FORMULA: Fv/Fm = (Fm - F0) / Fm = Fv / Fm
  compute_Fv_Fm <- function(df) {
    df$Fv_Fm <- df$Fv / df$Fm
    return(df)
  }
  
  # INNER FUNCTION: NAME COLUMN ADDITION
  # STRATEGY: Add filename as identifier column
  # PURPOSE: Track data source for later variable extraction
  add_name_column <- function(df, name) {
    df$Name <- name
    return(df)
  }
  
  # INNER FUNCTION: VARIABLE EXTRACTION
  # STRATEGY: Parse filename into separate variable columns
  # PURPOSE: Extract experimental variables from systematic naming
  # METHOD: Split on underscore separator (VAR1_VAR2_VAR3.TXT)
  divide_name <- function(df) {
    df <- tidyr::separate(
      data = df, 
      col = "Name", 
      into = c(var1, var2, var3),  # User-defined variable names
      sep = "_",                   # Underscore separator
      remove = TRUE                # Remove original Name column
    )
    return(df)
  }
  
  # MAIN PROCESSING PIPELINE
  # STRATEGY: Apply processing functions to all files
  
  # STEP 1: CLEAN ALL FILES
  # STRATEGY: lapply for efficient list processing
  # PURPOSE: Apply cleaning function to each file
  Liste <- lapply(files, remove_first_two_lines, area = "")
  # CREATE NAMED LIST
  # STRATEGY: Use filenames (without extension) as list names
  # PURPOSE: Maintain file identity through processing
  names(Liste) <- tools::file_path_sans_ext(basename(files))
  
  # STEP 2: TRANSPOSE DATA
  # REPLACE data.table::transpose with base R approach
  Liste <- lapply(Liste, function(df) {
    # Convert to matrix, transpose, then back to data.frame
    df_t <- as.data.frame(t(df[-1]))  # Exclude first column (X), transpose rest
    colnames(df_t) <- df$X  # Use X column as new column names
    return(df_t)
  })
  
  # STEP 3: CALCULATE Fv/Fm
  # STRATEGY: Apply calculation to all datasets
  # PURPOSE: Add derived parameter to all files
  Liste <- lapply(Liste, compute_Fv_Fm)
  
  # STEP 4: ADD FILENAME IDENTIFIERS
  # STRATEGY: Use names() to apply filename to each dataset
  # PURPOSE: Prepare for variable extraction
  Liste <- lapply(names(Liste), function(name) {
    add_name_column(Liste[[name]], name)
  })
  
  # STEP 5: EXTRACT VARIABLES FROM FILENAMES
  # STRATEGY: Parse systematic filenames into experimental variables
  # PURPOSE: Create grouping variables for statistical analysis
  Liste <- lapply(Liste, divide_name)
  
  # STEP 6: COMBINE ALL DATA
  # STRATEGY: Row-bind all processed datasets
  # PURPOSE: Create single analysis-ready dataframe
  df <- do.call(rbind, Liste)
  
  # NOTE: Area column addition commented out - not currently used
  #df <- cbind(Area = rep(areas, length.out = nrow(df)), df)
  
  return(df)
}

# ===========================================
# SECTION 4: VISUALIZATION FUNCTIONS
# ===========================================
# PURPOSE: Generate publication-quality plots with statistical annotations
# STRATEGY: Separate functions for different plot types with comprehensive options

# Function to plot Bar plot
#========================================================================================================================================
# STRATEGY: Comprehensive bar plot with automatic statistical testing
# PURPOSE: Publication-ready bar plots with significance testing and annotations
# FEATURES: Automatic normality testing, appropriate statistical tests, CLD annotations
# INPUT: Data, variables, measure column, ordering, and color options
# OUTPUT: ggplot object with statistical annotations and summary statistics

analyse_barplot <- function(data, var1, var2, measure_col, var1_order = NULL, var2_order = NULL, 
                           fill_color = "ivory1", line_color = "darkgrey", point_color = "darkgreen") {
  
  # Validate inputs
  if (!all(c(var1, var2, measure_col) %in% colnames(data))) {
    stop("One or more required columns missing from data")
  }
  
  # CONFIDENCE INTERVAL CALCULATION - Fix dplyr syntax
  conf_int <- data %>%
    dplyr::group_by(.data[[var1]], .data[[var2]]) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(.data[[measure_col]], na.rm = TRUE),
      sd = sd(.data[[measure_col]], na.rm = TRUE),
      se = sd / sqrt(n),
      .groups = 'drop'
    )
  
  # NORMALITY ASSESSMENT - Fix dplyr syntax
  shapiro_df <- data %>%
    dplyr::group_by(.data[[var1]], .data[[var2]]) %>%
    rstatix::shapiro_test(.data[[measure_col]])
  
  flag_normal <- check_normality(shapiro_df)
  
  if (flag_normal) {
    # PARAMETRIC ANALYSIS PATH
    
    # ANOVA TESTING - Fix dplyr syntax
    anova_results <- data %>%
      dplyr::group_by(.data[[var1]]) %>%
      rstatix::anova_test(formula = as.formula(paste(measure_col, "~", var2)))
    
    # TUKEY POST HOC TEST
    tukey_results <- test_tukey(data, var1, var2, measure_col)
    
    # COMPACT LETTER DISPLAY (PARAMETRIC)
    cld_table_parametric <- generate_cld_parametric(tukey_results, var1, var2)
    
    # MERGE AND PREPARE PLOTTING DATA
    df2 <- merge(conf_int, cld_table_parametric, by.x = c(var2, var1), by.y = c(var2, var1))
    
    # PRESERVE FACTOR ORDERING
    if(!is.null(var1_order)) {
      df2[[var1]] <- factor(df2[[var1]], levels = var1_order)
    }
    if(!is.null(var2_order)) {
      df2[[var2]] <- factor(df2[[var2]], levels = var2_order)
    }
    
    # PLOT CONSTRUCTION (PARAMETRIC) - Fix ggplot syntax
    p <- df2 %>%
      ggplot2::ggplot(ggplot2::aes(x = .data[[var2]], y = mean, fill = .data[[var2]])) +
      ggplot2::geom_col(color = line_color, 
               fill = fill_color, 
               alpha = 0.7,
               width = 0.8) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), 
                   color = line_color, 
                   width = 0.4, 
                   size = 0.8) +
      ggplot2::geom_point(data = data, ggplot2::aes(y = .data[[measure_col]]), 
                color = point_color, 
                alpha = 0.6, 
                size = 2, 
                position = ggplot2::position_jitter(width = 0.2)) +
      ggplot2::geom_text(ggplot2::aes(y = mean + se + 0.05 * max(mean + se, na.rm = TRUE), 
                   label = cld), 
               color = "black", 
               size = 4, 
               fontface = "bold") +
      ggplot2::facet_wrap(as.formula(paste("~", var1)), scales = "free") +
      ggplot2::labs(title = paste("Analysis of", measure_col, "by", var2, "and", var1),
           subtitle = "Parametric analysis (ANOVA + Tukey HSD)",
           x = var2, 
           y = measure_col) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.text = ggplot2::element_text(size = 12, face = "bold"),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "grey60")
      )
    
    # RETURN PARAMETRIC RESULTS
    return(list(
      plot = p,
      summary = conf_int,
      anova = anova_results,
      tukey = tukey_results,
      cld = cld_table_parametric,
      test_type = "parametric"
    ))
    
  } else {
    # NON-PARAMETRIC ANALYSIS PATH - Fix dplyr syntax
    kruskal_results <- data %>%
      dplyr::group_by(.data[[var1]]) %>%
      rstatix::kruskal_test(formula = as.formula(paste(measure_col, "~", var2)))
    
    # DUNN POST HOC TEST
    dunn_results <- test_dunn(data, var1, var2, measure_col)
    
    # COMPACT LETTER DISPLAY (NON-PARAMETRIC)
    cld_table_nonparametric <- generate_cld_nonparametric(dunn_results, var1, var2)
    
    # MERGE AND PREPARE PLOTTING DATA
    df2 <- merge(conf_int, cld_table_nonparametric, by.x = c(var2, var1), by.y = c(var2, var1))
    
    # PRESERVE FACTOR ORDERING
    if(!is.null(var1_order)) {
      df2[[var1]] <- factor(df2[[var1]], levels = var1_order)
    }
    if(!is.null(var2_order)) {
      df2[[var2]] <- factor(df2[[var2]], levels = var2_order)
    }
    
    # PLOT CONSTRUCTION (NON-PARAMETRIC) - Fix ggplot syntax
    p <- df2 %>%
      ggplot2::ggplot(ggplot2::aes(x = .data[[var2]], y = mean, fill = .data[[var2]])) +
      ggplot2::geom_col(color = line_color, 
               fill = fill_color, 
               alpha = 0.7,
               width = 0.8) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), 
                   color = line_color, 
                   width = 0.4, 
                   size = 0.8) +
      ggplot2::geom_point(data = data, ggplot2::aes(y = .data[[measure_col]]), 
                color = point_color, 
                alpha = 0.6, 
                size = 2, 
                position = ggplot2::position_jitter(width = 0.2)) +
      ggplot2::geom_text(ggplot2::aes(y = mean + se + 0.05 * max(mean + se, na.rm = TRUE), 
                   label = cld), 
               color = "black", 
               size = 4, 
               fontface = "bold") +
      ggplot2::facet_wrap(as.formula(paste("~", var1)), scales = "free") +
      ggplot2::labs(title = paste("Analysis of", measure_col, "by", var2, "and", var1),
           subtitle = "Non-parametric analysis (Kruskal-Wallis + Dunn test)",
           x = var2, 
           y = measure_col) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.text = ggplot2::element_text(size = 12, face = "bold"),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "grey60")
      )
    
    # RETURN NON-PARAMETRIC RESULTS
    return(list(
      plot = p,
      summary = conf_int,
      kruskal = kruskal_results,
      dunn = dunn_results,
      cld = cld_table_nonparametric,
      test_type = "non-parametric"
    ))
  }
}


#========================================================================================================================================
# STRATEGY: Advanced curve analysis using quantile Generalized Additive Models (qGAM)
# PURPOSE: Robust analysis of time-course or dose-response data with statistical comparisons
# METHOD: qGAM for non-linear trend fitting with confidence intervals
# FEATURES: Control group comparisons, median regression, dynamic axis labeling
# INPUT: Long-format data with time points, grouping variables, and control group
# OUTPUT: ggplot with smooth curves, confidence bands, and statistical annotations

analyse_curve <- function(df, col_vector, 
                          parameter_col, 
                          time_col, 
                          grouping_col, 
                          facet_col,
                          control_group,
                          k = 5,  # Smoothing parameter for GAM
                          user_params = list()) {
  
# ===========================================
# NOTE: All required packages loaded via global.R
# ===========================================
  
  # ===========================================
  # AXIS LABEL PREPARATION SECTION
  # ===========================================
  # STRATEGY: Dynamic axis labeling based on user parameters
  # PURPOSE: Informative, context-specific axis labels
  
  # X-AXIS LABEL WITH UNITS
  # STRATEGY: Include user-specified time units in axis label
  # PURPOSE: Clear indication of time scale being analyzed
  x_axis_label <- if (!is.null(user_params$unit)) {
    paste("Time (", user_params$unit, ")", sep = "")
  } else {
    "Time"  # Fallback if no unit specified
  }
  
  # Y-AXIS LABEL FROM PARAMETER NAME
  # STRATEGY: Extract root parameter name from selected columns
  # PURPOSE: Clean parameter name without time point suffixes
  y_axis_label <- if (!is.null(user_params$selected_params) && length(user_params$selected_params) > 0) {
    # PARAMETER NAME EXTRACTION
    # STRATEGY: Remove time point suffixes (_L1, _L2, etc.) from parameter names
    # PURPOSE: Get clean parameter name (e.g., "Fq" from "Fq_L1", "Fq_L2")
    param_name <- gsub("_.*", "", user_params$selected_params[1])
    param_name
  } else {
    parameter_col  # Fallback to column name
  }
  
  # ===========================================
  # INNER FUNCTION: DATA VALIDATION
  # ===========================================
  # STRATEGY: Comprehensive data validation and preparation
  # PURPOSE: Ensure data quality before expensive modeling
  
  validate_and_prepare_data <- function(df, parameter_col, time_col, grouping_col, facet_col) {
    # COLUMN EXISTENCE CHECK
    # STRATEGY: Verify all required columns are present
    # PURPOSE: Prevent cryptic errors during analysis
    required_cols <- c(parameter_col, time_col, grouping_col, facet_col)
    missing_cols <- setdiff(required_cols, colnames(df))
    if (length(missing_cols) > 0) {
      stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # DATA TYPE CONVERSION
    # STRATEGY: Ensure appropriate data types for modeling
    # PURPOSE: Prevent type-related errors in qGAM fitting
    df[[grouping_col]] <- as.factor(df[[grouping_col]])
    df[[facet_col]] <- as.factor(df[[facet_col]])
    df[[time_col]] <- suppressWarnings(as.numeric(as.character(df[[time_col]])))
    df[[parameter_col]] <- suppressWarnings(as.numeric(df[[parameter_col]]))
    
    # MISSING VALUE REMOVAL
    # STRATEGY: Complete case analysis
    # PURPOSE: qGAM requires complete data for fitting
    df <- df %>% 
      filter(!is.na(.data[[grouping_col]]) & 
               !is.na(.data[[facet_col]]) &
               !is.na(.data[[time_col]]) & 
               !is.na(.data[[parameter_col]]))
    
    # FINAL DATA CHECK
    # STRATEGY: Ensure data remains after cleaning
    # PURPOSE: Prevent analysis with empty datasets
    if (nrow(df) == 0) {
      stop("No valid data remaining after cleaning")
    }
    
    return(df)
  }
  
  # ===========================================
  # INNER FUNCTION: qGAM MODEL FITTING
  # ===========================================
  # STRATEGY: Group-wise qGAM fitting with prediction generation
  # PURPOSE: Generate smooth trend lines with confidence intervals
  
  fit_qgam_models <- function(df, grouping_col, facet_col, time_col, parameter_col, k) {
    # TIME GRID CREATION
    # STRATEGY: High-resolution time grid for smooth predictions
    # PURPOSE: Create smooth curves regardless of original time point density
    time_range <- range(df[[time_col]], na.rm = TRUE)
    time_grid <- seq(time_range[1], time_range[2], length.out = 200)  # 200 points for smoothness
    
    # GROUP-WISE MODEL FITTING
    # STRATEGY: Separate qGAM model for each group × facet combination
    # PURPOSE: Allow different curve shapes for different conditions
    qgam_preds <- df %>%
      group_by(!!sym(facet_col), !!sym(grouping_col)) %>%
      group_modify(~{
        tryCatch({
          # qGAM MODEL FITTING
          # STRATEGY: Quantile regression at median (qu = 0.5)
          # PURPOSE: Robust to outliers and non-normal distributions
          mod <- qgam::qgam(
            as.formula(paste(parameter_col, "~ s(", time_col, ", k=", k, ")")),
            data = .x,
            qu = 0.5  # Median regression - robust central tendency
          )
          
          # PREDICTION DATA PREPARATION
          # STRATEGY: Create new data frame for prediction
          # PURPOSE: Generate predictions on fine time grid
          newdat <- data.frame(setNames(list(time_grid), time_col))
          newdat[[grouping_col]] <- unique(.x[[grouping_col]])[1]
          newdat[[facet_col]] <- unique(.x[[facet_col]])[1]
          
          # PREDICTION WITH CONFIDENCE INTERVALS
          # STRATEGY: Standard error-based confidence intervals
          # PURPOSE: Show uncertainty in fitted curves
          preds <- predict(mod, newdata = newdat, se.fit = TRUE)
          newdat$fit <- preds$fit
          newdat$lwr <- preds$fit - 1.96 * preds$se.fit  # 95% CI lower bound
          newdat$upr <- preds$fit + 1.96 * preds$se.fit  # 95% CI upper bound
          
          return(newdat)
        }, error = function(e) {
          # GRACEFUL ERROR HANDLING
          # STRATEGY: Warning instead of stopping entire analysis
          # PURPOSE: Allow partial results when some groups fail
          warning(paste("Failed to fit qGAM model for", 
                        unique(.x[[facet_col]])[1], "-", unique(.x[[grouping_col]])[1], 
                        ":", e$message))
          return(data.frame())  # Return empty data frame
        })
      }) %>%
      ungroup()
    
    return(qgam_preds)
  }
  
  # ===========================================
  # INNER FUNCTION: STATISTICAL TESTING
  # ===========================================
  # STRATEGY: Group-vs-control comparisons using qGAM
  # PURPOSE: Statistical inference about group differences
  
  perform_statistical_tests <- function(df, grouping_col, facet_col, time_col, parameter_col, control_group) {
    # CONTROL GROUP VALIDATION
    # STRATEGY: Check if control group exists and is valid
    # PURPOSE: Prevent errors when control group is missing
    if (is.null(control_group) || !control_group %in% df[[grouping_col]]) {
      return(data.frame())  # Return empty if no valid control
    }
    
    # FACET-WISE STATISTICAL TESTING
    # STRATEGY: Separate statistical tests for each facet panel
    # PURPOSE: Independent statistical inference for each experimental condition
    results <- df %>%
      group_by(!!sym(facet_col)) %>%
      group_modify(~{
        # IDENTIFY COMPARISON GROUPS
        # STRATEGY: Compare all non-control groups to control
        # PURPOSE: Multiple treatment vs. control comparisons
        groups_to_compare <- setdiff(levels(factor(.x[[grouping_col]])), control_group)
        
        # RESULT DATAFRAME INITIALIZATION
        # STRATEGY: Pre-allocate results dataframe
        # PURPOSE: Consistent structure even if some tests fail
        test_results <- data.frame(
          Group = groups_to_compare,
          p.value = NA_real_,
          stringsAsFactors = FALSE
        )
        names(test_results)[1] <- grouping_col
        
        # GROUP-WISE TESTING LOOP
        # STRATEGY: Test each group against control individually
        # PURPOSE: Separate p-value for each comparison
        for (i in seq_along(groups_to_compare)) {
          group <- groups_to_compare[i]
          
          # SUBSET DATA FOR COMPARISON
          # STRATEGY: Only include control and current test group
          # PURPOSE: Two-group comparison for clear interpretation
          df_sub <- .x %>% 
            filter(.data[[grouping_col]] %in% c(control_group, group)) %>%
            mutate(temp_col = droplevels(factor(.data[[grouping_col]])))
          
          # Assign the new column with the correct name
          df_sub[[grouping_col]] <- df_sub$temp_col
          df_sub$temp_col <- NULL
          
          tryCatch({
            # COMPARATIVE qGAM MODEL
            # STRATEGY: Model with group-specific smooths
            # PURPOSE: Test if curves differ significantly between groups
            m1 <- qgam::qgam(
              as.formula(paste(parameter_col, "~ s(", time_col, ", by = ", grouping_col, ", k=5) +", grouping_col)),
              data = df_sub,
              qu = 0.5  # Median regression
            )
            
            # EXTRACT P-VALUE
            # STRATEGY: Use smooth term p-value for group difference
            # PURPOSE: Statistical significance of group × time interaction
            s <- summary(m1)
            if (length(s$s.pv) >= 2) {
              test_results$p.value[i] <- s$s.pv[2]  # Second smooth term p-value
            }
          }, error = function(e) {
            # ERROR HANDLING FOR INDIVIDUAL TESTS
            # STRATEGY: Warning for failed tests, continue with others
            # PURPOSE: Partial results better than complete failure
            warning(paste("Statistical test failed for", group, "in", unique(.x[[facet_col]])[1], ":", e$message))
          })
        }
        
        return(test_results)
      }) %>%
      ungroup()
    
    return(results)
  }
  
  # ===========================================
  # MAIN EXECUTION SECTION
  # ===========================================
  # STRATEGY: Sequential execution of analysis pipeline
  # PURPOSE: Orchestrate complete curve analysis workflow
  
  tryCatch({
    # STEP 1: DATA VALIDATION AND PREPARATION
    # STRATEGY: Clean and validate data before expensive computations
    # PURPOSE: Ensure data quality and prevent downstream errors
    df_clean <- validate_and_prepare_data(df, parameter_col, time_col, grouping_col, facet_col)
    
    # STEP 2: qGAM MODEL FITTING
    # STRATEGY: Fit smooth curves to each group
    # PURPOSE: Generate trend lines and confidence intervals
    qgam_preds <- fit_qgam_models(df_clean, grouping_col, facet_col, time_col, parameter_col, k)
    
    # STEP 3: STATISTICAL TESTING
    # STRATEGY: Test for significant differences from control
    # PURPOSE: Statistical inference about treatment effects
    stat_results <- perform_statistical_tests(df_clean, grouping_col, facet_col, time_col, parameter_col, control_group)
    
    # STEP 4: MEDIAN POINT CALCULATION
    # STRATEGY: Calculate median values at each time point for overlay
    # PURPOSE: Show actual data points on smooth curves
    median_points <- df_clean %>%
      group_by(!!sym(facet_col), !!sym(grouping_col), !!sym(time_col)) %>%
      summarise(median_value = median(.data[[parameter_col]], na.rm = TRUE), .groups = "drop")
    
    # STEP 5: COLOR MAPPING PREPARATION
    # STRATEGY: Create consistent color mapping for groups
    # PURPOSE: Coherent color scheme across plot elements
    unique_groups <- unique(df_clean[[grouping_col]])
    if (length(col_vector) >= length(unique_groups)) {
      # USE PROVIDED COLORS
      # STRATEGY: Use user-specified colors when sufficient
      color_mapping <- setNames(col_vector[1:length(unique_groups)], unique_groups)
    } else {
      # FALLBACK COLOR SCHEME
      # STRATEGY: Use RColorBrewer for professional color palette
      # PURPOSE: Attractive default colors when user doesn't specify enough
      color_mapping <- setNames(RColorBrewer::brewer.pal(max(3, length(unique_groups)), "Set1")[1:length(unique_groups)], unique_groups)
    }
    
    # ===========================================
    # PLOT CONSTRUCTION SECTION
    # ===========================================
    # STRATEGY: Layered ggplot with confidence bands, curves, and points
    # PURPOSE: Comprehensive visualization of curve analysis results
    
    p <- ggplot() +
      # CONFIDENCE RIBBON LAYER
      # STRATEGY: Show uncertainty bounds first (background layer)
      # PURPOSE: Visual indication of model uncertainty
      geom_ribbon(
        data = qgam_preds,
        aes(x = !!sym(time_col), ymin = lwr, ymax = upr, fill = !!sym(grouping_col)),
        alpha = 0.3,      # Semi-transparent for overlay effect
        linetype = 0      # No border lines on ribbon
      ) +
      
      # SMOOTH CURVE LAYER
      # STRATEGY: Main trend lines from qGAM predictions
      # PURPOSE: Show fitted curves for each group
      geom_line(
        data = qgam_preds,
        aes(x = !!sym(time_col), y = fit, color = !!sym(grouping_col)),
        size = 1
      ) +
      
      # MEDIAN POINTS OVERLAY
      # STRATEGY: Show actual data summaries on fitted curves
      # PURPOSE: Connection between model and observed data
      geom_point(
        data = median_points,
        aes(x = !!sym(time_col), y = median_value, color = !!sym(grouping_col)),
        size = 1.5, alpha = 0.7
      ) +
      
      # COLOR SCALES
      # STRATEGY: Apply consistent color mapping to all elements
      # PURPOSE: Coherent visual appearance
      scale_fill_manual(values = color_mapping) +
      scale_colour_manual(values = color_mapping) +
      
      # FACETING
      # STRATEGY: Separate panels for different experimental conditions
      # PURPOSE: Clear separation of different factor levels
      facet_wrap(as.formula(paste("~", facet_col))) +
      
      # THEME APPLICATION
      # STRATEGY: Clean, professional appearance
      # PURPOSE: Publication-ready styling
      theme_classic() +
      theme(
        legend.title = element_blank(),  # Remove legend title for cleaner look
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "plain", size = 10, color = "black", hjust = 0.5)
      ) +
      
      # AXIS LABELS
      # STRATEGY: Use dynamically generated labels
      # PURPOSE: Informative, context-specific labels
      labs(x = x_axis_label, y = y_axis_label)
  
    # ===========================================
    # STATISTICAL ANNOTATION SECTION
    # ===========================================
    # STRATEGY: Add p-value annotations when significant differences exist
    # PURPOSE: Clear indication of statistical significance
    
    if (nrow(stat_results) > 0 && !is.null(control_group)) {
      # ANNOTATION DATAFRAME PREPARATION
      # STRATEGY: Prepare text annotations with p-values
      # PURPOSE: Show statistical significance on plot
      annotation_df <- stat_results %>%
        filter(!is.na(p.value)) %>%
        mutate(
          # P-VALUE FORMATTING
          # STRATEGY: Show p-values only when significant
          # PURPOSE: Avoid cluttering plot with non-significant results
          label = ifelse(p.value < 0.05,
                         paste0("p = ", format.pval(p.value, digits = 2, eps = .001)),
                         ""),
          # ANNOTATION POSITIONING
          # STRATEGY: Position above highest data points
          # PURPOSE: Visible but non-interfering placement
          y = max(df_clean[[parameter_col]], na.rm = TRUE) * 1.05,
          x = median(df_clean[[time_col]], na.rm = TRUE)
        ) %>%
        filter(label != "")  # Only keep significant results
      
      # ADD ANNOTATIONS TO PLOT
      # STRATEGY: Overlay text annotations for significant comparisons
      # PURPOSE: Statistical information integrated into visualization
      if (nrow(annotation_df) > 0) {
        p <- p + geom_text(
          data = annotation_df,
          aes(x = x, y = y, label = label),
          size = 3, fontface = "bold",
          inherit.aes = FALSE
        )
      }
    }
    
    # ===========================================
    # RETURN RESULTS SECTION
    # ===========================================
    # STRATEGY: Return comprehensive results package
    # PURPOSE: Enable export and further analysis
    
    return(list(
      plot = p,                          # Main visualization
      qgam_predictions = qgam_preds,     # Model predictions for export
      statistical_results = stat_results, # Statistical test results
      median_points = median_points      # Summary data points
    ))
    
  }, error = function(e) {
    # GLOBAL ERROR HANDLING
    # STRATEGY: Informative error message for debugging
    # PURPOSE: Help users identify and fix problems
    stop(paste("Error in qGAM curve analysis:", e$message))
  })
}