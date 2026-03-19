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
# SECTION 1: DATA PROCESSING FUNCTIONS
# ===========================================
# PURPOSE: Handle FluorCam file processing and data preparation
# STRATEGY: Modular pipeline for file reading, cleaning, and transformation

# Function to process data files
#========================================================================================================================================
# STRATEGY: Complete data processing pipeline for FluorCam files
# PURPOSE: Transform raw .TXT files into analysis-ready dataframe
# WORKFLOW: File discovery → cleaning → calculation → naming → merging
# INPUT: File pattern, directory path, and variable naming scheme
# OUTPUT: Combined dataframe ready for statistical analysis

process_data_files <- function(pattern, var_names, dirpath) {
  
  # VALIDATION: Check inputs
  # STRATEGY: Ensure all required parameters are valid
  # PURPOSE: Prevent errors from invalid parameters
  if (is.null(pattern) || pattern == "") {
    stop("Pattern cannot be empty")
  }
  
  if (is.null(var_names) || length(var_names) == 0) {
    stop("Variable names must be provided")
  }
  
  if (!dir.exists(dirpath)) {
    stop("Directory does not exist: ", dirpath)
  }
  
  # GET NUMBER OF VARIABLES
  # STRATEGY: Dynamic variable count from provided names
  # PURPOSE: Support flexible number of variables
  num_vars <- length(var_names)
  
  # FILE DISCOVERY
  # STRATEGY: Use pattern matching to find relevant files
  # PURPOSE: Flexible file selection based on user input
  files <- list.files(path = dirpath, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No files found matching pattern: ", pattern)
    return(NULL)
  }
  
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
      stop("The file does not contain enough lines.")
    }

    # PARSE DATA TABLE
    # STRATEGY: Use read.table with tab separation (FluorCam standard)
    # PURPOSE: Convert cleaned text to structured dataframe
    data <- read.table(text = lines, sep = "\t", header = TRUE)
    return(data)
  }
  
  # INNER FUNCTION: Fv/Fm CALCULATION (CONDITIONAL WITH FALLBACK)
  # STRATEGY: Calculate Fv/Fm with multiple methods based on available columns
  # PURPOSE: Handle different FluorCam export formats gracefully
  # METHOD 1: Direct Fv/Fm calculation if Fv exists
  # METHOD 2: Calculate from Fm and Fo if Fv is absent: (Fm - Fo) / Fm
  # METHOD 3: No calculation if neither method is possible
  compute_Fv_Fm <- function(df) {
    # CHECK METHOD 1: Direct Fv/Fm calculation
    # STRATEGY: Preferred method when Fv is directly measured
    # PURPOSE: Use most accurate measurement when available
    if ("Fv" %in% colnames(df) && "Fm" %in% colnames(df)) {
      df$Fv_Fm <- df$Fv / df$Fm
      return(df)
    }
    
    # CHECK METHOD 2: Calculate Fv from Fm and Fo
    # STRATEGY: Fallback calculation using Fv = Fm - Fo
    # PURPOSE: Handle FluorCam exports that don't include Fv directly
    # FORMULA: Fv/Fm = (Fm - Fo) / Fm
    if ("Fm" %in% colnames(df) && "Fo" %in% colnames(df)) {
      df$Fv_Fm <- (df$Fm - df$Fo) / df$Fm
      return(df)
    }
    
    # NO CALCULATION POSSIBLE
    # STRATEGY: Return dataframe unchanged
    # PURPOSE: Allow analysis to continue without Fv/Fm when not calculable
    return(df)
  }
  
  # INNER FUNCTION: NAME COLUMN ADDITION
  # STRATEGY: Add filename as identifier column
  # PURPOSE: Track data source for later variable extraction
  add_name_column <- function(df, name) {
    df$Name <- name
    return(df)
  }
  
  # INNER FUNCTION: VARIABLE EXTRACTION (DYNAMIC)
  # STRATEGY: Parse filename into separate variable columns based on num_vars
  # PURPOSE: Extract experimental variables from systematic naming
  # METHOD: Split on underscore separator (VAR1_VAR2_..._VARN.TXT)
  divide_name <- function(df, var_names) {
    # Split Name column by underscore
    name_parts <- strsplit(df$Name, "_")
    
    # FIX: Convertir en matrice pour extraction plus robuste
    # STRATEGY: Ensure we get atomic vectors, not list columns
    # PURPOSE: Prevent "$ operator invalid for atomic vectors" error
    max_parts <- max(sapply(name_parts, length))
    
    # Check if we have the correct number of parts
    if (any(sapply(name_parts, length) != length(var_names))) {
      warning("Some filenames do not match the expected number of variables")
    }
    
    # Create new columns for each variable - FIXED VERSION
    for (i in seq_along(var_names)) {
      # FIX: Extract as CHARACTER VECTOR (not list)
      # STRATEGY: Use sapply with explicit character conversion
      # PURPOSE: Ensure atomic vector type for all columns
      df[[var_names[i]]] <- sapply(name_parts, function(x) {
        if (length(x) >= i) {
          as.character(x[i])  # Force to character
        } else {
          NA_character_       # Use typed NA
        }
      }, USE.NAMES = FALSE)  # Don't create names attribute
      
      # ADDITIONAL FIX: Ensure atomic vector
      # STRATEGY: Force vector type and remove any list structure
      # PURPOSE: Guarantee compatibility with dplyr operations
      df[[var_names[i]]] <- as.vector(df[[var_names[i]]])
    }
    
    # Remove original Name column
    df$Name <- NULL
    
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
  # STRATEGY: FluorCam data comes with parameters as rows, need columns
  # PURPOSE: Transform from parameter-per-row to parameter-per-column
  # METHOD: data.table::transpose with X column as names
  Liste <- lapply(Liste, data.table::transpose, make.names = "X")

  # STEP 3: CALCULATE Fv/Fm (CONDITIONAL)
  # STRATEGY: Apply calculation only if Fv exists
  # PURPOSE: Add derived parameter when possible, skip if not
  Liste <- lapply(Liste, compute_Fv_Fm)
  
  # STEP 4: ADD FILENAME IDENTIFIERS
  # STRATEGY: Use names() to apply filename to each dataset
  # PURPOSE: Prepare for variable extraction
  Liste <- lapply(names(Liste), function(name) {
    add_name_column(Liste[[name]], name)
  })
  
  # STEP 5: EXTRACT VARIABLES FROM FILENAMES (DYNAMIC)
  # STRATEGY: Parse systematic filenames into experimental variables
  # PURPOSE: Create grouping variables for statistical analysis
  Liste <- lapply(Liste, divide_name, var_names = var_names)
  
  # STEP 6: COMBINE ALL DATA
  # STRATEGY: Row-bind all processed datasets
  # PURPOSE: Create single analysis-ready dataframe
  df <- do.call(rbind, Liste)
  
  # STEP 7: REORDER COLUMNS
  # STRATEGY: Put variable columns first, then measurements
  # PURPOSE: Logical column arrangement for analysis
  measurement_cols <- setdiff(names(df), var_names)
  df <- df[, c(var_names, measurement_cols)]

  return(df)
}

# ===========================================
# SECTION 2: STATISTICAL TESTING FUNCTIONS
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
    current_p <- shapiro_df$p[i]

    if (is.na(current_p) || current_p <= 0.05) {
      # CRITICAL DECISION POINT
      # STRATEGY: Treat missing p-values conservatively as assumption failure
      # PURPOSE: Avoid invalid parametric inference when normality could not be assessed
      flag_normal <- FALSE
      break  # Exit immediately - no need to check remaining groups
    }
  }

  return(flag_normal)
}

# EMMEANS POST-HOC HELPER
#========================================================================================================================================
# STRATEGY: Run pairwise post-hoc contrasts on fitted linear models
# PURPOSE: Provide post-hoc comparisons for multi-factor ANOVA workflows

run_emmeans_pairwise <- function(model, target_factor, by_factors = NULL, adjust = "tukey") {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    return(NULL)
  }

  target_expr <- paste0("`", target_factor, "`")
  if (!is.null(by_factors) && length(by_factors) > 0) {
    by_expr <- paste(sprintf("`%s`", by_factors), collapse = ":")
    spec <- stats::as.formula(paste0("~ ", target_expr, " | ", by_expr))
    conditioning <- paste(by_factors, collapse = ",")
  } else {
    spec <- stats::as.formula(paste0("~ ", target_expr))
    conditioning <- "none"
  }

  emm <- emmeans::emmeans(model, specs = spec)
  pairwise_df <- as.data.frame(emmeans::contrast(emm, method = "pairwise", adjust = adjust))
  pairwise_df$target_factor <- target_factor
  pairwise_df$conditioned_on <- conditioning
  pairwise_df
}

run_art_emmeans_pairwise <- function(art_model, effect_term, target_factor, by_factors = NULL, adjust = "holm") {
  if (!requireNamespace("ARTool", quietly = TRUE)) {
    return(NULL)
  }

  term_model <- tryCatch(ARTool::artlm(art_model, effect_term), error = function(e) NULL)
  if (is.null(term_model)) {
    return(NULL)
  }

  run_emmeans_pairwise(
    model = term_model,
    target_factor = target_factor,
    by_factors = by_factors,
    adjust = adjust
  )
}

format_art_anova_table <- function(art_model) {
  anova_df <- as.data.frame(stats::anova(art_model))
  if (nrow(anova_df) == 0) {
    return(anova_df)
  }

  anova_df$Effect <- rownames(anova_df)
  rownames(anova_df) <- NULL
  anova_df <- dplyr::relocate(anova_df, Effect)
  class(anova_df) <- "data.frame"
  anova_df
}

summarise_factorial_response <- function(data, group_cols, measure_col, flag_normal = TRUE) {
  if (flag_normal) {
    return(
      data %>%
        dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
        dplyr::summarise(
          N = sum(!is.na(.data[[measure_col]])),
          mean_value = mean(.data[[measure_col]], na.rm = TRUE),
          sd_value = stats::sd(.data[[measure_col]], na.rm = TRUE),
          se = ifelse(N > 0, sd_value / sqrt(N), NA_real_),
          ci_lower = mean_value - se * stats::qt(0.975, df = pmax(N - 1, 1)),
          ci_upper = mean_value + se * stats::qt(0.975, df = pmax(N - 1, 1)),
          .groups = "drop"
        )
    )
  }

  data %>%
    dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
    dplyr::summarise(
      N = sum(!is.na(.data[[measure_col]])),
      mean_value = stats::median(.data[[measure_col]], na.rm = TRUE),
      sd_value = NA_real_,
      se = NA_real_,
      ci_lower = stats::quantile(.data[[measure_col]], 0.025, na.rm = TRUE),
      ci_upper = stats::quantile(.data[[measure_col]], 0.975, na.rm = TRUE),
      .groups = "drop"
    )
}

evaluate_factorial_decision <- function(
  data,
  factors,
  measure_col,
  flag_normal,
  decision_mode = "conservative",
  min_n_per_cell = 5,
  max_imbalance_ratio = 2,
  alpha_levene = 0.05
) {
  mode_clean <- if (is.null(decision_mode) || !(decision_mode %in% c("conservative", "robust_parametric"))) {
    "conservative"
  } else {
    decision_mode
  }

  keep <- !is.na(data[[measure_col]])
  for (fac in factors) {
    keep <- keep & !is.na(data[[fac]])
  }
  valid_df <- data[keep, c(factors, measure_col), drop = FALSE]

  if (nrow(valid_df) == 0) {
    return(list(
      analysis_method = "art",
      decision_mode = mode_clean,
      decision_reason = "No complete rows available for robust parametric checks; used ART.",
      diagnostics = list(normality = flag_normal, levene_p = NA_real_, min_cell_n = NA_integer_, max_cell_n = NA_integer_, imbalance_ratio = NA_real_)
    ))
  }

  counts_df <- valid_df %>%
    dplyr::count(dplyr::across(all_of(factors)), name = "n")

  min_cell_n <- min(counts_df$n)
  max_cell_n <- max(counts_df$n)
  imbalance_ratio <- if (min_cell_n > 0) max_cell_n / min_cell_n else Inf

  interaction_expr <- paste(sprintf("`%s`", factors), collapse = " * ")
  lev_formula <- stats::as.formula(paste0("`", measure_col, "` ~ ", interaction_expr))

  levene_p <- tryCatch({
    lev <- rstatix::levene_test(valid_df, lev_formula)
    as.numeric(lev$p[1])
  }, error = function(e) {
    NA_real_
  })

  balance_ok <- (min_cell_n >= min_n_per_cell) && is.finite(imbalance_ratio) && (imbalance_ratio <= max_imbalance_ratio)
  variance_ok <- !is.na(levene_p) && (levene_p > alpha_levene)

  if (mode_clean == "conservative") {
    method <- if (isTRUE(flag_normal)) "anova" else "art"
    reason <- if (method == "anova") {
      "Conservative strategy: all tested groups passed Shapiro-Wilk."
    } else {
      "Conservative strategy: at least one group failed Shapiro-Wilk (or returned NA), so ART was used."
    }
  } else {
    if (isTRUE(flag_normal)) {
      method <- "anova"
      reason <- "Robust strategy: all tested groups passed Shapiro-Wilk; ANOVA retained."
    } else if (variance_ok && balance_ok) {
      method <- "anova"
      reason <- paste0(
        "Robust strategy: normality was not fully met, but ANOVA retained because Levene p > ",
        alpha_levene,
        " and design balance criteria were met (min n per cell >= ", min_n_per_cell,
        ", imbalance ratio <= ", max_imbalance_ratio, ")."
      )
    } else {
      method <- "art"
      reason <- "Robust strategy: assumptions for robust parametric ANOVA were not met, so ART was used."
    }
  }

  list(
    analysis_method = method,
    decision_mode = mode_clean,
    decision_reason = reason,
    diagnostics = list(
      normality = flag_normal,
      levene_p = levene_p,
      min_cell_n = min_cell_n,
      max_cell_n = max_cell_n,
      imbalance_ratio = imbalance_ratio
    )
  )
}

# Convert emmeans pairwise contrasts to CLD letters
#========================================================================================================================================
# STRATEGY: Reuse multcompView letter generation from pairwise p-values
# PURPOSE: Display compact significance groups on multi-factor bar plots

build_cld_from_emmeans <- function(posthoc_df, target_factor, by_factors = NULL) {
  if (is.null(posthoc_df) || nrow(posthoc_df) == 0) {
    return(NULL)
  }

  if (!all(c("contrast", "p.value", "target_factor") %in% names(posthoc_df))) {
    return(NULL)
  }

  target_df <- posthoc_df[as.character(posthoc_df$target_factor) == target_factor, , drop = FALSE]
  if (nrow(target_df) == 0) {
    return(NULL)
  }

  contrast_split <- strsplit(as.character(target_df$contrast), " - ", fixed = TRUE)
  valid_pairs <- vapply(contrast_split, length, integer(1)) == 2
  target_df <- target_df[valid_pairs, , drop = FALSE]
  contrast_split <- contrast_split[valid_pairs]

  if (nrow(target_df) == 0) {
    return(NULL)
  }

  target_df$group1 <- vapply(contrast_split, `[[`, character(1), 1)
  target_df$group2 <- vapply(contrast_split, `[[`, character(1), 2)

  group_cols <- by_factors[by_factors %in% names(target_df)]

  if (length(group_cols) == 0) {
    letters_result <- generate_cld_letters(target_df$p.value, target_df$group1, target_df$group2)
    return(data.frame(
      target = names(letters_result),
      cld = unname(letters_result),
      stringsAsFactors = FALSE
    ))
  }

  target_df %>%
    dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
    dplyr::summarise(
      cld = list(generate_cld_letters(p.value, group1, group2)),
      .groups = "drop"
    ) %>%
    tidyr::unnest_longer(cld) %>%
    dplyr::mutate(
      !!target_factor := names(cld),
      cld = as.character(cld)
    ) %>%
    dplyr::select(all_of(group_cols), all_of(target_factor), cld)
}


# ===========================================
# SECTION 3: COMPACT LETTER DISPLAY FUNCTIONS
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

generate_cld_letters <- function(p_values, group1, group2) {
  original_groups <- unique(c(as.character(group1), as.character(group2)))
  safe_groups <- setNames(paste0("group", seq_along(original_groups)), original_groups)
  reverse_map <- setNames(names(safe_groups), safe_groups)

  comparison_names <- paste(
    unname(safe_groups[as.character(group1)]),
    unname(safe_groups[as.character(group2)]),
    sep = "-"
  )

  letters_result <- multcompView::multcompLetters(
    stats::setNames(p_values, comparison_names),
    Letters = letters
  )$Letters

  stats::setNames(unname(letters_result), reverse_map[names(letters_result)])
}

generate_cld_parametric <- function(tukey_df, var1_col, var2_col) {
  # STEP 1: EXTRACT UNIQUE VALUES
  # STRATEGY: Get all unique values of the grouping variable (facet variable)
  # PURPOSE: Process each facet group separately
  unique_var1 <- unique(tukey_df[[var1_col]])

  # STEP 2: INITIALIZE RESULT STORAGE
  # STRATEGY: Use list to collect results from each group
  # PURPOSE: Flexible storage that can handle variable group sizes
  result_list <- list()

  # STEP 3: PROCESS EACH FACET GROUP
  # STRATEGY: Loop through each unique facet value
  # PURPOSE: Generate separate letter displays for each facet panel
  for (group_val in unique_var1) {
    # FILTER DATA FOR CURRENT GROUP
    # STRATEGY: Subset tukey results for current facet level
    # PURPOSE: Generate CLD specific to this experimental condition
    group_data <- tukey_df[tukey_df[[var1_col]] == group_val, ]

    # GENERATE LETTERS FOR THIS GROUP
    # STRATEGY: Use multcompView package for standard CLD generation
    # PURPOSE: Convert p-values to letter groupings following statistical conventions
    letters_result <- generate_cld_letters(
      p_values = group_data$p.adj,
      group1 = group_data$group1,
      group2 = group_data$group2
    )

    # CONVERT TO DATA FRAME FORMAT
    # STRATEGY: Structure results in consistent dataframe format
    # PURPOSE: Enable easy merging with plotting data
    group_result <- data.frame(
      group_value = group_val,
      group_var = names(letters_result),
      cld = unname(letters_result),
      stringsAsFactors = FALSE
    )

    # ADD TO RESULTS COLLECTION
    # STRATEGY: Accumulate results in list for later combination
    # PURPOSE: Handle variable number of groups across facets
    result_list[[length(result_list) + 1]] <- group_result
  }

  # STEP 4: COMBINE ALL RESULTS
  # STRATEGY: Row-bind all group results into single dataframe
  # PURPOSE: Single dataframe for easy merging with plot data
  combined_result <- do.call(rbind, result_list)

  # STEP 5: RENAME COLUMNS TO MATCH EXPECTED FORMAT
  # STRATEGY: Use user-specified variable names for column names
  # PURPOSE: Consistent column naming for downstream functions
  names(combined_result)[1] <- var1_col
  names(combined_result)[2] <- var2_col

  return(combined_result)
}

# Generate CLD values for non-parametric data
#========================================================================================================================================
# STRATEGY: Process Dunn test results into compact letter display format
# PURPOSE: Same as parametric version but for non-parametric post-hoc tests
# INPUT: Dunn test results from rstatix::dunn_test()
# OUTPUT: Dataframe with groups and their significance letters

generate_cld_nonparametric <- function(dunn_df, var1_col, var2_col) {
  dunn_df %>%
    # GROUP BY FACET VARIABLE
    # STRATEGY: Process each facet level separately
    # PURPOSE: Generate separate letter displays for each experimental condition
    dplyr::group_by(dplyr::across(all_of(var1_col))) %>%
    dplyr::summarise(
      # MULTCOMP LETTER GENERATION
      # STRATEGY: Same logic as parametric version using multcompView
      # PURPOSE: Consistent letter generation across parametric and non-parametric analyses
      cld = list(generate_cld_letters(
        p_values = p.adj,
        group1 = group1,
        group2 = group2
      )),
      .groups = 'drop'  # Remove grouping after summarise
    ) %>%
    # UNNEST AND RESTRUCTURE
    # STRATEGY: Convert nested list structure to flat dataframe
    # PURPOSE: Consistent output format matching parametric version
    tidyr::unnest_longer(cld) %>%
    dplyr::mutate(!!var2_col := names(cld)) %>%
    dplyr::select(all_of(var1_col), all_of(var2_col), cld) # Select final columns
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

analyse_barplot <- function(
  data,
    var1, var2, measure_col,
    var1_order = NULL, var2_order = NULL,
    fill_color = "ivory1", line_color = "darkgrey", point_color = "darkgreen",
    parametric_strategy = "classical"
) {

  # ===========================================
  # INPUT VALIDATION SECTION
  # ===========================================
  # STRATEGY: Comprehensive input checking before processing
  # PURPOSE: Prevent errors and provide clear feedback

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

  analysis_keep <- !is.na(data[[var1]]) & !is.na(data[[var2]]) & !is.na(data[[measure_col]])
  analysis_data <- data[analysis_keep, , drop = FALSE]
  if (identical(var1, ".AllData") && var1 %in% names(analysis_data)) {
    analysis_data[[var1]] <- NULL
  }

  # ===========================================
  # DATA PREPARATION SECTION
  # ===========================================
  # STRATEGY: Convert to factors and apply user-specified ordering
  # PURPOSE: Control plot appearance and ensure consistent grouping

  # FACTOR CONVERSION WITH ORDERING
  # STRATEGY: Apply user-specified order if provided, otherwise use default
  # PURPOSE: User control over plot layout and legend order
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

  # ===========================================
  # NORMALITY TESTING - VERSION SIMPLIFIÉE
  # ===========================================
  # STRATEGY: Perform Shapiro-Wilk test for each group
  # PURPOSE: Determine if data meets normality assumptions
  # ASSUMPTION: Sample sizes are always between 3 and 5000
  
  shapiro_df <- data %>%
    group_by(across(all_of(c(var2, var1)))) %>%
    do({
      # Extract values for this group
      values <- .[[measure_col]]
      
      # Perform Shapiro test
      test_result <- tryCatch(
        shapiro.test(values),
        error = function(e) {
          list(statistic = NA_real_, p.value = NA_real_)
        }
      )
      
      # Return results as data frame
      data.frame(
        statistic = as.numeric(test_result$statistic),
        p = as.numeric(test_result$p.value),
        method = "Shapiro-Wilk",
        stringsAsFactors = FALSE
      )
    }) %>%
    ungroup()
  
  # NORMALITY DECISION
  # STRATEGY: Use helper function for consistent logic
  # PURPOSE: Single decision point for statistical method selection
  flag_normal <- check_normality(shapiro_df)
  strategy_clean <- if (is.null(parametric_strategy) || !(parametric_strategy %in% c("classical", "welch"))) {
    "classical"
  } else {
    parametric_strategy
  }

  group_count_df <- data %>%
    dplyr::group_by(dplyr::across(all_of(c(var2, var1)))) %>%
    dplyr::summarise(
      n = sum(!is.na(.data[[measure_col]])),
      .groups = 'drop'
    )

  valid_group_counts <- group_count_df$n[group_count_df$n > 0]
  min_group_n <- if (length(valid_group_counts) > 0) min(valid_group_counts) else NA_integer_
  max_group_n <- if (length(valid_group_counts) > 0) max(valid_group_counts) else NA_integer_
  imbalance_ratio <- if (length(valid_group_counts) > 0 && min_group_n > 0) max_group_n / min_group_n else NA_real_
  
  if (flag_normal) {
    # ===========================================
    # PARAMETRIC ANALYSIS BRANCH
    # ===========================================
    # STRATEGY: Full parametric pipeline with ANOVA and Tukey HSD
    # PURPOSE: When normality assumptions are met

    # SUMMARY STATISTICS
    # STRATEGY: Use summarise for mean ± standard error
    # PURPOSE: Generate values for bar heights and error bars
    # FIX: Use .data[[]] instead of get() for column names with underscores
    my_summary <- data %>%
      dplyr::group_by(dplyr::across(all_of(c(var2, var1)))) %>%
      dplyr::summarise(
        N = length(.data[[measure_col]]),                    # FIX: Changed from get()
        mean_value = mean(.data[[measure_col]], na.rm = TRUE),  # FIX: Changed from get()
        sd_value = sd(.data[[measure_col]], na.rm = TRUE),      # FIX: Changed from get()
        se = sd_value / sqrt(N),
        ci_lower = mean_value - se * qt(0.975, df = N - 1),
        ci_upper = mean_value + se * qt(0.975, df = N - 1),
        .groups = 'drop'
      )

    # PARAMETRIC TEST SELECTION
    # STRATEGY: Optionally switch to Welch when variance heterogeneity is detected
    # PURPOSE: Better one-way handling for heteroscedastic but approximately normal data
    formule <- as.formula(paste0("`", measure_col, "` ~ `", var2, "`"))

    levene_result <- NULL
    levene_p_values <- numeric(0)
    levene_min_p <- NA_real_
    use_welch <- FALSE

    if (identical(strategy_clean, "welch")) {
      levene_result <- tryCatch({
        data %>%
          dplyr::group_by(dplyr::across(all_of(var1))) %>%
          rstatix::levene_test(formule)
      }, error = function(e) {
        NULL
      })

      if (!is.null(levene_result) && "p" %in% names(levene_result)) {
        levene_p_values <- as.numeric(levene_result$p)
        valid_levene_p <- levene_p_values[!is.na(levene_p_values)]
        if (length(valid_levene_p) > 0) {
          levene_min_p <- min(valid_levene_p)
          use_welch <- any(valid_levene_p <= 0.05)
        }
      }
    }

    anova_result <- NULL
    tukey_results <- NULL
    welch_result <- NULL
    games_howell_results <- NULL
    decision_reason <- NULL
    method_used <- NULL

    if (use_welch) {
      welch_result <- data %>%
        dplyr::group_by(dplyr::across(all_of(var1))) %>%
        rstatix::welch_anova_test(formule)

      games_howell_results <- data %>%
        dplyr::group_by(dplyr::across(all_of(var1))) %>%
        rstatix::games_howell_test(formule) %>%
        as.data.frame()

      method_used <- "welch"
      decision_reason <- paste0(
        "Variance-robust one-way strategy selected; minimum Levene p-value = ",
        signif(levene_min_p, 3),
        ", so Welch ANOVA was used."
      )
    } else {
      anova_result <- data %>%
        group_by(dplyr::across(all_of(var1))) %>%
        rstatix::anova_test(formule)

      tukey_results <- data %>%
        group_by(dplyr::across(all_of(var1))) %>%
        rstatix::tukey_hsd(formule)

      method_used <- "anova"
      decision_reason <- if (identical(strategy_clean, "welch") && !is.na(levene_min_p)) {
        paste0(
          "Variance-robust one-way strategy selected; minimum Levene p-value = ",
          signif(levene_min_p, 3),
          ", so classical one-way ANOVA was retained."
        )
      } else if (identical(strategy_clean, "welch")) {
        "Variance-robust one-way strategy selected, but Levene test was unavailable; classical one-way ANOVA was retained."
      } else {
        "Classical one-way strategy selected; one-way ANOVA was used."
      }
    }

    # COMPACT LETTER DISPLAY
    # STRATEGY: Convert p-values to letter annotations
    # PURPOSE: Visual indication of statistical groupings
    cld_table_parametric <- generate_cld_parametric(
      if (identical(method_used, "welch")) games_howell_results else tukey_results,
      var1,
      var2
    )

    # MERGE SUMMARY WITH CLD
    # STRATEGY: Combine statistical results with summary data
    # PURPOSE: Single dataframe for plotting with all needed information
    df2 <- merge(my_summary, cld_table_parametric, by = c(var2, var1), all.x = TRUE)
    names(df2)[names(df2) == "mean_value"] <- measure_col
    
    # PRESERVE FACTOR ORDERING
    # STRATEGY: Ensure user-specified order is maintained after merge
    # PURPOSE: Plot appears as user intended
    if(!is.null(var1_order)) {
      df2[[var1]] <- factor(df2[[var1]], levels = var1_order)
    }
    if(!is.null(var2_order)) {
      df2[[var2]] <- factor(df2[[var2]], levels = var2_order)
    }

    # PLOT CONSTRUCTION (PARAMETRIC)
    # STRATEGY: Layered ggplot with statistical annotations
    # PURPOSE: Professional publication-quality visualization
    p <- df2 %>%
      ggplot(aes(x = .data[[var2]], y = .data[[measure_col]],
                 fill = .data[[var2]])) +

      # BAR LAYER
      # STRATEGY: geom_col for exact heights (not count-based)
      # PURPOSE: Show mean values with custom styling
      geom_col(color = line_color, width = 0.6, position = position_dodge2(padding = 0.05)) +

      # FILL COLOR SCALE
      # STRATEGY: Manual color specification for consistency
      # PURPOSE: User control over plot appearance
      scale_fill_manual(values = rep(fill_color, length(unique(df2[[var2]])))) +

      # Y-AXIS SCALING
      # STRATEGY: Start at zero with small expansion for CLD labels
      # PURPOSE: Honest representation with space for annotations
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +

      # DATA POINTS OVERLAY
      # STRATEGY: Show individual observations as points
      # PURPOSE: Transparency about data distribution and sample size
      geom_quasirandom(
        data = data,  # Use original data, not summary
        aes(x = .data[[var2]], y = .data[[measure_col]]),
        color = point_color, width = 0.3, alpha = 0.6
      ) +

      # ERROR BARS
      # STRATEGY: Standard error bars from summary statistics
      # PURPOSE: Show uncertainty in mean estimates
      geom_segment(aes(x = .data[[var2]], xend = .data[[var2]],
                       y = pmax(0, ci_lower),
                       yend = ci_upper),
                   color = "black") +

      # SIGNIFICANCE LETTERS
      # STRATEGY: Text annotations above bars
      # PURPOSE: Clear indication of statistical groupings
      geom_text(
                aes(x = .data[[var2]], y = ci_upper + (0.15 * ci_upper), label = cld),
                na.rm = TRUE,
                size = 6,
                inherit.aes = TRUE) +

      # THEME AND STYLING
      # STRATEGY: Clean, professional theme
      # PURPOSE: Publication-ready appearance
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.line.x = element_line(linewidth = 0.5),
        axis.line.y = element_line(linewidth = 0.5),
        panel.background = element_rect(fill = 'transparent', color = NA),
        plot.background = element_rect(fill = 'transparent', color = NA),
        axis.text.y = element_text(vjust = 1),
        text = element_text(size = 18),
        legend.position = "none",  # Remove legend (redundant with x-axis)
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "plain", size = 20, color = "black", hjust = 0.5)
      ) +

      # FACETING
      # STRATEGY: Panel separation by facet variable
      # PURPOSE: Clear separation of different experimental conditions
      facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +

      # AXIS LABELS
      # STRATEGY: Use variable names for clarity
      # PURPOSE: Self-documenting plots
      labs(x = var1, y = paste0(measure_col, " (mean)"))

    # RETURN PARAMETRIC RESULTS
    # STRATEGY: Return both plot and all statistical results
    # PURPOSE: Enable export of complete analysis
    return(list(
      plot = p,
      summary = my_summary,
      shapiro = shapiro_df,
      normality = flag_normal,
      anova = anova_result,
      tukey = tukey_results,
      welch = welch_result,
      games_howell = games_howell_results,
      cld = cld_table_parametric,
      analysis_data = analysis_data,
      model = "oneway_anova",
      method = method_used,
      decision_mode = strategy_clean,
      decision_reason = decision_reason,
      decision_diagnostics = list(
        levene_p = levene_min_p,
        min_cell_n = min_group_n,
        max_cell_n = max_group_n,
        imbalance_ratio = imbalance_ratio
      )
    ))
    
  } else {
    # ===========================================
    # NON-PARAMETRIC ANALYSIS BRANCH
    # ===========================================
    # STRATEGY: Non-parametric pipeline with Kruskal-Wallis and Dunn tests
    # PURPOSE: When normality assumptions are violated

    # MEDIAN WITH CONFIDENCE INTERVALS - REPLACE groupwiseMedian
    # STRATEGY: Use modern dplyr with quantile-based confidence intervals
    # PURPOSE: Non-parametric equivalent of mean ± SE
    # FIX: Use .data[[]] instead of get() for column names with underscores
    conf_int <- data %>%
      dplyr::group_by(dplyr::across(all_of(c(var2, var1)))) %>%
      dplyr::summarise(
        N = length(.data[[measure_col]]),                           # FIX: Changed from get()
        Median = median(.data[[measure_col]], na.rm = TRUE),        # FIX: Changed from get()
        Q1 = quantile(.data[[measure_col]], 0.25, na.rm = TRUE),    # FIX: Changed from get()
        Q3 = quantile(.data[[measure_col]], 0.75, na.rm = TRUE),    # FIX: Changed from get()
        Percentile.lower = quantile(.data[[measure_col]], 0.025, na.rm = TRUE),  # FIX
        Percentile.upper = quantile(.data[[measure_col]], 0.975, na.rm = TRUE),  # FIX
        .groups = 'drop'
      )

    # KRUSKAL-WALLIS TESTING - FORMULA APPROACH
    # STRATEGY: Non-parametric equivalent of ANOVA
    # PURPOSE: Test for overall differences between groups

    # GENERATE DYNAMIC FORMULA FOR KRUSKAL-WALLIS
    # STRATEGY: Same formula construction approach as parametric version
    # PURPOSE: Consistent approach across statistical methods
    formule <- as.formula(paste0("`", measure_col, "` ~ `", var2, "`"))

    # APPLY KRUSKAL-WALLIS TEST
    # STRATEGY: Group by facet variable for separate tests
    # PURPOSE: Non-parametric testing within each experimental condition
    kruskal_pval <- data %>%
      group_by(dplyr::across(all_of(var1))) %>%
      rstatix::kruskal_test(formule) %>%
      dplyr::select(all_of(var1), p)

    # SIGNIFICANCE CHECK
    # STRATEGY: Only proceed with post-hoc if overall test is significant
    # PURPOSE: Prevent multiple comparisons when not justified
    significant <- any(kruskal_pval$p < 0.05)

    if (significant) {
      # DUNN POST-HOC TESTING
      # STRATEGY: Non-parametric pairwise comparison
      # PURPOSE: Identify which specific groups differ
      
      # GENERATE DYNAMIC FORMULA FOR DUNN TEST
      # FIX: Use backticks for column names with special characters
      formule <- as.formula(paste0("`", measure_col, "` ~ `", var2, "`"))

      # APPLY DUNN TEST WITH MULTIPLE COMPARISON CORRECTION
      pval_dunn <- data %>%
        group_by(dplyr::across(all_of(var1))) %>%
        rstatix::dunn_test(formule, p.adjust.method = "BH") %>%
        as.data.frame()
      # COMPACT LETTER DISPLAY (NON-PARAMETRIC)
      # STRATEGY: Generate letters from Dunn test results
      # PURPOSE: Visual grouping for non-parametric results
      cld_table_nonparametric <- generate_cld_nonparametric(pval_dunn, var1, var2)

      # MERGE AND PREPARE PLOTTING DATA
      # STRATEGY: Combine statistical results with summary statistics
      # PURPOSE: Single dataframe containing all information for plotting
      df2 <- merge(conf_int, cld_table_nonparametric, by = c(var2, var1), all.x = TRUE)
      names(df2)[names(df2) == "Median"] <- measure_col

      # PRESERVE FACTOR ORDERING
      # STRATEGY: Ensure user-specified order is maintained after merge
      # PURPOSE: Plot appears as user intended
      if(!is.null(var1_order)) {
        df2[[var1]] <- factor(df2[[var1]], levels = var1_order)
      }
      if(!is.null(var2_order)) {
        df2[[var2]] <- factor(df2[[var2]], levels = var2_order)
      }

      # PLOT CONSTRUCTION (NON-PARAMETRIC)
      # STRATEGY: Similar to parametric but using medians and percentile CIs
      # PURPOSE: Appropriate visualization for non-parametric analysis
      p <- df2 %>%
        ggplot(aes(x = .data[[var2]], y = .data[[measure_col]], fill = .data[[var2]])) +

        # MEDIAN BARS
        # STRATEGY: Show medians instead of means
        # PURPOSE: Appropriate central tendency for non-normal data
        geom_col(color = line_color, width = 0.6, position = position_dodge2(padding = 0.05)) +

        # FILL COLOR SCALE
        # STRATEGY: Manual color specification for consistency
        # PURPOSE: User control over plot appearance
        scale_fill_manual(values = rep(fill_color, length(unique(df2[[var2]])))) +

        # Y-AXIS SCALING
        # STRATEGY: Start at zero with small expansion for CLD labels
        # PURPOSE: Honest representation with space for annotations
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +

        # DATA POINTS OVERLAY
        # STRATEGY: Same as parametric version
        # PURPOSE: Show actual data distribution
        geom_quasirandom(
          data = data,
          aes(x = .data[[var2]], y = .data[[measure_col]]),
          color = point_color, width = 0.3, alpha = 0.6
        ) +

        # CONFIDENCE INTERVAL BARS
        # STRATEGY: Use bootstrap percentile confidence intervals
        # PURPOSE: Show uncertainty in median estimates
        geom_segment(aes(x = .data[[var2]], xend = .data[[var2]],
                         y = pmax(0, Percentile.lower), yend = Percentile.upper),
                     color = "black") +

        # SIGNIFICANCE LETTERS
        # STRATEGY: Position relative to confidence intervals
        # PURPOSE: Clear statistical grouping indication
        geom_text(aes(x = .data[[var2]], y = Percentile.upper + (0.15 * Percentile.upper), label = cld),
                  size = 6, inherit.aes = TRUE) +

        # THEME (SAME AS PARAMETRIC)
        # STRATEGY: Consistent visual styling across analysis types
        # PURPOSE: Professional appearance regardless of statistical method
        theme_classic() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.line.x = element_line(linewidth = 0.5),
          axis.line.y = element_line(linewidth = 0.5),
          panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill = 'transparent', color = NA),
          axis.text.y = element_text(vjust = 1),
          text = element_text(size = 18),
          legend.position = "none",
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(face = "plain", size = 20, color = "black", hjust = 0.5)
        ) +
        facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
        labs(x = var1, y = paste0(measure_col, " (median)"))

      # RETURN NON-PARAMETRIC RESULTS
      # STRATEGY: Return all relevant non-parametric statistics
      # PURPOSE: Complete analysis package for export
      return(list(
        plot = p,
        summary = conf_int,
        shapiro = shapiro_df,
        normality = flag_normal,
        kruskal = kruskal_pval,
        dunn = pval_dunn,
        cld = cld_table_nonparametric,
        analysis_data = analysis_data,
        model = "oneway_anova",
        method = "kruskal",
        decision_mode = strategy_clean,
        decision_reason = if (identical(strategy_clean, "welch")) {
          "Variance-robust one-way strategy selected, but normality was not supported; Kruskal-Wallis was used."
        } else {
          "Classical one-way strategy selected, but normality was not supported; Kruskal-Wallis was used."
        },
        decision_diagnostics = list(
          levene_p = NA_real_,
          min_cell_n = min_group_n,
          max_cell_n = max_group_n,
          imbalance_ratio = imbalance_ratio
        )
      ))
    } else {
      # NO SIGNIFICANT DIFFERENCES CASE
      # STRATEGY: Return the same structured output without post-hoc results
      # PURPOSE: Keep downstream server and export code compatible
      names(conf_int)[names(conf_int) == "Median"] <- measure_col

      if(!is.null(var1_order)) {
        conf_int[[var1]] <- factor(conf_int[[var1]], levels = var1_order)
      }
      if(!is.null(var2_order)) {
        conf_int[[var2]] <- factor(conf_int[[var2]], levels = var2_order)
      }

      p <- conf_int %>%
        ggplot(aes(x = .data[[var2]], y = .data[[measure_col]], fill = .data[[var2]])) +
        geom_col(color = line_color, width = 0.6, position = position_dodge2(padding = 0.05)) +
        scale_fill_manual(values = rep(fill_color, length(unique(conf_int[[var2]])))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        geom_quasirandom(
          data = data,
          aes(x = .data[[var2]], y = .data[[measure_col]]),
          color = point_color, width = 0.3, alpha = 0.6
        ) +
        geom_segment(aes(x = .data[[var2]], xend = .data[[var2]],
                         y = pmax(0, Percentile.lower), yend = Percentile.upper),
                     color = "black") +
        theme_classic() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.line.x = element_line(linewidth = 0.5),
          axis.line.y = element_line(linewidth = 0.5),
          panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill = 'transparent', color = NA),
          axis.text.y = element_text(vjust = 1),
          text = element_text(size = 18),
          legend.position = "none",
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(face = "plain", size = 20, color = "black", hjust = 0.5)
        ) +
        facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
        labs(x = var1, y = paste0(measure_col, " (median)"))

      return(list(
        plot = p,
        summary = conf_int,
        shapiro = shapiro_df,
        normality = flag_normal,
        kruskal = kruskal_pval,
        dunn = NULL,
        cld = NULL,
        analysis_data = analysis_data,
        model = "oneway_anova",
        method = "kruskal",
        decision_mode = strategy_clean,
        decision_reason = if (identical(strategy_clean, "welch")) {
          "Variance-robust one-way strategy selected, but normality was not supported; Kruskal-Wallis was used."
        } else {
          "Classical one-way strategy selected, but normality was not supported; Kruskal-Wallis was used."
        },
        decision_diagnostics = list(
          levene_p = NA_real_,
          min_cell_n = min_group_n,
          max_cell_n = max_group_n,
          imbalance_ratio = imbalance_ratio
        ),
        message = "Data are not significantly different, the Dunn test was not performed."
      ))
    }
  }
}

# Two-way ANOVA bar plot analysis
#========================================================================================================================================
# STRATEGY: Fit A*B model with optional facet-wise stratification
# PURPOSE: Compare response across two experimental factors

analyse_barplot_twoway <- function(
  data,
  factor_a,
  factor_b,
  measure_col,
  facet_var = NULL,
  decision_mode = "conservative",
  fill_color = "ivory1",
  line_color = "darkgrey",
  point_color = "darkgreen",
  fill_palette = NULL,
  point_palette = NULL
) {
  required_cols <- c(factor_a, factor_b, measure_col)
  if (!is.null(facet_var)) {
    required_cols <- c(required_cols, facet_var)
  }

  analysis_keep <- stats::complete.cases(data[, required_cols, drop = FALSE])
  analysis_data <- data[analysis_keep, , drop = FALSE]

  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  data[[factor_a]] <- as.factor(data[[factor_a]])
  data[[factor_b]] <- as.factor(data[[factor_b]])
  if (!is.null(facet_var)) {
    data[[facet_var]] <- as.factor(data[[facet_var]])
  }

  shapiro_groups <- c(factor_a, factor_b)
  if (!is.null(facet_var)) {
    shapiro_groups <- c(shapiro_groups, facet_var)
  }

  shapiro_df <- data %>%
    dplyr::group_by(dplyr::across(all_of(shapiro_groups))) %>%
    dplyr::summarise(
      p = tryCatch(stats::shapiro.test(.data[[measure_col]])$p.value, error = function(e) NA_real_),
      .groups = "drop"
    )

  flag_normal <- check_normality(shapiro_df)

  decision_info <- evaluate_factorial_decision(
    data = data,
    factors = c(factor_a, factor_b),
    measure_col = measure_col,
    flag_normal = flag_normal,
    decision_mode = decision_mode
  )
  analysis_method <- decision_info$analysis_method

  summary_groups <- c(factor_a, factor_b)
  if (!is.null(facet_var)) {
    summary_groups <- c(summary_groups, facet_var)
  }

  summary_df <- summarise_factorial_response(data, summary_groups, measure_col, analysis_method == "anova")
  summary_label <- if (analysis_method == "anova") "mean" else "median"

  formula_tw <- stats::as.formula(paste0("`", measure_col, "` ~ `", factor_a, "` * `", factor_b, "`"))

  has_two_levels <- function(df, col, response_col, required_factors) {
    keep <- !is.na(df[[response_col]])
    for (fac in required_factors) {
      keep <- keep & !is.na(df[[fac]])
    }
    df_valid <- df[keep, , drop = FALSE]
    length(unique(df_valid[[col]])) >= 2
  }

  build_level_issue <- function(facet_value = NULL) {
    parts <- c()
    if (!has_two_levels(subset_df, factor_a, measure_col, c(factor_a, factor_b))) {
      parts <- c(parts, factor_a)
    }
    if (!has_two_levels(subset_df, factor_b, measure_col, c(factor_a, factor_b))) {
      parts <- c(parts, factor_b)
    }

    if (is.null(facet_value)) {
      paste0(
        "Two-way ANOVA requires at least 2 levels for each factor. Insufficient levels for: ",
        paste(parts, collapse = ", "),
        "."
      )
    } else {
      paste0(
        "Facet '", facet_value, "' skipped: insufficient levels for ",
        paste(parts, collapse = ", "),
        "."
      )
    }
  }

  warning_messages <- character()
  warning_messages <- c(warning_messages, decision_info$decision_reason)
  if (analysis_method == "art") {
    warning_messages <- c(
      warning_messages,
      "Used aligned rank transform (ART) non-parametric factorial model."
    )
  }

  interaction_term <- paste(factor_a, factor_b, sep = ":")

  if (!is.null(facet_var)) {
    facet_levels <- unique(as.character(data[[facet_var]]))
    anova_list <- lapply(facet_levels, function(fv) {
      subset_df <- data[as.character(data[[facet_var]]) == fv, , drop = FALSE]

      if (!has_two_levels(subset_df, factor_a, measure_col, c(factor_a, factor_b)) ||
          !has_two_levels(subset_df, factor_b, measure_col, c(factor_a, factor_b))) {
        warning_messages <<- c(warning_messages, build_level_issue(fv))
        return(NULL)
      }

      if (analysis_method == "anova") {
        res <- tryCatch(
          rstatix::anova_test(subset_df, formula_tw),
          error = function(e) {
            warning_messages <<- c(
              warning_messages,
              paste0("Facet '", fv, "' skipped: ", e$message)
            )
            NULL
          }
        )

        if (is.null(res)) {
          return(NULL)
        }

        res_df <- as.data.frame(rstatix::get_anova_table(res))
      } else {
        if (!requireNamespace("ARTool", quietly = TRUE)) {
          stop("Non-parametric two-way analysis requires package 'ARTool'.")
        }

        art_model <- tryCatch(
          ARTool::art(formula_tw, data = subset_df),
          error = function(e) {
            warning_messages <<- c(
              warning_messages,
              paste0("Facet '", fv, "' skipped: ", e$message)
            )
            NULL
          }
        )

        if (is.null(art_model)) {
          return(NULL)
        }

        res_df <- format_art_anova_table(art_model)
      }

      class(res_df) <- "data.frame"
      res_df[[facet_var]] <- fv
      res_df
    })

    anova_list <- Filter(Negate(is.null), anova_list)
    if (length(anova_list) == 0) {
      stop(
        paste(
          c(
            "Two-way analysis could not be computed for any stratification level.",
            warning_messages
          ),
          collapse = " "
        )
      )
    }

    anova_result <- dplyr::bind_rows(anova_list)
    anova_result <- anova_result %>% dplyr::relocate(all_of(facet_var))
  } else {
    subset_df <- data
    if (!has_two_levels(subset_df, factor_a, measure_col, c(factor_a, factor_b)) ||
        !has_two_levels(subset_df, factor_b, measure_col, c(factor_a, factor_b))) {
      stop(build_level_issue())
    }

    if (analysis_method == "anova") {
      anova_result <- as.data.frame(rstatix::get_anova_table(rstatix::anova_test(data, formula_tw)))
    } else {
      if (!requireNamespace("ARTool", quietly = TRUE)) {
        stop("Non-parametric two-way analysis requires package 'ARTool'.")
      }
      anova_result <- format_art_anova_table(ARTool::art(formula_tw, data = data))
    }
    class(anova_result) <- "data.frame"
  }

  posthoc_result <- NULL
  cld_result <- NULL
  if (requireNamespace("emmeans", quietly = TRUE)) {
    if (!is.null(facet_var)) {
      facet_levels <- unique(as.character(data[[facet_var]]))
      posthoc_list <- lapply(facet_levels, function(fv) {
        subset_df <- data[as.character(data[[facet_var]]) == fv, , drop = FALSE]

        if (!has_two_levels(subset_df, factor_a, measure_col, c(factor_a, factor_b)) ||
            !has_two_levels(subset_df, factor_b, measure_col, c(factor_a, factor_b))) {
          return(NULL)
        }

        if (analysis_method == "anova") {
          model_fit <- tryCatch(stats::lm(formula_tw, data = subset_df), error = function(e) NULL)
          if (is.null(model_fit)) {
            return(NULL)
          }

          a_within_b <- tryCatch(
            run_emmeans_pairwise(model_fit, target_factor = factor_a, by_factors = c(factor_b)),
            error = function(e) NULL
          )
          b_within_a <- tryCatch(
            run_emmeans_pairwise(model_fit, target_factor = factor_b, by_factors = c(factor_a)),
            error = function(e) NULL
          )
        } else {
          art_model <- tryCatch(ARTool::art(formula_tw, data = subset_df), error = function(e) NULL)
          if (is.null(art_model)) {
            return(NULL)
          }

          a_within_b <- tryCatch(
            run_art_emmeans_pairwise(art_model, interaction_term, factor_a, c(factor_b), adjust = "holm"),
            error = function(e) NULL
          )
          b_within_a <- tryCatch(
            run_art_emmeans_pairwise(art_model, interaction_term, factor_b, c(factor_a), adjust = "holm"),
            error = function(e) NULL
          )
        }

        combined <- dplyr::bind_rows(a_within_b, b_within_a)
        if (nrow(combined) == 0) {
          return(NULL)
        }

        combined[[facet_var]] <- fv
        combined
      })

      posthoc_list <- Filter(Negate(is.null), posthoc_list)
      if (length(posthoc_list) > 0) {
        posthoc_result <- dplyr::bind_rows(posthoc_list) %>% dplyr::relocate(all_of(facet_var))
      }
    } else {
      if (analysis_method == "anova") {
        model_fit <- tryCatch(stats::lm(formula_tw, data = data), error = function(e) NULL)
        if (!is.null(model_fit)) {
          a_within_b <- tryCatch(
            run_emmeans_pairwise(model_fit, target_factor = factor_a, by_factors = c(factor_b)),
            error = function(e) NULL
          )
          b_within_a <- tryCatch(
            run_emmeans_pairwise(model_fit, target_factor = factor_b, by_factors = c(factor_a)),
            error = function(e) NULL
          )
          posthoc_result <- dplyr::bind_rows(a_within_b, b_within_a)
        }
      } else {
        art_model <- tryCatch(ARTool::art(formula_tw, data = data), error = function(e) NULL)
        if (!is.null(art_model)) {
          a_within_b <- tryCatch(
            run_art_emmeans_pairwise(art_model, interaction_term, factor_a, c(factor_b), adjust = "holm"),
            error = function(e) NULL
          )
          b_within_a <- tryCatch(
            run_art_emmeans_pairwise(art_model, interaction_term, factor_b, c(factor_a), adjust = "holm"),
            error = function(e) NULL
          )
          posthoc_result <- dplyr::bind_rows(a_within_b, b_within_a)
        }
      }
    }
  } else {
    warning_messages <- c(
      warning_messages,
      "Post-hoc comparisons not run: package 'emmeans' is not installed."
    )
  }

  if (!is.null(posthoc_result) && nrow(posthoc_result) > 0) {
    cld_groups <- c(factor_b)
    if (!is.null(facet_var)) {
      cld_groups <- c(facet_var, cld_groups)
    }

    cld_result <- build_cld_from_emmeans(
      posthoc_df = posthoc_result,
      target_factor = factor_a,
      by_factors = cld_groups
    )

    if (!is.null(cld_result) && "target" %in% names(cld_result)) {
      names(cld_result)[names(cld_result) == "target"] <- factor_a
    }
  }

  plot_df <- summary_df
  if (!is.null(cld_result) && nrow(cld_result) > 0) {
    join_cols <- intersect(c(factor_a, factor_b, facet_var), names(cld_result))
    plot_df <- dplyr::left_join(summary_df, cld_result, by = join_cols)
    plot_df <- plot_df %>%
      dplyr::mutate(
        label_y = ci_upper + dplyr::if_else(ci_upper == 0, 0.05, 0.1 * abs(ci_upper))
      )
  }

  factor_a_levels <- unique(as.character(summary_df[[factor_a]]))
  factor_a_levels <- factor_a_levels[!is.na(factor_a_levels)]

  default_fill_palette <- if (length(factor_a_levels) > 1) {
    setNames(scales::hue_pal()(length(factor_a_levels)), factor_a_levels)
  } else {
    setNames(fill_color, factor_a_levels)
  }

  if (!is.null(fill_palette)) {
    custom_fill <- fill_palette[factor_a_levels]
    non_missing <- !is.na(custom_fill)
    default_fill_palette[names(custom_fill[non_missing])] <- custom_fill[non_missing]
  }
  fill_palette <- default_fill_palette

  default_point_palette <- if (length(factor_a_levels) > 1) {
    fill_palette
  } else {
    setNames(point_color, factor_a_levels)
  }

  if (!is.null(point_palette)) {
    custom_point <- point_palette[factor_a_levels]
    non_missing <- !is.na(custom_point)
    default_point_palette[names(custom_point[non_missing])] <- custom_point[non_missing]
  }
  point_palette <- default_point_palette

  p <- ggplot(
    plot_df,
    aes(x = .data[[factor_b]], y = mean_value, fill = .data[[factor_a]])
  ) +
    geom_col(
      position = position_dodge(width = 0.75),
      width = 0.65,
      color = line_color
    ) +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.2,
      position = position_dodge(width = 0.75)
    ) +
    geom_quasirandom(
      data = data,
      aes(
        x = .data[[factor_b]],
        y = .data[[measure_col]],
        color = .data[[factor_a]]
      ),
      dodge.width = 0.75,
      alpha = 0.5,
      width = 0.2,
      size = 1.2,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = fill_palette) +
    scale_color_manual(values = point_palette) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      text = element_text(size = 18),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    labs(
      x = factor_b,
      y = paste0(measure_col, " (", summary_label, ")"),
      fill = factor_a
    )

  if (!is.null(cld_result) && nrow(cld_result) > 0) {
    p <- p +
      geom_text(
        data = plot_df,
        aes(x = .data[[factor_b]], y = label_y, label = cld, group = .data[[factor_a]]),
        position = position_dodge(width = 0.75),
        na.rm = TRUE,
        size = 5,
        inherit.aes = FALSE
      )
  }

  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), nrow = 1, scales = "free_y")
  }

  return(list(
    plot = p,
    summary = summary_df,
    shapiro = shapiro_df,
    normality = flag_normal,
    anova2 = anova_result,
    posthoc = posthoc_result,
    cld = cld_result,
    analysis_data = analysis_data,
    model = "twoway_anova",
    method = analysis_method,
    decision_mode = decision_info$decision_mode,
    decision_reason = decision_info$decision_reason,
    decision_diagnostics = decision_info$diagnostics,
    message = if (length(warning_messages) > 0) paste(unique(warning_messages), collapse = " ") else NULL
  ))
}

# Three-way ANOVA bar plot analysis
#========================================================================================================================================
# STRATEGY: Fit A*B*C model in one analysis
# PURPOSE: Compare response across three experimental factors

analyse_barplot_threeway <- function(
  data,
  factor_a,
  factor_b,
  factor_c,
  measure_col,
  facet_var = NULL,
  decision_mode = "conservative",
  fill_color = "ivory1",
  line_color = "darkgrey",
  point_color = "darkgreen",
  fill_palette = NULL,
  point_palette = NULL
) {
  required_cols <- c(factor_a, factor_b, factor_c, measure_col)
  if (!is.null(facet_var)) {
    required_cols <- c(required_cols, facet_var)
  }
  analysis_keep <- stats::complete.cases(data[, required_cols, drop = FALSE])
  analysis_data <- data[analysis_keep, , drop = FALSE]
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  data[[factor_a]] <- as.factor(data[[factor_a]])
  data[[factor_b]] <- as.factor(data[[factor_b]])
  data[[factor_c]] <- as.factor(data[[factor_c]])
  if (!is.null(facet_var)) {
    data[[facet_var]] <- as.factor(data[[facet_var]])
  }

  has_two_levels <- function(df, col, response_col, required_factors) {
    keep <- !is.na(df[[response_col]])
    for (fac in required_factors) {
      keep <- keep & !is.na(df[[fac]])
    }
    df_valid <- df[keep, , drop = FALSE]
    length(unique(df_valid[[col]])) >= 2
  }

  required_factors <- c(factor_a, factor_b, factor_c)

  shapiro_groups <- required_factors
  if (!is.null(facet_var)) shapiro_groups <- c(shapiro_groups, facet_var)

  shapiro_df <- data %>%
    dplyr::group_by(dplyr::across(all_of(shapiro_groups))) %>%
    dplyr::summarise(
      p = tryCatch(stats::shapiro.test(.data[[measure_col]])$p.value, error = function(e) NA_real_),
      .groups = "drop"
    )

  flag_normal <- check_normality(shapiro_df)

  decision_info <- evaluate_factorial_decision(
    data = data,
    factors = c(factor_a, factor_b, factor_c),
    measure_col = measure_col,
    flag_normal = flag_normal,
    decision_mode = decision_mode
  )
  analysis_method <- decision_info$analysis_method

  summary_groups <- required_factors
  if (!is.null(facet_var)) summary_groups <- c(summary_groups, facet_var)

  summary_df <- summarise_factorial_response(data, summary_groups, measure_col, analysis_method == "anova")
  summary_label <- if (analysis_method == "anova") "mean" else "median"

  formula_th <- stats::as.formula(
    paste0("`", measure_col, "` ~ `", factor_a, "` * `", factor_b, "` * `", factor_c, "`")
  )

  warning_messages <- character()
  warning_messages <- c(warning_messages, decision_info$decision_reason)
  if (analysis_method == "art") {
    warning_messages <- c(
      warning_messages,
      "Used aligned rank transform (ART) non-parametric factorial model."
    )
  }

  interaction_term <- paste(factor_a, factor_b, factor_c, sep = ":")

  if (!is.null(facet_var)) {
    facet_levels <- unique(as.character(data[[facet_var]]))
    anova_list <- lapply(facet_levels, function(fv) {
      subset_df <- data[as.character(data[[facet_var]]) == fv, , drop = FALSE]

      bad_factors <- Filter(
        function(fac) !has_two_levels(subset_df, fac, measure_col, required_factors),
        required_factors
      )
      if (length(bad_factors) > 0) {
        warning_messages <<- c(
          warning_messages,
          paste0("Facet '", fv, "' skipped: insufficient levels for ", paste(bad_factors, collapse = ", "), ".")
        )
        return(NULL)
      }

      if (analysis_method == "anova") {
        res <- tryCatch(
          rstatix::anova_test(subset_df, formula_th),
          error = function(e) {
            warning_messages <<- c(warning_messages, paste0("Facet '", fv, "' skipped: ", e$message))
            NULL
          }
        )
        if (is.null(res)) return(NULL)

        res_df <- as.data.frame(rstatix::get_anova_table(res))
      } else {
        if (!requireNamespace("ARTool", quietly = TRUE)) {
          stop("Non-parametric three-way analysis requires package 'ARTool'.")
        }

        art_model <- tryCatch(
          ARTool::art(formula_th, data = subset_df),
          error = function(e) {
            warning_messages <<- c(warning_messages, paste0("Facet '", fv, "' skipped: ", e$message))
            NULL
          }
        )
        if (is.null(art_model)) return(NULL)

        res_df <- format_art_anova_table(art_model)
      }

      class(res_df) <- "data.frame"
      res_df[[facet_var]] <- fv
      res_df
    })

    anova_list <- Filter(Negate(is.null), anova_list)
    if (length(anova_list) == 0) {
      stop(paste(
        c("Three-way analysis could not be computed for any stratification level.", warning_messages),
        collapse = " "
      ))
    }
    anova_result <- dplyr::bind_rows(anova_list) %>% dplyr::relocate(all_of(facet_var))
  } else {
    bad_factors <- Filter(
      function(fac) !has_two_levels(data, fac, measure_col, required_factors),
      required_factors
    )
    if (length(bad_factors) > 0) {
      stop(paste0(
        "Three-way ANOVA requires at least 2 levels for each factor. Insufficient levels for: ",
        paste(bad_factors, collapse = ", "), "."
      ))
    }
    if (analysis_method == "anova") {
      anova_result <- as.data.frame(rstatix::get_anova_table(rstatix::anova_test(data, formula_th)))
    } else {
      if (!requireNamespace("ARTool", quietly = TRUE)) {
        stop("Non-parametric three-way analysis requires package 'ARTool'.")
      }
      anova_result <- format_art_anova_table(ARTool::art(formula_th, data = data))
    }
    class(anova_result) <- "data.frame"
  }

  posthoc_result <- NULL
  cld_result <- NULL
  if (requireNamespace("emmeans", quietly = TRUE)) {
    if (!is.null(facet_var)) {
      facet_levels <- unique(as.character(data[[facet_var]]))
      posthoc_list <- lapply(facet_levels, function(fv) {
        subset_df <- data[as.character(data[[facet_var]]) == fv, , drop = FALSE]

        bad_factors <- Filter(
          function(fac) !has_two_levels(subset_df, fac, measure_col, required_factors),
          required_factors
        )
        if (length(bad_factors) > 0) return(NULL)

        if (analysis_method == "anova") {
          model_fit <- tryCatch(stats::lm(formula_th, data = subset_df), error = function(e) NULL)
          if (is.null(model_fit)) return(NULL)

          a_within_bc <- tryCatch(run_emmeans_pairwise(model_fit, factor_a, c(factor_b, factor_c)), error = function(e) NULL)
          b_within_ac <- tryCatch(run_emmeans_pairwise(model_fit, factor_b, c(factor_a, factor_c)), error = function(e) NULL)
          c_within_ab <- tryCatch(run_emmeans_pairwise(model_fit, factor_c, c(factor_a, factor_b)), error = function(e) NULL)
        } else {
          art_model <- tryCatch(ARTool::art(formula_th, data = subset_df), error = function(e) NULL)
          if (is.null(art_model)) return(NULL)

          a_within_bc <- tryCatch(run_art_emmeans_pairwise(art_model, interaction_term, factor_a, c(factor_b, factor_c), adjust = "holm"), error = function(e) NULL)
          b_within_ac <- tryCatch(run_art_emmeans_pairwise(art_model, interaction_term, factor_b, c(factor_a, factor_c), adjust = "holm"), error = function(e) NULL)
          c_within_ab <- tryCatch(run_art_emmeans_pairwise(art_model, interaction_term, factor_c, c(factor_a, factor_b), adjust = "holm"), error = function(e) NULL)
        }

        combined <- dplyr::bind_rows(a_within_bc, b_within_ac, c_within_ab)
        if (nrow(combined) == 0) return(NULL)
        combined[[facet_var]] <- fv
        combined
      })
      posthoc_list <- Filter(Negate(is.null), posthoc_list)
      if (length(posthoc_list) > 0) {
        posthoc_result <- dplyr::bind_rows(posthoc_list) %>% dplyr::relocate(all_of(facet_var))
      }
    } else {
      if (analysis_method == "anova") {
        model_fit <- tryCatch(stats::lm(formula_th, data = data), error = function(e) NULL)
        if (!is.null(model_fit)) {
          a_within_bc <- tryCatch(run_emmeans_pairwise(model_fit, factor_a, c(factor_b, factor_c)), error = function(e) NULL)
          b_within_ac <- tryCatch(run_emmeans_pairwise(model_fit, factor_b, c(factor_a, factor_c)), error = function(e) NULL)
          c_within_ab <- tryCatch(run_emmeans_pairwise(model_fit, factor_c, c(factor_a, factor_b)), error = function(e) NULL)
          posthoc_result <- dplyr::bind_rows(a_within_bc, b_within_ac, c_within_ab)
        }
      } else {
        art_model <- tryCatch(ARTool::art(formula_th, data = data), error = function(e) NULL)
        if (!is.null(art_model)) {
          a_within_bc <- tryCatch(run_art_emmeans_pairwise(art_model, interaction_term, factor_a, c(factor_b, factor_c), adjust = "holm"), error = function(e) NULL)
          b_within_ac <- tryCatch(run_art_emmeans_pairwise(art_model, interaction_term, factor_b, c(factor_a, factor_c), adjust = "holm"), error = function(e) NULL)
          c_within_ab <- tryCatch(run_art_emmeans_pairwise(art_model, interaction_term, factor_c, c(factor_a, factor_b), adjust = "holm"), error = function(e) NULL)
          posthoc_result <- dplyr::bind_rows(a_within_bc, b_within_ac, c_within_ab)
        }
      }
    }
  } else {
    warning_messages <- c(
      warning_messages,
      "Post-hoc comparisons not run: package 'emmeans' is not installed."
    )
  }

  if (!is.null(posthoc_result) && nrow(posthoc_result) > 0) {
    cld_groups <- c(factor_b, factor_c)
    if (!is.null(facet_var)) {
      cld_groups <- c(facet_var, cld_groups)
    }

    cld_result <- build_cld_from_emmeans(
      posthoc_df = posthoc_result,
      target_factor = factor_a,
      by_factors = cld_groups
    )

    if (!is.null(cld_result) && "target" %in% names(cld_result)) {
      names(cld_result)[names(cld_result) == "target"] <- factor_a
    }
  }

  plot_df <- summary_df
  if (!is.null(cld_result) && nrow(cld_result) > 0) {
    join_cols <- intersect(c(factor_a, factor_b, factor_c, facet_var), names(cld_result))
    plot_df <- dplyr::left_join(summary_df, cld_result, by = join_cols)
    plot_df <- plot_df %>%
      dplyr::mutate(
        label_y = ci_upper + dplyr::if_else(ci_upper == 0, 0.05, 0.1 * abs(ci_upper))
      )
  }

  factor_a_levels <- unique(as.character(summary_df[[factor_a]]))
  factor_a_levels <- factor_a_levels[!is.na(factor_a_levels)]

  default_fill_palette <- if (length(factor_a_levels) > 1) {
    setNames(scales::hue_pal()(length(factor_a_levels)), factor_a_levels)
  } else {
    setNames(fill_color, factor_a_levels)
  }

  if (!is.null(fill_palette)) {
    custom_fill <- fill_palette[factor_a_levels]
    non_missing <- !is.na(custom_fill)
    default_fill_palette[names(custom_fill[non_missing])] <- custom_fill[non_missing]
  }
  fill_palette <- default_fill_palette

  default_point_palette <- if (length(factor_a_levels) > 1) {
    fill_palette
  } else {
    setNames(point_color, factor_a_levels)
  }

  if (!is.null(point_palette)) {
    custom_point <- point_palette[factor_a_levels]
    non_missing <- !is.na(custom_point)
    default_point_palette[names(custom_point[non_missing])] <- custom_point[non_missing]
  }
  point_palette <- default_point_palette

  p <- ggplot(
    plot_df,
    aes(x = .data[[factor_b]], y = mean_value, fill = .data[[factor_a]])
  ) +
    geom_col(
      position = position_dodge(width = 0.75),
      width = 0.65,
      color = line_color
    ) +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.2,
      position = position_dodge(width = 0.75)
    ) +
    geom_quasirandom(
      data = data,
      aes(
        x = .data[[factor_b]],
        y = .data[[measure_col]],
        color = .data[[factor_a]]
      ),
      dodge.width = 0.75,
      alpha = 0.5,
      width = 0.2,
      size = 1.2,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = fill_palette) +
    scale_color_manual(values = point_palette) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      text = element_text(size = 18),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    labs(
      x = factor_b,
      y = paste0(measure_col, " (", summary_label, ")"),
      fill = factor_a
    )

  if (!is.null(cld_result) && nrow(cld_result) > 0) {
    p <- p +
      geom_text(
        data = plot_df,
        aes(x = .data[[factor_b]], y = label_y, label = cld, group = .data[[factor_a]]),
        position = position_dodge(width = 0.75),
        na.rm = TRUE,
        size = 5,
        inherit.aes = FALSE
      )
  }

  if (!is.null(facet_var)) {
    p <- p + facet_grid(
      as.formula(paste0("`", facet_var, "` ~ `", factor_c, "`")),
      scales = "free_y"
    )
  } else {
    p <- p + facet_wrap(as.formula(paste("~", factor_c)), nrow = 1, scales = "free_y")
  }

  return(list(
    plot = p,
    summary = summary_df,
    shapiro = shapiro_df,
    normality = flag_normal,
    anova3 = anova_result,
    posthoc = posthoc_result,
    cld = cld_result,
    analysis_data = analysis_data,
    model = "threeway_anova",
    method = analysis_method,
    decision_mode = decision_info$decision_mode,
    decision_reason = decision_info$decision_reason,
    decision_diagnostics = decision_info$diagnostics,
    message = if (length(warning_messages) > 0) paste(unique(warning_messages), collapse = " ") else NULL
  ))
}

# MANOVA analysis helper
#========================================================================================================================================
# STRATEGY: Multivariate response model with optional second factor and stratification
# PURPOSE: Test joint response of several parameters in one model

analyse_manova <- function(
  data,
  response_cols,
  factor_a,
  factor_b = NULL,
  facet_var = NULL
) {
  if (length(response_cols) < 2) {
    stop("MANOVA requires at least two response variables")
  }

  required_cols <- c(response_cols, factor_a)
  if (!is.null(factor_b) && factor_b != "") {
    required_cols <- c(required_cols, factor_b)
  }
  if (!is.null(facet_var) && facet_var != "") {
    required_cols <- c(required_cols, facet_var)
  }

  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  for (col in response_cols) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
  }

  data[[factor_a]] <- as.factor(data[[factor_a]])
  if (!is.null(factor_b) && factor_b != "") {
    data[[factor_b]] <- as.factor(data[[factor_b]])
  }
  if (!is.null(facet_var) && facet_var != "") {
    data[[facet_var]] <- as.factor(data[[facet_var]])
  }

  response_term <- paste(sprintf("`%s`", response_cols), collapse = ", ")
  rhs_term <- if (!is.null(factor_b) && factor_b != "") {
    paste0("`", factor_a, "` * `", factor_b, "`")
  } else {
    paste0("`", factor_a, "`")
  }
  formula_mv <- stats::as.formula(paste0("cbind(", response_term, ") ~ ", rhs_term))

  run_manova <- function(df_sub, facet_value = NULL) {
    fit <- stats::manova(formula_mv, data = df_sub)
    sm <- summary(fit, test = "Pillai")
    sm_df <- as.data.frame(sm$stats)
    sm_df$Effect <- rownames(sm_df)
    rownames(sm_df) <- NULL
    if (!is.null(facet_value)) {
      sm_df[[facet_var]] <- facet_value
      sm_df <- sm_df %>% dplyr::relocate(all_of(facet_var))
    }
    sm_df
  }

  if (!is.null(facet_var) && facet_var != "") {
    facet_levels <- unique(as.character(data[[facet_var]]))
    manova_table <- dplyr::bind_rows(lapply(facet_levels, function(fv) {
      run_manova(data[as.character(data[[facet_var]]) == fv, , drop = FALSE], facet_value = fv)
    }))
  } else {
    manova_table <- run_manova(data)
  }

  primary_response <- response_cols[1]
  p <- ggplot(
    data,
    aes(
      x = .data[[factor_a]],
      y = .data[[primary_response]],
      fill = if (!is.null(factor_b) && factor_b != "") .data[[factor_b]] else .data[[factor_a]]
    )
  ) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_quasirandom(alpha = 0.5, width = 0.2, size = 1.2) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      text = element_text(size = 18),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    labs(
      x = factor_a,
      y = paste0(primary_response, " (display variable)"),
      fill = if (!is.null(factor_b) && factor_b != "") factor_b else factor_a
    )

  if (!is.null(facet_var) && facet_var != "") {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), nrow = 1, scales = "free_y")
  }

  return(list(
    plot = p,
    manova = manova_table,
    responses = response_cols,
    normality = NA,
    shapiro = NULL,
    model = "manova"
  ))
}

# Convert bar plot data to a smoothed curve-style plot
#========================================================================================================================================
# STRATEGY: Build a smooth line+ribbon view from grouped bar plot data
# PURPOSE: Match the visual language of curve analysis when toggling from bar plots
# INPUT: Raw wide data plus the x/facet/value columns selected in the bar plot UI
# OUTPUT: List with plot object and intermediate summary/smoothed data

build_converted_curve_plot <- function(data, x_col, facet_col, value_col, x_order = NULL, colors = NULL) {
  required_cols <- c(x_col, facet_col, value_col)
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  plot_data <- data[, required_cols]
  x_values <- plot_data[[x_col]]
  x_numeric <- suppressWarnings(as.numeric(as.character(x_values)))
  x_breaks <- NULL

  extract_first_numeric <- function(x) {
    x_chr <- trimws(as.character(x))
    out <- rep(NA_real_, length(x_chr))
    has_match <- !is.na(x_chr) & grepl("-?[0-9]+(?:\\.[0-9]+)?", x_chr, perl = TRUE)
    if (any(has_match)) {
      extracted <- sub(".*?(-?[0-9]+(?:\\.[0-9]+)?).*", "\\1", x_chr[has_match], perl = TRUE)
      out[has_match] <- suppressWarnings(as.numeric(extracted))
    }
    out
  }

  if (all(is.na(x_numeric))) {
    x_levels_raw <- if (is.null(x_order) || length(x_order) == 0) {
      unique(as.character(x_values))
    } else {
      as.character(x_order)
    }

    parsed_levels <- extract_first_numeric(x_levels_raw)
    parsed_ok <- all(!is.na(parsed_levels)) && length(unique(parsed_levels)) == length(parsed_levels)

    if (parsed_ok) {
      level_order_idx <- order(parsed_levels)
      x_labels <- x_levels_raw[level_order_idx]
      x_breaks <- parsed_levels[level_order_idx]
      x_numeric <- parsed_levels[match(as.character(x_values), x_levels_raw)]
      use_labels <- TRUE
    } else {
      x_numeric <- as.numeric(factor(x_values, levels = x_levels_raw))
      x_labels <- x_levels_raw
      x_breaks <- seq_along(x_labels)
      use_labels <- TRUE
    }
  } else {
    x_labels <- NULL
    x_breaks <- NULL
    use_labels <- FALSE
  }

  plot_data$x_numeric <- x_numeric

  mean_data <- aggregate(
    plot_data[[value_col]],
    by = list(facet = plot_data[[facet_col]], x = plot_data$x_numeric),
    FUN = mean,
    na.rm = TRUE
  )
  names(mean_data)[3] <- "mean_value"

  se_calc <- function(x) {
    valid_n <- sum(!is.na(x))
    if (valid_n <= 1) {
      return(0)
    }
    stats::sd(x, na.rm = TRUE) / sqrt(valid_n)
  }

  se_data <- aggregate(
    plot_data[[value_col]],
    by = list(facet = plot_data[[facet_col]], x = plot_data$x_numeric),
    FUN = se_calc
  )
  names(se_data)[3] <- "se_value"

  summary_data <- merge(mean_data, se_data, by = c("facet", "x"), all = TRUE)
  summary_data <- summary_data[order(summary_data$facet, summary_data$x), ]
  summary_data$se_value[is.na(summary_data$se_value)] <- 0

  smooth_single_group <- function(group_df) {
    group_df <- group_df[order(group_df$x), ]
    facet_value <- as.character(group_df$facet[1])

    raw_group <- plot_data[as.character(plot_data[[facet_col]]) == facet_value, c("x_numeric", value_col)]
    names(raw_group) <- c("x", "y")
    raw_group <- raw_group[stats::complete.cases(raw_group), , drop = FALSE]

    unique_x <- sort(unique(raw_group$x))

    if (length(unique_x) == 1) {
      return(data.frame(
        facet = facet_value,
        x = unique_x,
        fit = group_df$mean_value[1],
        lwr = pmax(0, group_df$mean_value[1] - group_df$se_value[1]),
        upr = group_df$mean_value[1] + group_df$se_value[1],
        stringsAsFactors = FALSE
      ))
    }

    x_grid <- seq(min(unique_x), max(unique_x), length.out = max(100, length(unique_x) * 25))

    fit_values <- NULL
    se_values <- NULL

    if (length(unique_x) >= 3 && nrow(raw_group) >= 5) {
      k_eff <- min(5, length(unique_x) - 1)
      if (k_eff >= 3) {
        qgam_result <- tryCatch({
          model <- qgam::qgam(y ~ s(x, k = k_eff), data = raw_group, qu = 0.5)
          preds <- stats::predict(model, newdata = data.frame(x = x_grid), se.fit = TRUE)
          list(fit = as.numeric(preds$fit), se = pmax(0, as.numeric(preds$se.fit)))
        }, error = function(e) {
          NULL
        })

        if (!is.null(qgam_result)) {
          fit_values <- qgam_result$fit
          se_values <- qgam_result$se
        }
      }
    }

    if (is.null(fit_values) || is.null(se_values)) {
      if (length(unique(group_df$x)) >= 4) {
        fit_model <- stats::smooth.spline(group_df$x, group_df$mean_value, spar = 0.6)
        se_model <- stats::smooth.spline(group_df$x, group_df$se_value, spar = 0.6)
        fit_values <- stats::predict(fit_model, x = x_grid)$y
        se_values <- pmax(0, stats::predict(se_model, x = x_grid)$y)
      } else {
        fit_values <- stats::spline(group_df$x, group_df$mean_value, xout = x_grid, method = "natural")$y
        se_values <- pmax(0, stats::spline(group_df$x, group_df$se_value, xout = x_grid, method = "natural")$y)
      }
    }

    data.frame(
      facet = facet_value,
      x = x_grid,
      fit = fit_values,
      lwr = pmax(0, fit_values - se_values),
      upr = fit_values + se_values,
      stringsAsFactors = FALSE
    )
  }

  smoothed_data <- do.call(
    rbind,
    lapply(split(summary_data, summary_data$facet), smooth_single_group)
  )

  facet_levels <- unique(summary_data$facet)
  if (is.null(colors) || length(colors) < length(facet_levels)) {
    colors <- scales::hue_pal()(length(facet_levels))
  }
  color_mapping <- setNames(colors[seq_along(facet_levels)], facet_levels)

  converted_plot <- ggplot() +
    geom_ribbon(
      data = smoothed_data,
      aes(x = x, ymin = lwr, ymax = upr, fill = facet, group = facet),
      alpha = 0.3,
      linetype = 0
    ) +
    geom_line(
      data = smoothed_data,
      aes(x = x, y = fit, color = facet, group = facet),
      linewidth = 1
    ) +
    geom_point(
      data = summary_data,
      aes(x = x, y = mean_value, color = facet, group = facet),
      size = 1.5,
      alpha = 0.7
    ) +
    scale_fill_manual(values = color_mapping) +
    scale_colour_manual(values = color_mapping) +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      text = element_text(size = 18)
    ) +
    labs(
      x = x_col,
      y = value_col,
      color = facet_col,
      fill = facet_col,
      title = paste("Curve representation of", value_col)
    )

  if (use_labels) {
    converted_plot <- converted_plot +
      scale_x_continuous(breaks = x_breaks, labels = x_labels)
  }

  return(list(
    plot = converted_plot,
    summary_data = summary_data,
    smoothed_data = smoothed_data
  ))
}

# Plot curve qGAM version
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
  # STRATEGY: Centralized package management
  # PURPOSE: Avoid redundant library calls and version conflicts

  # ===========================================
  # AXIS LABEL PREPARATION SECTION
  # ===========================================
  # STRATEGY: Dynamic axis labeling based on user parameters
  # PURPOSE: Informative, context-specific axis labels
  
  # X-AXIS LABEL WITH UNITS
  # STRATEGY: Include user-specified time units in axis label
  # PURPOSE: Clear indication of time scale being analyzed
  x_axis_label <- if (!is.null(user_params$unit)) {
    user_params$unit
  } else {
    ""
  }

  # Y-AXIS LABEL FROM PARAMETER NAME
  # STRATEGY: Extract root parameter name from selected columns
  # PURPOSE: Clean parameter name without time point suffixes
  y_axis_label <- if (!is.null(user_params$selected_params) && length(user_params$selected_params) > 0) {
    param_name <- gsub("_.*", "", user_params$selected_params[1])
    param_name
  } else {
    parameter_col
  }

  compute_effective_k <- function(n_time, k_requested) {
    if (is.na(n_time) || n_time < 3) {
      return(NA_integer_)
    }
    # mgcv/qgam spline basis needs k >= 3; clamp user k to data-compatible range.
    k_eff <- min(as.integer(k_requested), n_time)
    as.integer(max(3L, k_eff))
  }

  fit_qgam_safely <- function(build_formula, data, k_start, qu = 0.5) {
    k_candidates <- seq.int(as.integer(k_start), 3L, by = -1L)
    final_warning <- NULL

    for (k_try in k_candidates) {
      warning_messages <- character(0)
      model_fit <- withCallingHandlers(
        qgam::qgam(
          build_formula(k_try),
          data = data,
          qu = qu
        ),
        warning = function(w) {
          warning_messages <<- c(warning_messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )

      step_failure <- any(grepl("step failure", warning_messages, ignore.case = TRUE))
      if (!step_failure || k_try <= 3L) {
        final_warning <- if (length(warning_messages) > 0) paste(unique(warning_messages), collapse = " | ") else NULL
        return(list(model = model_fit, k_used = k_try, warning = final_warning))
      }
    }

    NULL
  }
  
  # ===========================================
  # INNER FUNCTION: DATA VALIDATION
  # ===========================================
  # STRATEGY: Comprehensive data validation and preparation
  # PURPOSE: Ensure data quality before expensive modeling

  validate_and_prepare_data <- function(df, parameter_col, time_col, grouping_col, facet_col) {
    required_cols <- c(parameter_col, time_col, grouping_col, facet_col)
    missing_cols <- setdiff(required_cols, colnames(df))
    if (length(missing_cols) > 0) {
      stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    }
    
    df[[grouping_col]] <- as.factor(df[[grouping_col]])
    df[[facet_col]] <- as.factor(df[[facet_col]])
    df[[time_col]] <- suppressWarnings(as.numeric(as.character(df[[time_col]])))
    df[[parameter_col]] <- suppressWarnings(as.numeric(df[[parameter_col]]))
    
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
  
  # ===========================================
  # INNER FUNCTION: qGAM MODEL FITTING
  # ===========================================
  # STRATEGY: Group-wise qGAM fitting with prediction generation
  # PURPOSE: Generate smooth trend lines with confidence intervals

  fit_qgam_models <- function(df, grouping_col, facet_col, time_col, parameter_col, k) {
    time_range <- range(df[[time_col]], na.rm = TRUE)
    time_grid <- seq(time_range[1], time_range[2], length.out = 200)
    
    qgam_preds <- df %>%
      group_by(dplyr::across(all_of(c(facet_col, grouping_col)))) %>%
      group_modify(~{
        tryCatch({
          n_time <- length(unique(.x[[time_col]]))
          if (n_time < 3) {
            warning(paste(
              "Skipped qGAM fit for",
              unique(.x[[facet_col]])[1], "-", unique(.x[[grouping_col]])[1],
              ": fewer than 3 distinct time points."
            ))
            return(data.frame())
          }

          k_eff <- compute_effective_k(n_time, k)

          fit_res <- fit_qgam_safely(
            build_formula = function(k_value) {
              as.formula(paste0(parameter_col, " ~ s(", time_col, ", k=", k_value, ")"))
            },
            data = .x,
            k_start = k_eff,
            qu = 0.5
          )
          if (is.null(fit_res) || is.null(fit_res$model)) {
            return(data.frame())
          }
          mod <- fit_res$model
          
          newdat <- data.frame(setNames(list(time_grid), time_col))
          newdat[[grouping_col]] <- unique(.x[[grouping_col]])[1]
          newdat[[facet_col]] <- unique(.x[[facet_col]])[1]
          newdat$k_used <- fit_res$k_used
          
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
  
  # ===========================================
  # INNER FUNCTION: STATISTICAL TESTING
  # ===========================================
  # STRATEGY: Group-vs-control comparisons using qGAM
  # PURPOSE: Statistical inference about group differences

  perform_statistical_tests <- function(df,
                                    grouping_col,
                                    facet_col,
                                    time_col,
                                    parameter_col,
                                    control_group,
                                    k = 5) {
  # STEP 1: GET ALL UNIQUE FACETS
  # STRATEGY: Process each experimental condition separately
  # PURPOSE: Separate statistical testing for each facet level
  facets <- unique(df[[facet_col]])

  # STEP 2: INITIALIZE RESULTS DATAFRAME
  # STRATEGY: Pre-define result structure for consistent output
  # PURPOSE: Ensure consistent column names and types
  results <- data.frame(
    facet = character(),
    group = character(),
    p.value = numeric(),
    stringsAsFactors = FALSE
  )

  # STEP 3: LOOP THROUGH FACETS
  # STRATEGY: Separate analysis for each experimental condition
  # PURPOSE: Independent statistical testing within each condition
  for (current_facet in facets) {
    facet_data <- df[df[[facet_col]] == current_facet,]
    groups_to_compare <- setdiff(levels(factor(facet_data[[grouping_col]])),
                                              control_group)

    # STEP 4: PROCESS EACH GROUP VS CONTROL
    # STRATEGY: Pairwise comparisons against control group
    # PURPOSE: Identify which groups differ significantly from control
    for (group in groups_to_compare) {
      # CREATE SUBSET WITH JUST CONTROL AND TEST GROUP
      # STRATEGY: Two-group comparison for focused testing
      # PURPOSE: Simplify model and improve statistical power
      df_sub <- facet_data[facet_data[[grouping_col]] %in%
                          c(control_group, group),]
      df_sub[[grouping_col]] <- droplevels(factor(df_sub[[grouping_col]]))

      # CREATE RESULT ROW FOR THIS TEST
      # STRATEGY: Pre-allocate result structure with NA p-value
      # PURPOSE: Consistent output even when tests fail
      result_row <- data.frame(
        facet = current_facet,
        group = group,
        p.value = NA,
        stringsAsFactors = FALSE
      )

      tryCatch({
        n_time <- length(unique(df_sub[[time_col]]))
        if (n_time < 3) {
          warning(paste("Statistical test skipped for", group, "in", current_facet,
                        ": fewer than 3 distinct time points."))
          next
        }

        k_eff <- compute_effective_k(n_time, k)

        fit_res <- fit_qgam_safely(
          build_formula = function(k_value) {
            as.formula(paste0(
              parameter_col,
              " ~ s(", time_col, ", by = ", grouping_col, ", k=", k_value, ") + ",
              grouping_col
            ))
          },
          data = df_sub,
          k_start = k_eff,
          qu = 0.5
        )
        if (is.null(fit_res) || is.null(fit_res$model)) {
          next
        }

        # COMPARATIVE qGAM MODEL
        # STRATEGY: Single model with group-specific smooths
        # PURPOSE: Test whether smooth curves differ between groups
        m1 <- fit_res$model

        # EXTRACT P-VALUE FROM MODEL SUMMARY
        # STRATEGY: Extract smooth term p-value for group difference
        # PURPOSE: Statistical test of whether curves differ significantly
        s <- summary(m1)
        if (length(s$s.pv) >= 2) {
          result_row$p.value <- s$s.pv[2]  # Second smooth term p-value
        }

        # ADD SUCCESSFUL RESULT TO DATAFRAME
        # STRATEGY: Accumulate successful test results
        # PURPOSE: Build complete results dataframe
        results <- rbind(results, result_row)

      }, error = function(e) {
        # ERROR HANDLING FOR INDIVIDUAL TESTS
        # STRATEGY: Warning instead of stopping entire analysis
        # PURPOSE: Partial results are better than no results
        warning(paste("Statistical test failed for", group, "in", current_facet, ":", e$message))
      })
    }
  }
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

    # Compute per-curve effective k for transparency and reporting.
    curve_k_table <- df_clean %>%
      dplyr::group_by(dplyr::across(all_of(c(facet_col, grouping_col)))) %>%
      dplyr::summarise(
        n_time = dplyr::n_distinct(.data[[time_col]]),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        k_requested = as.integer(k),
        k_effective = vapply(n_time, compute_effective_k, integer(1), k_requested = k)
      )

    k_used_values <- curve_k_table$k_effective[!is.na(curve_k_table$k_effective)]
    n_time_values <- curve_k_table$n_time[!is.na(curve_k_table$n_time)]
    k_summary <- list(
      requested = as.integer(k),
      used_min = if (length(k_used_values) > 0) min(k_used_values) else NA_integer_,
      used_max = if (length(k_used_values) > 0) max(k_used_values) else NA_integer_,
      skipped_curves = sum(is.na(curve_k_table$k_effective)),
      n_time_min = if (length(n_time_values) > 0) min(n_time_values) else NA_integer_,
      n_time_max = if (length(n_time_values) > 0) max(n_time_values) else NA_integer_
    )
    
    # STEP 2: qGAM MODEL FITTING
    # STRATEGY: Fit smooth curves to each group
    # PURPOSE: Generate trend lines and confidence intervals
    qgam_preds <- fit_qgam_models(df_clean, grouping_col, facet_col, time_col, parameter_col, k)

    if (nrow(qgam_preds) > 0 && "k_used" %in% names(qgam_preds)) {
      used_k_table <- qgam_preds %>%
        dplyr::distinct(dplyr::across(all_of(c(facet_col, grouping_col))), k_used)

      curve_k_table <- curve_k_table %>%
        dplyr::left_join(used_k_table, by = c(facet_col, grouping_col)) %>%
        dplyr::mutate(k_effective = dplyr::if_else(!is.na(k_used), as.integer(k_used), k_effective)) %>%
        dplyr::select(-k_used)
    }
    
    # STEP 3: STATISTICAL TESTING
    # STRATEGY: Test for significant differences from control
    # PURPOSE: Statistical inference about treatment effects
    stat_results <- perform_statistical_tests(df_clean, grouping_col, facet_col, time_col, parameter_col, control_group, k = k)
    
    # STEP 4: MEDIAN POINT CALCULATION
    # STRATEGY: Calculate median values at each time point for overlay
    # PURPOSE: Show actual data points on smooth curves
    median_points <- aggregate(
      df_clean[[parameter_col]],
      by = list(
        facet = df_clean[[facet_col]],
        group = df_clean[[grouping_col]],
        time = df_clean[[time_col]]
      ),
      FUN = function(x) median(x, na.rm = TRUE)
    )
    names(median_points) <- c(facet_col, grouping_col, time_col, "median_value")
    
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
        aes(
          x = .data[[time_col]],
          ymin = .data[["lwr"]],
          ymax = .data[["upr"]],
          fill = .data[[grouping_col]]
        ),
        alpha = 0.3,      # Semi-transparent for overlay effect
        linetype = 0      # No border lines on ribbon
      ) +

      # SMOOTH CURVE LAYER
      # STRATEGY: Main trend lines from qGAM predictions
      # PURPOSE: Show fitted curves for each group
      geom_line(
        data = qgam_preds,
        aes(
          x = .data[[time_col]],
          y = .data[["fit"]],
          color = .data[[grouping_col]]
        ),
        linewidth = 1
      ) +

      # MEDIAN POINTS OVERLAY
      # STRATEGY: Show actual data summaries on fitted curves
      # PURPOSE: Connection between model and observed data
      geom_point(
        data = median_points,
        aes(
          x = .data[[time_col]],
          y = .data[["median_value"]],
          color = .data[[grouping_col]]
        ),
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
        text = element_text(size = 18),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "plain", size = 20, color = "black", hjust = 0.5)
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
          size = 6, fontface = "bold",
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
      median_points = median_points,     # Summary data points
      analysis_data = df_clean,
      k_summary = k_summary,
      k_details = curve_k_table
    ))
    
  }, error = function(e) {
    # GLOBAL ERROR HANDLING
    # STRATEGY: Informative error message for debugging
    # PURPOSE: Help users identify and fix problems
    stop(paste("Error in qGAM curve analysis:", e$message))
  })
}