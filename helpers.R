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

# Function to process data files
#========================================================================================================================================

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