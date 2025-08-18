# Load the necessary packages for the Shiny app Fluorcam

########################################################################################################################################
# Global Configuration and Package Management for FluorCam Data Analysis Toolbox
########################################################################################################################################
# STRATEGY: Centralized package management and global configuration
# - Systematic package loading with automatic installation
# - Audited dependency list with only actually used packages
# - Single source of truth for all package requirements

# ===========================================
# SECTION 1: AUDITED PACKAGE DEPENDENCIES
# ===========================================
# STRATEGY: Only include packages that are actually used in the codebase
# PURPOSE: Minimize dependencies, faster loading, cleaner deployment

required_packages <- c(
  # ===========================================
  # CORE SHINY FRAMEWORK (4 packages)
  # ===========================================
  "shiny",                    # Main Shiny framework - USED: ui.R, server.R
  "shinydashboard",           # Dashboard layout - USED: ui.R (dashboardPage, dashboardHeader, etc.)
  "shinyFiles",               # Directory selection - USED: server.R (shinyDirChoose, parseDirPath)
  "shinyWidgets",             # Enhanced widgets - USED: ui.R (colourInput, rank_list)
  
  # ===========================================
  # DATA MANIPULATION (5 packages)
  # ===========================================
  "dplyr",                    # Data manipulation - USED: helpers.R (group_by, filter, mutate, etc.)
  "tidyr",                    # Data reshaping - USED: helpers.R (separate, pivot_longer, unnest_longer)
  "data.table",               # High-performance operations - USED: helpers.R (transpose function)
  "forcats",                  # Factor handling - USED: server.R (factor operations)
  "stringr",                  # String manipulation - USED: server.R (str_detect in time mapping)
  
  # ===========================================
  # FILE SYSTEM OPERATIONS (2 packages)
  # ===========================================
  "fs",                       # File system operations - USED: server.R (fs::path_home)
  "zip",                      # Archive creation - USED: server.R (export functionality)
  
  # ===========================================
  # STATISTICAL ANALYSIS (4 packages)
  # ===========================================
  "rstatix",                  # Statistical tests - USED: helpers.R (shapiro_test, anova_test, tukey_hsd, etc.)
  "multcompView",             # Compact letter display - USED: helpers.R (multcompLetters)
  "Rmisc",                    # Summary statistics - USED: helpers.R (summarySE)
  
  # ===========================================
  # ADVANCED MODELING (1 package)
  # ===========================================
  "qgam",                     # Quantile GAM - USED: helpers.R (curve analysis)
  
  # ===========================================
  # VISUALIZATION (3 packages)
  # ===========================================
  "ggplot2",                  # Grammar of graphics - USED: helpers.R (all plotting)
  "ggbeeswarm",               # Bee swarm plots - USED: helpers.R (geom_quasirandom)
  "RColorBrewer",             # Color palettes - USED: helpers.R (brewer.pal fallback)
  
  # ===========================================
  # USER INTERFACE (3 packages)
  # ===========================================
  "colourpicker",             # Color selection - USED: ui.R (colourInput)
  "sortable",                 # Drag-and-drop - USED: ui.R (rank_list)
  "DT",                       # Interactive tables - USED: ui.R, server.R (datatable, renderDataTable)
  
  # ===========================================
  # UTILITY (1 package)
  # ===========================================
  "htmltools"                 # HTML utilities - USED: ui.R (tags$head, tags$style, etc.)
)

# PACKAGE COUNT SUMMARY: 23 packages (down from 28)
# REMOVED: readr, plyr, ggpubr, svglite, shinycssloaders (5 packages)

# ===========================================
# SECTION 2: ENHANCED INSTALLATION STRATEGY
# ===========================================
# STRATEGY: Robust package management with better error handling and feedback

cat("FluorCam Analysis Toolbox - Package Management\n")
cat("================================================\n")

# STEP 1: IDENTIFY MISSING PACKAGES
not_installed_packages <- setdiff(required_packages, installed.packages()[,"Package"])

if (length(not_installed_packages) > 0) {
  cat("Installing missing packages:", length(not_installed_packages), "packages\n")
  cat("Packages to install:", paste(not_installed_packages, collapse = ", "), "\n")
  cat("This may take a few minutes...\n\n")
  
  # ENHANCED INSTALLATION WITH ERROR HANDLING
  tryCatch({
    install.packages(not_installed_packages, dependencies = TRUE)
    cat("✓ Package installation completed successfully!\n\n")
  }, error = function(e) {
    cat("✗ Error during package installation:\n")
    cat(e$message, "\n")
    stop("Please resolve package installation issues before continuing.")
  })
} else {
  cat("✓ All required packages are already installed.\n\n")
}

# ===========================================
# SECTION 3: PACKAGE LOADING WITH VALIDATION
# ===========================================
# STRATEGY: Load packages with individual validation and clear feedback

cat("Loading packages...\n")

# INDIVIDUAL PACKAGE LOADING WITH ERROR HANDLING
failed_packages <- character(0)

for(pkg in required_packages) {
  result <- tryCatch({
    library(pkg, character.only = TRUE, quietly = TRUE)
    TRUE
  }, error = function(e) {
    failed_packages <<- c(failed_packages, pkg)
    FALSE
  })
  
  if(result) {
    cat("✓", pkg, "\n")
  } else {
    cat("✗", pkg, "(FAILED)\n")
  }
}

# FINAL VALIDATION
if(length(failed_packages) > 0) {
  cat("\n✗ Failed to load packages:", paste(failed_packages, collapse = ", "), "\n")
  stop("Please resolve package loading issues before continuing.")
} else {
  cat("\n✓ All packages loaded successfully!\n")
  cat("================================================\n")
}

# ===========================================
# SECTION 4: GLOBAL CONFIGURATION
# ===========================================
# STRATEGY: Set application-wide options for optimal performance

# INCREASE FILE UPLOAD LIMIT (for large datasets)
options(shiny.maxRequestSize = 100*1024^2)  # 100MB limit

# SET CONSISTENT LOCALE
tryCatch({
  Sys.setlocale("LC_NUMERIC", "C")  # Consistent number formatting
}, error = function(e) {
  warning("Could not set LC_NUMERIC locale")
})

# MEMORY OPTIMIZATION
gc()  # Garbage collection after package loading

cat("FluorCam Analysis Toolbox ready!\n")
cat("Total packages loaded:", length(required_packages), "\n")
cat("================================================\n\n")