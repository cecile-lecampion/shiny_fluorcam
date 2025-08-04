# Load the necessary packages for the Shiny app Fluorcam

required_packages <- c("shiny", "shinyFiles", "fs", "tidyr", "data.table", "sortable", "colourpicker", 
                       "ggbeeswarm", "RColorBrewer", "rstatix", "rcompanion", "forcats", "Rmisc", 
                       "plyr", "multcompView", "ggplot2", "dplyr", "zip", "shinydashboard", "DT",  "readr", 
                       "ggpubr", "sortable", "shinyWidgets", "shinycssloaders","htmltools", "shinycssloaders")


# Identifier ceux qui ne sont pas déjà installés dans ce répertoire
not_installed_packages <- setdiff(required_packages, installed.packages()[,"Package"])

if (length(not_installed_packages) > 0) {
  install.packages(not_installed_packages)
}

# Charger tous les packages
lapply(required_packages, library, character.only = TRUE)

# for(pkg in required_packages){
#   if(!require(pkg, character.only = TRUE)){
#     install.packages(pkg, dependencies = TRUE)
#     library(pkg, character.only = TRUE)
#   }
# }