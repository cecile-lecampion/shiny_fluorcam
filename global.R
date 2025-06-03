# Load the necessary packages for the Shiny app Fluorcam

required_packages <- c("shiny", "shinyFiles", "fs", "tidyr", "data.table", "sortable", "colourpicker", 
                       "ggbeeswarm", "RColorBrewer", "rstatix", "rcompanion", "forcats", "Rmisc", 
                       "plyr", "multcompView", "ggplot2", "dplyr", "zip")

for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}