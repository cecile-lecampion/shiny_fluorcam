########################################################################################################################################
# Define the UI
########################################################################################################################################
source("global.R",  local = TRUE) # Load the global variables and packages

ui <- fluidPage(
  titlePanel("Fluorcam data analysis tool box"),    # Title of the application
  
  sidebarLayout(                                    # Define the layout of the sidebar
    sidebarPanel(
      tags$p(
        style = "font-size: 120%; color: blue;",    # Define the style of the text
        tags$strong("1- Load data:")
      ),
      shinyDirButton("dir", "Directory selection", "Select Directory"), # Button to select the directory
      verbatimTextOutput("dirpath"),                                    # Display the selected directory
      textInput("pattern", "Select files pattern", value = ".TXT"),     # Define the pattern of the files to load
      actionButton("show_all", "Show Files : All/Short list"),          # Button to show all files or only the first 5
      #textInput("areas", "Define names of areas", value = "Area 1,Area 2,Area 3,Area 4"), # Define the names of the areas
      tags$hr(),                                                        # Add a horizontal line
      tags$p(tags$strong("Collect informations about sample in file name :"),             # Add a paragraph with a strong text
             tags$br(),
             "Your file name must respect the pattern : VAR1_VAR2_VAR3.TXT",
             tags$br(),
             "Define correspondance to variable :"),
      textInput("var1", "Name of VAR1", value = "Day"),              # Define the name of VAR1
      textInput("var2", "Name of VAR2", value = "Line"),             # Define the name of VAR2
      textInput("var3", "Name of VAR3", value = "PlantID"),          # Define the name of VAR3
      actionButton("load", "Load data"),
      tags$p(
        style = "font-size: 120%; margin-top: 20px; color: blue;",
        tags$strong("2- Graph and statistical analysis parameters :") # Add a paragraph with a strong text
      ),
      selectInput("graph_type", "Select the type of graph", 
                  choices = c("Bar plot", "Curve")), # Define the type of graph
      uiOutput("rootSelect"),                                   # Display the root selection
      uiOutput("columnSelect"),                                            # Display the column selection
      uiOutput("editParamsBtn"), # bouton affiché seulement pour Curve
      selectInput("facet_var", "Select the variable to facet by", choices = c("var1", "var2")),
      tags$p(tags$strong("Define order of the lines and of the facets : \n"), 
             "Drag the lines to change the order.",
             uiOutput("var2_order_ui"), 
             uiOutput("var1_order_ui")),
     
     # Define the dynamic color inputs
      tags$p(tags$strong("Select colors for your graph:")),
      uiOutput("dynamic_color_inputs"),
      tags$hr(),                                                          # Add a horizontal line
      actionButton("start_analysis", "Start Analysis"),                    # Button to start the analysis
      tags$hr(),                                                          # Add a horizontal line
      tags$p(tags$strong("Dowload statistic test table :")), 
      downloadButton("download_parametric", "Download All Analysis Results for parametric data"), # Button to download the results for parametric data
      downloadButton("download_non_parametric", "Download All Analysis Results for non-parametric data"), # Button to download the results for non-parametric data
      
      # Add a download button and format selection input
      tags$hr(),                                                        # Add a horizontal line
      tags$p(tags$strong("Save the plot :")),
      selectInput("file_format", "Choose file format", choices = c("svg", "png", "pdf")),
      downloadButton("save_plot", "Save Plot")
      
    ),
    
    # Define the main panel   
    mainPanel( 
      tableOutput("selected_files"),          # Display the selected files
      tags$hr(),
      tableOutput("processed_data"),          # Display the processed data
      uiOutput("toggle_button"),              # Display the toggle button to controle the visible part of the generated table
      tags$hr(),
      textOutput("selectedValue"),            # Display the selected value, the one that will be analysed
      verbatimTextOutput("normality_result"), # Display the normality result
      plotOutput("plot_result")               # Display the plot result
    )
  )
)