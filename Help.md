# FluorCam Data Analysis Toolbox - User Guide

## Overview

The **FluorCam Data Analysis Toolbox** is a user-friendly Shiny application designed to analyze Fv/Fm measurements from FluorCam .TXT files. This tool allows researchers to process, visualize, and statistically analyze chlorophyll fluorescence data without requiring R programming knowledge.

---

## Getting Started

### 1. File Name Configuration

**Purpose**: Define how your FluorCam files are named to enable automatic data processing.

**Required File Naming Pattern**: `VAR1_VAR2_VAR3.TXT`

**Example**: `Day1_LineA_Plant001.TXT`

**Steps**:
1. Define what each variable represents:
   - **VAR1**: Usually time points (e.g., Day, Week, Hour)
   - **VAR2**: Usually treatments or lines (e.g., Line, Treatment, Genotype)
   - **VAR3**: Usually individual identifiers (e.g., PlantID, Sample, Replicate)

2. Enter descriptive names for each variable in the text boxes
3. The preview will show your expected filename format

**Important**: All your .TXT files must follow this exact naming pattern for the analysis to work correctly.

---

### 2. Load Data

**Purpose**: Import your FluorCam .TXT files for analysis.

**Steps**:
1. **Select Directory**: Click "Select Directory" to choose the folder containing your .TXT files
2. **File Pattern**: Specify the file extension (default: `.TXT`)
3. **Preview Files**: Click "Toggle File List" to see which files will be loaded
4. **Load Data**: Click "Load Data" to import all files matching your pattern

**What happens**: The application will:
- Scan your directory for files matching the pattern
- Parse filenames according to your variable configuration
- Load and combine all data into a single dataset
- Display a preview of your data in the "Data Overview" tab

---

### 3. Analysis Parameters

**Purpose**: Configure how your data will be analyzed and visualized.

**Graph Type**:
- **Bar Plot**: Shows mean values with error bars (ideal for comparing groups)
- **Line Chart**: Shows trends over time or treatments (ideal for time series)

**Root and Column Selection**:
- **Root**: Choose the measurement parameter (usually Fv/Fm)
- **Column**: Select specific measurement columns from your data

**Facet Variable**: 
- Choose how to split your data into separate panels
- **VAR1**: Creates panels based on your first variable (e.g., time points)
- **VAR2**: Creates panels based on your second variable (e.g., treatments)

**Customization Options**:
- **Drag to reorder**: Arrange the order of categories in your plots
- **Select colors**: Choose custom colors for different groups
- **Color pickers**: Available for fill, line, and point colors

**Start Analysis**: Click to generate your plot and statistical analysis

---

### 4. Export Results

**Purpose**: Save your analysis results and plots for publication or further analysis.

**Statistical Results**:
- **Parametric Analysis**: Downloads results assuming normal distribution
- **Non-parametric Analysis**: Downloads results for non-normal data

**Save Plot**:
- **Format Options**:
  - **SVG (Vector)**: Best for publications, infinitely scalable
  - **PNG (High-res)**: Good for presentations, 300 DPI resolution
  - **PDF (Print)**: Perfect for printing and publications
  
- **Units**: Choose between centimeters (scientific standard) or inches
- **Width**: Set the plot width (affects height proportionally)

---

## Understanding Your Results

### Data Overview Tab
- **File List**: Shows all loaded files and their status
- **Data Preview**: Displays your combined dataset with parsed variables

### Analysis Results Tab
- **Selected Variable**: Shows which measurement you're analyzing
- **Normality Test**: Indicates if your data follows a normal distribution
  - "Data follow a normal law" → Use parametric statistics
  - "Data don't follow a normal law" → Use non-parametric statistics
- **Plot**: Your publication-ready visualization

---

## Best Practices

### File Organization
- Keep all .TXT files in a single directory
- Use consistent naming patterns across all files
- Avoid special characters in filenames (use only letters, numbers, and underscores)

### Data Quality
- Ensure your FluorCam measurements are complete
- Check for outliers in the Data Overview before analysis
- Verify that variable parsing is correct by checking the data preview

### Statistical Analysis
- Always check the normality test results
- Use appropriate statistical tests based on your data distribution
- Consider biological replicates vs. technical replicates in your experimental design

### Publication-Ready Outputs
- Use SVG format for vector graphics in publications
- Set appropriate plot dimensions (typically 15-20 cm width for journals)
- Save both statistical results and plots for complete documentation

---

## Troubleshooting

### Common Issues

**"No files found"**
- Check that your files have the correct extension (.TXT)
- Ensure files follow the VAR1_VAR2_VAR3.TXT naming pattern
- Verify the selected directory contains your data files

**"Error loading data"**
- Check for corrupted or incomplete .TXT files
- Ensure all files have the same data structure
- Verify that filenames don't contain special characters

**"Analysis not showing results"**
- Confirm data is loaded (check Data Overview tab)
- Ensure you've selected valid parameters
- Check that you clicked "Start Analysis"

**"Plot not displaying"**
- Verify your data contains the selected measurement column
- Check for missing values in your dataset
- Ensure you have at least 2 data points for visualization

### Getting Help

For technical issues or questions about the application:
1. Check that all file naming conventions are followed
2. Verify your data format matches FluorCam output
3. Ensure all required fields are filled before running analysis

---

## Citation

If you use this tool in your research, please cite:

**FluorCam Data Analysis Toolbox** (2025). Cécile Lecampion & Ben Field. Available at: [Your Repository URL]

---

## Technical Information

- **Built with**: R Shiny, shinydashboard
- **Compatible with**: FluorCam .TXT output files
- **Supported formats**: .TXT, .CSV data files
- **Export formats**: CSV (statistics), SVG/PNG/PDF (plots)
- **License**: MIT License

---

*Last updated: August 2025*

