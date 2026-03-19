required_packages <- c(
  "dplyr",
  "tidyr",
  "data.table",
  "ggplot2",
  "ggbeeswarm",
  "rstatix",
  "rcompanion",
  "multcompView"
)

invisible(lapply(required_packages, library, character.only = TRUE))

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
script_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
repo_root <- dirname(script_dir)

source(file.path(repo_root, "helpers.R"))

cat("Smoke test start\n")

# 1) One-way workflow using the enriched one-way demo set.
oneway_df <- process_data_files(
  pattern = "TXT$",
  var_names = c("Batch", "Treatment", "Rep"),
  dirpath = file.path(repo_root, "sample_data", "fluorcam_oneway_demo")
)
oneway_df$.AllData <- "AllData"

oneway_result <- analyse_barplot(
  data = oneway_df,
  var1 = ".AllData",
  var2 = "Treatment",
  measure_col = "Fv_Fm",
  var2_order = unique(oneway_df$Treatment),
  parametric_strategy = "welch"
)

stopifnot(
  is.list(oneway_result),
  !is.null(oneway_result$plot),
  !is.null(oneway_result$analysis_data)
)
cat("[OK] one-way\n")

# 2) Two-way workflow using the enriched two-way demo set.
twoway_df <- process_data_files(
  pattern = "TXT$",
  var_names = c("Genotype", "Dose", "Rep"),
  dirpath = file.path(repo_root, "sample_data", "fluorcam_twoway_demo")
)

twoway_result <- analyse_barplot_twoway(
  data = twoway_df,
  factor_a = "Genotype",
  factor_b = "Dose",
  measure_col = "Fv_Fm"
)

stopifnot(
  is.list(twoway_result),
  !is.null(twoway_result$plot),
  !is.null(twoway_result$analysis_data)
)
cat("[OK] two-way\n")

# 3) Three-way workflow using the enriched three-way demo set.
threeway_df <- process_data_files(
  pattern = "TXT$",
  var_names = c("UV", "Light", "Temp", "Rep"),
  dirpath = file.path(repo_root, "sample_data", "fluorcam_threeway_demo")
)

threeway_result <- analyse_barplot_threeway(
  data = threeway_df,
  factor_a = "UV",
  factor_b = "Light",
  factor_c = "Temp",
  measure_col = "Fv_Fm"
)

stopifnot(
  is.list(threeway_result),
  !is.null(threeway_result$plot),
  !is.null(threeway_result$analysis_data)
)
cat("[OK] three-way\n")

# 4) Convert-to-curve workflow with time-like labels.
convert_df <- process_data_files(
  pattern = "TXT$",
  var_names = c("Time", "Genotype", "Rep"),
  dirpath = file.path(repo_root, "sample_data", "fluorcam_convert_curve_demo")
)

convert_bar_result <- analyse_barplot(
  data = convert_df,
  var1 = "Genotype",
  var2 = "Time",
  measure_col = "Fv_Fm",
  var1_order = unique(convert_df$Genotype),
  var2_order = unique(convert_df$Time)
)

convert_curve_result <- build_converted_curve_plot(
  data = convert_df,
  x_col = "Time",
  facet_col = "Genotype",
  value_col = "Fv_Fm",
  x_order = unique(convert_df$Time)
)

stopifnot(
  is.list(convert_bar_result),
  !is.null(convert_bar_result$plot),
  is.list(convert_curve_result),
  !is.null(convert_curve_result$plot),
  nrow(convert_curve_result$smoothed_data) > nrow(convert_curve_result$summary_data)
)
cat("[OK] convert-to-curve\n")

cat("Smoke test done\n")
