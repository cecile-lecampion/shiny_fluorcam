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

cat("Running regression checks...\n")

# Regression 1: CLD generation must preserve hyphenated labels.
tukey_df <- data.frame(
  Treatment = "T0",
  group1 = "WT-1",
  group2 = "Mut-1",
  p.adj = 0.01,
  stringsAsFactors = FALSE
)

cld_result <- generate_cld_parametric(tukey_df, "Treatment", "Genotype")
stopifnot(
  is.data.frame(cld_result),
  identical(sort(cld_result$Genotype), sort(c("WT-1", "Mut-1")))
)

# Regression 2: Sample FluorCam data must support Fv_Fm in bar plot analysis.
sample_df <- process_data_files(
  pattern = "TXT$",
  var_names = c("Day", "Line", "Plant"),
  dirpath = file.path(repo_root, "sample_data", "fluorcam_PSI")
)

barplot_result <- analyse_barplot(
  data = sample_df,
  var1 = "Day",
  var2 = "Line",
  measure_col = "Fv_Fm",
  var1_order = unique(sample_df$Day),
  var2_order = unique(sample_df$Line)
)

stopifnot(
  is.list(barplot_result),
  isTRUE("Fv_Fm" %in% names(sample_df)),
  !is.null(barplot_result$plot),
  !is.null(barplot_result$normality)
)

# Regression 2b: Bar plot conversion should build a smoothed curve representation.
converted_curve <- build_converted_curve_plot(
  data = sample_df,
  x_col = "Day",
  facet_col = "Line",
  value_col = "Fv_Fm",
  x_order = unique(sample_df$Day)
)

stopifnot(
  is.list(converted_curve),
  !is.null(converted_curve$plot),
  nrow(converted_curve$smoothed_data) > nrow(converted_curve$summary_data)
)

# Regression 3: Non-significant non-parametric branch must still return a list.
nonparam_df <- data.frame(
  Day = rep(c("T0", "T24h"), each = 8),
  Line = rep(rep(c("WT1", "H1"), each = 4), 2),
  Fv_Fm = c(0, 0, 0, 100, 0, 0, 0, 100, 0, 0, 0, 100, 0, 0, 0, 100)
)

nonparam_result <- analyse_barplot(
  data = nonparam_df,
  var1 = "Day",
  var2 = "Line",
  measure_col = "Fv_Fm"
)

stopifnot(
  is.list(nonparam_result),
  identical(nonparam_result$normality, FALSE),
  is.null(nonparam_result$dunn),
  !is.null(nonparam_result$plot),
  identical(
    nonparam_result$message,
    "Data are not significantly different, the Dunn test was not performed."
  )
)

# Regression 4: Two-way ANOVA helper should return model output and plot.
twoway_result <- analyse_barplot_twoway(
  data = sample_df,
  factor_a = "Day",
  factor_b = "Line",
  measure_col = "Fv_Fm",
  facet_var = NULL
)

stopifnot(
  is.list(twoway_result),
  identical(twoway_result$model, "twoway_anova"),
  !is.null(twoway_result$anova2),
  nrow(twoway_result$anova2) > 0,
  !is.null(twoway_result$plot)
)

# Regression 5: Factorial sample data should support two-way + stratification.
factorial_df <- process_data_files(
  pattern = "TXT$",
  var_names = c("UV", "Light", "Temp", "Rep"),
  dirpath = file.path(repo_root, "sample_data", "fluorcam_factorial")
)

twoway_facet_result <- analyse_barplot_twoway(
  data = factorial_df,
  factor_a = "UV",
  factor_b = "Light",
  measure_col = "Fm",
  facet_var = "Temp"
)

stopifnot(
  is.list(twoway_facet_result),
  identical(twoway_facet_result$model, "twoway_anova"),
  !is.null(twoway_facet_result$anova2),
  nrow(twoway_facet_result$anova2) > 0,
  !is.null(twoway_facet_result$plot)
)

if (requireNamespace("emmeans", quietly = TRUE)) {
  stopifnot(
    !is.null(twoway_facet_result$posthoc),
    nrow(twoway_facet_result$posthoc) > 0
  )
}

# Regression 6: Three-way ANOVA helper should return model output and plot.
threeway_df <- factorial_df

threeway_result <- analyse_barplot_threeway(
  data = threeway_df,
  factor_a = "UV",
  factor_b = "Light",
  factor_c = "Temp",
  measure_col = "Fm"
)

stopifnot(
  is.list(threeway_result),
  identical(threeway_result$model, "threeway_anova"),
  !is.null(threeway_result$anova3),
  nrow(threeway_result$anova3) > 0,
  !is.null(threeway_result$plot)
)

if (requireNamespace("emmeans", quietly = TRUE)) {
  stopifnot(
    !is.null(threeway_result$posthoc),
    nrow(threeway_result$posthoc) > 0
  )
}

# Regression 7: Three-way ANOVA with facet_var (stratification) should run per-facet.
threeway_facet_result <- analyse_barplot_threeway(
  data = factorial_df,
  factor_a = "UV",
  factor_b = "Light",
  factor_c = "Temp",
  measure_col = "Fm",
  facet_var = "Rep"
)

stopifnot(
  is.list(threeway_facet_result),
  identical(threeway_facet_result$model, "threeway_anova"),
  !is.null(threeway_facet_result$anova3),
  nrow(threeway_facet_result$anova3) > 0,
  "Rep" %in% names(threeway_facet_result$anova3),
  !is.null(threeway_facet_result$plot)
)

if (requireNamespace("emmeans", quietly = TRUE)) {
  stopifnot(
    !is.null(threeway_facet_result$posthoc),
    nrow(threeway_facet_result$posthoc) > 0,
    "Rep" %in% names(threeway_facet_result$posthoc)
  )
}

cat("All regression checks passed.\n")